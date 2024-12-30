const std = @import("std");
const MemoryPool = std.heap.MemoryPool;

const Atom = struct {
    value: []const u8,

    pub fn equals(this: Atom, other: Atom) bool {
        return std.mem.eql(u8, this.value, other.value);
    }
};
const Pair = struct {
    left: *const Sexpr,
    right: *const Sexpr,
};
const Sexpr = union(enum) {
    atom_var: Atom,
    atom_lit: Atom,
    pair: Pair,

    const @"return" = Sexpr.lit("return");
    const @"var" = Sexpr.lit("var");
    const atom = Sexpr.lit("atom");
    const nil = Sexpr.lit("nil");
    const identity = Sexpr.lit("identity");
    const @"eqAtoms?" = Sexpr.lit("eqAtoms?");
    const @"true" = Sexpr.lit("true");
    const @"false" = Sexpr.lit("false");

    pub fn lit(v: []const u8) Sexpr {
        return .{ .atom_lit = .{ .value = v } };
    }

    pub fn equals(this: *const Sexpr, other: *const Sexpr) bool {
        if (this == other) return true;
        return switch (this.*) {
            .atom_lit => |this_atom| switch (other.*) {
                .atom_lit => |other_atom| this_atom.equals(other_atom),
                else => false,
            },
            .atom_var => |this_atom| switch (other.*) {
                .atom_var => |other_atom| this_atom.equals(other_atom),
                else => false,
            },
            .pair => |this_pair| switch (other.*) {
                .pair => |other_pair| this_pair.left.equals(other_pair.left) and this_pair.right.equals(other_pair.right),
                else => false,
            },
        };
    }

    pub fn isLit(this: Sexpr) bool {
        return switch (this) {
            .atom_lit => true,
            else => false,
        };
    }

    pub fn fromBool(b: bool) *const Sexpr {
        return if (b) &Sexpr.true else &Sexpr.false;
    }
};

const Fnk = struct { name: *const Sexpr, body: MatchCases };
const MatchCases = std.ArrayListUnmanaged(MatchCaseDefinition);
const MatchCaseDefinition = struct {
    pattern: *const Sexpr,
    fn_name: *const Sexpr,
    template: *const Sexpr,
    next: ?MatchCases,
};

const Binding = struct {
    name: []const u8,
    value: *const Sexpr,
};
const Bindings = std.ArrayList(Binding);

const FnkCollection = std.ArrayHashMap(*const Sexpr, MatchCases, struct {
    pub fn hash(self: @This(), s: *const Sexpr) u32 {
        return switch (s.*) {
            .atom_lit => |a| std.array_hash_map.hashString(a.value),
            .atom_var => |a| std.hash.uint32(std.array_hash_map.hashString(a.value)),
            .pair => |p| {
                // return std.hash.uint32(hash(self, p.left.*)) ^ hash(self, p.right.*);
                var hasher = std.hash.Wyhash.init(0);
                std.hash.autoHash(&hasher, struct {
                    left: u32,
                    right: u32,
                }{ .left = hash(self, p.left), .right = hash(self, p.right) });
                return @truncate(hasher.final());
            },
        };
    }
    pub fn eql(self: @This(), a: *const Sexpr, b: *const Sexpr, b_index: usize) bool {
        _ = self;
        _ = b_index;
        return Sexpr.equals(a, b);
    }
}, true);

// Design decision 1: strings live on the input buffer
// Design decision 2: Sexprs are never released :(

const ExecutionState = struct {
    // only decorative, i guess
    cur_fn_name: *const Sexpr,

    cur_value: *const Sexpr,
    cur_cases: ?[]const MatchCaseDefinition,
    cur_bindings: Bindings,

    parent: ?*ExecutionState,

    pub fn stateAfterEnteringFnk(game: *PermamentGameStuff, fn_name: *const Sexpr, input: *const Sexpr) !*ExecutionState {
        const bindings = std.ArrayList(Binding).init(game.arena_for_bindings.allocator());

        if (fn_name.equals(&Sexpr.identity)) {
            const res = try game.pool_for_states.create();
            res.* = ExecutionState{
                .cur_value = input,
                .cur_fn_name = fn_name,
                .cur_cases = null,
                .cur_bindings = bindings,
                .parent = null,
            };
            return res;
        }
        if (fn_name.equals(&Sexpr.@"eqAtoms?")) {
            const val = switch (input.*) {
                .atom_lit, .atom_var => Sexpr.fromBool(false),
                .pair => |p| Sexpr.fromBool(p.left.*.isLit() and p.right.*.isLit() and Sexpr.equals(p.left, p.right)),
            };

            const res = try game.pool_for_states.create();
            res.* = ExecutionState{
                .cur_value = val,
                .cur_fn_name = fn_name,
                .cur_cases = null,
                .cur_bindings = bindings,
                .parent = null,
            };
            return res;
        }

        const cases = try game.findFunktion(fn_name);
        const res = try game.pool_for_states.create();
        res.* = ExecutionState{
            .cur_bindings = bindings,
            .cur_cases = cases.items,
            .cur_fn_name = fn_name,
            .cur_value = input,
            .parent = null,
        };
        return res;
    }

    pub fn nextThing(this: *ExecutionState, mem: *PermamentGameStuff) !?*ExecutionState {
        if (this.cur_cases) |cases| {
            const initial_bindings_count = this.cur_bindings.items.len;
            for (cases) |case| {
                if (!try generateBindings(case.pattern, this.cur_value, &this.cur_bindings)) {
                    undoLastBindings(&this.cur_bindings, initial_bindings_count);
                    continue;
                }

                const argument = try fillTemplate(case.template, this.cur_bindings, &mem.pool_for_sexprs);

                var inner_execution = try ExecutionState.stateAfterEnteringFnk(mem, case.fn_name, argument);

                if (case.next) |next| {
                    this.cur_cases = next.items;
                    this.cur_value = undefined;
                    inner_execution.parent = this;
                    return inner_execution;
                } else {
                    inner_execution.parent = this.parent;
                    this.cur_bindings.deinit();
                    mem.pool_for_states.destroy(this);
                    return inner_execution;
                }
            }
            return error.BAD_INPUT;
        } else if (this.parent) |parent| {
            // no cases left
            this.cur_bindings.deinit();
            mem.pool_for_states.destroy(this);
            parent.cur_value = this.cur_value;
            return parent;
        } else {
            return null;
        }
    }
};

const PermamentGameStuff = struct {
    all_fnks: FnkCollection,
    pool_for_sexprs: MemoryPool(Sexpr),
    pool_for_states: MemoryPool(ExecutionState),
    arena_for_cases: std.heap.ArenaAllocator,
    arena_for_bindings: std.heap.ArenaAllocator,

    pub fn init(
        input_raw: []const u8,
        fn_name_raw: []const u8,
        all_fnks_raw: []const u8,
        allocator: std.mem.Allocator,
    ) !struct { game: PermamentGameStuff, first_state: *ExecutionState } {
        const pool_for_states = MemoryPool(ExecutionState).init(allocator);
        var pool_for_sexprs = MemoryPool(Sexpr).init(allocator);
        var arena_for_cases = std.heap.ArenaAllocator.init(allocator);
        const arena_for_bindings = std.heap.ArenaAllocator.init(allocator);
        var fnk_collection = FnkCollection.init(allocator);
        var remaining_fnk_input = all_fnks_raw;
        while (true) {
            skipWhitespace(&remaining_fnk_input);
            if (remaining_fnk_input.len == 0) break;
            const fnk = try parseFnk(&remaining_fnk_input, &pool_for_sexprs, arena_for_cases.allocator());
            try fnk_collection.put(fnk.name, fnk.body);
        }

        var game = PermamentGameStuff{
            .all_fnks = fnk_collection,
            .pool_for_sexprs = pool_for_sexprs,
            .pool_for_states = pool_for_states,
            .arena_for_cases = arena_for_cases,
            .arena_for_bindings = arena_for_bindings,
        };

        // TODO: move this to Game
        const fn_name = try parseSingleSexpr(fn_name_raw, &game.pool_for_sexprs);
        const input = try parseSingleSexpr(input_raw, &game.pool_for_sexprs);
        const first_state = try ExecutionState.stateAfterEnteringFnk(&game, fn_name, input);

        return .{
            .game = game,
            .first_state = first_state,
        };
    }

    pub fn deinit(this: *PermamentGameStuff) void {
        this.pool_for_sexprs.deinit();
        this.pool_for_states.deinit();
        this.all_fnks.deinit();
        this.arena_for_cases.deinit();
    }

    // TODO: compile fnks as needed
    fn findFunktion(this: PermamentGameStuff, name: *const Sexpr) !*const MatchCases {
        return if (this.all_fnks.getPtr(name)) |fnk| fnk else error.BAD_INPUT;
    }
};

const Game = struct {
    permanent_stuff: PermamentGameStuff,
    cur_state: *ExecutionState,

    pub fn init(
        input_raw: []const u8,
        fn_name_raw: []const u8,
        all_fnks_raw: []const u8,
        allocator: std.mem.Allocator,
    ) !Game {
        const result = try PermamentGameStuff.init(input_raw, fn_name_raw, all_fnks_raw, allocator);
        return .{
            .permanent_stuff = result.game,
            .cur_state = result.first_state,
        };
    }

    pub fn deinit(this: *Game) void {
        this.permanent_stuff.deinit();
    }

    pub fn getFinalResult(this: *Game) !*const Sexpr {
        while (try this.cur_state.nextThing(&this.permanent_stuff)) |x| {
            this.cur_state = x;
        }
        return this.cur_state.cur_value;
    }
};

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Run `zig build test` to run the tests.\n", .{});
    defer {
        bw.flush() catch std.debug.panic("flush failed!", .{});
    }

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) std.debug.panic("leaked memory!", .{});
    }
    const allocator = gpa.allocator();
    _ = allocator; // autofix
}

test "main test" {
    var game = try Game.init("input", "fn_name",
        \\  fn_name {
        \\      input -> output;    
        \\  }
    , std.testing.allocator);
    defer game.deinit();

    try expectEqualSexprs(
        &Sexpr{ .atom_lit = Atom{ .value = "input" } },
        game.cur_state.cur_value,
    );
    try expectEqualSexprs(
        &Sexpr{ .atom_lit = Atom{ .value = "fn_name" } },
        game.cur_state.cur_fn_name,
    );

    const actual = try game.getFinalResult();
    const expected = Sexpr{ .atom_lit = .{ .value = "output" } };
    try expectEqualSexprs(&expected, actual);
}

fn parseSexpr(input: *[]const u8, pool: *MemoryPool(Sexpr)) !*const Sexpr {
    const result = try parseSexprTrue(input.*, pool);
    input.* = result.rest;
    return result.sexpr;
}

fn parseSingleSexpr(input: []const u8, pool: *MemoryPool(Sexpr)) !*const Sexpr {
    var result = try parseSexprTrue(input, pool);
    skipWhitespace(&result.rest);
    if (result.rest.len > 0) return error.BAD_INPUT;
    return result.sexpr;
}

fn parseSexprTrue(input: []const u8, pool: *MemoryPool(Sexpr)) error{ OutOfMemory, BAD_INPUT }!struct { sexpr: *const Sexpr, rest: []const u8 } {
    var rest = input;
    skipWhitespace(&rest);
    if (rest[0] == '(') {
        const asdf = try parseSexprInsideParens(rest[1..], pool);
        return .{ .sexpr = asdf.sexpr, .rest = asdf.rest };
    }
    const asdf = try parseAtom(rest);
    const res = try pool.create();
    if (asdf.is_var) {
        res.* = Sexpr{ .atom_var = asdf.atom };
    } else {
        res.* = Sexpr{ .atom_lit = asdf.atom };
    }
    return .{ .sexpr = res, .rest = asdf.rest };
}

fn parseSexprInsideParens(input: []const u8, pool: *MemoryPool(Sexpr)) !struct { sexpr: *const Sexpr, rest: []const u8 } {
    var rest = input;
    skipWhitespace(&rest);
    if (rest[0] == ')') {
        return .{ .sexpr = &Sexpr.nil, .rest = rest[1..] };
    }
    if (rest[0] == '.') {
        const final_asdf = try parseSexprTrue(rest[1..], pool);
        rest = final_asdf.rest;
        skipWhitespace(&rest);
        if (rest[0] != ')') return error.BAD_INPUT;
        return .{ .sexpr = final_asdf.sexpr, .rest = rest[1..] };
    }
    const first_asdf = try parseSexprTrue(rest, pool);
    const rest_asdf = try parseSexprInsideParens(first_asdf.rest, pool);

    const res = try pool.create();
    res.* = Sexpr{ .pair = Pair{ .left = first_asdf.sexpr, .right = rest_asdf.sexpr } };

    return .{ .sexpr = res, .rest = rest_asdf.rest };
}

fn parseAtom(input: []const u8) !struct { atom: Atom, is_var: bool, rest: []const u8 } {
    const word_breaks = .{ '(', ')', ':', '.', ';' } ++ std.ascii.whitespace;
    const rest = std.mem.trimLeft(u8, input, &std.ascii.whitespace);
    const word_end = std.mem.indexOfAnyPos(u8, rest, 0, &word_breaks) orelse rest.len;
    const is_variable = rest[0] == '@';
    return .{
        .atom = Atom{ .value = rest[(if (is_variable) 1 else 0)..word_end] },
        .is_var = is_variable,
        .rest = rest[word_end..],
    };
}

fn parseFnk(input: *[]const u8, pool: *MemoryPool(Sexpr), allocator: std.mem.Allocator) !Fnk {
    const result = try parseFnkTrue(input.*, pool, allocator);
    input.* = result.rest;
    return result.fnk;
}

fn parseFnkTrue(input: []const u8, pool: *MemoryPool(Sexpr), allocator: std.mem.Allocator) !struct { fnk: Fnk, rest: []const u8 } {
    var rest = input;
    skipWhitespace(&rest);
    const name = try parseSexpr(&rest, pool);
    skipWhitespace(&rest);
    // try parseChar(&rest, ':');
    // skipWhitespace(&rest);
    try parseChar(&rest, '{');
    const cases = try parseMatchCases(&rest, pool, allocator);
    skipWhitespace(&rest);
    return .{ .fnk = Fnk{ .name = name, .body = cases }, .rest = rest };
}

fn parseMatchCases(input: *[]const u8, pool: *MemoryPool(Sexpr), allocator: std.mem.Allocator) !MatchCases {
    var list = std.ArrayListUnmanaged(MatchCaseDefinition){};
    skipWhitespace(input);
    while (!parseCharIfPossible(input, '}')) {
        const pattern = try parseSexpr(input, pool);
        skipWhitespace(input);
        try parseChar(input, '-');
        try parseChar(input, '>');
        skipWhitespace(input);
        const fn_name_or_template = try parseSexpr(input, pool);
        skipWhitespace(input);
        var fn_name: *const Sexpr = undefined;
        var template: *const Sexpr = undefined;
        if (parseCharIfPossible(input, ':')) {
            fn_name = fn_name_or_template;
            template = try parseSexpr(input, pool);
            skipWhitespace(input);
        } else {
            fn_name = &Sexpr.identity;
            template = fn_name_or_template;
        }
        var next: ?MatchCases = undefined;
        if (parseCharIfPossible(input, ';')) {
            next = null;
        } else {
            try parseChar(input, '{');
            next = try parseMatchCases(input, pool, allocator);
        }
        skipWhitespace(input);

        try list.append(allocator, .{
            .pattern = pattern,
            .fn_name = fn_name,
            .template = template,
            .next = next,
        });
    }
    return list;
}

fn skipWhitespace(input: *[]const u8) void {
    input.* = std.mem.trimLeft(u8, input.*, &std.ascii.whitespace);
    while (std.mem.startsWith(u8, input.*, "//")) {
        input.* = input.*[(std.mem.indexOfScalar(u8, input.*, '\n').? + 1)..];
        input.* = std.mem.trimLeft(u8, input.*, &std.ascii.whitespace);
    }
}

fn parseChar(input: *[]const u8, comptime expected: u8) !void {
    if (input.*[0] != expected) return error.BAD_INPUT;
    input.* = input.*[1..];
}

fn parseCharIfPossible(input: *[]const u8, comptime expected: u8) bool {
    if (input.*[0] != expected) return false;
    input.* = input.*[1..];
    return true;
}

fn expectEqualSexprs(expected: *const Sexpr, actual: *const Sexpr) !void {
    switch (expected.*) {
        .atom_lit => |expected_atom| switch (actual.*) {
            .atom_lit => |actual_atom| {
                return std.testing.expectEqualStrings(expected_atom.value, actual_atom.value);
            },
            .atom_var => |actual_atom| {
                std.debug.print("expected literal '{s}' but found variable '{s}'\n", .{ expected_atom.value, actual_atom.value });
                return error.TestExpectedEqual;
            },
            .pair => |actual_pair| {
                std.debug.print("expected literal '{s}' but found a pair {any}\n", .{ expected_atom.value, actual_pair });
                return error.TestExpectedEqual;
            },
        },
        .atom_var => |expected_atom| switch (actual.*) {
            .atom_lit => |actual_atom| {
                std.debug.print("expected variable '{s}' but found literal '{s}'\n", .{ expected_atom.value, actual_atom.value });
                return error.TestExpectedEqual;
            },
            .atom_var => |actual_atom| {
                return std.testing.expectEqualStrings(expected_atom.value, actual_atom.value);
            },
            .pair => |actual_pair| {
                std.debug.print("expected variable '{s}' but found a pair {any}\n", .{ expected_atom.value, actual_pair });
                return error.TestExpectedEqual;
            },
        },
        .pair => |expected_pair| switch (actual.*) {
            .atom_lit => |actual_atom| {
                std.debug.print("expected pair but found literal '{s}'\n", .{actual_atom.value});
                return error.TestExpectedEqual;
            },
            .atom_var => |actual_atom| {
                std.debug.print("expected pair but found literal '{s}'\n", .{actual_atom.value});
                return error.TestExpectedEqual;
            },
            .pair => |actual_pair| {
                try expectEqualSexprs(expected_pair.left, actual_pair.left);
                try expectEqualSexprs(expected_pair.right, actual_pair.right);
            },
        },
    }
}

fn generateBindings(pattern: *const Sexpr, value: *const Sexpr, bindings: *Bindings) !bool {
    switch (pattern.*) {
        .atom_var => |pat| {
            // TODO: return false if variable was already bound
            try bindings.append(.{ .name = pat.value, .value = value });
            return true;
        },
        .atom_lit => |pat| {
            switch (value.*) {
                .pair => return false,
                .atom_lit => |val| return val.equals(pat),
                .atom_var => return error.BAD_INPUT,
            }
        },
        .pair => |pat| {
            switch (value.*) {
                .atom_lit => return false,
                .atom_var => return error.BAD_INPUT,
                .pair => |val| {
                    return (try generateBindings(pat.left, val.left, bindings)) and (try generateBindings(pat.right, val.right, bindings));
                },
            }
        },
    }
}

fn fillTemplate(template: *const Sexpr, bindings: Bindings, pool: *MemoryPool(Sexpr)) !*const Sexpr {
    switch (template.*) {
        .atom_var => |templ| {
            for (0..bindings.items.len) |k| {
                const bind = bindings.items[bindings.items.len - k - 1];
                if (std.mem.eql(u8, bind.name, templ.value)) {
                    return bind.value;
                }
            }
            return error.BAD_INPUT;
        },
        .atom_lit => return template,
        .pair => |templ| {
            const left = try fillTemplate(templ.left, bindings, pool);
            const right = try fillTemplate(templ.right, bindings, pool);
            const result: *Sexpr = try pool.create();
            result.* = Sexpr{ .pair = Pair{ .left = left, .right = right } };
            return result;
        },
    }
}

fn undoLastBindings(bindings: *Bindings, original_count: usize) void {
    bindings.shrinkAndFree(original_count);
}
