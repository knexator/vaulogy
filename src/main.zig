const std = @import("std");
const MemoryPool = std.heap.MemoryPool;

const DEBUG = false;

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

    const debug = Sexpr.lit("DEBUGGG");
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

const PermamentGameStuff = struct {
    all_fnks: FnkCollection,
    pool_for_sexprs: MemoryPool(Sexpr),
    arena_for_cases: std.heap.ArenaAllocator,
    arena_for_bindings: std.heap.ArenaAllocator,

    pub fn init(
        all_fnks_raw: []const u8,
        allocator: std.mem.Allocator,
    ) !PermamentGameStuff {
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

        return PermamentGameStuff{
            .all_fnks = fnk_collection,
            .pool_for_sexprs = pool_for_sexprs,
            .arena_for_cases = arena_for_cases,
            .arena_for_bindings = arena_for_bindings,
        };
    }

    pub fn deinit(this: *PermamentGameStuff) void {
        this.pool_for_sexprs.deinit();
        this.all_fnks.deinit();
        this.arena_for_cases.deinit();
        this.arena_for_bindings.deinit();
    }

    // TODO: compile fnks as needed
    fn findFunktion(this: PermamentGameStuff, name: *const Sexpr) !*const MatchCases {
        return if (this.all_fnks.getPtr(name)) |fnk| fnk else error.BAD_INPUT;
    }
};

const StackThing = struct {
    // only decorative, i guess
    cur_fn_name: *const Sexpr,
    cur_cases: ?[]const MatchCaseDefinition,
    cur_bindings: Bindings,

    pub fn init(fn_name: *const Sexpr, permanent_stuff: *PermamentGameStuff) !StackThing {
        // const bindings = std.ArrayList(Binding).init(std.heap.page_allocator);
        const bindings = std.ArrayList(Binding).init(permanent_stuff.arena_for_bindings.allocator());

        const cases = if (fn_name.equals(&Sexpr.identity) or fn_name.equals(&Sexpr.@"eqAtoms?"))
            null
        else
            (try permanent_stuff.findFunktion(fn_name)).*.items;
        return StackThing{
            .cur_bindings = bindings,
            .cur_cases = cases,
            .cur_fn_name = fn_name,
        };
    }

    pub fn deinit(this: *StackThing) void {
        // _ = this; // autofix
        this.cur_bindings.deinit();
    }
};

const Game = struct {
    permanent_stuff: PermamentGameStuff,
    // cur_states: std.ArrayList(ExecutionState),

    active_value: *const Sexpr,
    stack: std.ArrayList(StackThing),

    pub fn init(
        result: *Game,
        input_raw: []const u8,
        fn_name_raw: []const u8,
        all_fnks_raw: []const u8,
        allocator: std.mem.Allocator,
    ) !void {
        result.permanent_stuff = try PermamentGameStuff.init(all_fnks_raw, allocator);

        const fn_name = try parseSingleSexpr(fn_name_raw, &result.permanent_stuff.pool_for_sexprs);
        const input = try parseSingleSexpr(input_raw, &result.permanent_stuff.pool_for_sexprs);
        // const first_state = try ExecutionState.stateAfterEnteringFnk(&game, fn_name, input);

        var stack = std.ArrayList(StackThing).init(allocator);
        try stack.append(try StackThing.init(fn_name, &result.permanent_stuff));

        result.stack = stack;
        result.active_value = input;
    }

    pub fn deinit(this: *Game) void {
        this.permanent_stuff.deinit();
        this.stack.deinit();
    }
    pub fn advanceStep(this: *Game) !?*const Sexpr {
        if (this.stack.items.len > 0) {
            const last_stack_ptr: *StackThing = &this.stack.items[this.stack.items.len - 1];
            if (last_stack_ptr.cur_cases) |cases| {
                const initial_bindings_count = last_stack_ptr.cur_bindings.items.len;
                for (cases) |case| {
                    if (!(try generateBindings(case.pattern, this.active_value, &(last_stack_ptr.cur_bindings)))) {
                        undoLastBindings(&last_stack_ptr.cur_bindings, initial_bindings_count);
                        continue;
                    }
                    const argument = try fillTemplate(case.template, last_stack_ptr.cur_bindings, &this.permanent_stuff.pool_for_sexprs);
                    const new_stack_thing = try StackThing.init(case.fn_name, &this.permanent_stuff);

                    if (case.next) |next| {
                        last_stack_ptr.cur_cases = next.items;
                    } else {
                        // TODO: remove it now from stack
                        last_stack_ptr.cur_cases = null;
                    }

                    try this.stack.append(new_stack_thing);

                    this.active_value = argument;

                    return null;
                }
                return error.BAD_INPUT;
            } else {
                // ran out of cases
                // TODO: handle built in fnks
                _ = this.stack.pop();
                return null;
            }
        } else {
            return this.active_value;
        }
    }

    pub fn getFinalResult(this: *Game) !*const Sexpr {
        while (true) {
            if (try this.advanceStep()) |res| {
                return res;
            }
        }
    }
};

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    // try stdout.print("Run `zig build test` to run the tests.\n", .{});
    defer {
        bw.flush() catch std.debug.panic("flush failed!", .{});
    }

    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true, .never_unmap = true, .retain_metadata = true }){};
    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) std.debug.panic("leaked memory!", .{});
    }
    const allocator = gpa.allocator();

    var game: Game = undefined;
    try Game.init(&game, "(1)", "stuff",
        \\ stuff {
        \\      nil -> hola;
        \\      (@a . @rest) -> @a {
        \\          @b -> @rest {
        \\              @c -> @b;
        \\          }
        \\      }
        \\ }
    , allocator);
    defer game.deinit();

    const actual = try game.getFinalResult();
    // const expected = Sexpr{ .atom_lit = .{ .value = "output" } };
    // _ = expected; // autofix
    // try expectEqualSexprs(&expected, actual);

    try stdout.print("result: ", .{});
    try writeSexpr2(actual, stdout.any());
    // try writeSexpr(actual, stdout.any(), allocator);
    try stdout.print("\n", .{});
}

test "main test" {
    var game: Game = undefined;
    try Game.init(&game, "input", "fn_name",
        \\  fn_name {
        \\      input -> output;    
        \\  }
    , std.testing.allocator);
    defer game.deinit();

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
    if (DEBUG) {
        const stderr = std.io.getStdErr().writer();
        stderr.print("\nGenerating bindings for pattern ", .{}) catch unreachable;
        writeSexpr2(pattern, stderr.any()) catch unreachable;
        stderr.print(" and value ", .{}) catch unreachable;
        writeSexpr2(value, stderr.any()) catch unreachable;
        stderr.print("\n", .{}) catch unreachable;

        // stderr.print("cur bindings are:\n", .{}) catch unreachable;
        // for (new_bindings.items) |binding| {
        //     stderr.print("name: {s}, value: ", .{binding.name}) catch unreachable;
        //     writeSexpr2(binding.value, stderr.any()) catch unreachable;
        //     stderr.print("\n", .{}) catch unreachable;
        // }
    }

    try bindings.append(.{ .name = "xxx", .value = value });

    switch (pattern.*) {
        .atom_var => |pat| {
            switch (value.*) {
                .atom_var => return error.BAD_INPUT,
                else => {
                    // TODO: return false if variable was already bound
                    try bindings.append(.{ .name = pat.value, .value = value });
                    return true;
                },
            }
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
                    const a = try generateBindings(pat.left, val.left, bindings);
                    const b = try generateBindings(pat.right, val.right, bindings);
                    return a and b;
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

test "apply another nested fnk, with ExecutionState" {
    var game: Game = undefined;
    try Game.init(&game, "(1)", "stuff",
        \\ stuff {
        \\      nil -> hola;
        \\      (@a . @rest) -> @a {
        \\          @b -> @rest {
        \\              @c -> @b;
        \\          }
        \\      }
        \\ }
    , std.testing.allocator);
    defer game.deinit();

    // try expectEqualSexprs(
    //     &Sexpr{ .atom_lit = Atom{ .value = "stuff" } },
    //     game.cur_state.cur_fn_name,
    // );

    const expected = try parseSingleSexpr("1", &game.permanent_stuff.pool_for_sexprs);
    const actual = try game.getFinalResult();

    try expectEqualSexprs(expected, actual);
}

fn writeSexpr2(s: *const Sexpr, w: std.io.AnyWriter) !void {
    switch (s.*) {
        .atom_lit => |atom| {
            try w.writeAll(atom.value);
        },
        .atom_var => |atom| {
            try w.writeAll("@");
            try w.writeAll(atom.value);
        },
        .pair => |p| {
            try w.writeAll("(");
            try writeSexpr2(p.left, w);
            try w.writeAll(" . ");
            try writeSexpr2(p.right, w);
            try w.writeAll(")");
        },
    }
}

fn writeSexpr(s: *const Sexpr, w: std.io.AnyWriter, temp_allocator: std.mem.Allocator) !void {
    switch (s.*) {
        .atom_lit => |atom| {
            try w.writeAll(atom.value);
        },
        .atom_var => |atom| {
            try w.writeAll("@");
            try w.writeAll(atom.value);
        },
        .pair => {
            var asdf = std.ArrayList(*const Sexpr).init(temp_allocator);
            defer asdf.deinit();

            const sentinel = try asListPlusSentinel(s, &asdf);
            try w.writeAll("(");
            for (asdf.items, 0..) |item, k| {
                try writeSexpr(item, w, temp_allocator);
                if (k + 1 < asdf.items.len) {
                    try w.writeAll(" ");
                }
            }
            if (sentinel.equals(&Sexpr.nil)) {
                try w.writeAll(")");
            } else {
                try w.writeAll(" . ");
                try writeSexpr(sentinel, w, temp_allocator);
                try w.writeAll(")");
            }
        },
    }
}

fn asListPlusSentinel(s: *const Sexpr, l: *std.ArrayList(*const Sexpr)) !*const Sexpr {
    switch (s.*) {
        .atom_lit, .atom_var => return s,
        .pair => |p| {
            try l.append(p.left);
            return try asListPlusSentinel(p.right, l);
        },
    }
}
