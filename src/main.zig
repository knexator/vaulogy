const std = @import("std");
const MemoryPool = std.heap.MemoryPool;

const parsing = @import("parsing.zig");

// Design decision 1: strings live on the input buffer
// Design decision 2: Sexprs are never released :(

const DEBUG = false;

pub const Atom = struct {
    value: []const u8,

    pub fn equals(this: Atom, other: Atom) bool {
        return std.mem.eql(u8, this.value, other.value);
    }
};
pub const Pair = struct {
    left: *const Sexpr,
    right: *const Sexpr,
};
pub const Sexpr = union(enum) {
    atom_var: Atom,
    atom_lit: Atom,
    pair: Pair,

    pub const debug = Sexpr.lit("DEBUGGG");
    pub const @"return" = Sexpr.lit("return");
    pub const @"var" = Sexpr.lit("var");
    pub const atom = Sexpr.lit("atom");
    pub const nil = Sexpr.lit("nil");
    pub const identity = Sexpr.lit("identity");
    pub const @"eqAtoms?" = Sexpr.lit("eqAtoms?");
    pub const @"true" = Sexpr.lit("true");
    pub const @"false" = Sexpr.lit("false");

    pub fn pair(a: *const Sexpr, b: *const Sexpr) Sexpr {
        return .{ .pair = .{ .left = a, .right = b } };
    }

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

    pub fn isLit(this: *const Sexpr) bool {
        return switch (this.*) {
            .atom_lit => true,
            else => false,
        };
    }

    pub fn fromBool(b: bool) *const Sexpr {
        return if (b) &Sexpr.true else &Sexpr.false;
    }
};

pub const Fnk = struct { name: *const Sexpr, body: MatchCases };
pub const MatchCases = std.ArrayListUnmanaged(MatchCaseDefinition);
pub const MatchCaseDefinition = struct {
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

const builtin_fnks = [_]struct { name: *const Sexpr, fnk: fn (v: *const Sexpr) *const Sexpr }{
    .{ .name = &Sexpr.identity, .fnk = builtin_fnk_identity },
    .{ .name = &Sexpr.@"eqAtoms?", .fnk = @"builtin_fnk_eqAtoms?" },
};

fn builtin_fnk_identity(v: *const Sexpr) *const Sexpr {
    return v;
}

fn @"builtin_fnk_eqAtoms?"(v: *const Sexpr) *const Sexpr {
    return switch (v.*) {
        .atom_lit, .atom_var => Sexpr.fromBool(false),
        .pair => |p| Sexpr.fromBool(p.left.isLit() and p.right.isLit() and Sexpr.equals(p.left, p.right)),
    };
}

const PermamentGameStuff = struct {
    all_fnks: FnkCollection,
    pool_for_sexprs: MemoryPool(Sexpr),
    arena_for_cases: std.heap.ArenaAllocator,
    arena_for_bindings: std.heap.ArenaAllocator,
    allocator_for_stack: std.mem.Allocator,

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
            parsing.skipWhitespace(&remaining_fnk_input);
            if (remaining_fnk_input.len == 0) break;
            const fnk = try parsing.parseFnk(&remaining_fnk_input, &pool_for_sexprs, arena_for_cases.allocator());
            try fnk_collection.put(fnk.name, fnk.body);
        }

        return PermamentGameStuff{
            .all_fnks = fnk_collection,
            .pool_for_sexprs = pool_for_sexprs,
            .arena_for_cases = arena_for_cases,
            .arena_for_bindings = arena_for_bindings,
            .allocator_for_stack = allocator,
        };
    }

    pub fn deinit(this: *PermamentGameStuff) void {
        this.pool_for_sexprs.deinit();
        this.all_fnks.deinit();
        this.arena_for_cases.deinit();
        this.arena_for_bindings.deinit();
    }

    fn findFunktion(this: *PermamentGameStuff, name: *const Sexpr) error{ OutOfMemory, BAD_INPUT }!*const MatchCases {
        if (this.all_fnks.getPtr(name)) |fnk| {
            return fnk;
        } else switch (name.*) {
            .atom_lit, .atom_var => return error.BAD_INPUT,
            .pair => |p| {
                // try to compile it!
                const asdf = try Game.applyFnk(this, p.left, p.right);
                const cases = try fnkFromSexpr(asdf, this.arena_for_cases.allocator(), &this.pool_for_sexprs);
                if (DEBUG) {
                    const stderr = std.io.getStdErr().writer();
                    stderr.print("\ncompiled a fnk, the cases are: ", .{}) catch unreachable;
                    writeSexpr2(asdf, stderr.any()) catch unreachable;
                    stderr.print("\n", .{}) catch unreachable;
                }
                try this.all_fnks.put(name, cases);
                return this.all_fnks.getPtr(name).?;
            },
        }
    }
};

const StackThing = struct {
    cur_fn_name: *const Sexpr,
    cur_cases: ?[]const MatchCaseDefinition,
    cur_bindings: Bindings,

    pub fn init(input: *const Sexpr, fn_name: *const Sexpr, permanent_stuff: *PermamentGameStuff) !union(enum) {
        builtin: *const Sexpr,
        stack_thing: StackThing,
    } {
        inline for (builtin_fnks) |builtin| {
            if (builtin.name.equals(fn_name)) {
                return .{ .builtin = builtin.fnk(input) };
            }
        }

        const bindings = std.ArrayList(Binding).init(permanent_stuff.arena_for_bindings.allocator());
        const cases = (try permanent_stuff.findFunktion(fn_name)).*.items;
        return .{ .stack_thing = StackThing{
            .cur_bindings = bindings,
            .cur_cases = cases,
            .cur_fn_name = fn_name,
        } };
    }

    pub fn deinit(this: *StackThing) void {
        // _ = this; // autofix
        this.cur_bindings.deinit();
    }
};

const ExecutionThread = struct {
    active_value: *const Sexpr,
    stack: std.ArrayList(StackThing),

    score: struct {
        successful_matches: usize,
        max_stack: usize,
    },

    pub fn init(
        input: *const Sexpr,
        fn_name: *const Sexpr,
        permanent_stuff: *PermamentGameStuff,
    ) !ExecutionThread {
        var stack = std.ArrayList(StackThing).init(permanent_stuff.allocator_for_stack);
        switch (try StackThing.init(input, fn_name, permanent_stuff)) {
            .builtin => |res| return ExecutionThread{
                .active_value = res,
                .stack = stack,
                .score = .{
                    .successful_matches = 0,
                    .max_stack = 0,
                },
            },
            .stack_thing => |first| {
                try stack.append(first);
                return ExecutionThread{
                    .active_value = input,
                    .stack = stack,
                    .score = .{
                        .successful_matches = 0,
                        .max_stack = 1,
                    },
                };
            },
        }
    }

    pub fn initFromText(
        input_raw: []const u8,
        fn_name_raw: []const u8,
        permanent_stuff: *PermamentGameStuff,
    ) !ExecutionThread {
        const fn_name = try parsing.parseSingleSexpr(fn_name_raw, &permanent_stuff.pool_for_sexprs);
        const input = try parsing.parseSingleSexpr(input_raw, &permanent_stuff.pool_for_sexprs);
        return ExecutionThread.init(input, fn_name, permanent_stuff);
    }

    pub fn advanceStep(this: *ExecutionThread, permanent_stuff: *PermamentGameStuff) !?*const Sexpr {
        if (this.stack.items.len > 0) {
            const last_stack_ptr: *StackThing = &this.stack.items[this.stack.items.len - 1];
            if (last_stack_ptr.cur_cases) |cases| {
                const initial_bindings_count = last_stack_ptr.cur_bindings.items.len;
                for (cases) |case| {
                    if (!(try generateBindings(case.pattern, this.active_value, &last_stack_ptr.cur_bindings))) {
                        undoLastBindings(&last_stack_ptr.cur_bindings, initial_bindings_count);
                        continue;
                    }
                    const argument = try fillTemplate(case.template, last_stack_ptr.cur_bindings, &permanent_stuff.pool_for_sexprs);
                    this.active_value = argument;

                    if (case.next) |next| {
                        last_stack_ptr.cur_cases = next.items;
                    } else {
                        _ = this.stack.pop();
                    }

                    this.score.successful_matches += 1;

                    const new_thing = try StackThing.init(this.active_value, case.fn_name, permanent_stuff);
                    switch (new_thing) {
                        .stack_thing => |x| {
                            try this.stack.append(x);
                            this.score.max_stack = @max(this.score.max_stack, this.stack.items.len);
                        },
                        .builtin => |r| {
                            this.active_value = r;
                        },
                    }

                    return null;
                }
                return error.BAD_INPUT;
            } else {
                // ran out of cases
                inline for (builtin_fnks) |builtin| {
                    std.debug.assert(!Sexpr.equals(builtin.name, last_stack_ptr.cur_fn_name));
                }
                _ = this.stack.pop();
                return null;
            }
        } else {
            return this.active_value;
        }
    }

    pub fn getFinalResult(this: *ExecutionThread, permanent_stuff: *PermamentGameStuff) !*const Sexpr {
        while (true) {
            if (try this.advanceStep(permanent_stuff)) |res| {
                return res;
            }
        }
    }

    pub fn deinit(this: *ExecutionThread) void {
        this.stack.deinit();
    }
};

const Game = struct {
    permanent_stuff: PermamentGameStuff,
    execution: ExecutionThread,

    pub fn init(
        result: *Game,
        input_raw: []const u8,
        fn_name_raw: []const u8,
        all_fnks_raw: []const u8,
        allocator: std.mem.Allocator,
    ) !void {
        result.permanent_stuff = try PermamentGameStuff.init(all_fnks_raw, allocator);

        const fn_name = try parsing.parseSingleSexpr(fn_name_raw, &result.permanent_stuff.pool_for_sexprs);
        const input = try parsing.parseSingleSexpr(input_raw, &result.permanent_stuff.pool_for_sexprs);
        result.execution = try ExecutionThread.init(input, fn_name, &result.permanent_stuff);
    }

    pub fn deinit(this: *Game) void {
        this.permanent_stuff.deinit();
        this.execution.deinit();
    }

    pub fn getFinalResult(this: *Game) !*const Sexpr {
        return this.execution.getFinalResult(&this.permanent_stuff);
    }

    pub fn applyFnk(permanent_stuff: *PermamentGameStuff, fn_name: *const Sexpr, input: *const Sexpr) !*const Sexpr {
        var exec = try ExecutionThread.init(input, fn_name, permanent_stuff);
        defer exec.deinit();
        return exec.getFinalResult(permanent_stuff);
    }
};

pub fn main() !u8 {
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

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    std.debug.assert(args.skip());

    const verb = args.next().?;

    if (std.mem.eql(u8, verb, "run")) {
        const fnks_collection_raw: []const u8 = blk: {
            const filename = args.next().?;
            const file = try std.fs.cwd().openFile(filename, .{});
            defer file.close();
            break :blk try file.readToEndAlloc(allocator, std.math.maxInt(usize));
        };
        defer allocator.free(fnks_collection_raw);

        const fn_name_raw: []const u8 = args.next().?;

        var should_free_input = false;
        const input_raw: []const u8 = blk: {
            const v = args.next().?;
            if (std.mem.eql(u8, v, "file")) {
                should_free_input = true;
                const n = args.next().?;
                if (std.mem.eql(u8, n, "raw")) {
                    const file_name = args.next().?;
                    const file = try std.fs.cwd().openFile(file_name, .{});
                    defer file.close();
                    var br = std.io.bufferedReader(file.reader());
                    var contents = std.ArrayList(u8).init(allocator);
                    defer contents.deinit();
                    try contents.append('(');
                    while (true) {
                        const cur_byte = br.reader().readByte() catch break;
                        if (cur_byte == 0x0D) continue; // ignore the CR char, assuming that it will be followed by a NL
                        try contents.append('x');
                        try contents.append(std.fmt.digitToChar(cur_byte >> 4, .upper));
                        try contents.append(std.fmt.digitToChar(cur_byte & 0x0F, .upper));
                        try contents.append(' ');
                    }
                    try contents.append(')');
                    break :blk try contents.toOwnedSlice();
                } else {
                    const file_name = n;
                    const file = try std.fs.cwd().openFile(file_name, .{});
                    defer file.close();
                    break :blk try file.readToEndAlloc(allocator, std.math.maxInt(usize));
                }
            } else {
                break :blk v;
            }
        };
        defer if (should_free_input) allocator.free(input_raw);

        std.debug.assert(!args.skip());

        var game: Game = undefined;
        try Game.init(&game, input_raw, fn_name_raw, fnks_collection_raw, allocator);
        // try Game.init(&game, "(1)", "stuff",
        //     \\ stuff {
        //     \\      nil -> hola;
        //     \\      (@a . @rest) -> @a {
        //     \\          @b -> @rest {
        //     \\              @c -> @b;
        //     \\          }
        //     \\      }
        //     \\ }
        // , allocator);
        defer game.deinit();

        const actual = try game.getFinalResult();
        // const expected = Sexpr{ .atom_lit = .{ .value = "output" } };
        // _ = expected; // autofix
        // try expectEqualSexprs(&expected, actual);

        try stdout.print("result: ", .{});
        // try writeSexpr2(actual, stdout.any());
        try writeSexpr(actual, stdout.any(), allocator);
        try stdout.print("\n", .{});
    } else {
        try stdout.print(
            \\  valid commands:
            \\      run [fnk-lib] [fnk-name] [input]
            \\      run [fnk-lib] [fnk-name] file [input-filename]
            \\      run [fnk-lib] [fnk-name] file raw [input-filename]
            \\      score [my-fnk-lib] [target-fnk-lib]
        , .{});
        return 1;
    }
    return 0;
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
    const expected = Sexpr.lit("output");
    try expectEqualSexprs(&expected, actual);

    try expectEqualSexprs(&expected, try Game.applyFnk(&game.permanent_stuff, &Sexpr.lit("fn_name"), &Sexpr.lit("input")));
}

test "with comptime" {
    var game: Game = undefined;
    try Game.init(&game, "2", "stuff",
        \\  
        \\  stuff {
        \\      @digit -> (compileMap . ( 
        \\          (0 . a) 
        \\          (1 . b) 
        \\          (2 . c) 
        \\          (3 . d)
        \\      )): @digit;
        \\  }
        \\
        \\  compileMap {
        \\      nil -> nil;
        \\      ((@key . @value) . @rest) -> compileMap: @rest {
        \\          @rest_compiled -> ( ((atom . @key) identity (atom . @value) . return) . @rest_compiled );
        \\      }
        \\  }
    , std.testing.allocator);
    defer game.deinit();

    const actual = try game.getFinalResult();
    // const expected = Sexpr.pair(
    //      &Sexpr.lit("b0"),
    //      &Sexpr.pair(
    //         &Sexpr.lit("b1"),
    //         &Sexpr.nil,
    //      ),
    // );
    const expected = Sexpr.lit("c");
    // const expected = Sexpr{ .atom_lit = .{ .value = "output" } };
    try expectEqualSexprs(&expected, actual);
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
    //     &Sexpr.lit("stuff"),
    //     game.cur_state.cur_fn_name,
    // );

    const expected = try parsing.parseSingleSexpr("1", &game.permanent_stuff.pool_for_sexprs);
    const actual = try game.getFinalResult();

    try expectEqualSexprs(expected, actual);
}

test "scoring bubbleUp" {
    var mem = try PermamentGameStuff.init(
        \\ 
        \\ bubbleUp {
        \\      (X . @rest) -> (X . @rest);
        \\      (@a . @b) -> bubbleUp: @b {
        \\          (X . @rest) -> (X @a . @rest);
        \\      }
        \\ }
    , std.testing.allocator);
    defer mem.deinit();

    var exec = try ExecutionThread.initFromText("(a b X c d)", "bubbleUp", &mem);
    defer exec.deinit();

    const expected = Sexpr.pair(&Sexpr.lit("X"), &Sexpr.pair(
        &Sexpr.lit("a"),
        &Sexpr.pair(&Sexpr.lit("b"), &Sexpr.pair(
            &Sexpr.lit("c"),
            &Sexpr.pair(
                &Sexpr.lit("d"),
                &Sexpr.nil,
            ),
        )),
    ));

    const actual = try exec.getFinalResult(&mem);
    try expectEqualSexprs(&expected, actual);

    try std.testing.expectEqual(3, exec.score.max_stack);
    try std.testing.expectEqual(5, exec.score.successful_matches);
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

    // try bindings.append(.{ .name = "xxx", .value = value });

    switch (pattern.*) {
        .atom_var => |pat| {
            switch (value.*) {
                .atom_var => return error.BAD_INPUT,
                else => {
                    // TODO: return error.BAD_INPUT if variable was already bound
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

fn fnkFromSexpr(s: *const Sexpr, allocator_for_cases: std.mem.Allocator, pool: *MemoryPool(Sexpr)) !MatchCases {
    return (try fnkFromSexprHelper(s, allocator_for_cases, pool)).?;
}

fn fnkFromSexprHelper(s: *const Sexpr, arena: std.mem.Allocator, pool: *MemoryPool(Sexpr)) !?MatchCases {
    var cases = std.ArrayListUnmanaged(MatchCaseDefinition){};
    switch (s.*) {
        .atom_lit => return if (s.equals(&Sexpr.@"return")) null else error.BAD_INPUT,
        .atom_var => return error.BAD_INPUT,
        .pair => |p| {
            var cur_parent = p;
            while (true) {
                const cur = cur_parent.left;
                const pattern = try internalFromExternal(cur.pair.left, pool);
                const fn_name = cur.pair.right.pair.left;
                const template = try internalFromExternal(cur.pair.right.pair.right.pair.left, pool);
                const next = try fnkFromSexprHelper(cur.pair.right.pair.right.pair.right, arena, pool);
                try cases.append(arena, .{
                    .pattern = pattern,
                    .fn_name = fn_name,
                    .template = template,
                    .next = next,
                });
                switch (cur_parent.right.*) {
                    .atom_lit => |a| {
                        if (a.equals(Sexpr.nil.atom_lit)) {
                            break;
                        } else {
                            return error.BAD_INPUT;
                        }
                    },
                    .atom_var => return error.BAD_INPUT,
                    .pair => |p2| {
                        cur_parent = p2;
                    },
                }
            }
            return cases;
        },
    }
}

// ((atom . aaa) . (var . bbb)) => (aaa . @bbb)
fn internalFromExternal(s: *const Sexpr, pool: *MemoryPool(Sexpr)) !*const Sexpr {
    switch (s.*) {
        .atom_var, .atom_lit => return error.BAD_INPUT,
        .pair => |p| {
            if (p.left.equals(&Sexpr.atom)) {
                return p.right;
            } else if (p.left.equals(&Sexpr.@"var")) {
                switch (p.right.*) {
                    .pair => return error.BAD_INPUT,
                    .atom_var => return error.BAD_INPUT,
                    .atom_lit => |a| {
                        const res: *Sexpr = try pool.create();
                        res.* = Sexpr{ .atom_var = a };
                        return res;
                    },
                }
            } else {
                const res = try pool.create();
                res.* = Sexpr{ .pair = Pair{
                    .left = try internalFromExternal(p.left, pool),
                    .right = try internalFromExternal(p.right, pool),
                } };
                return res;
            }
        },
    }
}
