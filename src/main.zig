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

    pub const debug = Sexpr.doLit("DEBUGGG");
    pub const @"return" = Sexpr.doLit("return");
    pub const @"var" = Sexpr.doLit("var");
    pub const atom = Sexpr.doLit("atom");
    pub const nil = Sexpr.doLit("nil");
    pub const identity = Sexpr.doLit("identity");
    pub const @"eqAtoms?" = Sexpr.doLit("eqAtoms?");
    pub const @"true" = Sexpr.doLit("true");
    pub const @"false" = Sexpr.doLit("false");
    pub const input = Sexpr.doLit("input");

    pub const var_v1 = Sexpr.doVar("v1");

    pub const pair_nil_nil = Sexpr.doPair(&Sexpr.nil, &Sexpr.nil);

    pub fn doPair(a: *const Sexpr, b: *const Sexpr) Sexpr {
        return .{ .pair = .{ .left = a, .right = b } };
    }

    pub fn doLit(v: []const u8) Sexpr {
        return .{ .atom_lit = .{ .value = v } };
    }

    pub fn doVar(v: []const u8) Sexpr {
        return .{ .atom_var = .{ .value = v } };
    }

    pub fn isFullyResolved(x: *const Sexpr) bool {
        return switch (x.*) {
            .atom_lit => true,
            .atom_var => false,
            .pair => |p| p.left.isFullyResolved() and p.right.isFullyResolved(),
        };
    }

    pub fn assertLit(x: *const Sexpr) void {
        std.debug.assert(x.isFullyResolved());
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

    pub fn isPair(this: *const Sexpr) bool {
        return switch (this.*) {
            .pair => true,
            else => false,
        };
    }

    pub fn fromBool(b: bool) *const Sexpr {
        return if (b) &Sexpr.true else &Sexpr.false;
    }

    pub fn getAt(this: *const Sexpr, address: SexprAddress) ?*const Sexpr {
        var res = this;
        for (address) |item| {
            res = switch (res.*) {
                .pair => |p| switch (item) {
                    .left => p.left,
                    .right => p.right,
                },
                else => return null,
            };
        }
        return res;
    }

    pub fn setAt(this: *const Sexpr, mem: *VeryPermamentGameStuff, address: SexprAddress, value: *const Sexpr) !*const Sexpr {
        if (address.len == 0) {
            return value;
        } else {
            const first = address[0];
            const rest = address[1..];
            switch (this.*) {
                .pair => |p| {
                    switch (first) {
                        .left => {
                            const new_left = try p.left.setAt(mem, rest, value);
                            return mem.storeSexpr(Sexpr.doPair(new_left, p.right));
                        },
                        .right => {
                            const new_right = try p.right.setAt(mem, rest, value);
                            return mem.storeSexpr(Sexpr.doPair(p.left, new_right));
                        },
                    }
                },
                else => return error.BAD_INPUT,
            }
        }
    }

    pub fn changeAllVariablesToNil(self: *const Sexpr, mem: *VeryPermamentGameStuff) !*const Sexpr {
        return switch (self.*) {
            .atom_lit => self,
            .atom_var => &Sexpr.nil,
            .pair => |p| if (p.left.isFullyResolved() and p.right.isFullyResolved())
                self
            else
                try mem.storeSexpr(Sexpr.doPair(
                    try p.left.changeAllVariablesToNil(mem),
                    try p.right.changeAllVariablesToNil(mem),
                )),
        };
    }

    pub fn format(value: *const Sexpr, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: std.io.AnyWriter) !void {
        std.debug.assert(std.mem.eql(u8, fmt, ""));
        std.debug.assert(std.meta.eql(options, .{}));
        switch (value.*) {
            .atom_lit => |a| try writer.writeAll(a.value),
            .atom_var => |a| {
                try writer.writeAll("@");
                try writer.writeAll(a.value);
            },
            .pair => |p| {
                try writer.writeAll("(");
                try p.left.format("", options, writer);
                var rest = p.right;
                while (rest.isPair()) {
                    try writer.writeAll(" ");
                    try rest.pair.left.format("", options, writer);
                    rest = rest.pair.right;
                }
                if (!rest.equals(&Sexpr.nil)) {
                    try writer.writeAll(" . ");
                    try rest.format("", options, writer);
                }
                try writer.writeAll(")");
            },
        }
    }
};

// TODO: make a proper struct
pub const emptySexprAddress: SexprAddress = &.{};
pub const SexprAddressItem = enum { left, right };
pub const SexprAddress = []SexprAddressItem;
pub fn equalSexprAddress(a: SexprAddress, b: SexprAddress) bool {
    return std.mem.eql(SexprAddressItem, a, b);
}

pub const CaseAddress = []usize;
pub const FullAddress = struct {
    case_address: CaseAddress,
    sexpr_address: SexprAddress,
    which: enum { pattern, template, fnk_name },

    pub fn equals(this: FullAddress, other: FullAddress) bool {
        return this.which == other.which and
            std.mem.eql(usize, this.case_address, other.case_address) and
            equalSexprAddress(this.sexpr_address, other.sexpr_address);
    }
};

pub const Fnk = struct {
    name: *const Sexpr,
    body: FnkBody,

    pub fn format(value: Fnk, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: std.io.AnyWriter) !void {
        std.debug.assert(std.mem.eql(u8, fmt, ""));
        std.debug.assert(std.meta.eql(options, .{}));
        try writer.print("{any} {{\n{any}}}", .{ value.name, value.body });
    }
};
pub const FnkBody = struct {
    cases: MatchCases,
    pub fn format(value: FnkBody, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: std.io.AnyWriter) !void {
        std.debug.assert(std.mem.eql(u8, fmt, ""));
        std.debug.assert(std.meta.eql(options, .{}));
        for (value.cases.items) |case| {
            try writer.print("{any}\n", .{case});
        }
    }
};
pub const MatchCases = std.ArrayListUnmanaged(MatchCaseDefinition);
pub const MatchCaseDefinition = struct {
    pattern: *const Sexpr,
    fnk_name: *const Sexpr,
    template: *const Sexpr,
    next: ?MatchCases,

    pub fn format(value: MatchCaseDefinition, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: std.io.AnyWriter) !void {
        std.debug.assert(std.mem.eql(u8, fmt, ""));
        std.debug.assert(std.meta.eql(options, .{}));
        if (value.fnk_name.equals(&Sexpr.identity)) {
            try writer.print("{any} -> {any}", .{ value.pattern, value.template });
        } else {
            try writer.print("{any} -> {any}: {any}", .{ value.pattern, value.fnk_name, value.template });
        }
        if (value.next) |next| {
            try writer.print(" {{\n{any}}}", .{FnkBody{ .cases = next }});
        } else {
            try writer.writeAll(";");
        }
    }
};

const Binding = struct {
    name: []const u8,
    value: *const Sexpr,
};
const Bindings = std.ArrayList(Binding);

const SexprContext = struct {
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
};
const FnkSet = std.ArrayHashMap(*const Sexpr, void, SexprContext, true);
pub const FnkCollection = std.ArrayHashMap(*const Sexpr, FnkBody, SexprContext, true);

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

fn fnkSize(fnk: FnkBody) usize {
    var res: usize = 0;
    for (fnk.cases.items) |case| {
        res += 1;
        if (case.next) |next| {
            res += fnkSize(.{ .cases = next });
        }
    }
    return res;
}

pub const VeryPermamentGameStuff = struct {
    pool_for_sexprs: MemoryPool(Sexpr),
    arena_for_cases: std.heap.ArenaAllocator,
    arena_for_bindings: std.heap.ArenaAllocator,
    allocator_for_stack: std.mem.Allocator,
    gpa: std.mem.Allocator,

    pub fn init(
        allocator: std.mem.Allocator,
    ) VeryPermamentGameStuff {
        const pool_for_sexprs = MemoryPool(Sexpr).init(allocator);
        const arena_for_cases = std.heap.ArenaAllocator.init(allocator);
        const arena_for_bindings = std.heap.ArenaAllocator.init(allocator);

        return VeryPermamentGameStuff{
            .pool_for_sexprs = pool_for_sexprs,
            .arena_for_cases = arena_for_cases,
            .arena_for_bindings = arena_for_bindings,
            .allocator_for_stack = allocator,
            .gpa = allocator,
        };
    }

    pub fn deinit(this: *VeryPermamentGameStuff) void {
        this.pool_for_sexprs.deinit();
        this.arena_for_cases.deinit();
        this.arena_for_bindings.deinit();
    }

    pub fn storeSexpr(this: *VeryPermamentGameStuff, s: Sexpr) !*const Sexpr {
        const res = try this.pool_for_sexprs.create();
        res.* = s;
        return res;
    }
};

pub const ScoringRun = struct {
    mem: *VeryPermamentGameStuff,
    all_fnks: FnkCollection,
    used_fnks: FnkSet,
    score: struct {
        code_size: usize,
        compile_time: usize,
    },

    pub fn initFromFnks(
        all_fnks: FnkCollection,
        mem: *VeryPermamentGameStuff,
    ) !ScoringRun {
        const used_fnks = FnkSet.init(mem.gpa);

        return ScoringRun{
            .mem = mem,
            .used_fnks = used_fnks,
            .all_fnks = all_fnks,
            .score = .{ .code_size = 0, .compile_time = 0 },
        };
    }

    pub fn init(
        all_fnks_raw: []const u8,
        mem: *VeryPermamentGameStuff,
    ) !ScoringRun {
        var all_fnks = FnkCollection.init(mem.gpa);
        var parser = parsing.Parser{ .remaining_text = all_fnks_raw };
        try parser.parseFnkCollection(&all_fnks, &mem.pool_for_sexprs, mem.arena_for_cases.allocator());

        return ScoringRun.initFromFnks(all_fnks, mem);
    }

    pub fn deinit(this: *ScoringRun) void {
        this.used_fnks.deinit();
        this.all_fnks.deinit();
    }

    fn findFunktion(this: *ScoringRun, name: *const Sexpr) error{
        OutOfMemory,
        BAD_INPUT,
        FnkNotFound,
        NoMatchingCase,
        InvalidMetaFnk,
    }!*const FnkBody {
        if (this.all_fnks.getPtr(name)) |fnk| {
            if (this.used_fnks.get(name) == null) {
                try this.used_fnks.put(name, {});
                this.score.code_size += fnkSize(fnk.*);
            }
            return fnk;
        } else switch (name.*) {
            .atom_lit, .atom_var => return error.FnkNotFound,
            .pair => |p| {
                // try to compile it!
                var exec = try ExecutionThread.init(p.right, p.left, this);
                defer exec.deinit();
                const asdf = try exec.getFinalResult(this);
                this.score.compile_time += exec.score.successful_matches;
                const cases = try fnkFromSexpr(asdf, this.mem.arena_for_cases.allocator(), &this.mem.pool_for_sexprs);
                if (DEBUG) {
                    const stderr = std.io.getStdErr().writer();
                    stderr.print("\ncompiled a fnk, the cases are: {any}\n", .{asdf}) catch unreachable;
                }
                try this.all_fnks.put(name, cases);
                return this.all_fnks.getPtr(name).?;
            },
        }
    }
};

pub const StackThing = struct {
    cur_fnk_name: *const Sexpr,
    cur_cases: []const MatchCaseDefinition,
    cur_bindings: Bindings,

    pub fn init(input: *const Sexpr, fn_name: *const Sexpr, scoring_run: *ScoringRun) !union(enum) {
        builtin: *const Sexpr,
        stack_thing: StackThing,
    } {
        inline for (builtin_fnks) |builtin| {
            if (builtin.name.equals(fn_name)) {
                return .{ .builtin = builtin.fnk(input) };
            }
        }

        const bindings = std.ArrayList(Binding).init(scoring_run.mem.arena_for_bindings.allocator());
        const cases = (try scoring_run.findFunktion(fn_name)).*.cases.items;
        return .{ .stack_thing = StackThing{
            .cur_bindings = bindings,
            .cur_cases = cases,
            .cur_fnk_name = fn_name,
        } };
    }

    pub fn deinit(this: *StackThing) void {
        // _ = this; // autofix
        this.cur_bindings.deinit();
    }
};

pub const ExecutionThread = struct {
    active_value: *const Sexpr,
    stack: std.ArrayList(StackThing),

    score: struct {
        successful_matches: usize,
        max_stack: usize,
    },

    // TODO: remove at comptime in cli app
    last_visual_state: VisualState = .just_started,
    pub const VisualState = union(enum) {
        just_started,
        failed_to_match: MatchCaseDefinition,
        matched: struct {
            tail_optimized: bool,
            added_new_fnk_to_stack: bool,
            case: MatchCaseDefinition,
            old_active_value: *const Sexpr,
            // TODO: redundant if tail_optimized = false
            old_fnk_name: *const Sexpr,
            discarded_cases: []const MatchCaseDefinition,
        },
        ended: *const Sexpr,
    };

    pub fn init(
        input: *const Sexpr,
        fn_name: *const Sexpr,
        scoring_run: *ScoringRun,
    ) !ExecutionThread {
        var stack = std.ArrayList(StackThing).init(scoring_run.mem.allocator_for_stack);
        switch (try StackThing.init(input, fn_name, scoring_run)) {
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
        scoring_run: *ScoringRun,
    ) !ExecutionThread {
        var permanent_stuff = scoring_run.mem;
        const fn_name = try parsing.parseSingleSexpr(fn_name_raw, &permanent_stuff.pool_for_sexprs);
        const input = try parsing.parseSingleSexpr(input_raw, &permanent_stuff.pool_for_sexprs);
        return ExecutionThread.init(input, fn_name, scoring_run);
    }

    // TODO: remove duplication, maybe
    pub fn advanceTinyStep(this: *ExecutionThread, scoring_run: *ScoringRun) !?*const Sexpr {
        var permanent_stuff = scoring_run.mem;
        if (this.stack.items.len > 0) {
            const last_stack_ptr: *StackThing = &this.stack.items[this.stack.items.len - 1];
            const initial_bindings_count = last_stack_ptr.cur_bindings.items.len;

            if (last_stack_ptr.cur_cases.len == 0) return error.NoMatchingCase;

            const case = last_stack_ptr.cur_cases[0];
            const rest_of_cases = last_stack_ptr.cur_cases[1..];

            std.log.debug("case fnk: {any}", .{case.fnk_name});

            if (!(try generateBindings(case.pattern, this.active_value, &last_stack_ptr.cur_bindings))) {
                undoLastBindings(&last_stack_ptr.cur_bindings, initial_bindings_count);
                last_stack_ptr.cur_cases = rest_of_cases;
                this.last_visual_state = .{ .failed_to_match = case };
                return null;
            }

            const old_fnk_name = last_stack_ptr.cur_fnk_name;
            const old_active_value = this.active_value;
            const argument = try fillTemplate(case.template, last_stack_ptr.cur_bindings, &permanent_stuff.pool_for_sexprs);
            this.active_value = argument;

            var tail_optimized: bool = undefined;
            if (case.next) |next| {
                last_stack_ptr.cur_cases = next.items;
                tail_optimized = false;
            } else {
                _ = this.stack.pop();
                tail_optimized = true;
            }

            this.score.successful_matches += 1;

            var added_new_fnk_to_stack: bool = undefined;
            const new_thing = try StackThing.init(this.active_value, case.fnk_name, scoring_run);
            switch (new_thing) {
                .stack_thing => |x| {
                    try this.stack.append(x);
                    this.score.max_stack = @max(this.score.max_stack, this.stack.items.len);
                    added_new_fnk_to_stack = true;
                },
                .builtin => |r| {
                    this.active_value = r;
                    added_new_fnk_to_stack = false;
                },
            }

            this.last_visual_state = .{ .matched = .{
                .tail_optimized = tail_optimized,
                .added_new_fnk_to_stack = added_new_fnk_to_stack,
                .case = case,
                .old_active_value = old_active_value,
                .old_fnk_name = old_fnk_name,
                .discarded_cases = rest_of_cases,
            } };

            return null;
        } else {
            this.last_visual_state = .{ .ended = this.active_value };
            return this.active_value;
        }
    }

    pub fn advanceStep(this: *ExecutionThread, scoring_run: *ScoringRun) !?*const Sexpr {
        var permanent_stuff = scoring_run.mem;
        if (this.stack.items.len > 0) {
            const last_stack_ptr: *StackThing = &this.stack.items[this.stack.items.len - 1];
            const initial_bindings_count = last_stack_ptr.cur_bindings.items.len;
            for (last_stack_ptr.cur_cases) |case| {
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

                const new_thing = try StackThing.init(this.active_value, case.fnk_name, scoring_run);
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
            return error.NoMatchingCase;
        } else {
            return this.active_value;
        }
    }

    pub fn getFinalResult(this: *ExecutionThread, scoring_run: *ScoringRun) !*const Sexpr {
        while (true) {
            if (try this.advanceStep(scoring_run)) |res| {
                return res;
            }
        }
    }

    pub fn deinit(this: *ExecutionThread) void {
        this.stack.deinit();
    }
};

const SingleRunHelper = struct {
    permanent_stuff: VeryPermamentGameStuff,
    scoring_run: ScoringRun,
    execution: ExecutionThread,

    pub fn init(
        result: *SingleRunHelper,
        input_raw: []const u8,
        fn_name_raw: []const u8,
        all_fnks_raw: []const u8,
        allocator: std.mem.Allocator,
    ) !void {
        result.permanent_stuff = VeryPermamentGameStuff.init(allocator);
        errdefer result.permanent_stuff.deinit();
        result.scoring_run = try ScoringRun.init(all_fnks_raw, &result.permanent_stuff);
        errdefer result.scoring_run.deinit();

        const fn_name = try parsing.parseSingleSexpr(fn_name_raw, &result.permanent_stuff.pool_for_sexprs);
        const input = try parsing.parseSingleSexpr(input_raw, &result.permanent_stuff.pool_for_sexprs);
        result.execution = try ExecutionThread.init(input, fn_name, &result.scoring_run);
    }

    pub fn deinit(this: *SingleRunHelper) void {
        this.permanent_stuff.deinit();
        this.scoring_run.deinit();
        this.execution.deinit();
    }

    pub fn getFinalResult(this: *SingleRunHelper) !*const Sexpr {
        return this.execution.getFinalResult(&this.scoring_run);
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
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    std.debug.assert(args.skip());

    const verb = args.next() orelse "help";

    if (std.mem.eql(u8, verb, "run") or std.mem.eql(u8, verb, "debug")) {
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
        if (std.mem.eql(u8, verb, "run")) {
            var game: SingleRunHelper = undefined;
            SingleRunHelper.init(&game, input_raw, fn_name_raw, fnks_collection_raw, allocator) catch |err| switch (err) {
                error.BAD_INPUT => {
                    try stdout.print("Bad grammar somewhere!\n", .{});
                    return 1;
                },
                else => return err,
            };
            defer game.deinit();
            const result = game.getFinalResult() catch |err| switch (err) {
                error.BAD_INPUT => {
                    try stdout.print("Found bad input during execution!\n", .{});
                    return 1;
                },
                else => return err,
            };
            try stdout.print("result: {any}\n", .{result});
        } else {
            var mem = VeryPermamentGameStuff.init(allocator);
            defer mem.deinit();
            var scoring_run = try ScoringRun.init(fnks_collection_raw, &mem);
            defer scoring_run.deinit();
            var exec = try ExecutionThread.initFromText(input_raw, fn_name_raw, &scoring_run);
            defer exec.deinit();

            // TODO: show already bound vars
            var step: usize = 0;
            while (exec.stack.items.len > 0) {
                try stdout.print("Step {d}:\n", .{step});
                for (exec.stack.items, 0..) |stack, k| {
                    try stdout.print("{d}: {any}\n", .{ k, stack.cur_fnk_name });
                }
                const last_thing = exec.stack.getLast();
                try stdout.print("matching {any} with\n", .{exec.active_value});
                for (last_thing.cur_cases) |case| {
                    try stdout.print("\t{any} -> {any}: {any}{s}\n", .{
                        case.pattern,
                        case.fnk_name,
                        case.template,
                        if (case.next == null) ";" else " { ... }",
                    });
                }
                try stdout.print("\n", .{});
                _ = try exec.advanceStep(&scoring_run);
                step += 1;
            }
            // try stdout.print("{s}\n", .{"-" ** 10});
            try stdout.print("final result: {any}\n", .{exec.active_value});
        }
    } else if (std.mem.eql(u8, verb, "score")) {
        var n_correct: u32 = 0;
        var n_total: u32 = 0;

        const player_fnks_collection_raw: []const u8 = blk: {
            const filename = args.next().?;
            const file = try std.fs.cwd().openFile(filename, .{});
            defer file.close();
            break :blk try file.readToEndAlloc(allocator, std.math.maxInt(usize));
        };
        defer allocator.free(player_fnks_collection_raw);

        const target_fnks_collection_raw: []const u8 = blk: {
            const filename = args.next().?;
            const file = try std.fs.cwd().openFile(filename, .{});
            defer file.close();
            break :blk try file.readToEndAlloc(allocator, std.math.maxInt(usize));
        };
        defer allocator.free(target_fnks_collection_raw);

        var mem = VeryPermamentGameStuff.init(allocator);
        defer mem.deinit();
        var player_score = try ScoringRun.init(player_fnks_collection_raw, &mem);
        defer player_score.deinit();
        var target_score = try ScoringRun.init(target_fnks_collection_raw, &mem);
        defer target_score.deinit();

        var it = target_score.all_fnks.iterator();
        while (it.next()) |x| {
            const fnk_name = x.key_ptr.*;
            const fnk_body = x.value_ptr.*;

            var result: union(enum) {
                target_fnk_not_defined: void,
                fnk_not_found: struct {
                    input: *const Sexpr,
                },
                ran_out_of_cases: struct {
                    input: *const Sexpr,
                },
                bad_result: struct {
                    input: *const Sexpr,
                    expected: *const Sexpr,
                    actual: *const Sexpr,
                },
                score: struct {
                    time: usize,
                    max_stack: usize,
                },
            } = .{ .score = .{ .time = 0, .max_stack = 0 } };
            for (fnk_body.cases.items) |case| {
                std.debug.assert(case.fnk_name.equals(&Sexpr.identity));
                std.debug.assert(case.next == null);
                Sexpr.assertLit(case.pattern);
                Sexpr.assertLit(case.template);
                const cur_input = case.pattern;
                const expected_output = case.template;

                var exec = ExecutionThread.init(cur_input, fnk_name, &player_score) catch |err| switch (err) {
                    error.FnkNotFound => {
                        result = .{ .target_fnk_not_defined = {} };
                        break;
                    },
                    // TODO: good error messages for everything
                    else => return err,
                };
                defer exec.deinit();

                const actual_output = exec.getFinalResult(&player_score) catch |err| switch (err) {
                    error.FnkNotFound => {
                        result = .{ .fnk_not_found = .{ .input = cur_input } };
                        break;
                    },
                    error.NoMatchingCase => {
                        result = .{ .ran_out_of_cases = .{ .input = cur_input } };
                        break;
                    },
                    error.OutOfMemory => return err,
                    // TODO: good error messages for everything
                    else => return err,
                };
                if (!actual_output.equals(expected_output)) {
                    result = .{ .bad_result = .{ .input = cur_input, .expected = expected_output, .actual = actual_output } };
                    break;
                } else {
                    result.score.time += exec.score.successful_matches;
                    result.score.max_stack = @max(result.score.max_stack, exec.score.max_stack);
                }
            }
            switch (result) {
                .target_fnk_not_defined => {
                    try stdout.print("no fnk named {any} in the solutions file\n", .{fnk_name});
                },
                .fnk_not_found => |f| {
                    try stdout.print("tried to call an invalid fnk when applying fnk {any} to input {any}\n", .{ fnk_name, f.input });
                },
                .ran_out_of_cases => |f| {
                    try stdout.print("ran out of cases when applying fnk {any} to input {any}\n", .{ fnk_name, f.input });
                },
                .bad_result => |f| {
                    try stdout.print("failed fnk {any}:\texpected {any} for input {any}, got {any}\n", .{ fnk_name, f.expected, f.input, f.actual });
                },
                .score => |s| {
                    const fnk_name_str = try std.fmt.allocPrint(allocator, "fnk {any}:", .{fnk_name});
                    defer allocator.free(fnk_name_str);
                    const rest = try std.fmt.allocPrint(allocator, "max stack {d}, required time {d}", .{ s.max_stack, s.time });
                    defer allocator.free(rest);
                    try stdout.print("{s: <40}{s}\n", .{ fnk_name_str, rest });
                    // try stdout.print("fnk {s}:\tmax stack {d}, required time {d}\n", .{ fnk_name_str, s.max_stack, s.time });
                    n_correct += 1;
                },
            }
            n_total += 1;
        }
        try stdout.print("global stats: code size {d}, compile time {d}, {d}/{d} correct\n", .{ player_score.score.code_size, player_score.score.compile_time, n_correct, n_total });
        return if (n_correct == n_total) 0 else 1;
    } else if (std.mem.eql(u8, verb, "see-fnk")) {
        const fnks_collection_raw: []const u8 = blk: {
            const filename = args.next().?;
            const file = try std.fs.cwd().openFile(filename, .{});
            defer file.close();
            break :blk try file.readToEndAlloc(allocator, std.math.maxInt(usize));
        };
        defer allocator.free(fnks_collection_raw);

        const fn_name_raw: []const u8 = args.next().?;
        std.debug.assert(!args.skip());

        var mem = VeryPermamentGameStuff.init(allocator);
        defer mem.deinit();
        var run = try ScoringRun.init(fnks_collection_raw, &mem);
        defer run.deinit();
        const fn_name = try parsing.parseSingleSexpr(fn_name_raw, &mem.pool_for_sexprs);
        const result = try run.findFunktion(fn_name);
        try stdout.print("{any}\n", .{Fnk{ .name = fn_name, .body = result.* }});
    } else {
        try stdout.print(
            \\  valid commands:
            \\      run [fnk-lib] [fnk-name] [input]
            \\      run [fnk-lib] [fnk-name] file [input-filename]
            \\      run [fnk-lib] [fnk-name] file raw [input-filename]
            \\      score [my-fnk-lib] [target-fnk-lib]
            \\      debug [fnk-lib] [fnk-name] [input]
            \\      see-fnk [fnk-lib] [fnk-name]
        , .{});
        return 1;
    }
    return 0;
}

test "main test" {
    var game: SingleRunHelper = undefined;
    try SingleRunHelper.init(&game, "input", "fn_name",
        \\  fn_name {
        \\      input -> output;    
        \\  }
    , std.testing.allocator);
    defer game.deinit();

    const actual = try game.getFinalResult();
    const expected = Sexpr.doLit("output");
    try expectEqualSexprs(&expected, actual);

    var exec = try ExecutionThread.init(&Sexpr.doLit("input"), &Sexpr.doLit("fn_name"), &game.scoring_run);
    defer exec.deinit();
    try expectEqualSexprs(&expected, try exec.getFinalResult(&game.scoring_run));
}

test "with comptime" {
    var game: SingleRunHelper = undefined;
    try SingleRunHelper.init(&game, "2", "stuff",
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
    const expected = Sexpr.doLit("c");
    // const expected = Sexpr{ .atom_lit = .{ .value = "output" } };
    try expectEqualSexprs(&expected, actual);
}

test "apply another nested fnk, with ExecutionState" {
    var game: SingleRunHelper = undefined;
    try SingleRunHelper.init(&game, "(1)", "stuff",
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
    var mem = VeryPermamentGameStuff.init(std.testing.allocator);
    defer mem.deinit();
    var scoring = try ScoringRun.init(
        \\ 
        \\ bubbleUp {
        \\      (X . @rest) -> (X . @rest);
        \\      (@a . @b) -> bubbleUp: @b {
        \\          (X . @rest) -> (X @a . @rest);
        \\      }
        \\ }
    , &mem);
    defer scoring.deinit();

    var exec = try ExecutionThread.initFromText("(a b X c d)", "bubbleUp", &scoring);
    defer exec.deinit();

    const expected = Sexpr.doPair(&Sexpr.doLit("X"), &Sexpr.doPair(
        &Sexpr.doLit("a"),
        &Sexpr.doPair(&Sexpr.doLit("b"), &Sexpr.doPair(
            &Sexpr.doLit("c"),
            &Sexpr.doPair(
                &Sexpr.doLit("d"),
                &Sexpr.nil,
            ),
        )),
    ));

    const actual = try exec.getFinalResult(&scoring);
    try expectEqualSexprs(&expected, actual);

    try std.testing.expectEqual(3, exec.score.max_stack);
    try std.testing.expectEqual(5, exec.score.successful_matches);
}

test "scoring with comptime" {
    var mem = VeryPermamentGameStuff.init(std.testing.allocator);
    defer mem.deinit();
    var scoring = try ScoringRun.init(
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
    , &mem);
    defer scoring.deinit();

    var exec = try ExecutionThread.initFromText("2", "stuff", &scoring);
    defer exec.deinit();

    const expected = Sexpr.doLit("c");

    const actual = try exec.getFinalResult(&scoring);
    try expectEqualSexprs(&expected, actual);

    try std.testing.expectEqual(1, exec.score.max_stack);
    try std.testing.expectEqual(2, exec.score.successful_matches);

    try std.testing.expectEqual(4, scoring.score.code_size);
    try std.testing.expectEqual(9, scoring.score.compile_time);
}

fn expectEqualSexprs(expected: *const Sexpr, actual: *const Sexpr) !void {
    switch (expected.*) {
        .atom_lit => |expected_atom| switch (actual.*) {
            .atom_lit => |actual_atom| {
                return std.testing.expectEqualStrings(expected_atom.value, actual_atom.value);
            },
            .atom_var => |actual_atom| {
                std.log.err("expected literal '{s}' but found variable '{s}'\n", .{ expected_atom.value, actual_atom.value });
                return error.TestExpectedEqual;
            },
            .pair => |actual_pair| {
                std.log.err("expected literal '{s}' but found a pair {any}\n", .{ expected_atom.value, actual_pair });
                return error.TestExpectedEqual;
            },
        },
        .atom_var => |expected_atom| switch (actual.*) {
            .atom_lit => |actual_atom| {
                std.log.err("expected variable '{s}' but found literal '{s}'\n", .{ expected_atom.value, actual_atom.value });
                return error.TestExpectedEqual;
            },
            .atom_var => |actual_atom| {
                return std.testing.expectEqualStrings(expected_atom.value, actual_atom.value);
            },
            .pair => |actual_pair| {
                std.log.err("expected variable '{s}' but found a pair {any}\n", .{ expected_atom.value, actual_pair });
                return error.TestExpectedEqual;
            },
        },
        .pair => |expected_pair| switch (actual.*) {
            .atom_lit => |actual_atom| {
                std.log.err("expected pair but found literal '{s}'\n", .{actual_atom.value});
                return error.TestExpectedEqual;
            },
            .atom_var => |actual_atom| {
                std.log.err("expected pair but found literal '{s}'\n", .{actual_atom.value});
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
        stderr.print("\nGenerating bindings for pattern {any} and value {any}\n", .{ pattern, value }) catch unreachable;

        // stderr.print("cur bindings are:\n", .{}) catch unreachable;
        // for (new_bindings.items) |binding| {
        //     stderr.print("name: {s}, value: {any}\n", .{binding.name, binding.value}) catch unreachable;
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

fn asListPlusSentinel(s: *const Sexpr, l: *std.ArrayList(*const Sexpr)) !*const Sexpr {
    switch (s.*) {
        .atom_lit, .atom_var => return s,
        .pair => |p| {
            try l.append(p.left);
            return try asListPlusSentinel(p.right, l);
        },
    }
}

fn fnkFromSexpr(s: *const Sexpr, allocator_for_cases: std.mem.Allocator, pool: *MemoryPool(Sexpr)) !FnkBody {
    return .{ .cases = (try fnkFromSexprHelper(s, allocator_for_cases, pool)).? };
}

fn fnkFromSexprHelper(s: *const Sexpr, arena: std.mem.Allocator, pool: *MemoryPool(Sexpr)) !?MatchCases {
    var cases = std.ArrayListUnmanaged(MatchCaseDefinition){};
    switch (s.*) {
        .atom_lit => return if (s.equals(&Sexpr.@"return")) null else error.InvalidMetaFnk,
        .atom_var => return error.BAD_INPUT,
        .pair => |p| {
            var cur_parent = p;
            while (true) {
                const cur = cur_parent.left;
                const pattern = try internalFromExternal(cur.pair.left, pool);
                const fnk_name = cur.pair.right.pair.left;
                const template = try internalFromExternal(cur.pair.right.pair.right.pair.left, pool);
                const next = try fnkFromSexprHelper(cur.pair.right.pair.right.pair.right, arena, pool);
                try cases.append(arena, .{
                    .pattern = pattern,
                    .fnk_name = fnk_name,
                    .template = template,
                    .next = next,
                });
                switch (cur_parent.right.*) {
                    .atom_lit => |a| {
                        if (a.equals(Sexpr.nil.atom_lit)) {
                            break;
                        } else {
                            return error.InvalidMetaFnk;
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
        .atom_var, .atom_lit => return error.InvalidMetaFnk,
        .pair => |p| {
            if (p.left.equals(&Sexpr.atom)) {
                return p.right;
            } else if (p.left.equals(&Sexpr.@"var")) {
                switch (p.right.*) {
                    .pair => return error.InvalidMetaFnk,
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
