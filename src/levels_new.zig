const presenter = @import("presenter.zig");
const funk = @import("kommon").funktional;
const BuiltinLevel = presenter.BuiltinLevel;
const Sample = presenter.Sample;
const core = @import("main.zig");
const Sexpr = core.Sexpr;
const std = @import("std");
const assert = std.debug.assert;

const SexprPool = std.heap.MemoryPool(Sexpr);
const kommon = @import("kommon");

fn safeAt(arr: []const *const Sexpr, index: usize) ?*const Sexpr {
    return kommon.safeAt(*const Sexpr, arr, index);
}

pub const Level = struct {
    fnk_name: *const Sexpr,
    generate_sample: *const fn (k: usize, pool: *SexprPool, arena: std.mem.Allocator) core.OoM!?Sample,

    pub fn samplesIterator(level: Level) SamplesIterator {
        return .{ .k = 0, .level = level };
    }

    pub const SamplesIterator = struct {
        k: usize,
        level: Level,

        pub fn next(self: *SamplesIterator, pool: *SexprPool, arena: std.mem.Allocator) !?Sample {
            const result = try self.level.generate_sample(self.k, pool, arena);
            if (result) |r| {
                self.k += 1;
                return r;
            } else return null;
        }
    };
};

const Solutions = struct {
    fn isVowel(in: *const Sexpr) ?*const Sexpr {
        return Sexpr.fromBool(Helpers.isVowel(in));
    }
};

const Helpers = struct {
    fn isVowel(in: *const Sexpr) bool {
        return (in.equals(Vals.lowercase[0]) or in.equals(Vals.lowercase[4]) or
            in.equals(Vals.uppercase[0]) or in.equals(Vals.uppercase[4]));
    }

    fn isB(in: *const Sexpr) bool {
        return in.equals(Vals.uppercase[1]);
    }

    fn hasSomeB(in: *const Sexpr) bool {
        if (in.isPair()) {
            return hasSomeB(in.pair.left) or hasSomeB(in.pair.right);
        } else return isB(in);
    }
};

pub const levels: []const Level = &.{
    .{
        .fnk_name = &Sexpr.doLit("uppercase"),
        .generate_sample = struct {
            fn generate_sample(k: usize, _: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                if (k < Vals.lowercase.len) {
                    return .{ .input = Vals.lowercase[k], .output = Vals.uppercase[k] };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("lowercase"),
        .generate_sample = struct {
            fn generate_sample(k: usize, _: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                if (k < Vals.lowercase.len) {
                    return .{ .input = Vals.uppercase[k], .output = Vals.lowercase[k] };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("isVowel"),
        .generate_sample = struct {
            fn generate_sample(k: usize, _: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                const both = Vals.lowercase ++ Vals.uppercase;
                if (k < both.len) {
                    return .{
                        .input = both[k],
                        .output = Solutions.isVowel(both[k]),
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("swap"),
        .generate_sample = struct {
            fn generate_sample(k: usize, pool: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                if (k < 100) {
                    var random_instance: std.Random.DefaultPrng = .init(@intCast(k));
                    const random = random_instance.random();
                    const left = try randomSexpr(pool, &(Vals.lowercase ++ Vals.uppercase), random, 4);
                    const right = try randomSexpr(pool, &(Vals.lowercase ++ Vals.uppercase), random, 4);
                    return .{
                        .input = try store(pool, Sexpr.doPair(left, right)),
                        .output = try store(pool, Sexpr.doPair(right, left)),
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("firstAsUppercase"),
        .generate_sample = struct {
            fn generate_sample(k: usize, pool: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                const k1 = @mod(k, Vals.lowercase.len);
                const k2 = @divFloor(k, Vals.lowercase.len);
                if (k2 < Vals.lowercase.len) {
                    return .{
                        .input = try store(pool, Sexpr.doPair(Vals.lowercase[k1], Vals.lowercase[k2])),
                        .output = Vals.uppercase[k1],
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("uppercaseIfVowel"),
        .generate_sample = struct {
            fn generate_sample(k: usize, _: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                if (k < Vals.lowercase.len) {
                    const in = Vals.lowercase[k];
                    return .{
                        .input = in,
                        .output = if (Helpers.isVowel(in))
                            Vals.uppercase[k]
                        else
                            in,
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("pairToUppercase"),
        .generate_sample = struct {
            fn generate_sample(k: usize, pool: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                const k1 = @mod(k, Vals.lowercase.len);
                const k2 = @divFloor(k, Vals.lowercase.len);
                if (k2 < Vals.lowercase.len) {
                    return .{
                        .input = try store(pool, Sexpr.doPair(Vals.lowercase[k1], Vals.lowercase[k2])),
                        .output = try store(pool, Sexpr.doPair(Vals.uppercase[k1], Vals.uppercase[k2])),
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("changeCase"),
        .generate_sample = struct {
            fn generate_sample(k: usize, _: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                if (k < Vals.lowercase.len) {
                    return .{
                        .input = Vals.lowercase[k],
                        .output = Vals.uppercase[k],
                    };
                } else if (k < Vals.lowercase.len * 2) {
                    return .{
                        .input = Vals.uppercase[k - Vals.lowercase.len],
                        .output = Vals.lowercase[k - Vals.lowercase.len],
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("letterToBothCases"),
        .generate_sample = struct {
            fn generate_sample(k: usize, pool: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                if (k < Vals.lowercase.len) {
                    return .{
                        .input = Vals.lowercase[k],
                        .output = try store(pool, Sexpr.doPair(Vals.lowercase[k], Vals.uppercase[k])),
                    };
                } else if (k < Vals.lowercase.len * 2) {
                    const k2 = k - Vals.lowercase.len;
                    return .{
                        .input = Vals.uppercase[k2],
                        .output = try store(pool, Sexpr.doPair(Vals.lowercase[k2], Vals.uppercase[k2])),
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("hasSomeB"),
        .generate_sample = struct {
            fn generate_sample(k: usize, pool: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                if (k < 100) {
                    var random_instance: std.Random.DefaultPrng = .init(@intCast(k));
                    const random = random_instance.random();
                    const input = try randomSexpr(pool, &Vals.uppercase, random, 5);
                    return .{
                        .input = input,
                        .output = Sexpr.fromBool(Helpers.hasSomeB(input)),
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("second"),
        .generate_sample = struct {
            fn generate_sample(k: usize, pool: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                if (k < 100) {
                    var random_instance: std.Random.DefaultPrng = .init(@intCast(k));
                    const random = random_instance.random();
                    const first = randomChoice(&Vals.lowercase, random);
                    const second = randomChoice(&Vals.lowercase, random);
                    const rest = try randomList(pool, &Vals.lowercase, random, random.intRangeAtMost(usize, 0, 7));
                    return .{
                        .input = try toListWithSentinel(pool, &.{ first, second }, rest),
                        .output = second,
                    };
                } else return null;
            }
        }.generate_sample,
    },
};

fn store(pool: *SexprPool, s: Sexpr) !*const Sexpr {
    const res = try pool.create();
    res.* = s;
    return res;
}

fn toListWithSentinel(pool: *SexprPool, items: []const *const Sexpr, sentinel: *const Sexpr) !*const Sexpr {
    var result = sentinel;
    for (0..items.len) |k| {
        result = try store(pool, Sexpr.doPair(items[items.len - 1 - k], result));
    }
    return result;
}

fn randomList(pool: *SexprPool, options: []const *const Sexpr, random: std.Random, len: usize) !*const Sexpr {
    if (len == 0) {
        return Sexpr.builtin.nil;
    } else {
        const first = randomChoice(options, random);
        const rest = try randomList(pool, options, random, len - 1);
        return try store(pool, Sexpr.doPair(first, rest));
    }
}

fn randomChoice(options: []const *const Sexpr, random: std.Random) *const Sexpr {
    assert(options.len > 0);
    return options[random.uintLessThan(usize, options.len)];
}

fn randomSexpr(pool: *SexprPool, atoms: []const *const Sexpr, random: std.Random, max_depth: usize) !*const Sexpr {
    if (max_depth == 0 or random.float(f32) < 0.3) {
        return randomChoice(atoms, random);
    } else {
        return try store(pool, Sexpr.doPair(
            try randomSexpr(pool, atoms, random, max_depth - 1),
            try randomSexpr(pool, atoms, random, max_depth - 1),
        ));
    }
}

const Vals = struct {
    const lowercase: [6]*const Sexpr = .{
        &Sexpr.doLit("a"),
        &Sexpr.doLit("b"),
        &Sexpr.doLit("c"),
        &Sexpr.doLit("d"),
        &Sexpr.doLit("e"),
        &Sexpr.doLit("f"),
    };
    const uppercase: [6]*const Sexpr = .{
        &Sexpr.doLit("A"),
        &Sexpr.doLit("B"),
        &Sexpr.doLit("C"),
        &Sexpr.doLit("D"),
        &Sexpr.doLit("E"),
        &Sexpr.doLit("F"),
    };
};
