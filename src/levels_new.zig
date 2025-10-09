const presenter = @import("presenter.zig");
const funk = @import("kommon").funktional;
const BuiltinLevel = presenter.BuiltinLevel;
const Sample = presenter.Sample;
const core = @import("main.zig");
const Sexpr = core.Sexpr;
const std = @import("std");
const assert = std.debug.assert;

const SexprPool = std.heap.MemoryPool(Sexpr);

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
        if (in.equals(Vals.lowercase[0]) or in.equals(Vals.lowercase[4]) or
            in.equals(Vals.uppercase[0]) or in.equals(Vals.uppercase[4]))
        {
            return Sexpr.builtin.true;
        } else return Sexpr.builtin.false;
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
                var random_instance: std.Random.DefaultPrng = .init(@intCast(k));
                const random = random_instance.random();
                const left = try Vals.randomSexpr(pool, &(Vals.lowercase ++ Vals.uppercase), random, 4);
                const right = try Vals.randomSexpr(pool, &(Vals.lowercase ++ Vals.uppercase), random, 4);
                if (k < 100) {
                    return .{
                        .input = try store(pool, Sexpr.doPair(left, right)),
                        .output = try store(pool, Sexpr.doPair(right, left)),
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

const Vals = struct {
    pub const lowercase: [6]*const Sexpr = .{
        &Sexpr.doLit("a"),
        &Sexpr.doLit("b"),
        &Sexpr.doLit("c"),
        &Sexpr.doLit("d"),
        &Sexpr.doLit("e"),
        &Sexpr.doLit("f"),
    };
    pub const uppercase: [6]*const Sexpr = .{
        &Sexpr.doLit("A"),
        &Sexpr.doLit("B"),
        &Sexpr.doLit("C"),
        &Sexpr.doLit("D"),
        &Sexpr.doLit("E"),
        &Sexpr.doLit("F"),
    };

    fn randomSexpr(pool: *SexprPool, atoms: []const *const Sexpr, random: std.Random, max_depth: usize) !*const Sexpr {
        if (max_depth == 0 or random.float(f32) < 0.3) {
            return atoms[random.uintLessThan(usize, atoms.len)];
        } else {
            return try store(pool, Sexpr.doPair(
                try randomSexpr(pool, atoms, random, max_depth - 1),
                try randomSexpr(pool, atoms, random, max_depth - 1),
            ));
        }
    }
};
