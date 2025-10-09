const presenter = @import("presenter.zig");
const funk = @import("kommon").funktional;
const BuiltinLevel = presenter.BuiltinLevel;
const Sample = presenter.Sample;
const core = @import("main.zig");
const Sexpr = core.Sexpr;
const std = @import("std");
const assert = std.debug.assert;

pub const Level = struct {
    fnk_name: *const Sexpr,
    generate_sample: *const fn (k: usize, pool: *std.heap.MemoryPool(Sexpr), arena: std.mem.Allocator) core.OoM!?Sample,

    pub fn samplesIterator(level: Level) SamplesIterator {
        return .{ .k = 0, .level = level };
    }

    pub const SamplesIterator = struct {
        k: usize,
        level: Level,

        pub fn next(self: *SamplesIterator, pool: *std.heap.MemoryPool(Sexpr), arena: std.mem.Allocator) !?Sample {
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
            fn generate_sample(k: usize, _: *std.heap.MemoryPool(Sexpr), _: std.mem.Allocator) core.OoM!?Sample {
                if (k < Vals.lowercase.len) {
                    return .{ .input = Vals.lowercase[k], .output = Vals.uppercase[k] };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("lowercase"),
        .generate_sample = struct {
            fn generate_sample(k: usize, _: *std.heap.MemoryPool(Sexpr), _: std.mem.Allocator) core.OoM!?Sample {
                if (k < Vals.lowercase.len) {
                    return .{ .input = Vals.uppercase[k], .output = Vals.lowercase[k] };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("isVowel"),
        .generate_sample = struct {
            fn generate_sample(k: usize, _: *std.heap.MemoryPool(Sexpr), _: std.mem.Allocator) core.OoM!?Sample {
                if (k < Vals.lowercase.len) {
                    return .{
                        .input = Vals.lowercase[k],
                        .output = Solutions.isVowel(Vals.lowercase[k]),
                    };
                } else if (k < Vals.lowercase.len * 2) {
                    return .{
                        .input = Vals.uppercase[k - Vals.lowercase.len],
                        .output = Solutions.isVowel(Vals.uppercase[k - Vals.lowercase.len]),
                    };
                } else return null;
            }
        }.generate_sample,
    },
    // .{
    //     .fnk_name = &Sexpr.doLit("uppercase"),
    //     .generate_sample = struct {
    //         fn generate_sample(k: usize, pool: *std.heap.MemoryPool(Sexpr), arena: std.mem.Allocator) core.OoM!?Sample {
    //             if (k < Vals.lowercase.len) {
    //                 return .{ .input = Vals.lowercase[k], .output = Vals.uppercase[k] };
    //             } else return null;
    //         }
    //     }.generate_sample,
    // },
};

const Vals = struct {
    pub const lowercase: []const *const Sexpr = &.{
        &Sexpr.doLit("a"),
        &Sexpr.doLit("b"),
        &Sexpr.doLit("c"),
        &Sexpr.doLit("d"),
        &Sexpr.doLit("e"),
        &Sexpr.doLit("f"),
    };
    pub const uppercase: []const *const Sexpr = &.{
        &Sexpr.doLit("A"),
        &Sexpr.doLit("B"),
        &Sexpr.doLit("C"),
        &Sexpr.doLit("D"),
        &Sexpr.doLit("E"),
        &Sexpr.doLit("F"),
    };
};
