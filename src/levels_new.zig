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

pub const levels: []const Level = &.{.{
    .fnk_name = &Sexpr.doLit("uppercase"),
    .generate_sample = struct {
        fn generate_sample(k: usize, pool: *std.heap.MemoryPool(Sexpr), arena: std.mem.Allocator) core.OoM!?Sample {
            _ = pool;
            _ = arena;
            switch (k) {
                0 => return .{ .input = Vals.a, .output = Vals.A },
                1 => return .{ .input = Vals.b, .output = Vals.B },
                else => return null,
            }
        }
    }.generate_sample,
}};

const Vals = struct {
    pub const a: *const Sexpr = &Sexpr.doLit("a");
    pub const b: *const Sexpr = &Sexpr.doLit("b");
    pub const A: *const Sexpr = &Sexpr.doLit("A");
    pub const B: *const Sexpr = &Sexpr.doLit("B");
};
