const std = @import("std");

const kommon = @import("kommon.zig");
const Vec2 = kommon.math.Vec2;

pub fn Grid2D(T: type) type {
    // 0 1 2
    // 3 4 5
    // 6 7 8
    return struct {
        // TODO: IVec2
        width: usize,
        height: usize,
        data: []T,

        const Self = @This();

        pub fn at(self: Self, i: usize, j: usize) !T {
            // TODO: bound check
            return self.data[j * self.width + i];
        }

        const GridIterator = struct {
            grid: Self,
            i: kommon.itertools.Iterator(usize),
            j: kommon.itertools.Iterator(usize),

            pub fn init(grid: Self) GridIterator {
                return .{
                    .grid = grid,
                    .i = .init(0, grid.width - 1),
                    .j = .init(0, grid.height - 1),
                };
            }

            pub fn next(self: *GridIterator) ?struct { row: usize, col: usize, value: T } {
                // ideal:
                // for (0..self.grid.height) |j| {
                //     for (0..self.grid.width) |i| {
                //         yield ...;
                //     }
                // }

                if (self.j.cur()) |j| {
                    if (self.i.next()) |i| {
                        return .{ .row = j, .col = i, .value = try self.grid.at(i, j) };
                    } else {
                        self.j.advance();
                        self.i.reset();
                        return self.next();
                    }
                } else {
                    return null;
                }
            }
        };
        pub fn iterator(self: Self) GridIterator {
            return .init(self);
        }

        pub fn fromAscii(allocator: std.mem.Allocator, ascii: []const u8) !Self {
            if (T != u8) @compileError("fromAscii only works on Grid2D(u8)");
            var lines = std.mem.splitScalar(u8, ascii, '\n');
            const width = lines.peek().?.len;
            const height = kommon.itertools.iteratorLen(lines);
            const data = try allocator.alloc(T, width * height);
            var j: usize = 0;
            while (lines.next()) |line| {
                if (line.len != width) return error.NotAnAsciiRectangle;
                std.mem.copyForwards(T, data[j * width ..], line);
                j += 1;
            }
            return .{
                .width = width,
                .height = height,
                .data = data,
            };
        }
    };
}
