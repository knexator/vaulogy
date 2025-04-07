pub const math = @import("math.zig");
pub const funktional = @import("funktional.zig");
pub const hex = @import("hex.zig");
pub const sdl = @import("sdl.zig");
pub const Timekeeper = @import("Timekeeper.zig");
pub const Grid2D = @import("grid_2D.zig").Grid2D;
pub const itertools = @import("itertools.zig");
pub const input = @import("input.zig");
pub const BFS = @import("bfs.zig").BFS;

comptime {
    const std = @import("std");
    std.testing.refAllDeclsRecursive(@This());
}

pub fn safeAt(T: type, arr: []T, index: usize) ?T {
    if (index >= arr.len) {
        return null;
    }
    return arr[index];
}
