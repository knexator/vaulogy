pub const math = @import("math.zig");
pub const funktional = @import("funktional.zig");
pub const hex = @import("hex.zig");
pub const sdl = @import("sdl.zig");
pub const Timekeeper = @import("Timekeeper.zig");

comptime {
    const std = @import("std");
    std.testing.refAllDeclsRecursive(@This());
}
