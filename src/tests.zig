const std = @import("std");

test {
    std.testing.refAllDecls(@import("main.zig"));

    // Can't run these since the file declares some extern fns
    // std.testing.refAllDecls(@import("webgame.zig"));
}
