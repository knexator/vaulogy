const std = @import("std");

test {
    std.testing.refAllDecls(@import("main.zig"));
    std.testing.refAllDecls(@import("webgame.zig"));
    std.testing.refAllDecls(@import("presenter.zig"));
}
