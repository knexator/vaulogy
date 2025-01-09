const std = @import("std");

const js = struct {
    pub const debug = struct {
        extern fn logInt(arg: u32) void;
        extern fn logFloat(arg: f32) void;
    };

    pub const canvas = struct {
        extern fn beginPath() void;
        extern fn moveTo(x: f32, y: f32) void;
        extern fn lineTo(x: f32, y: f32) void;
        extern fn closePath() void;
        extern fn fill() void;
        extern fn stroke() void;
        extern fn setLineWidth(w: f32) void;
        extern fn setFillColor(r: u8, g: u8, b: u8) void;
        extern fn setStrokeColor(r: u8, g: u8, b: u8) void;
        extern fn setGlobalAlpha(a: f32) void;
        extern fn clear() void;
        extern fn fillRect(x: f32, y: f32, w: f32, h: f32) void;
        extern fn getWidth() u32;
        extern fn getHeight() u32;

        // TODO: save/restore, translate/rotate/scale/resetTransform, rect/arc, fillText
    };
};

const KeyCode = @import("./tools/generate_keycodes_js.zig").KeyCode;

export fn keydown(code: KeyCode) void {
    if (code == .KeyA) {
        total_time = 0;
    }
}

var total_time: f32 = 0;
export fn frame(delta_seconds: f32) void {
    total_time += delta_seconds;
}

export fn draw() void {
    js.canvas.setFillColor(255, 0, 0);
    js.canvas.fillRect(total_time * 100, 100, 100, 100);
    js.canvas.setFillColor(0, 128, 255);
    js.canvas.fillRect(150, 150, 100, 100);
}
