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
        extern fn fillRect(x: f32, y: f32, w: f32, h: f32) void;
        extern fn getWidth() u32;
        extern fn getHeight() u32;

        // TODO: save/restore, translate/rotate/scale/resetTransform, rect/arc, fillText
    };
};

fn TVec2(comptime Scalar: type) type {
    return struct {
        x: Scalar,
        y: Scalar,

        const Self = @This();

        pub fn new(x: Scalar, y: Scalar) Self {
            return .{ .x = x, .y = y };
        }

        pub fn add(a: Self, b: Self) Self {
            return Self.new(a.x + b.x, a.y + b.y);
        }

        pub fn scale(v: Self, s: Scalar) Self {
            return Self.new(v.x * s, v.y * s);
        }

        pub fn rotate(v: Self, turns: f32) Self {
            const c = @cos(turns * std.math.tau);
            const s = @sin(turns * std.math.tau);
            return Self.new(
                v.x * c - v.y * s,
                v.x * s + v.y * c,
            );
        }
    };
}
const Vec2 = TVec2(f32);
const Color = struct {
    r: u8,
    g: u8,
    b: u8,

    const white = new(255, 255, 255);
    const black = new(0, 0, 0);

    pub fn new(r: u8, g: u8, b: u8) Color {
        return .{ .r = r, .g = g, .b = b };
    }
};

const layer1 = struct {
    pub fn getCanvasSize() Vec2 {
        return Vec2.new(@floatFromInt(js.canvas.getWidth()), @floatFromInt(js.canvas.getHeight()));
    }

    pub fn setFillColor(c: Color) void {
        js.canvas.setFillColor(c.r, c.g, c.b);
    }

    pub fn setStrokeColor(c: Color) void {
        js.canvas.setStrokeColor(c.r, c.g, c.b);
    }

    pub fn moveTo(p: Vec2) void {
        js.canvas.moveTo(p.x, p.y);
    }

    pub fn lineTo(p: Vec2) void {
        js.canvas.lineTo(p.x, p.y);
    }

    pub fn clear(color: Color) void {
        layer1.setFillColor(color);
        const size = layer1.getCanvasSize();
        js.canvas.fillRect(0, 0, size.x, size.y);
    }

    pub fn pathLoop(all_positions: []Vec2) void {
        if (all_positions.len < 3) programmerError();
        js.canvas.beginPath();
        moveTo(all_positions[0]);
        for (all_positions[1..]) |pos| {
            lineTo(pos);
        }
        js.canvas.closePath();
    }
};

const KeyCode = @import("./tools/generate_keycodes_js.zig").KeyCode;

export fn keydown(code: KeyCode) void {
    if (code == .KeyA) {
        total_time = 0;
    }
}

// Goal for now: hardcoded animation showing a fnk application

var total_time: f32 = 0;
export fn frame(delta_seconds: f32) void {
    total_time += delta_seconds;
}

const core = @import("main.zig");
const Atom = core.Atom;
const Pair = core.Pair;
const Sexpr = core.Sexpr;

const COLORS = struct {
    const background = Color.new(128, 128, 128);
};

const ScreenPoint = struct {
    pos: Vec2,
    scale: f32,
    turns: f32 = 0,

    pub fn screenPositionFromLocalPosition(t: ScreenPoint, local: Vec2) Vec2 {
        return local.scale(t.scale).rotate(t.turns).add(t.pos);
    }
};

const layer2 = struct {
    pub fn drawAtomDebug(p: ScreenPoint) void {
        const local_positions = [_]Vec2{
            Vec2.new(-0.5, 0),
            Vec2.new(0, 1),
            Vec2.new(2, 1),
            Vec2.new(2, -1),
            Vec2.new(0, -1),
        };
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = p.screenPositionFromLocalPosition(pos);
        }
        layer1.pathLoop(&screen_positions);
        layer1.setFillColor(Color.white);
        layer1.setStrokeColor(Color.black);
        js.canvas.fill();
        js.canvas.stroke();
    }
};

export fn draw() void {
    layer1.clear(COLORS.background);
    layer2.drawAtomDebug(.{ .pos = Vec2.new(150, 150), .scale = 100 });
}

fn programmerError() void {
    js.debug.logInt(666);
    std.debug.panic("programmer error!", .{});
}
