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
        extern fn arc(x: f32, y: f32, radius: f32, startAngle: f32, endAngle: f32, counterclockwise: bool) void;
        extern fn ellipse(x: f32, y: f32, radiusX: f32, radiusY: f32, rotation: f32, startAngle: f32, endAngle: f32, counterclockwise: bool) void;
        extern fn getWidth() u32;
        extern fn getHeight() u32;

        // TODO: save/restore, translate/rotate/scale/resetTransform, rect, fillText
    };
};

const Vec2 = struct {
    pub const Scalar = f32;

    x: Scalar,
    y: Scalar,

    const Self = @This();

    pub const zero = new(0, 0);
    pub const one = new(1, 1);
    pub const half = new(0.5, 0.5);
    pub const e1 = new(1, 0);
    pub const e2 = new(0, 1);

    pub fn new(x: Scalar, y: Scalar) Self {
        return .{ .x = x, .y = y };
    }

    pub fn add(a: Self, b: Self) Self {
        return new(a.x + b.x, a.y + b.y);
    }

    pub fn sub(a: Self, b: Self) Self {
        return new(a.x - b.x, a.y - b.y);
    }

    pub fn scale(v: Self, s: Scalar) Self {
        return new(v.x * s, v.y * s);
    }

    pub fn perpCW(v: Self) Self {
        return new(-v.y, v.x);
    }

    pub fn rotate(v: Self, turns: f32) Self {
        const c = @cos(turns * std.math.tau);
        const s = @sin(turns * std.math.tau);
        return new(
            v.x * c - v.y * s,
            v.x * s + v.y * c,
        );
    }

    test "rotate" {
        try Vec2.expectApproxEqAbs(Vec2.e2, rotate(Vec2.e1, 0.25), 0.001);
    }

    pub fn normalized(v: Self) Self {
        return v.scale(1 / v.mag());
    }

    pub fn mag(v: Self) Scalar {
        return @sqrt(v.magSq());
    }

    pub fn magSq(v: Self) Scalar {
        return dot(v, v);
    }

    pub fn dot(a: Self, b: Self) Scalar {
        return a.x * b.x + a.y * b.y;
    }

    pub fn lerp(a: Self, b: Self, t: f32) Self {
        return new(
            std.math.lerp(a.x, b.x, t),
            std.math.lerp(a.y, b.y, t),
        );
    }

    pub fn expectApproxEqRel(expected: Vec2, actual: Vec2, tolerance: anytype) !void {
        try std.testing.expectApproxEqRel(expected.x, actual.x, tolerance);
        try std.testing.expectApproxEqRel(expected.y, actual.y, tolerance);
    }

    pub fn expectApproxEqAbs(expected: Vec2, actual: Vec2, tolerance: anytype) !void {
        try std.testing.expectApproxEqAbs(expected.x, actual.x, tolerance);
        try std.testing.expectApproxEqAbs(expected.y, actual.y, tolerance);
    }
};

const Color = struct {
    r: u8,
    g: u8,
    b: u8,

    const white = new(255, 255, 255);
    const black = new(0, 0, 0);

    pub fn new(r: u8, g: u8, b: u8) Color {
        return .{ .r = r, .g = g, .b = b };
    }

    pub fn gray(v: u8) Color {
        return new(v, v, v);
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

    pub fn circle(center: Vec2, radius: f32) void {
        js.canvas.arc(center.x, center.y, radius, 0, std.math.tau, false);
    }
};

const KeyCode = @import("./tools/generate_keycodes_js.zig").KeyCode;

// Goal for now: hardcoded animation showing a fnk application

const core = @import("main.zig");
const Atom = core.Atom;
const Pair = core.Pair;
const Sexpr = core.Sexpr;

const COLORS = struct {
    const background = Color.new(128, 128, 128);
};

const Point = struct {
    pos: Vec2 = .zero,
    scale: f32 = 1,
    turns: f32 = 0,

    pub fn lerp(a: Point, b: Point, t: f32) Point {
        // TODO: properly handle rotation
        return .{
            .pos = Vec2.lerp(a.pos, b.pos, t),
            .scale = std.math.lerp(a.scale, b.scale, t),
            .turns = std.math.lerp(a.turns, b.turns, t),
        };
    }

    pub fn applyToLocalPosition(parent: Point, local: Vec2) Vec2 {
        return local.scale(parent.scale).rotate(parent.turns).add(parent.pos);
    }

    pub fn applyToLocalPoint(parent: Point, local: Point) Point {
        return .{
            .pos = parent.applyToLocalPosition(local.pos),
            .scale = parent.scale * local.scale,
            .turns = parent.turns + local.turns,
        };
    }

    pub fn expectApproxEqRel(expected: Point, actual: Point, tolerance: anytype) !void {
        try std.testing.expectApproxEqRel(expected.scale, actual.scale, tolerance);
        try std.testing.expectApproxEqRel(expected.turns, actual.turns, tolerance);
        try Vec2.expectApproxEqRel(expected.pos, actual.pos, tolerance);
    }

    pub fn expectApproxEqAbs(expected: Point, actual: Point, tolerance: anytype) !void {
        try std.testing.expectApproxEqAbs(expected.scale, actual.scale, tolerance);
        try std.testing.expectApproxEqAbs(expected.turns, actual.turns, tolerance);
        try Vec2.expectApproxEqAbs(expected.pos, actual.pos, tolerance);
    }

    pub fn inverseApplyToLocalPoint(applied: Point, local: Point) Point {
        const scale = applied.scale / local.scale;
        const turns = applied.turns - local.turns;
        return .{
            .pos = applied.pos.sub(local.pos.scale(scale).rotate(turns)),
            .scale = scale,
            .turns = turns,
        };
    }

    test "inverse apply" {
        const parent: Point = .{ .pos = .zero, .scale = 2, .turns = 0.25 };
        const local: Point = .{ .pos = .e1 };
        const applied = parent.applyToLocalPoint(local);
        try expectApproxEqAbs(.{ .pos = .new(0, 2), .scale = 2, .turns = 0.25 }, applied, 0.0001);
        try expectApproxEqAbs(parent, applied.inverseApplyToLocalPoint(local), 0.0001);
    }
};

pub const Camera = struct {
    // an object at [camera.topleft] will be drawn on the top left of the screen
    // an object at [camera.topleft.addX(1) will be drawn 'asdf' pixels to the right of that

    topleft: Vec2,
    // how many pixels in a world unit
    asdf: f32,

    pub fn fromStuff(screen_side: f32, original_world: Point, target_screen_relative: Point) Camera {
        const asdf = target_screen_relative.scale * screen_side / original_world.scale;
        return .{
            .topleft = original_world.pos.sub(
                target_screen_relative.pos.scale(screen_side).scale(1 / asdf),
            ),
            .asdf = asdf,
        };
    }

    test "fromStuff" {
        {
            const original = Point{ .pos = .new(3, 4), .scale = 1 };
            const target_relative = Point{ .pos = Vec2.half, .scale = 0.1 };
            const screen_side = 300;

            const camera = fromStuff(screen_side, original, target_relative);
            try Point.expectApproxEqRel(
                .{ .pos = .new(150, 150), .scale = 30 },
                camera.screenFromWorld(original),
                0.000001,
            );
        }
    }

    pub fn screenFromWorld(this: Camera, world_point: Point) Point {
        return .{
            .pos = this.screenFromWorldPosition(world_point.pos),
            .scale = this.screenFromWorldScale(world_point.scale),
            .turns = world_point.turns,
        };
    }

    pub fn screenFromWorldPosition(this: Camera, world_pos: Vec2) Vec2 {
        return world_pos.sub(this.topleft).scale(this.asdf);
    }

    pub fn screenFromWorldScale(this: Camera, world_scale: f32) f32 {
        return this.asdf * world_scale;
    }

    pub fn worldFromScreen(this: Camera, screen_point: Point) Point {
        return .{
            .pos = screen_point.pos.scale(1 / this.asdf).add(this.topleft),
            .scale = screen_point.scale / this.asdf,
            .turns = screen_point.turns,
        };
    }

    test "basic camera" {
        const camera = Camera{ .topleft = .new(2, 3), .asdf = 100 };
        try std.testing.expectEqual(
            Point{ .pos = Vec2.zero, .scale = 100 },
            camera.screenFromWorld(
                .{ .pos = .new(2, 3), .scale = 1 },
            ),
        );
        try std.testing.expectEqual(
            Point{ .pos = .new(100, 100), .scale = 50 },
            camera.screenFromWorld(
                .{ .pos = .new(3, 4), .scale = 0.5 },
            ),
        );
    }
};

const Drawer = struct {
    camera: Camera,

    pub fn drawAtomDebug(this: Drawer, world_point: Point) void {
        const screen_point = this.camera.screenFromWorld(world_point);
        const local_positions = [_]Vec2{
            Vec2.new(-0.5, 0),
            Vec2.new(0, 1),
            Vec2.new(2, 1),
            Vec2.new(2, -1),
            Vec2.new(0, -1),
        };
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        layer1.pathLoop(&screen_positions);
        js.canvas.setLineWidth(1);
        layer1.setFillColor(Color.white);
        layer1.setStrokeColor(Color.black);
        js.canvas.fill();
        js.canvas.stroke();
    }

    pub fn drawAtomPatternDebug(this: Drawer, world_point: Point) void {
        const screen_point = this.camera.screenFromWorld(world_point);
        const local_positions = [_]Vec2{
            Vec2.new(0.5, 0),
            Vec2.new(0, 1),
            Vec2.new(-1, 1),
            Vec2.new(-1, -1),
            Vec2.new(0, -1),
        };
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        layer1.pathLoop(&screen_positions);
        js.canvas.setLineWidth(1);
        layer1.setFillColor(Color.white);
        layer1.setStrokeColor(Color.black);
        js.canvas.fill();
        js.canvas.stroke();
    }

    pub fn drawCable(this: Drawer, world_from: Vec2, world_to: Vec2, world_scale: f32, offset: f32) void {
        const screen_from = this.camera.screenFromWorldPosition(world_from);
        const screen_to = this.camera.screenFromWorldPosition(world_to);
        const scale = this.camera.screenFromWorldScale(world_scale);
        layer1.setStrokeColor(Color.black);
        js.canvas.setLineWidth(1);
        js.canvas.beginPath();
        layer1.moveTo(screen_from);
        layer1.lineTo(screen_to);
        js.canvas.stroke();

        js.canvas.beginPath();
        const delta = screen_to.sub(screen_from);
        const length = delta.mag();
        const dir = delta.scale(1 / length);
        var done: f32 = 0;
        layer1.moveTo(screen_from);
        while (done < length) : (done += 1) {
            layer1.lineTo(screen_from.add(dir.scale(done)).add(dir.perpCW().scale(cableOffset(done + offset * scale, scale))));
        }
        layer1.lineTo(screen_to);
        js.canvas.stroke();
    }

    pub fn drawAsdfDevice(this: Drawer, world_point: Point) void {
        this.drawCable(
            world_point.applyToLocalPosition(.new(0, -0.25)),
            world_point.applyToLocalPosition(.new(0, -0.75)),
            world_point.scale,
            0,
        );
        this.drawCable(
            world_point.applyToLocalPosition(.new(0.25, 0)),
            world_point.applyToLocalPosition(.new(0.5, 0)),
            world_point.scale,
            0,
        );
        const screen_point = this.camera.screenFromWorld(world_point);

        layer1.setStrokeColor(Color.white);
        js.canvas.beginPath();
        js.canvas.ellipse(screen_point.pos.x - screen_point.scale * 0.2 + 1, screen_point.pos.y, screen_point.scale * 0.05, screen_point.scale * 0.25, 0, std.math.pi * 1.5, std.math.pi * 0.5, true);
        js.canvas.stroke();

        // Back face
        // layer1.setFillColor(Color.gray(128 - 32));
        // js.canvas.beginPath();
        // js.canvas.ellipse(screen_point.pos.x - screen_point.scale * 0.2, screen_point.pos.y, screen_point.scale * 0.05, screen_point.scale * 0.25, 0, std.math.pi * 1.5, std.math.pi * 0.5, true);
        // js.canvas.lineTo(screen_point.pos.x + screen_point.scale * 0.2, screen_point.pos.y + screen_point.scale * 0.25);
        // js.canvas.ellipse(screen_point.pos.x + screen_point.scale * 0.2, screen_point.pos.y, screen_point.scale * 0.05, screen_point.scale * 0.25, 0, std.math.pi * 0.5, std.math.pi * 1.5, false);
        // js.canvas.closePath();
        // js.canvas.fill();

        layer1.setFillColor(Color.white);
        js.canvas.beginPath();
        js.canvas.ellipse(screen_point.pos.x - screen_point.scale * 0.2, screen_point.pos.y, screen_point.scale * 0.05, screen_point.scale * 0.25, 0, std.math.pi * 1.5, std.math.pi * 0.5, false);
        js.canvas.lineTo(screen_point.pos.x - screen_point.scale * 0.1, screen_point.pos.y + screen_point.scale * 0.25);
        js.canvas.lineTo(screen_point.pos.x - screen_point.scale * 0.05, screen_point.pos.y + screen_point.scale * 0.2);
        js.canvas.lineTo(screen_point.pos.x + screen_point.scale * 0.075, screen_point.pos.y + screen_point.scale * 0.15);
        js.canvas.lineTo(screen_point.pos.x + screen_point.scale * 0.15, screen_point.pos.y + screen_point.scale * 0.2);
        js.canvas.lineTo(screen_point.pos.x + screen_point.scale * 0.1, screen_point.pos.y + screen_point.scale * 0.25);
        js.canvas.ellipse(screen_point.pos.x + screen_point.scale * 0.2, screen_point.pos.y, screen_point.scale * 0.05, screen_point.scale * 0.25, 0, std.math.pi * 0.5, std.math.pi * 1.5, true);
        js.canvas.closePath();
        js.canvas.fill();
    }

    pub fn drawCords(this: Drawer, points: []const Vec2, world_scale: f32, offsets: []const f32) void {
        std.debug.assert(points.len == 1 + offsets.len);
        if (points.len < 2) programmerError();
        this.drawCord(points[0], points[1], world_scale, offsets[0]);
        for (points[1 .. points.len - 1], points[2..], offsets[1..]) |a, b, offset| {
            this.drawCord(a, b, world_scale, offset);
        }
    }

    pub fn drawCord(this: Drawer, world_from: Vec2, world_to: Vec2, world_scale: f32, offset: f32) void {
        const screen_from = this.camera.screenFromWorld(.{ .pos = world_from }).pos;
        const screen_to = this.camera.screenFromWorld(.{ .pos = world_to }).pos;
        const scale = this.camera.screenFromWorldScale(world_scale);
        layer1.setStrokeColor(Color.black);
        js.canvas.setLineWidth(scale * 0.1);
        js.canvas.beginPath();
        layer1.moveTo(screen_from);
        layer1.lineTo(screen_to);
        js.canvas.stroke();

        js.canvas.setLineWidth(1);
        js.canvas.beginPath();
        const delta = screen_to.sub(screen_from);
        const length = delta.mag();
        const dir = delta.scale(1 / length);
        var done: f32 = 0;
        layer1.moveTo(screen_from);
        while (done < length) : (done += 1) {
            layer1.lineTo(screen_from.add(dir.scale(done)).add(dir.perpCW().scale(cableOffset(done + offset * scale, scale))));
        }
        layer1.lineTo(screen_to);
        js.canvas.stroke();
    }

    fn cableOffset(x: f32, scale: f32) f32 {
        const z = x * 20 / scale;
        const y = @sin(z) + 0.2 * @sin(z * 1.3) + 0.3 * @sin(z * 3.1);
        return y * 0.1 * scale;
    }
};

// 1 world unit = half an atom

const DebugAnimation = struct {
    const Phase = enum {
        moving_up,
        interlocking,
        floating_away,

        pub fn duration(x: Phase) f32 {
            return switch (x) {
                .moving_up => 2,
                .interlocking => 1,
                .floating_away => 1,
            };
        }

        pub fn next(x: Phase) Phase {
            return switch (x) {
                .moving_up => .interlocking,
                .interlocking => .floating_away,
                .floating_away => .moving_up,
            };
        }
    };

    local_t: f32 = 0,
    phase: Phase = .moving_up,

    pub fn update(this: *DebugAnimation, delta_seconds: f32) void {
        this.local_t += delta_seconds / this.phase.duration();
        if (this.local_t >= 1) {
            this.local_t = 0;
            this.phase = this.phase.next();
        }
    }

    pub fn draw(this: DebugAnimation) void {
        const canvas_size = layer1.getCanvasSize();
        const canvas_side = canvas_size.y;
        std.debug.assert(std.math.approxEqRel(
            f32,
            canvas_side * 16 / 9,
            canvas_size.x,
            0.01,
        ));
        const camera = Camera.fromStuff(
            canvas_side,
            .{ .pos = .zero, .scale = 1 },
            .{ .pos = .new(0.5, 0.5), .scale = 0.5 / 4.0 },
        );
        const drawer = Drawer{ .camera = camera };
        layer1.clear(COLORS.background);

        const t = this.local_t;
        if (this.phase == .floating_away) {
            const y = -4 * t;
            drawer.drawAtomDebug(.{ .pos = .new(1, y), .scale = 1 });
            drawer.drawAtomDebug(.{ .pos = .new(0, y - 1.25), .scale = 1, .turns = -0.25 });
            defer drawer.drawAsdfDevice(.{ .pos = .new(0, y), .scale = 1 });
            drawer.drawAtomPatternDebug(.{ .pos = .new(4, y), .scale = 1 });

            drawer.drawCable(.new(-3, 0), .zero, 1, -4);
            drawer.drawCords(&.{
                .zero,
                .new(0.8, 1),
                .new(4, 1),
                .new(4 + 0.5, 0),
                .new(4 + 2, 0),
            }, 1, &.{ -4, -4, 0, 0 });
            drawer.drawAtomDebug(.{ .pos = .new(4 + 2.5, 0), .scale = 1 });
        } else {
            drawer.drawAtomDebug(.{ .pos = .new(1, 0), .scale = 1 });
            drawer.drawAtomDebug(.{ .pos = .new(0, -1.25), .scale = 1, .turns = -0.25 });
            defer drawer.drawAsdfDevice(.{ .pos = Vec2.zero, .scale = 1 });
            const x = switch (this.phase) {
                .moving_up => 4.8,
                .interlocking => 4.8 - t * 0.8,
                else => unreachable,
            };
            const y = switch (this.phase) {
                .moving_up => 2.5 * (1 - t),
                .interlocking => 0,
                else => unreachable,
            };
            drawer.drawAtomPatternDebug(.{ .pos = .new(x, y), .scale = 1 });
            // drawer.drawAtomPatternDebug(.{ .pos = .new(4.25, 2.5 + 0.25 - t), .scale = 1, .turns = 0.05 });
            drawer.drawCable(.new(-3, 0), .zero, 1, -y - x);
            drawer.drawCords(&.{
                .zero,
                .new(4.8 - x, y + 1),
                .new(x, y + 1),
                .new(x + 0.5, y),
                .new(x + 2, y),
            }, 1, &.{ -y - x, -x, 0, 0 });
            drawer.drawAtomDebug(.{ .pos = .new(x + 2.5, y), .scale = 1 });
        }
    }
};

const DebugAnimation2 = struct {
    deviceAsdf: Point = .{ .pos = .zero },
    // pattern_lower_left: Point = .{ .pos = .new(3.8, 3.5), .turns = 0.00 },

    global_t: f32 = 0,

    pub fn update(this: *DebugAnimation2, delta_seconds: f32) void {
        this.global_t += delta_seconds;
        this.global_t = @min(this.global_t, 1);
    }

    // fn animation(this: *DebugAnimation2) void {
    //     lerpTo( pattern_lower_left, Point{.pos = .new(3, 1), .turns = 0.05} );
    //     pattern_lower_left = lerp()
    // }

    pub fn draw(this: DebugAnimation2) void {
        const canvas_size = layer1.getCanvasSize();
        const canvas_side = canvas_size.y;
        std.debug.assert(std.math.approxEqRel(
            f32,
            canvas_side * 16 / 9,
            canvas_size.x,
            0.01,
        ));
        const camera = Camera.fromStuff(
            canvas_side,
            .{ .pos = .zero, .scale = 1 },
            .{ .pos = .new(0.5, 0.5), .scale = 0.5 / 4.0 },
        );
        const drawer = Drawer{ .camera = camera };
        layer1.clear(COLORS.background);

        drawer.drawAtomDebug(this.deviceAsdf.applyToLocalPoint(.{ .pos = .new(1, 0) }));
        drawer.drawAtomDebug(this.deviceAsdf.applyToLocalPoint(.{ .pos = .new(0, -1.25), .turns = -0.25 }));
        defer drawer.drawAsdfDevice(this.deviceAsdf);

        const cur_pattern_lower_left = Point.lerp(
            .{ .pos = .new(3.8, 3.5), .turns = 0.00 },
            .{ .pos = .new(3, 1), .turns = 0.05 },
            this.global_t,
        );

        drawer.drawAtomPatternDebug(cur_pattern_lower_left.inverseApplyToLocalPoint(.{ .pos = .new(-1, 1) }));
        // drawer.drawAtomPatternDebug(this.pattern_lower_left.inverseApplyToLocalPoint(.{ .pos = .new(-1, 1) }));

        drawer.drawCable(
            this.deviceAsdf.applyToLocalPosition(.new(-3, 0)),
            this.deviceAsdf.pos,
            1,
            this.global_t * 1.5,
        );
        drawer.drawCords(&.{
            this.deviceAsdf.applyToLocalPosition(.new(0, 0.25)),
            this.deviceAsdf.applyToLocalPosition(Vec2.lerp(.new(0, 3.5), .one, @min(this.global_t * 1.5, 1))),
            cur_pattern_lower_left.pos,
            cur_pattern_lower_left.applyToLocalPosition(.new(1, 0)),
            cur_pattern_lower_left.applyToLocalPosition(.new(1.5, -1)),
        }, 1, &.{ this.global_t * 2, this.global_t * 2, 0, 0 });
    }
};

var debug_animation: if (false) DebugAnimation else DebugAnimation2 = .{};
var paused = true;

export fn keydown(code: KeyCode) void {
    if (code == .KeyA) {
        debug_animation = .{};
    } else if (code == .KeyD) {
        paused = !paused;
    }
}

export fn frame(delta_seconds: f32) void {
    if (paused) return;
    debug_animation.update(delta_seconds);
}

export fn draw() void {
    debug_animation.draw();
}

fn programmerError() void {
    js.debug.logInt(666);
    std.debug.panic("programmer error!", .{});
}
