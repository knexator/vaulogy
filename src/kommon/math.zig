const std = @import("std");

pub fn inRange(value: anytype, min_inclusive: anytype, max_exclusive: anytype) bool {
    return min_inclusive <= value and value < max_exclusive;
}

pub fn in01(value: f32) bool {
    return 0 <= value and value <= 1;
}

pub fn inverse_lerp(min: f32, max: f32, value: f32) f32 {
    return (value - min) / (max - min);
}

pub fn remap(value: f32, old_min: f32, old_max: f32, new_min: f32, new_max: f32) f32 {
    return lerp(new_min, new_max, inverse_lerp(old_min, old_max, value));
}

pub fn towards(v: *f32, goal: f32, max_delta: f32) void {
    if (@abs(v.* - goal) <= max_delta) {
        v.* = goal;
    } else if (v.* < goal) {
        v.* += max_delta;
    } else {
        v.* -= max_delta;
    }
}

const lerp_towards_float = lerp_towards;
pub fn lerp_towards(v: *f32, goal: f32, ratio: f32, delta_seconds: f32) void {
    // TODO: make this framerate independent
    _ = delta_seconds;
    v.* = std.math.lerp(v.*, goal, ratio);
}

pub fn lerp_towards_range(v: *f32, min: f32, max: f32, ratio: f32, delta_seconds: f32) void {
    std.debug.assert(min <= max);
    if (v.* < min) {
        lerp_towards(v, min, ratio, delta_seconds);
    } else if (v.* > max) {
        lerp_towards(v, max, ratio, delta_seconds);
    }
}

pub const clamp = std.math.clamp;
pub const lerp = std.math.lerp;

pub fn clamp01(value: anytype) @TypeOf(value, 0.0) {
    return std.math.clamp(value, 0.0, 1.0);
}

pub fn smoothstep(x: anytype, edge0: anytype, edge1: anytype) @TypeOf(x, edge0, edge1) {
    const y = std.math.clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0);
    return y * y * (3.0 - 2.0 * y);
}

pub fn tof32(value: anytype) f32 {
    const T = @TypeOf(value);
    return switch (@typeInfo(T)) {
        .float, .comptime_float => value,
        .int, .comptime_int => @floatFromInt(value),
        else => @compileError("Expected an int, float or vector of one, found " ++ @typeName(T)),
    };
}

pub const UVec2 = ZVec2(usize);
pub const IVec2 = ZVec2(isize);

pub fn ZVec2(T: type) type {
    return extern struct {
        pub const Scalar = T;

        x: Scalar,
        y: Scalar,

        const Self = @This();

        pub const zero = new(0, 0);
        pub const one = new(1, 1);
        pub const e1 = new(1, 0);
        pub const e2 = new(0, 1);

        pub fn equals(a: Self, b: Self) bool {
            return a.x == b.x and a.y == b.y;
        }

        pub fn new(x: Scalar, y: Scalar) Self {
            return .{ .x = x, .y = y };
        }

        pub fn both(v: Scalar) Self {
            return new(v, v);
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

        pub fn mul(a: Self, b: Self) Self {
            return new(a.x * b.x, a.y * b.y);
        }

        pub fn addX(a: Self, b: Scalar) Self {
            return new(a.x + b, a.y);
        }

        pub fn addY(a: Self, b: Scalar) Self {
            return new(a.x, a.y + b);
        }

        pub fn magSq(v: Self) Scalar {
            return dot(v, v);
        }

        pub fn dot(a: Self, b: Self) Scalar {
            return a.x * b.x + a.y * b.y;
        }
    };
}

pub const Vec2 = extern struct {
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

    pub fn both(v: Scalar) Self {
        return new(v, v);
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

    pub fn mul(a: Self, b: Self) Self {
        return new(a.x * b.x, a.y * b.y);
    }

    pub fn div(a: Self, b: Self) Self {
        return new(a.x / b.x, a.y / b.y);
    }

    pub fn addX(a: Self, b: Scalar) Self {
        return new(a.x + b, a.y);
    }

    pub fn addY(a: Self, b: Scalar) Self {
        return new(a.x, a.y + b);
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

    pub fn fromTurns(turns: f32) Self {
        return e1.rotate(turns);
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

pub const Rect = struct {
    top_left: Vec2,
    size: Vec2,

    pub fn getCenter(self: Rect) Vec2 {
        return self.top_left.add(self.size.scale(0.5));
    }

    pub fn get(self: Rect, which: enum { top_center, top_right }) Vec2 {
        return switch (which) {
            .top_center => self.top_left.addX(self.size.x / 2),
            .top_right => self.top_left.addX(self.size.x),
        };
    }

    pub fn contains(self: Rect, p: Vec2) bool {
        return inRange(p.x, self.top_left.x, self.top_left.x + self.size.x) and
            inRange(p.y, self.top_left.y, self.top_left.y + self.size.y);
    }

    pub fn plusMargin(self: Rect, v: f32) Rect {
        return .{ .top_left = self.top_left.sub(.new(v, v)), .size = self.size.add(Vec2.new(v, v).scale(2)) };
    }

    pub fn fromCenterAndSize(center: Vec2, size: Vec2) Rect {
        return .{ .top_left = center.sub(size.scale(0.5)), .size = size };
    }
};

pub const FColor = extern struct {
    r: f32,
    g: f32,
    b: f32,
    a: f32 = 1,

    pub fn toArray(c: FColor) [4]f32 {
        return .{ c.r, c.g, c.b, c.a };
    }
};

pub const Color = extern struct {
    r: u8,
    g: u8,
    b: u8,
    a: u8 = 255,

    pub const white = new(255, 255, 255);
    pub const black = new(0, 0, 0);
    pub const cyan = new(0, 255, 255);

    pub fn new(r: u8, g: u8, b: u8) Color {
        return .{ .r = r, .g = g, .b = b };
    }

    pub fn from01(r: f32, g: f32, b: f32) Color {
        std.debug.assert(in01(r) and in01(g) and in01(b));
        return Color.new(
            @intFromFloat(r * 255),
            @intFromFloat(g * 255),
            @intFromFloat(b * 255),
        );
    }

    pub fn fromHex(comptime str: []const u8) Color {
        @setEvalBranchQuota(10000);
        const error_message = std.fmt.comptimePrint("bad format for str {s}", .{str});
        if (str.len != 7 or str[0] != '#') @compileError(error_message);
        errdefer @compileError(error_message);
        return comptime Color{
            .r = try std.fmt.parseInt(u8, str[1..3], 16),
            .g = try std.fmt.parseInt(u8, str[3..5], 16),
            .b = try std.fmt.parseInt(u8, str[5..7], 16),
        };
    }

    pub fn fromHexAtRunTime(str: []const u8) !Color {
        if (str.len != 7 or str[0] != '#') return error.BadHexCode;
        var it = std.mem.window(u8, str[1..], 2, 2);
        return Color{
            .r = std.fmt.parseInt(u8, it.next().?, 16) catch return error.BadHexCode,
            .g = std.fmt.parseInt(u8, it.next().?, 16) catch return error.BadHexCode,
            .b = std.fmt.parseInt(u8, it.next().?, 16) catch return error.BadHexCode,
        };
    }

    pub fn gray(v: u8) Color {
        return new(v, v, v);
    }

    pub fn withAlpha(c: Color, a: u8) Color {
        return Color{ .r = c.r, .g = c.g, .b = c.b, .a = a };
    }

    pub fn withAlpha01(c: Color, a: f32) Color {
        return withAlpha(c, @intFromFloat(a * 255));
    }

    pub fn toFColor(c: Color) FColor {
        return .{
            .r = tof32(c.r) / 255,
            .g = tof32(c.g) / 255,
            .b = tof32(c.b) / 255,
            .a = tof32(c.a) / 255,
        };
    }
};

pub const Point = struct {
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

    pub fn lerp_towards(self: *Point, goal: Point, ratio: f32, delta_seconds: f32) void {
        lerp_towards_float(&self.pos.x, goal.pos.x, ratio, delta_seconds);
        lerp_towards_float(&self.pos.y, goal.pos.y, ratio, delta_seconds);
        lerp_towards_float(&self.turns, goal.turns, ratio, delta_seconds);
        lerp_towards_float(&self.scale, goal.scale, ratio, delta_seconds);
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

    // TODO: document these
    pub fn inverseApplyToLocalPoint(applied: Point, local: Point) Point {
        const scale = applied.scale / local.scale;
        const turns = applied.turns - local.turns;
        return .{
            .pos = applied.pos.sub(local.pos.scale(scale).rotate(turns)),
            .scale = scale,
            .turns = turns,
        };
    }

    pub fn inverseApplyGetLocal(parent: Point, applied: Point) Point {
        return .{
            .pos = applied.pos.sub(parent.pos).rotate(-parent.turns).scale(1 / parent.scale),
            .scale = applied.scale / parent.scale,
            .turns = applied.turns - parent.turns,
        };
    }

    test "inverse apply" {
        const parent: Point = .{ .pos = .zero, .scale = 2, .turns = 0.25 };
        const local: Point = .{ .pos = .e1 };
        const applied = parent.applyToLocalPoint(local);
        try expectApproxEqAbs(.{ .pos = .new(0, 2), .scale = 2, .turns = 0.25 }, applied, 0.0001);
        try expectApproxEqAbs(parent, applied.inverseApplyToLocalPoint(local), 0.0001);
        try expectApproxEqAbs(local, parent.inverseApplyGetLocal(applied), 0.0001);
    }

    pub fn inverseApplyGetLocalPosition(parent: Point, applied: Vec2) Vec2 {
        return inverseApplyGetLocal(parent, .{ .pos = applied }).pos;
    }
};

pub const Camera = struct {
    const aspect_ratio: f32 = 16.0 / 9.0;

    center: Vec2,
    /// how many world units fit between the top and bottom of the camera view
    height: f32,

    pub fn fromTopleftAndHeight(top_left: Vec2, height: f32) Camera {
        return .{ .center = top_left.add(
            Vec2.new(aspect_ratio, 1).scale(height).scale(0.5),
        ), .height = height };
    }

    pub fn toRect(self: Camera) Rect {
        const size = Vec2.new(self.height * aspect_ratio, self.height);
        const top_left = self.center.sub(size.scale(0.5));
        return Rect{ .top_left = top_left, .size = size };
    }

    pub fn fromRect(rect: Rect) Camera {
        std.debug.assert(std.math.approxEqRel(f32, rect.size.y, rect.size.x * aspect_ratio, 0.001));
        return fromTopleftAndHeight(rect.top_left, rect.size.y);
    }

    pub fn lerp(a: Camera, b: Camera, t: f32) Camera {
        return Camera{
            .center = Vec2.lerp(a.center, b.center, t),
            .height = std.math.lerp(a.height, b.height, t),
        };
    }

    pub fn remap(old_cam: Camera, old_pos: Point, new_cam: Camera) Point {
        return worldFromScreen(new_cam, old_cam.screenFromWorld(old_pos));
    }

    /// relative_screen_pos is in ([0..aspect_ratio], [0..1])
    pub fn worldFromScreenPosition(self: Camera, relative_screen_pos: Vec2) Vec2 {
        const rect = self.toRect();
        return rect.top_left.add(relative_screen_pos.scale(self.height));
    }

    /// relative_screen_point.pos is in ([0..aspect_ratio], [0..1])
    fn worldFromScreen(camera: Camera, relative_screen_point: Point) Point {
        return .{
            .pos = camera.worldFromScreenPosition(relative_screen_point.pos),
            .scale = relative_screen_point.scale * camera.height,
            .turns = relative_screen_point.turns,
        };
    }

    /// assumes the screen as a height of 1
    fn screenFromWorld(camera: Camera, world_point: Point) Point {
        const window_height = 1;
        const rect = camera.toRect();
        const local = Point.inverseApplyGetLocal(Point{
            .pos = rect.top_left,
            .scale = rect.size.y,
        }, world_point);
        const screen = Point{ .pos = .zero, .scale = window_height };
        return screen.applyToLocalPoint(local);
    }

    fn screenFromWorldPosition(camera: Camera, world_position: Vec2) Vec2 {
        return screenFromWorld(camera, .{ .pos = world_position }).pos;
    }

    test "convert between screen and world" {
        // TODO: maybe change to fuzz
        var rnd_state = std.Random.DefaultPrng.init(std.testing.random_seed);
        var rnd = Random.init(rnd_state.random());

        const big_rect = Rect{ .top_left = .both(1000), .size = .both(2000) };

        const camera = Camera.fromTopleftAndHeight(
            rnd.inRect(big_rect),
            rnd.between(0.01, 100),
        );

        const world_pos = rnd.inRect(big_rect);
        try Vec2.expectApproxEqAbs(
            world_pos,
            camera.worldFromScreenPosition(camera.screenFromWorldPosition(world_pos)),
            0.001,
        );

        const screen_pos = rnd.inRect(big_rect);
        try Vec2.expectApproxEqAbs(
            screen_pos,
            camera.screenFromWorldPosition(camera.worldFromScreenPosition(screen_pos)),
            0.001,
        );
    }

    pub fn zoom(original: Camera, fixed_world_pos: Vec2, new_height: f32) Camera {
        const fixed_screen_pos = original.screenFromWorldPosition(fixed_world_pos);
        return Camera.fromMapping(
            .{ .pos = fixed_world_pos, .scale = 1.0 },
            .{ .pos = fixed_screen_pos, .scale = 1.0 / new_height },
        );
    }

    pub fn fromMapping(source_world_point: Point, target_screen_point: Point) Camera {
        const camera_height = source_world_point.scale / target_screen_point.scale;
        const camera_top_left = source_world_point.pos.sub(target_screen_point.pos.scale(camera_height));

        return Camera.fromTopleftAndHeight(
            camera_top_left,
            camera_height,
        );
    }

    test "fromMapping" {
        // TODO: maybe change to fuzz
        var rnd_state = std.Random.DefaultPrng.init(std.testing.random_seed);
        var rnd = Random.init(rnd_state.random());

        const big_rect = Rect{ .top_left = .both(1000), .size = .both(2000) };

        const world_point = Point{
            .pos = rnd.inRect(big_rect),
            .scale = rnd.between(0.01, 100),
        };
        const screen_point = Point{
            .pos = rnd.inRect(big_rect),
            .scale = rnd.between(0.01, 100),
        };

        const camera = Camera.fromMapping(world_point, screen_point);

        try Point.expectApproxEqRel(
            screen_point,
            camera.screenFromWorld(world_point),
            0.001,
        );
    }

    test "zoom" {
        var rnd_state = std.Random.DefaultPrng.init(std.testing.random_seed);
        var rnd = Random.init(rnd_state.random());

        const big_rect = Rect{ .top_left = .both(1000), .size = .both(2000) };

        const original_camera = Camera.fromTopleftAndHeight(
            rnd.inRect(big_rect),
            rnd.between(0.01, 100),
        );
        const fixed_world_position = rnd.inRect(big_rect);
        const new_height = rnd.between(0.01, 100);

        const new_camera = original_camera.zoom(fixed_world_position, new_height);

        try Vec2.expectApproxEqRel(
            original_camera.screenFromWorldPosition(fixed_world_position),
            new_camera.screenFromWorldPosition(fixed_world_position),
            0.001,
        );

        try std.testing.expectApproxEqRel(new_height, new_camera.height, 0.001);
    }
};

pub const Random = struct {
    rnd: std.Random,

    pub fn init(rnd: std.Random) Random {
        return .{ .rnd = rnd };
    }

    pub fn between(this: Random, at_least: f32, less_than: f32) f32 {
        return this.rnd.float(f32) * (less_than - at_least) + at_least;
    }

    pub fn inRect(this: Random, rect: Rect) Vec2 {
        return Vec2.new(
            this.between(rect.top_left.x, rect.top_left.x + rect.size.x),
            this.between(rect.top_left.y, rect.top_left.y + rect.size.y),
        );
    }

    pub fn around0(this: Random, radius: f32) f32 {
        return this.between(-radius, radius);
    }

    pub fn direction(this: Random) Vec2 {
        return Vec2.e1.rotate(this.rnd.float(f32));
    }

    pub fn color(this: Random) Color {
        return Color.new(
            this.rnd.int(u8),
            this.rnd.int(u8),
            this.rnd.int(u8),
        );
    }
};
