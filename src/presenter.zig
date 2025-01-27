//! This should be unchanged regardless of platform

const std = @import("std");

const core = @import("main.zig");
const Atom = core.Atom;
const Pair = core.Pair;
const Sexpr = core.Sexpr;
const Fnk = core.Fnk;
const FnkBody = core.FnkBody;
const FnkCollection = core.FnkCollection;
const VeryPermamentGameStuff = core.VeryPermamentGameStuff;
const parsing = @import("parsing.zig");

const OoM = error{ OutOfMemory, TODO };

const MouseButton = enum { left, right, middle };
pub const MouseState = struct {
    // TODO: rename these, make into a Vec2
    clientX: f32,
    clientY: f32,
    buttons: struct {
        left: bool,
        middle: bool,
        right: bool,
    },

    pub const init: MouseState = .{
        .clientX = 0,
        .clientY = 0,
        .buttons = .{
            .left = false,
            .middle = false,
            .right = false,
        },
    };

    pub fn pos(self: MouseState, camera: Camera) Vec2 {
        return camera.worldFromScreen(Vec2.new(self.clientX, self.clientY));
    }

    pub fn isDown(self: MouseState, button: MouseButton) bool {
        return switch (button) {
            .left => self.buttons.left,
            .middle => self.buttons.middle,
            .right => self.buttons.right,
        };
    }
};

pub const Mouse = struct {
    cur: MouseState,
    prev: MouseState,

    pub fn wasPressed(self: Mouse, button: MouseButton) bool {
        return self.cur.isDown(button) and !self.prev.isDown(button);
    }
};

pub const Platform = struct {
    gpa: std.mem.Allocator,
    getPlayerData: fn (mem: *VeryPermamentGameStuff) OoM!?PlayerData,
    setPlayerData: fn (player_data: PlayerData, mem: *VeryPermamentGameStuff) OoM!void,
    getMouse: fn () Mouse,
};

pub const PlayerData = struct {
    // TODO: this field should not be here.
    ascii_data: []const u8,

    fnks: FnkCollection,
    first_time: bool = true,

    pub fn empty(mem: *VeryPermamentGameStuff) PlayerData {
        return PlayerData{
            .ascii_data = "",
            .fnks = FnkCollection.init(mem.gpa),
        };
    }

    pub fn fromAscii2(data: []const u8, mem: *VeryPermamentGameStuff) !usize {
        const ascii_data = try mem.gpa.dupe(u8, data);
        var parser = parsing.Parser{ .remaining_text = ascii_data };
        var fnks = FnkCollection.init(mem.gpa);
        errdefer fnks.deinit();
        try parser.parseFnkCollection(&fnks, &mem.pool_for_sexprs, mem.arena_for_cases.allocator());
        return fnks.capacity();
        // return fnks.capacity();
    }

    pub fn fromAscii(data: []const u8, mem: *VeryPermamentGameStuff) !PlayerData {
        const ascii_data = try mem.gpa.dupe(u8, data);
        var parser = parsing.Parser{ .remaining_text = ascii_data };
        var fnks = FnkCollection.init(mem.gpa);
        errdefer fnks.deinit();
        try parser.parseFnkCollection(&fnks, &mem.pool_for_sexprs, mem.arena_for_cases.allocator());
        return PlayerData{
            .fnks = fnks,
            .ascii_data = ascii_data,
        };
    }

    pub fn toAscii(this: PlayerData, alloc: std.mem.Allocator) ![]const u8 {
        var result = std.ArrayList(u8).init(alloc);

        var it = this.fnks.iterator();
        while (it.next()) |x| {
            const fnk = Fnk{ .name = x.key_ptr.*, .body = x.value_ptr.* };
            const str = try std.fmt.allocPrint(alloc, "{any}\n", .{fnk});
            defer alloc.free(str);
            try result.appendSlice(str);
        }

        return result.toOwnedSlice();
    }

    pub fn deinit(this: *PlayerData, mem: *VeryPermamentGameStuff) void {
        this.fnks.deinit();
        mem.gpa.free(this.ascii_data);
    }

    test "PlayerData" {
        var mem = VeryPermamentGameStuff.init(std.testing.allocator);
        defer mem.deinit();

        var sut = try PlayerData.fromAscii(
            \\
            \\foo {
            \\  x -> y;
            \\}
            \\bar {
            \\  @a -> foo: x {
            \\      @r -> @a;
            \\  }
            \\}
        , &mem);
        defer sut.deinit(&mem);

        try std.testing.expectEqual(2, sut.fnks.count());

        const str = try sut.toAscii(mem.gpa);
        defer mem.gpa.free(str);
        var sut2 = try PlayerData.fromAscii(str, &mem);
        defer sut2.deinit(&mem);

        try std.testing.expectEqual(2, sut2.fnks.count());
    }
};

pub const Vec2 = struct {
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

    pub fn mul(a: Self, b: Self) Self {
        return new(a.x * b.x, a.y * b.y);
    }

    pub fn addX(a: Self, b: f32) Self {
        return new(a.x + b, a.y);
    }

    pub fn addY(a: Self, b: f32) Self {
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

fn inRange(value: f32, min_inclusive: f32, max_exclusive: f32) bool {
    return min_inclusive <= value and value < max_exclusive;
}

pub const Rect = struct {
    top_left: Vec2,
    size: Vec2,

    pub fn contains(self: Rect, p: Vec2) bool {
        return inRange(p.x, self.top_left.x, self.top_left.x + self.size.x) and
            inRange(p.y, self.top_left.y, self.top_left.y + self.size.y);
    }
};

pub const Color = struct {
    r: u8,
    g: u8,
    b: u8,

    pub const white = new(255, 255, 255);
    pub const black = new(0, 0, 0);
    pub const cyan = new(0, 255, 255);

    pub fn new(r: u8, g: u8, b: u8) Color {
        return .{ .r = r, .g = g, .b = b };
    }

    pub fn from01(r: f32, g: f32, b: f32) Color {
        return Color.new(
            @intFromFloat(r * 255),
            @intFromFloat(g * 255),
            @intFromFloat(b * 255),
        );
    }

    pub fn gray(v: u8) Color {
        return new(v, v, v);
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

    pub fn lerp(a: Camera, b: Camera, t: f32) Camera {
        return Camera{
            .center = Vec2.lerp(a.center, b.center, t),
            .height = std.math.lerp(a.height, b.height, t),
        };
    }

    /// screen_pos is in ([0..aspect_ratio], [0..1])
    pub fn worldFromScreen(self: Camera, screen_pos: Vec2) Vec2 {
        const rect = self.toRect();
        return rect.top_left.add(screen_pos.scale(self.height));
    }
};

pub const AtomVisuals = struct {
    profile: []const Vec2,
    color: Color,
};

pub const Drawer = struct {
    clear: fn (color: Color) void,
    drawRect: fn (camera: Camera, rect: Rect) void,
    drawAtomDebug: fn (camera: Camera, world_point: Point) void,
    drawAtom: fn (camera: Camera, world_point: Point, visuals: AtomVisuals) void,
    drawPatternAtomOutline: fn (camera: Camera, world_point: Point) void,
    drawPatternAtomDebug: fn (camera: Camera, world_point: Point) void,
    drawPairHolder: fn (camera: Camera, world_point: Point) void,
    drawPatternPairHolder: fn (camera: Camera, world_point: Point) void,
    drawPatternAtom: fn (camera: Camera, world_point: Point, visuals: AtomVisuals) void,
    drawCable: fn (camera: Camera, world_from: Vec2, world_to: Vec2, world_scale: f32, offset: f32) void,

    const dummySignatures = struct {
        pub fn color(c: Color) void {
            _ = c;
            unreachable;
        }
        pub fn camera_point_visuals(camera: Camera, world_point: Point, visuals: AtomVisuals) void {
            _ = camera;
            _ = world_point;
            _ = visuals;
            unreachable;
        }
        pub fn camera_point(camera: Camera, world_point: Point) void {
            _ = camera;
            _ = world_point;
            unreachable;
        }
    };
    // TODO: all should be unreachable
    pub const dummy = Drawer{
        .clear = dummySignatures.color,
        .drawRect = undefined,
        .drawAtomDebug = undefined,
        .drawAtom = dummySignatures.camera_point_visuals,
        .drawPatternAtomDebug = undefined,
        .drawPairHolder = dummySignatures.camera_point,
        .drawPatternPairHolder = dummySignatures.camera_point,
        .drawPatternAtom = dummySignatures.camera_point_visuals,
        .drawCable = struct {
            pub fn anon(camera: Camera, world_from: Vec2, world_to: Vec2, world_scale: f32, offset: f32) void {
                _ = camera;
                _ = world_from;
                _ = world_to;
                _ = world_scale;
                _ = offset;
                unreachable;
            }
        }.anon,
    };
};

fn defaultFnkBody(mem: *VeryPermamentGameStuff) FnkBody {
    const default_fnk =
        \\default {
        \\  true -> (nil . true);
        \\  (nil . true) -> false;
        \\  (true . nil) -> true;
        \\  (true . nil) -> true;
        \\}
    ;
    var parser = parsing.Parser{ .remaining_text = default_fnk };
    const fnk = parser.parseFnkNew(&mem.pool_for_sexprs, mem.arena_for_cases.allocator()) catch unreachable;
    return fnk.body;
}

/// The full game, from loading screen to end credits
pub fn Presenter(platform: Platform, drawer: Drawer) type {
    return struct {
        const Self = @This();

        mem: VeryPermamentGameStuff,
        persistence: PlayerData,

        state: union(enum) {
            /// not used for now
            intro: IntroSequence(platform, drawer),
            level_select: LevelSelect(platform, drawer),
            editing_fnk: EditingFnk(platform, drawer),
        },

        pub fn init() !Self {
            const platform_alloc = platform.gpa;
            var mem = VeryPermamentGameStuff.init(platform_alloc);
            var player_data = (try platform.getPlayerData(&mem)) orelse PlayerData.empty(&mem);

            if (!player_data.first_time) return error.TODO;
            const tutorial_fnk =
                \\planetFromOlympian {
                \\  Hermes -> Mercury;
                \\  Aphrodite -> Venus;
                \\  Ares -> Mars;
                \\  Zeus -> Jupiter;
                \\  Kronos -> Saturn;
                \\  Poseidon -> Neptune;
                \\  // Hades -> Pluto;
                \\}
            ;
            var parser = parsing.Parser{ .remaining_text = tutorial_fnk };
            const fnk = try parser.parseFnkNew(&mem.pool_for_sexprs, mem.arena_for_cases.allocator());
            try player_data.fnks.put(fnk.name, fnk.body);
            try platform.setPlayerData(player_data, &mem);

            try Artist(platform, drawer).init();

            const result = Self{
                .mem = mem,
                .persistence = player_data,
                .state = .{
                    // .level_select = .init(),
                    .editing_fnk = try .init(Fnk{
                        .name = try mem.storeSexpr(Sexpr.doLit("default")),
                        .body = defaultFnkBody(&mem),
                    }, &mem),
                },
            };
            return result;
        }

        pub fn update(self: *Self, delta_seconds: f32) !void {
            try switch (self.state) {
                .level_select => |*ui| if (ui.update(delta_seconds)) |level_index| {
                    const fnk_name = levels[level_index].fnk_name;
                    const fnk_body = self.persistence.fnks.get(fnk_name) orelse defaultFnkBody(&self.mem);
                    self.state = .{ .editing_fnk = try .init(
                        Fnk{ .name = fnk_name, .body = fnk_body },
                        &self.mem,
                    ) };
                },
                inline else => |*x| x.update(delta_seconds),
            };
        }

        pub fn draw(self: Self) OoM!void {
            try switch (self.state) {
                inline else => |x| x.draw(),
            };
        }
    };
}

const Random = struct {
    rnd: std.Random,

    fn between(this: Random, at_least: f32, less_than: f32) f32 {
        return this.rnd.float(f32) * (less_than - at_least) + at_least;
    }

    fn inRect(this: Random, rect: Rect) Vec2 {
        return Vec2.new(
            this.between(rect.top_left.x, rect.top_left.x + rect.size.x),
            this.between(rect.top_left.y, rect.top_left.y + rect.size.y),
        );
    }

    fn around0(this: Random, radius: f32) f32 {
        return this.between(-radius, radius);
    }

    fn direction(this: Random) Vec2 {
        return Vec2.e1.rotate(this.rnd.float(f32));
    }

    fn color(this: Random) Color {
        return Color.new(
            this.rnd.int(u8),
            this.rnd.int(u8),
            this.rnd.int(u8),
        );
    }
};

// pub fn Template(platform: Platform, drawer: Drawer) type {
//     _ = platform;
//     _ = drawer;
//     return struct {
//         const Self = @This();
//         pub fn init() Self {}
//         pub fn update(self: *Self, delta_seconds: f32) void {
//             _ = self;
//             _ = delta_seconds;
//         }
//         pub fn draw(self: Self) void {
//             _ = self;
//         }
//     };
// }

const Level = struct { fnk_name: *const Sexpr };
const levels: []const Level = &.{
    .{ .fnk_name = &Sexpr.doLit("planetFromOlympian") },
    .{ .fnk_name = &Sexpr.doPair(&Sexpr.nil, &Sexpr.doLit("input")) },
    .{ .fnk_name = &Sexpr.doPair(&Sexpr.doLit("foo"), &Sexpr.doPair(&Sexpr.doLit("input"), &Sexpr.nil)) },
};

/// Like Drawer, but higher level
fn Artist(platform: Platform, drawer: Drawer) type {
    const AtomVisualCache = struct {
        var visuals_cache: std.StringHashMap(AtomVisuals) = std.StringHashMap(AtomVisuals).init(platform.gpa);

        const hardcoded_visuals = .{
            .identity = AtomVisuals{
                .color = Color.white,
                .profile = &.{},
            },
            .nil = AtomVisuals{
                .color = .from01(0.45, 0.45, 0.45),
                .profile = &.{.new(0.75, -0.25)},
            },
            .input = AtomVisuals{
                .color = .from01(0.1, 0.6, 0.6),
                .profile = &.{ .new(0.2, 0.2), .new(0.8, 0.2) },
            },
            .true = AtomVisuals{
                .color = .from01(0.5, 0.9, 0.5),
                .profile = &blk: {
                    var buffer: [10]Vec2 = undefined;
                    for (0..10) |k| {
                        // TODO: don't hardcode the '10' in 3 places
                        const t = @as(f32, @floatFromInt(k)) / 10.0;
                        buffer[k] = Vec2.new(t, -0.2 * @sin(t * std.math.pi));
                    }
                    const res = buffer;
                    break :blk res;
                },
            },
        };

        pub fn init() !void {
            inline for (std.meta.fields(@TypeOf(hardcoded_visuals))) |field| {
                const atom_name = field.name;
                const atom_visuals = @field(hardcoded_visuals, field.name);
                try visuals_cache.put(atom_name, atom_visuals);
            }
        }

        fn newAtomProfile(name: []const u8) ![]const Vec2 {
            var rnd_state = std.Random.DefaultPrng.init(name.len);
            var rnd = Random{ .rnd = rnd_state.random() };

            const profile = try platform.gpa.alloc(Vec2, rnd.rnd.intRangeLessThan(usize, 2, 15));
            for (profile) |*p| {
                p.* = Vec2.new(rnd.between(0, 1), rnd.around0(0.2));
            }
            std.mem.sortUnstable(Vec2, profile, {}, struct {
                pub fn lessThanFn(context: void, lhs: Vec2, rhs: Vec2) bool {
                    _ = context;
                    return lhs.x < rhs.x;
                }
            }.lessThanFn);
            return profile;
        }

        fn newAtomColor(name: []const u8) !Color {
            var rnd_state = std.Random.DefaultPrng.init(name.len);
            var rnd = Random{ .rnd = rnd_state.random() };
            const color = rnd.color();
            return color;
        }

        pub fn getAtomVisuals(name: []const u8) !AtomVisuals {
            const v = try visuals_cache.getOrPut(name);
            if (!v.found_existing) {
                const res = AtomVisuals{
                    .color = try newAtomColor(name),
                    .profile = try newAtomProfile(name),
                };
                v.value_ptr.* = res;
            }
            return v.value_ptr.*;
        }
    };

    return struct {
        pub fn init() !void {
            return AtomVisualCache.init();
        }

        pub fn drawPatternOutline(camera: Camera, world_point: Point) !void {
            drawer.drawPatternAtomOutline(camera, world_point);
        }

        pub fn drawAtom(camera: Camera, world_point: Point, name: []const u8) !void {
            const visuals = try AtomVisualCache.getAtomVisuals(name);
            drawer.drawAtom(camera, world_point, visuals);
        }

        pub fn drawPatternAtom(camera: Camera, world_point: Point, name: []const u8) !void {
            const visuals = try AtomVisualCache.getAtomVisuals(name);
            drawer.drawPatternAtom(camera, world_point, visuals);
        }

        pub fn drawSexpr(camera: Camera, world_point: Point, sexpr: *const Sexpr) !void {
            switch (sexpr.*) {
                .atom_lit => |lit| {
                    try drawAtom(camera, world_point, lit.value);
                },
                .pair => |pair| {
                    drawer.drawPairHolder(camera, world_point);
                    try drawSexpr(camera, world_point.applyToLocalPoint(.{
                        .pos = .new(0.5, -0.5),
                        .scale = 0.5,
                    }), pair.left);
                    try drawSexpr(camera, world_point.applyToLocalPoint(.{
                        .pos = .new(0.5, 0.5),
                        .scale = 0.5,
                    }), pair.right);
                },
                else => std.log.err("TODO", .{}),
            }
        }

        pub fn drawPatternSexpr(camera: Camera, world_point: Point, sexpr: *const Sexpr) !void {
            switch (sexpr.*) {
                .atom_lit => |lit| {
                    try drawPatternAtom(camera, world_point, lit.value);
                },
                .pair => |pair| {
                    drawer.drawPatternPairHolder(camera, world_point);
                    try drawPatternSexpr(camera, world_point.applyToLocalPoint(.{
                        .pos = .new(-1, -0.5),
                        .scale = 0.5,
                    }), pair.left);
                    try drawPatternSexpr(camera, world_point.applyToLocalPoint(.{
                        .pos = .new(-1, 0.5),
                        .scale = 0.5,
                    }), pair.right);
                },
                else => std.log.err("TODO", .{}),
            }
        }

        // TODO: move these into a static class
        pub fn overlapsPatternAtom(atom_point: Point, needle_pos: Vec2) bool {
            const p = atom_point.inverseApplyGetLocal(.{ .pos = needle_pos }).pos;
            return inRange(p.y, -1, 1) and
                inRange(p.x, -1, 0.5 * (1 - @abs(p.y)));
        }

        pub fn overlapsPatternSexpr(alloc: std.mem.Allocator, sexpr: *const Sexpr, sexpr_pos: Point, needle_pos: Vec2) !?core.SexprAddress {
            var result = std.ArrayList(core.SexprAddressItem).init(alloc);
            defer result.deinit();
            // TODO (low priority): probably can be made more efficient by using less changes of coordinates

            var cur_sexpr_pos = sexpr_pos;
            var cur_sexpr = sexpr;
            while (true) {
                switch (cur_sexpr.*) {
                    .atom_lit, .atom_var => {
                        if (overlapsPatternAtom(cur_sexpr_pos, needle_pos)) {
                            return try result.toOwnedSlice();
                        } else {
                            return null;
                        }
                    },
                    .pair => |pair| {
                        const p = cur_sexpr_pos.inverseApplyGetLocal(.{ .pos = needle_pos }).pos;

                        if (overlapsPatternAtom(cur_sexpr_pos, needle_pos)) {
                            return try result.toOwnedSlice();
                        } else if (inRange(p.y, -1, 0)) {
                            try result.append(.left);
                            cur_sexpr = pair.left;
                            cur_sexpr_pos = cur_sexpr_pos.applyToLocalPoint(.{
                                .pos = .new(-1, -0.5),
                                .scale = 0.5,
                            });
                        } else if (inRange(p.y, 0, 1)) {
                            try result.append(.right);
                            cur_sexpr = pair.right;
                            cur_sexpr_pos = cur_sexpr_pos.applyToLocalPoint(.{
                                .pos = .new(-1, 0.5),
                                .scale = 0.5,
                            });
                        } else {
                            return null;
                        }
                    },
                }
            }
        }

        pub fn overlapsAtom(atom_point: Point, needle_pos: Vec2) bool {
            const p = atom_point.inverseApplyGetLocal(.{ .pos = needle_pos }).pos;
            return inRange(p.y, -1, 1) and
                inRange(p.x, -0.5 * (1 - @abs(p.y)), 2);
        }

        pub fn sexprPatternChildView(parent: Point, address: core.SexprAddress) Point {
            var result = parent;
            for (address) |cur| {
                result = result.applyToLocalPoint(.{
                    .pos = switch (cur) {
                        .left => .new(-1, -0.5),
                        .right => .new(-1, 0.5),
                    },
                    .scale = 0.5,
                });
            }
            return result;
        }
    };
}

pub fn EditingFnk(platform: Platform, drawer: Drawer) type {
    return struct {
        const Self = @This();
        const artist = Artist(platform, drawer);

        // TODO: use actual Case addresses
        const Address = usize;
        const CaseState = struct {
            // TODO: generic tree type to avoid duplication
            pattern: *const Sexpr,
            fn_name: *const Sexpr,
            template: *const Sexpr,
            // TODO: add 'next'

            pattern_point: Point,
        };

        mem: *VeryPermamentGameStuff,

        fnk_name: *const Sexpr,
        cases: std.ArrayList(CaseState),
        sample_input: *const Sexpr,

        focus: union(enum) {
            hovering_case: Address,
            grabbing_case: struct {
                case: CaseState,
                address_if_released: ?Address,
            },
            hovering_sexpr: struct {
                full_address: core.FullAddress,
                point: Point,
            },
            grabbing_sexpr: struct {
                sexpr: *const Sexpr,
                unfolded: Address,
                address_if_released: ?core.FullAddress,
                point: Point,
            },
        } = .{ .hovering_case = 0 },

        const camera = Camera{ .center = .new(6, 3), .height = 15.0 };

        pub fn init(fnk: Fnk, mem: *VeryPermamentGameStuff) !Self {
            var cases = std.ArrayList(CaseState).init(platform.gpa);
            for (fnk.body.cases.items, 0..) |case, k| {
                _ = k;
                try cases.append(.{
                    .fn_name = case.fn_name,
                    .pattern = case.pattern,
                    .template = case.template,
                    // TODO: this, to avoid the initial jump
                    .pattern_point = .{},
                });
            }
            return .{
                .mem = mem,
                .fnk_name = fnk.name,
                .cases = cases,
                .sample_input = &Sexpr.true,
            };
        }

        pub fn deinit(self: *Self) void {
            self.cases.deinit();
        }

        pub fn update(self: *Self, delta_seconds: f32) !void {
            var cur_top_line: f32 = 2;
            switch (self.focus) {
                .grabbing_case => |*grabbing| {
                    grabbing.case.pattern_point.lerp_towards(.{
                        .pos = platform.getMouse().cur.pos(camera),
                        .scale = if (grabbing.address_if_released == null) 0.5 else 1,
                    }, 0.6, delta_seconds);
                    grabbing.address_if_released = null;
                    for (self.cases.items, 0..) |*case, k| {
                        if (inRange(grabbing.case.pattern_point.pos.y - cur_top_line, -1, 1.5)) {
                            grabbing.address_if_released = k;
                            cur_top_line += 2.5;
                        }
                        defer cur_top_line += 1.5;
                        const pattern_point = Point{
                            .pos = .new(5, cur_top_line + 0.5),
                            .scale = 0.5,
                        };
                        case.pattern_point.lerp_towards(pattern_point, 0.6, delta_seconds);
                    }
                    if (inRange(grabbing.case.pattern_point.pos.y - cur_top_line, -1, 1.5)) {
                        grabbing.address_if_released = self.cases.items.len;
                    }
                },
                .grabbing_sexpr => |*grabbing| {
                    grabbing.point.lerp_towards(if (grabbing.address_if_released) |goal|
                        artist.sexprPatternChildView(self.cases.items[goal.case_address].pattern_point, goal.sexpr_address)
                        // TODO: this 'anim' should be smoothly undone when the sexpr is released
                            .applyToLocalPoint(.{ .turns = 0.02, .pos = .new(-0.5, 0) })
                    else
                        Point{
                            .pos = platform.getMouse().cur.pos(camera),
                            .scale = 1,
                        }, 0.6, delta_seconds);

                    const unfolded = grabbing.unfolded;
                    // TODO: remove duplication
                    grabbing.address_if_released = null;
                    for (self.cases.items, 0..) |*case, k| {
                        const is_folded: bool = k != unfolded;
                        defer cur_top_line += if (is_folded) 1.5 else 2.5;
                        const pattern_point = Point{
                            .pos = .new(5, cur_top_line + if (is_folded) tof32(0.5) else 1.0),
                            .scale = if (is_folded) 0.5 else 1,
                        };
                        case.pattern_point.lerp_towards(pattern_point, 0.6, delta_seconds);
                        const mouse_pos = platform.getMouse().cur.pos(camera);
                        const local_pos = pattern_point.inverseApplyGetLocalPosition(mouse_pos);
                        if (try artist.overlapsPatternSexpr(platform.gpa, case.pattern, pattern_point, mouse_pos)) |local_address| {
                            grabbing.unfolded = k;
                            grabbing.address_if_released = .{ .case_address = k, .sexpr_address = local_address, .which = .pattern };
                        } else if (inRange(local_pos.y, -1, 1) and inRange(mouse_pos.x, 0, 5)) {
                            grabbing.unfolded = k;
                        }
                    }
                },
                .hovering_sexpr => |*hovering| {
                    const unfolded = hovering.full_address.case_address;
                    const hovered_case = self.cases.items[hovering.full_address.case_address];
                    hovering.point.lerp_towards(
                        artist.sexprPatternChildView(
                            hovered_case.pattern_point,
                            hovering.full_address.sexpr_address,
                        ).applyToLocalPoint(.{ .pos = .new(-0.0, 0), .scale = 1.1 }),
                        0.6,
                        delta_seconds,
                    );
                    // TODO: remove duplication
                    // TODO: 'hover nothing' state
                    for (self.cases.items, 0..) |*case, k| {
                        const is_folded: bool = k != unfolded;
                        defer cur_top_line += if (is_folded) 1.5 else 2.5;
                        const pattern_point = Point{
                            .pos = .new(5, cur_top_line + if (is_folded) tof32(0.5) else 1.0),
                            .scale = if (is_folded) 0.5 else 1,
                        };
                        case.pattern_point.lerp_towards(pattern_point, 0.6, delta_seconds);
                        const mouse_pos = platform.getMouse().cur.pos(camera);
                        const local_pos = pattern_point.inverseApplyGetLocalPosition(mouse_pos);
                        if (try artist.overlapsPatternSexpr(
                            platform.gpa,
                            case.pattern,
                            pattern_point,
                            mouse_pos,
                        )) |local_address| {
                            const new_address = core.FullAddress{
                                .case_address = k,
                                .which = .pattern,
                                .sexpr_address = local_address,
                            };
                            if (!hovering.full_address.equals(new_address)) {
                                self.focus = .{
                                    .hovering_sexpr = .{
                                        .point = artist.sexprPatternChildView(
                                            case.pattern_point,
                                            local_address,
                                        ),
                                        .full_address = new_address,
                                    },
                                };
                            }
                        } else if (inRange(local_pos.y, -1, 1) and inRange(mouse_pos.x, 0, 5)) {
                            self.focus = .{ .hovering_case = k };
                        }
                    }
                },
                .hovering_case => |unfolded| {
                    for (self.cases.items, 0..) |*case, k| {
                        const is_folded: bool = k != unfolded;
                        defer cur_top_line += if (is_folded) 1.5 else 2.5;
                        const pattern_point = Point{
                            .pos = .new(5, cur_top_line + if (is_folded) tof32(0.5) else 1.0),
                            .scale = if (is_folded) 0.5 else 1,
                        };
                        case.pattern_point.lerp_towards(pattern_point, 0.6, delta_seconds);
                        const mouse_pos = platform.getMouse().cur.pos(camera);
                        const local_pos = pattern_point.inverseApplyGetLocalPosition(mouse_pos);
                        if (try artist.overlapsPatternSexpr(platform.gpa, case.pattern, pattern_point, mouse_pos)) |local_address| {
                            self.focus = .{ .hovering_sexpr = .{
                                .point = artist.sexprPatternChildView(
                                    case.pattern_point,
                                    local_address,
                                ),
                                .full_address = .{
                                    .case_address = k,
                                    .which = .pattern,
                                    .sexpr_address = local_address,
                                },
                            } };
                        } else if (inRange(local_pos.y, -1, 1) and inRange(mouse_pos.x, 0, 5)) {
                            self.focus = .{ .hovering_case = k };
                        }
                    }
                },
            }

            if (platform.getMouse().wasPressed(.left)) {
                switch (self.focus) {
                    .grabbing_case => |grabbing| {
                        if (grabbing.address_if_released) |address| {
                            try self.cases.insert(address, grabbing.case);
                            self.focus = .{ .hovering_case = address };
                        } else {
                            self.focus = .{ .hovering_case = 0 };
                        }
                    },
                    .grabbing_sexpr => |grabbing| {
                        if (grabbing.address_if_released) |address| {
                            // TODO: correctly modify the case
                            self.cases.items[address.case_address].pattern = try self.cases.items[address.case_address].pattern.setAt(self.mem, address.sexpr_address, grabbing.sexpr);
                            self.focus = .{ .hovering_sexpr = .{
                                .full_address = address,
                                .point = grabbing.point,
                            } };
                        } else {
                            self.focus = .{ .hovering_case = grabbing.unfolded };
                        }
                    },
                    .hovering_case => |unfolded| {
                        const asdf = self.cases.orderedRemove(unfolded);
                        self.focus = .{ .grabbing_case = .{
                            .case = asdf,
                            .address_if_released = unfolded,
                        } };
                    },
                    .hovering_sexpr => |hovering| {
                        const case = self.cases.items[hovering.full_address.case_address];
                        self.focus = .{
                            .grabbing_sexpr = .{
                                .address_if_released = hovering.full_address,
                                .sexpr = case.pattern.getAt(hovering.full_address.sexpr_address).?,
                                .unfolded = hovering.full_address.case_address,
                                .point = hovering.point,
                            },
                        };
                    },
                }
            }
        }

        pub fn draw(self: Self) !void {
            drawer.clear(Color.gray(128));
            if (false) {
                const debug_sexpr = &Sexpr.doPair(&Sexpr.nil, &Sexpr.doPair(&Sexpr.nil, &Sexpr.nil));
                const pos = Point{ .pos = .new(10, 0) };
                const res = try artist.overlapsPatternSexpr(platform.gpa, debug_sexpr, pos, platform.getMouse().cur.pos(camera));
                defer if (res) |x| platform.gpa.free(x);
                std.log.debug("overlap: {any}", .{res});
                try artist.drawPatternSexpr(
                    camera,
                    pos,
                    debug_sexpr,
                );
            }
            {
                try artist.drawSexpr(
                    camera,
                    .{ .pos = .new(1, 0) },
                    self.sample_input,
                );
                try artist.drawSexpr(
                    camera,
                    .{ .pos = .new(0, -1.25), .turns = -0.25 },
                    self.fnk_name,
                );
                drawer.drawCable(
                    camera,
                    .new(-7, 0),
                    .new(0.5, 0),
                    1,
                    0,
                );
            }

            const DIST_TO_TEMPLATE = 4;
            for (self.cases.items) |case| {
                const pattern_point = case.pattern_point;
                try artist.drawPatternSexpr(
                    camera,
                    pattern_point,
                    case.pattern,
                );
                if (case.pattern_point.scale >= 0.9) {
                    try artist.drawSexpr(
                        camera,
                        pattern_point.applyToLocalPoint(.{ .pos = .new(DIST_TO_TEMPLATE, 0) }),
                        case.template,
                    );
                    drawer.drawCable(
                        camera,
                        pattern_point.applyToLocalPosition(.new(0.5, 0)),
                        pattern_point.applyToLocalPosition(.new(DIST_TO_TEMPLATE - 0.5, 0)),
                        pattern_point.scale,
                        0,
                    );
                }

                const pos = pattern_point.applyToLocalPosition(.new(0, 1));
                drawer.drawCable(
                    camera,
                    pos.sub(.new(5, 0)),
                    pos,
                    1,
                    0,
                );
            }

            switch (self.focus) {
                .hovering_case => {},
                .grabbing_sexpr => |grabbing| {
                    try artist.drawPatternSexpr(
                        camera,
                        grabbing.point,
                        grabbing.sexpr,
                    );
                },
                .grabbing_case => |grabbing| {
                    const pattern_point = grabbing.case.pattern_point;
                    try artist.drawPatternSexpr(
                        camera,
                        pattern_point,
                        grabbing.case.pattern,
                    );
                    try artist.drawSexpr(
                        camera,
                        pattern_point.applyToLocalPoint(.{ .pos = .new(DIST_TO_TEMPLATE, 0) }),
                        grabbing.case.template,
                    );
                    drawer.drawCable(
                        camera,
                        pattern_point.applyToLocalPosition(.new(0.5, 0)),
                        pattern_point.applyToLocalPosition(.new(DIST_TO_TEMPLATE - 0.5, 0)),
                        pattern_point.scale,
                        0,
                    );
                },
                .hovering_sexpr => |hovering| {
                    const full_address = hovering.full_address;
                    const case = self.cases.items[full_address.case_address];
                    if (full_address.which != .pattern) return error.TODO;
                    try artist.drawPatternSexpr(
                        camera,
                        hovering.point,
                        case.pattern.getAt(full_address.sexpr_address).?,
                    );
                    // try artist.drawPatternOutline(camera, artist.sexprPatternChildView(
                    //     case.pattern_point,
                    //     full_address.sexpr_address,
                    // ));
                },
            }
        }
    };
}

test {
    std.testing.refAllDecls(EditingFnk(.{
        .gpa = std.testing.allocator,
        .getMouse = struct {
            pub fn anon() Mouse {
                unreachable;
            }
        }.anon,
        .getPlayerData = undefined,
        .setPlayerData = undefined,
    }, Drawer.dummy));
}

const UI = struct {
    const cam = Camera.fromTopleftAndHeight(Vec2.zero, 15);

    pub const State = struct {
        hot: ?usize = null,
        active: ?usize = null,
        buttons: []Button,

        pub fn isHot(self: State, k: usize) bool {
            return if (self.hot) |hot| hot == k else false;
        }

        pub fn isActive(self: State, k: usize) bool {
            return if (self.active) |active| active == k else false;
        }
    };

    pub const Button = struct {
        pos: Rect,
        hot_t: f32 = 0,
        active_t: f32 = 0,
    };
};

fn towards(v: *f32, goal: f32, max_delta: f32) void {
    if (@abs(v.* - goal) <= max_delta) {
        v.* = goal;
    } else if (v.* < goal) {
        v.* += max_delta;
    } else {
        v.* -= max_delta;
    }
}

const lerp_towards_float = lerp_towards;
fn lerp_towards(v: *f32, goal: f32, ratio: f32, delta_seconds: f32) void {
    // TODO: make this framerate independent
    _ = delta_seconds;
    v.* = std.math.lerp(v.*, goal, ratio);
}

pub fn LevelSelect(platform: Platform, drawer: Drawer) type {
    const artist = Artist(platform, drawer);
    return struct {
        const Self = @This();

        // asdf: [3]UI.Button = .{
        //     .{ .pos = Rect{ .top_left = .new(2, 2.5), .size = .one } },
        //     .{ .pos = Rect{ .top_left = .new(2, 5), .size = .one } },
        //     .{ .pos = Rect{ .top_left = .new(2, 7.5), .size = .one } },
        // },

        ui_state: UI.State,

        pub fn init() Self {
            const res = platform.gpa.alloc(UI.Button, levels.len) catch unreachable;
            for (res, 0..) |*b, k| {
                b.* = .{ .pos = Rect{ .top_left = .new(2, 2.5 + 2.5 * @as(f32, @floatFromInt(k))), .size = .one } };
            }
            return Self{ .ui_state = .{ .buttons = res } };
        }

        pub fn update(self: *Self, delta_seconds: f32) ?usize {
            const mouse = platform.getMouse();
            self.ui_state.hot = null;
            for (self.ui_state.buttons, 0..) |button, k| {
                if (button.pos.contains(mouse.cur.pos(UI.cam))) {
                    self.ui_state.hot = k;
                    if (self.ui_state.active == null and mouse.cur.isDown(.left)) {
                        self.ui_state.active = k;
                    }
                }

                if (self.ui_state.isActive(k) and self.ui_state.isHot(k) and !mouse.cur.isDown(.left)) {
                    return k;
                }
            }
            if (!mouse.cur.isDown(.left)) {
                if (self.ui_state.active) |_| {
                    self.ui_state.active = null;
                }
            } else if (self.ui_state.active == null) {
                // TODO: better
                self.ui_state.active = 999;
            }
            for (self.ui_state.buttons, 0..) |*button, k| {
                lerp_towards(
                    &button.hot_t,
                    if (self.ui_state.isHot(k)) 1 else 0,
                    0.6,
                    delta_seconds,
                );
                lerp_towards(
                    &button.active_t,
                    if (self.ui_state.isActive(k)) 1 else 0,
                    0.6,
                    delta_seconds,
                );
            }
            return null;
        }

        pub fn draw(self: Self) OoM!void {
            drawer.clear(Color.gray(128));
            for (self.ui_state.buttons, 0..) |button, k| {
                drawer.drawRect(UI.cam, button.pos);
                if (button.hot_t > 0 or button.active_t > 0) {
                    try artist.drawSexpr(UI.cam, .{
                        .pos = button.pos.top_left.add(.new(2 - button.active_t, 0.5)),
                        .scale = button.hot_t,
                    }, levels[k].fnk_name);
                }
            }
        }
    };
}

pub fn IntroSequence(platform: Platform, drawer: Drawer) type {
    _ = platform;
    return struct {
        const Self = @This();

        t: f32,
        background_atoms: [40]struct {
            cur: Point,
            vel: Point,
        },

        const initial_camera = Camera{
            .center = .zero,
            .height = 50,
        };
        const second_camera = Camera{
            .center = .new(4, 0),
            .height = 12,
        };

        const snap = .{
            .pos = Point{ .pos = .new(3, 0) },
            // the velocity at the moment of snapping
            .vel = Point{
                .pos = .new(-1, -1),
                .turns = -0.1,
            },
        };

        pub fn init() Self {
            var background_atoms: @FieldType(Self, "background_atoms") = undefined;
            var rnd_state = std.Random.DefaultPrng.init(14);
            const rnd = Random{ .rnd = rnd_state.random() };

            for (&background_atoms) |*atom| {
                atom.* = .{ .cur = .{
                    .pos = rnd.inRect(initial_camera.toRect()),
                    .turns = rnd.rnd.floatNorm(f32) / 100.0,
                }, .vel = Point{
                    .pos = rnd.direction().scale(0.2),
                    .turns = rnd.around0(0.02),
                } };
            }
            // hack
            background_atoms[30] = .{ .cur = .{
                .pos = rnd.inRect(initial_camera.toRect()),
                .turns = rnd.rnd.floatNorm(f32) / 100.0,
            }, .vel = Point{
                .pos = rnd.direction().scale(0.2),
                .turns = rnd.around0(0.02),
            } };
            return .{ .t = 0, .background_atoms = background_atoms };
        }

        pub fn update(self: *Self, delta_seconds: f32) void {
            for (&self.background_atoms) |*atom| {
                atom.cur.pos = atom.cur.pos.add(atom.vel.pos.scale(delta_seconds));
                atom.cur.turns += delta_seconds * atom.vel.turns;
            }
            self.t += delta_seconds;
            // self.t = std.math.clamp(self.t, 0, 1);
        }

        pub fn draw(self: Self) void {
            drawer.clear(Color.gray(128));
            const camera = Camera.lerp(
                initial_camera,
                second_camera,
                smoothstep(self.t, 2, 6),
            );
            for (self.background_atoms) |atom| {
                drawer.drawAtomDebug(camera, atom.cur);
            }

            if (self.t <= 8) {
                drawer.drawCable(camera, .new(-50, 0), .new(-0.5, 0), 1, 0);
                drawer.drawAtomDebug(
                    camera,
                    Point{ .pos = .zero, .scale = 1, .turns = 0 },
                );

                const cur = Point.lerp(
                    .{
                        .pos = snap.pos.pos.sub(snap.vel.pos).scale(8),
                        .turns = (snap.pos.turns - snap.vel.turns) * 8,
                    },
                    snap.pos,
                    clamp(self.t / 8, 0, 1),
                );
                drawer.drawPatternAtomDebug(camera, cur);
                drawer.drawAtomDebug(camera, cur.applyToLocalPoint(.{ .pos = .new(3.8, 0) }));
                drawer.drawCable(
                    camera,
                    cur.applyToLocalPosition(.new(0.5, 0)),
                    cur.applyToLocalPosition(.new(3.3, 0)),
                    1,
                    53.5,
                );
            } else {
                const pull = smoothstep(self.t, 8.3, 12) * 6.8;
                drawer.drawCable(camera, .new(-50, 0), .new(6.3 - pull, 0), 1, pull);
                drawer.drawAtomDebug(camera, snap.pos.applyToLocalPoint(.{ .pos = .new(3.8 - pull, 0) }));

                const cur = Point.lerp(
                    snap.pos,
                    .{
                        .pos = snap.pos.pos.sub(snap.vel.pos).add(.new(0, 3)).scale(-4),
                        .turns = (snap.pos.turns - snap.vel.turns) * -12,
                    },
                    clamp((self.t - 8) / 8, 0, 1),
                );
                drawer.drawAtomDebug(camera, cur.applyToLocalPoint(.{ .pos = .new(-3, 0) }));
                drawer.drawPatternAtomDebug(camera, cur);
            }
        }
    };
}

const clamp = std.math.clamp;
const lerp = std.math.lerp;

fn clamp01(value: anytype) @TypeOf(value, 0.0) {
    return std.math.clamp(value, 0.0, 1.0);
}

fn smoothstep(x: anytype, edge0: anytype, edge1: anytype) @TypeOf(x, edge0, edge1) {
    const y = std.math.clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0);
    return y * y * (3.0 - 2.0 * y);
}

fn tof32(value: anytype) f32 {
    const T = @TypeOf(value);
    return switch (@typeInfo(T)) {
        .float, .comptime_float => value,
        .int, .comptime_int => @floatFromInt(value),
        else => @compileError("Expected an int, float or vector of one, found " ++ @typeName(T)),
    };
}
