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

const OoM = error{OutOfMemory};

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
        _ = &parser;
        var fnks = FnkCollection.init(mem.gpa);
        _ = &fnks;
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

    pub fn new(r: u8, g: u8, b: u8) Color {
        return .{ .r = r, .g = g, .b = b };
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

pub const AtomProfile = []const Vec2;
pub const Drawer = struct {
    clear: fn (color: Color) void,
    drawRect: fn (camera: Camera, rect: Rect) void,
    drawAtomDebug: fn (camera: Camera, world_point: Point) void,
    drawAtom: fn (camera: Camera, world_point: Point, profile: AtomProfile) void,
    drawAtomPatternDebug: fn (camera: Camera, world_point: Point) void,
    drawCable: fn (camera: Camera, world_from: Vec2, world_to: Vec2, world_scale: f32, offset: f32) void,
};

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
            editing_level: EditingFnk(platform, drawer),
        },

        fn defaultFnkBody(mem: *VeryPermamentGameStuff) FnkBody {
            const default_fnk =
                \\default {
                \\  foo -> bar;
                \\}
            ;
            var parser = parsing.Parser{ .remaining_text = default_fnk };
            const fnk = parser.parseFnkNew(&mem.pool_for_sexprs, mem.arena_for_cases.allocator()) catch unreachable;
            return fnk.body;
        }

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
                    .level_select = .init(),
                },
            };
            return result;
        }

        pub fn update(self: *Self, delta_seconds: f32) void {
            switch (self.state) {
                .level_select => |*ui| if (ui.update(delta_seconds)) |level_index| {
                    const fnk_name = levels[level_index].fnk_name;
                    const fnk_body = self.persistence.fnks.get(fnk_name) orelse defaultFnkBody(&self.mem);
                    self.state = .{ .editing_level = .init(
                        Fnk{ .name = fnk_name, .body = fnk_body },
                    ) };
                },
                inline else => |*x| x.update(delta_seconds),
            }
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
    .{ .fnk_name = &Sexpr.doLit("testing") },
};

/// Like Drawer, but higher level
fn Artist(platform: Platform, drawer: Drawer) type {
    const AtomProfilesCache = struct {
        var profiles_cache: std.StringHashMap(AtomProfile) = std.StringHashMap(AtomProfile).init(platform.gpa);

        const hardcoded_profiles = .{
            .identity = [_]Vec2{},
            .nil = [_]Vec2{Vec2.new(0.75, -0.25)},
            .input = [_]Vec2{ Vec2.new(0.2, 0.2), Vec2.new(0.8, 0.2) },
        };

        pub fn init() !void {
            inline for (std.meta.fields(@TypeOf(hardcoded_profiles))) |field| {
                const magic: []const Vec2 = @as([*]const Vec2, @ptrCast(@alignCast(field.default_value)))[0..@typeInfo(field.type).array.len];
                try profiles_cache.put(field.name, magic);
            }
        }

        pub fn getAtomProfile(name: []const u8) !AtomProfile {
            const v = try profiles_cache.getOrPut(name);
            if (!v.found_existing) {
                std.log.debug("new! {s}", .{name});
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

                v.value_ptr.* = profile;
            }
            return v.value_ptr.*;
        }
    };

    return struct {
        pub fn init() !void {
            return AtomProfilesCache.init();
        }

        pub fn drawAtom(camera: Camera, world_point: Point, name: []const u8) !void {
            const profile = try AtomProfilesCache.getAtomProfile(name);
            drawer.drawAtom(camera, world_point, profile);
        }

        pub fn getAtomProfile(name: []const u8) !AtomProfile {
            return AtomProfilesCache.getAtomProfile(name);
        }
    };
}

pub fn EditingFnk(platform: Platform, drawer: Drawer) type {
    return struct {
        const Self = @This();
        const artist = Artist(platform, drawer);

        fnk: Fnk,
        sample_input: *const Sexpr,

        pub fn init(fnk: Fnk) Self {
            return .{ .fnk = fnk, .sample_input = &Sexpr.true };
        }

        pub fn update(self: *Self, delta_seconds: f32) void {
            _ = self;
            _ = delta_seconds;
        }

        pub fn draw(self: Self) void {
            _ = self;
            drawer.clear(Color.gray(128));
        }
    };
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

fn lerp_towards(v: *f32, goal: f32, ratio: f32, delta_seconds: f32) void {
    // TODO: make this framerate independent
    _ = delta_seconds;
    v.* = std.math.lerp(v.*, goal, ratio);
}

pub fn LevelSelect(platform: Platform, drawer: Drawer) type {
    const artist = Artist(platform, drawer);
    // _ = platform;
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
            for (self.ui_state.buttons) |button| {
                drawer.drawRect(UI.cam, button.pos);
                if (button.hot_t > 0 or button.active_t > 0) {
                    // drawer.drawAtomDebug(UI.cam, .{
                    //     .pos = button.pos.top_left.add(.new(2 - button.active_t, 0.5)),
                    //     .scale = button.hot_t,
                    // });
                    try artist.drawAtom(UI.cam, .{
                        .pos = button.pos.top_left.add(.new(2 - button.active_t, 0.5)),
                        .scale = button.hot_t,
                    }, if (button.pos.top_left.y < 5) "nl" else "iput");
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
                drawer.drawAtomPatternDebug(camera, cur);
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
                drawer.drawAtomPatternDebug(camera, cur);
            }
        }
    };
}

const clamp = std.math.clamp;

fn clamp01(value: anytype) @TypeOf(value, 0.0) {
    return std.math.clamp(value, 0.0, 1.0);
}

fn smoothstep(x: anytype, edge0: anytype, edge1: anytype) @TypeOf(x, edge0, edge1) {
    const y = std.math.clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0);
    return y * y * (3.0 - 2.0 * y);
}
