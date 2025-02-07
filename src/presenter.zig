//! This should be unchanged regardless of platform

const std = @import("std");

const math = @import("kommon/math.zig");
pub const Vec2 = math.Vec2;
pub const Rect = math.Rect;
pub const Camera = math.Camera;
pub const Color = math.Color;
pub const Point = math.Point;
const Random = math.Random;
const tof32 = math.tof32;
const lerp = math.lerp;
const in01 = math.in01;
const clamp = math.clamp;
const clamp01 = math.clamp01;
const remap = math.remap;
const inRange = math.inRange;

const core = @import("main.zig");
const Atom = core.Atom;
const Pair = core.Pair;
const Sexpr = core.Sexpr;
const Fnk = core.Fnk;
const FnkBody = core.FnkBody;
const FnkCollection = core.FnkCollection;
const VeryPermamentGameStuff = core.VeryPermamentGameStuff;
const parsing = @import("parsing.zig");

const OoM = error{ OutOfMemory, TODO, BAD_INPUT };

pub const KeyboardButton = std.meta.FieldEnum(KeyboardState);
pub const KeyboardState = struct {
    left: bool,
    right: bool,
    up: bool,
    down: bool,

    pub const init: KeyboardState = std.mem.zeroes(KeyboardState);
};

pub const Keyboard = struct {
    cur: KeyboardState,
    prev: KeyboardState,

    pub fn isDown(self: Keyboard, button: KeyboardButton) bool {
        return switch (button) {
            inline else => |x| @field(self.cur, @tagName(x)),
        };
    }

    pub fn wasPressed(self: Keyboard, button: KeyboardButton) bool {
        return self.cur.isDown(button) and !self.prev.isDown(button);
    }
};

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
    getKeyboard: fn () Keyboard,
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
    drawCaseHolder: fn (camera: Camera, world_point: Point) void,
    drawFnkHolder: fn (camera: Camera, world_point: Point) void,
    drawVariable: fn (camera: Camera, world_point: Point, visuals: AtomVisuals) void,
    drawPatternVariable: fn (camera: Camera, world_point: Point, visuals: AtomVisuals) void,

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
        .drawVariable = dummySignatures.camera_point_visuals,
        .drawPatternVariable = dummySignatures.camera_point_visuals,
        .drawPatternAtomDebug = undefined,
        .drawCaseHolder = dummySignatures.camera_point,
        .drawFnkHolder = dummySignatures.camera_point,
        .drawPairHolder = dummySignatures.camera_point,
        .drawPatternPairHolder = dummySignatures.camera_point,
        .drawPatternAtom = dummySignatures.camera_point_visuals,
        .drawPatternAtomOutline = dummySignatures.camera_point,
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

fn defaultFnkBody1(mem: *VeryPermamentGameStuff) FnkBody {
    const default_fnk =
        \\default1 {
        \\  (nil . true) -> false;
        \\  @true -> default1: (nil . true) {
        \\      false -> true;
        \\      @foo -> default2: false {
        \\          false -> true;
        \\          @thing -> true;
        \\      }
        \\  }
        \\  @asdf -> default1: (@asdf . nil) {
        \\      @hola -> @asdf;
        \\  }
        \\  @asdf -> nil {
        \\      nil -> nil;
        \\  }
        \\  true -> default1: (nil . true) {
        \\      false -> true;
        \\      @foo -> default2: false {
        \\          false -> true;
        \\          @thing -> true;
        \\      }
        \\  }
        \\  @true -> ( @true . false );
        \\  (true . nil) -> true;
        \\  (true . (true . nil)) -> true;
        \\}
    ;
    var parser = parsing.Parser{ .remaining_text = default_fnk };
    const fnk = parser.parseFnkNew(&mem.pool_for_sexprs, mem.arena_for_cases.allocator()) catch unreachable;
    return fnk.body;
}

fn defaultFnkBody2(mem: *VeryPermamentGameStuff) FnkBody {
    const default_fnk =
        \\default2 {
        \\  true -> false;
        \\  @xxx -> default2: true {
        \\      @result -> (final . @result);
        \\  }
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

        // TODO: remove this hack
        scoring_run: core.ScoringRun,

        camera: Camera,

        state: union(enum) {
            /// not used for now
            intro: IntroSequence(platform, drawer),
            level_select: LevelSelect(platform, drawer),
            editing_fnk: EditingFnk(platform, drawer),
            executing_fnk: ExecutingFnk(platform, drawer),
        },

        pub fn init(result: *Self) !void {
            const platform_alloc = platform.gpa;
            result.mem = VeryPermamentGameStuff.init(platform_alloc);
            var player_data = (try platform.getPlayerData(&result.mem)) orelse PlayerData.empty(&result.mem);

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
            const fnk = try parser.parseFnkNew(&result.mem.pool_for_sexprs, result.mem.arena_for_cases.allocator());
            try player_data.fnks.put(fnk.name, fnk.body);
            try platform.setPlayerData(player_data, &result.mem);

            try player_data.fnks.put(try result.mem.storeSexpr(Sexpr.doLit("default2")), defaultFnkBody2(&result.mem));
            result.persistence = player_data;

            result.state = .{ .editing_fnk = try .init(Fnk{
                .name = try result.mem.storeSexpr(Sexpr.doLit("default1")),
                .body = defaultFnkBody1(&result.mem),
            }, &result.mem) };

            result.scoring_run = undefined;

            try Artist(platform, drawer).init();
        }

        pub fn update(self: *Self, delta_seconds: f32) !void {
            inline for (.{
                .{ KeyboardButton.left, Vec2.new(-1, 0) },
                .{ KeyboardButton.right, Vec2.new(1, 0) },
                .{ KeyboardButton.up, Vec2.new(0, -1) },
                .{ KeyboardButton.down, Vec2.new(0, 1) },
            }) |key_dir| {
                if (platform.getKeyboard().isDown(key_dir[0])) {
                    global_camera.center = global_camera.center.add(key_dir[1].scale(delta_seconds * 20));
                }
            }

            switch (self.state) {
                .level_select => |*ui| if (ui.update(delta_seconds)) |level_index| {
                    const fnk_name = levels[level_index].fnk_name;
                    const fnk_body = self.persistence.fnks.get(fnk_name) orelse defaultFnkBody1(&self.mem);
                    self.state = .{ .editing_fnk = try .init(
                        Fnk{ .name = fnk_name, .body = fnk_body },
                        &self.mem,
                    ) };
                },
                .editing_fnk => |*editing| if (try editing.update(delta_seconds)) {
                    // todo
                    const fnk = try editing.getFnk();
                    try self.persistence.fnks.put(fnk.name, fnk.body);
                    self.scoring_run = try core.ScoringRun.initFromFnks(
                        self.persistence.fnks,
                        &self.mem,
                    );
                    self.state = .{ .executing_fnk = try .init(
                        editing.sample_input,
                        fnk.name,
                        &self.scoring_run,
                    ) };
                },
                // TODO
                .executing_fnk => |*executing| if (try executing.update(delta_seconds)) |final_value| {
                    _ = final_value;
                    self.state = .{ .level_select = .init() };
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
    .{ .fnk_name = &Sexpr.doLit("default") },
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
                    const N = 10;
                    var buffer: [N]Vec2 = undefined;
                    for (0..N) |k| {
                        const t = tof32(k) / N;
                        buffer[k] = Vec2.new(t, -0.2 * @sin(t * std.math.pi));
                    }
                    const res = buffer;
                    break :blk res;
                },
            },
            .false = AtomVisuals{
                .color = .from01(0.9, 0.5, 0.5),
                .profile = &.{ .new(1.0 / 6.0, 0.2), .new(0.5, -0.2), .new(5.0 / 6.0, 0.2) },
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
            const seed = std.array_hash_map.hashString(name);
            var rnd_state = std.Random.DefaultPrng.init(seed);
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
            const seed = std.array_hash_map.hashString(name);
            var rnd_state = std.Random.DefaultPrng.init(seed);
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

        pub fn drawHoldedFnk(camera: Camera, fnk_point: Point, is_main: f32, value: *const Sexpr) !void {
            drawer.drawFnkHolder(camera, fnk_point
                .applyToLocalPoint(.{ .scale = lerp(1, 0.5, is_main) })
                .applyToLocalPoint(.{ .pos = .new(lerp(-1.5, -2.5, is_main), 0), .turns = 0.25 }));
            if (!value.equals(&Sexpr.identity)) {
                try drawSexpr(
                    camera,
                    fnk_point,
                    value,
                );
            }
        }

        pub fn drawOffscreenCableTo(camera: Camera, pattern: Point) void {
            // TODO: store some state to avoid cable jumps? or maybe make the cable periodic
            drawer.drawCable(camera, pattern.applyToLocalPosition(.new(-CABLE_OFFSCREEN_DIST, 0)), pattern.applyToLocalPosition(.new(-0.5, 0)), pattern.scale, -CABLE_OFFSCREEN_DIST);
        }

        pub fn drawCableTo(camera: Camera, origin: Vec2, pattern: Point) void {
            drawer.drawCable(camera, origin, pattern.applyToLocalPosition(.new(-0.5, 0)), pattern.scale, 0);
        }

        pub fn drawPatternOutline(camera: Camera, world_point: Point) !void {
            drawer.drawPatternAtomOutline(camera, world_point);
        }

        pub fn drawVariable(camera: Camera, world_point: Point, name: []const u8) !void {
            const visuals = try AtomVisualCache.getAtomVisuals(name);
            drawer.drawVariable(camera, world_point, visuals);
        }

        pub fn drawPatternVariable(camera: Camera, world_point: Point, name: []const u8) !void {
            const visuals = try AtomVisualCache.getAtomVisuals(name);
            drawer.drawPatternVariable(camera, world_point, visuals);
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
                .atom_var => |x| {
                    try drawVariable(camera, world_point, x.value);
                },
            }
        }

        pub fn drawBothSexpr(camera: Camera, world_point: Point, is_pattern: f32, sexpr: *const Sexpr) !void {
            std.debug.assert(in01(is_pattern));
            if (is_pattern > 0.5) {
                try drawPatternSexpr(
                    camera,
                    world_point.applyToLocalPoint(.{ .turns = remap(
                        is_pattern,
                        0.5,
                        1,
                        0.5,
                        0,
                    ) }),
                    sexpr,
                );
            } else {
                try drawSexpr(
                    camera,
                    world_point.applyToLocalPoint(.{ .turns = remap(
                        is_pattern,
                        0.5,
                        0,
                        -0.5,
                        0,
                    ) }),
                    sexpr,
                );
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
                .atom_var => |x| {
                    try drawPatternVariable(camera, world_point, x.value);
                },
            }
        }
    };
}

const SexprView = struct {
    pub fn overlapsPatternAtom(atom_point: Point, needle_pos: Vec2) bool {
        const p = atom_point.inverseApplyGetLocal(.{ .pos = needle_pos }).pos;
        return inRange(p.y, -1, 1) and
            inRange(p.x, -1, 0.5 * (1 - @abs(p.y)));
    }

    pub fn overlapsSexpr(alloc: std.mem.Allocator, sexpr: *const Sexpr, sexpr_pos: Point, needle_pos: Vec2) !?core.SexprAddress {
        var result = std.ArrayList(core.SexprAddressItem).init(alloc);
        defer result.deinit();
        // TODO (low priority): probably can be made more efficient by using less changes of coordinates

        var cur_sexpr_pos = sexpr_pos;
        var cur_sexpr = sexpr;
        while (true) {
            switch (cur_sexpr.*) {
                .atom_lit, .atom_var => {
                    if (overlapsAtom(cur_sexpr_pos, needle_pos, .atom)) {
                        return try result.toOwnedSlice();
                    } else {
                        return null;
                    }
                },
                .pair => |pair| {
                    const p = cur_sexpr_pos.inverseApplyGetLocal(.{ .pos = needle_pos }).pos;

                    if (overlapsAtom(cur_sexpr_pos, needle_pos, .pair)) {
                        return try result.toOwnedSlice();
                    } else if (inRange(p.y, -1, 0)) {
                        try result.append(.left);
                        cur_sexpr = pair.left;
                        cur_sexpr_pos = cur_sexpr_pos.applyToLocalPoint(.{
                            .pos = .new(0.5, -0.5),
                            .scale = 0.5,
                        });
                    } else if (inRange(p.y, 0, 1)) {
                        try result.append(.right);
                        cur_sexpr = pair.right;
                        cur_sexpr_pos = cur_sexpr_pos.applyToLocalPoint(.{
                            .pos = .new(0.5, 0.5),
                            .scale = 0.5,
                        });
                    } else {
                        return null;
                    }
                },
            }
        }
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

    pub fn overlapsAtom(atom_point: Point, needle_pos: Vec2, kind: enum { atom, pair }) bool {
        const p = atom_point.inverseApplyGetLocal(.{ .pos = needle_pos }).pos;
        return inRange(p.y, -1, 1) and
            if (kind == .pair)
            inRange(p.x, -0.5 * (1 - @abs(p.y)), 0.5 - 0.25 * (1 - @abs(@abs(p.y) - 0.5) / 0.5))
        else
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

    pub fn sexprChildView(parent: Point, address: core.SexprAddress) Point {
        var result = parent;
        for (address) |cur| {
            result = result.applyToLocalPoint(.{
                .pos = switch (cur) {
                    .left => .new(0.5, -0.5),
                    .right => .new(0.5, 0.5),
                },
                .scale = 0.5,
            });
        }
        return result;
    }
};

const DIST_TO_TEMPLATE = 5;
const FNK_NAME_OFFSET = Point{
    .pos = .new(DIST_TO_TEMPLATE - 1, -0.75),
    .turns = -0.25,
    .scale = 0.5,
};
const SAMPLE_INPUT_POS = Point{ .pos = .new(1, 0) };
const MAIN_FNK_POS = Point{ .pos = .new(0, -1.25), .turns = -0.25 };
const DIST_BETWEEN_QUEUED_FNKS = 3.5;
const CABLE_OFFSCREEN_DIST = 15;

const CaseState = struct {
    // TODO: generic tree type to avoid duplication
    pattern: *const Sexpr,
    fnk_name: *const Sexpr,
    template: *const Sexpr,
    next: ?CaseGroup,

    pattern_point_relative_to_parent: Point,
};
const CaseGroup = struct {
    cases: std.ArrayListUnmanaged(CaseState),
    unfolded: usize,

    pub fn caseAt(self: CaseGroup, address: core.CaseAddress) !CaseState {
        return (try caseRefAt(self, address)).*;
    }

    // TODO: could this triplication be removed?
    pub fn caseRefAt(self: CaseGroup, address: core.CaseAddress) !*CaseState {
        if (address.len == 0) {
            return error.BAD_INPUT;
        } else if (address.len == 1) {
            return &self.cases.items[address[0]];
        } else if (self.cases.items[address[0]].next) |next| {
            return next.caseRefAt(address[1..]);
        } else {
            return error.BAD_INPUT;
        }
    }

    pub fn insertAt(self: *CaseGroup, mem: *VeryPermamentGameStuff, address: core.CaseAddress, case: CaseState) !void {
        if (address.len == 0) {
            return error.BAD_INPUT;
        } else if (address.len == 1) {
            try self.cases.insert(mem.gpa, address[0], case);
        } else if (self.cases.items[address[0]].next) |*next| {
            try next.insertAt(mem, address[1..], case);
        } else if (address.len == 2 and address[1] == 0) {
            var new_next: CaseGroup = .{
                .unfolded = 0,
                .cases = std.ArrayListUnmanaged(CaseState){},
            };
            try new_next.cases.append(mem.gpa, case);
            self.cases.items[address[0]].next = new_next;
        } else {
            return error.BAD_INPUT;
        }
    }

    pub fn removeAt(self: *CaseGroup, address: core.CaseAddress) !CaseState {
        if (address.len == 0) {
            return error.BAD_INPUT;
        } else if (address.len == 1) {
            return self.cases.orderedRemove(address[0]);
        } else if (self.cases.items[address[0]].next) |*next| {
            const result = next.removeAt(address[1..]);
            if (next.cases.items.len == 0) {
                self.cases.items[address[0]].next = null;
            }
            return result;
        } else {
            return error.BAD_INPUT;
        }
    }

    pub fn setUnfolded(self: *CaseGroup, address: core.CaseAddress) !void {
        if (address.len == 0) {
            return error.BAD_INPUT;
        } else if (address.len == 1) {
            self.unfolded = address[0];
        } else if (self.cases.items[address[0]].next) |*next| {
            try next.setUnfolded(address[1..]);
        } else {
            return error.BAD_INPUT;
        }
    }

    pub fn getGlobalPointOf(self: CaseGroup, parent_point: Point, full_address: core.FullAddress) !Point {
        switch (full_address.which) {
            .pattern => return SexprView.sexprPatternChildView(
                try self.getPatternGlobalPoint(parent_point, full_address.case_address),
                full_address.sexpr_address,
            ),
            .template => return SexprView.sexprChildView(
                (try self.getPatternGlobalPoint(
                    parent_point,
                    full_address.case_address,
                )).applyToLocalPoint(.{ .pos = .new(DIST_TO_TEMPLATE, 0) }),
                full_address.sexpr_address,
            ),
            .fnk_name => return SexprView.sexprChildView(
                (try self.getPatternGlobalPoint(
                    parent_point,
                    full_address.case_address,
                )).applyToLocalPoint(FNK_NAME_OFFSET),
                full_address.sexpr_address,
            ),
        }
    }

    pub fn getPatternGlobalPoint(self: CaseGroup, parent_point: Point, address: core.CaseAddress) !Point {
        if (address.len == 0) {
            return parent_point;
        } else if (address.len == 1) {
            return parent_point.applyToLocalPoint(
                self.cases.items[address[0]].pattern_point_relative_to_parent,
            );
        } else if (self.cases.items[address[0]].next) |*next| {
            return next.getPatternGlobalPoint(parent_point.applyToLocalPoint(
                self.cases.items[address[0]].pattern_point_relative_to_parent,
            ), address[1..]);
        } else {
            return error.BAD_INPUT;
        }
    }

    pub fn getSexprAt(self: CaseGroup, full_address: core.FullAddress) !*const core.Sexpr {
        const case = try self.caseAt(full_address.case_address);
        return switch (full_address.which) {
            .pattern => case.pattern.getAt(full_address.sexpr_address) orelse error.BAD_INPUT,
            .template => case.template.getAt(full_address.sexpr_address) orelse error.BAD_INPUT,
            .fnk_name => case.fnk_name.getAt(full_address.sexpr_address) orelse error.BAD_INPUT,
        };
    }

    pub fn setSexprAt(self: CaseGroup, mem: *VeryPermamentGameStuff, full_address: core.FullAddress, value: *const core.Sexpr) !void {
        const case_ref = try self.caseRefAt(full_address.case_address);
        switch (full_address.which) {
            .pattern => case_ref.pattern = try case_ref.pattern.setAt(mem, full_address.sexpr_address, value),
            .template => case_ref.template = try case_ref.template.setAt(mem, full_address.sexpr_address, value),
            .fnk_name => case_ref.fnk_name = try case_ref.fnk_name.setAt(mem, full_address.sexpr_address, value),
        }
    }
};

// TODO: this shouldn't be a global
var global_camera = Camera{ .center = .new(7, 3), .height = 15.0 };

pub fn EditingFnk(platform: Platform, drawer: Drawer) type {
    return struct {
        const Self = @This();
        const artist = Artist(platform, drawer);

        const SexprPlace = union(enum) {
            full_address: core.FullAddress,
            toolbar: usize,
            sample_input: core.SexprAddress,
            main_fnk_name: core.SexprAddress,

            pub fn equals(self: @This(), other: @This()) bool {
                if (std.meta.activeTag(self) != std.meta.activeTag(other)) return false;
                return switch (self) {
                    .full_address => |self_full| self_full.equals(other.full_address),
                    .toolbar => |self_toolbar| self_toolbar == other.toolbar,
                    .sample_input => |self_local| core.equalSexprAddress(self_local, other.sample_input),
                    .main_fnk_name => |self_local| core.equalSexprAddress(self_local, other.main_fnk_name),
                };
            }

            pub fn getGlobalPoint(address: @This(), self: Self) !Point {
                return switch (address) {
                    .full_address => |full_address| try self.cases.getGlobalPointOf(
                        Point{},
                        full_address,
                    ),
                    .toolbar => |index| toolbar.things[index].point,
                    .sample_input => |local| SexprView.sexprChildView(SAMPLE_INPUT_POS, local),
                    .main_fnk_name => |local| SexprView.sexprChildView(MAIN_FNK_POS, local),
                };
            }

            pub fn getSexpr(address: @This(), self: Self) !*const Sexpr {
                return switch (address) {
                    .full_address => |full_address| try self.cases.getSexprAt(full_address),
                    .toolbar => |index| toolbar.things[index].value,
                    .sample_input => |local| self.sample_input.getAt(local).?,
                    .main_fnk_name => |local| self.fnk_name.getAt(local).?,
                };
            }

            pub fn setSexpr(address: @This(), self: *Self, value: *const Sexpr) !void {
                switch (address) {
                    .full_address => |full_address| try self.cases.setSexprAt(self.mem, full_address, value),
                    .sample_input => |local_address| {
                        self.sample_input = try self.sample_input.setAt(self.mem, local_address, value);
                    },
                    .toolbar, .main_fnk_name => unreachable,
                }
            }

            pub fn isPattern(address: @This()) bool {
                return switch (address) {
                    .full_address => |full_address| full_address.which == .pattern,
                    else => false,
                };
            }

            pub fn acceptsDrop(address: @This()) bool {
                return switch (address) {
                    .toolbar => false,
                    .main_fnk_name => false,
                    .full_address => true,
                    .sample_input => true,
                };
            }
        };

        mem: *VeryPermamentGameStuff,

        fnk_name: *const Sexpr,
        cases: CaseGroup,
        sample_input: *const Sexpr,

        focus: union(enum) {
            nothing,
            hovering_special_case: f32,
            hovering_case: core.CaseAddress,
            grabbing_case: struct {
                case: CaseState,
                address_if_released: ?core.CaseAddress,
            },
            hovering_sexpr: struct {
                address: SexprPlace,
                global_point: Point,
            },
            grabbing_sexpr: struct {
                sexpr: *const Sexpr,
                address_if_released: ?SexprPlace,
                point: Point,
                is_pattern: f32,
            },
        } = .{ .nothing = {} },

        const toolbar = struct {
            const atom_values = [_]Sexpr{
                Sexpr.doPair(&Sexpr.nil, &Sexpr.nil),
                Sexpr.nil,
                Sexpr.true,
                Sexpr.false,
            };
            const things = blk: {
                var result: [atom_values.len]struct { value: *const Sexpr, point: Point, index: usize } = undefined;
                for (&atom_values, 0..) |*atom, k| {
                    result[k] = .{ .value = atom, .point = .{
                        .pos = .new(tof32(k) * 1.6 + 2.5, -2.5),
                        .scale = 0.5,
                    }, .index = k };
                }
                const xx = result;
                break :blk xx;
            };

            const special_case_point = Point{ .pos = .new(9, -2.5), .scale = 0.5 };
            const special_case_value = CaseState{
                .fnk_name = &Sexpr.identity,
                .pattern = &Sexpr.var_v1,
                .template = &Sexpr.var_v1,
                .next = null,
                .pattern_point_relative_to_parent = special_case_point,
            };

            pub fn overlapsWithSpecialCase(mouse_pos: Vec2) bool {
                const local_point = special_case_point
                    .applyToLocalPoint(.{ .pos = .new(2, 0) })
                    .inverseApplyGetLocalPosition(mouse_pos);

                return local_point.mag() < 2;
            }

            pub fn findOverlap(mouse_pos: Vec2) ?std.meta.Elem(@TypeOf(things)) {
                for (things) |thing| {
                    if (SexprView.overlapsAtom(thing.point, mouse_pos, .atom)) {
                        return thing;
                    }
                }
                return null;
            }

            pub fn draw() !void {
                for (things) |thing| {
                    try artist.drawSexpr(global_camera, thing.point, thing.value);
                }

                try artist.drawPatternSexpr(global_camera, special_case_point
                    .applyToLocalPoint(.{ .pos = .new(1, 0) }), special_case_value.pattern);
                try artist.drawSexpr(global_camera, special_case_point
                    .applyToLocalPoint(.{ .pos = .new(3, 0) }), special_case_value.template);
                // TODO: artist.drawCableBetween(camera, pattern_pos, template_pos);
                drawer.drawCable(
                    global_camera,
                    special_case_point.applyToLocalPosition(.new(1.5, 0)),
                    special_case_point.applyToLocalPosition(.new(2.5, 0)),
                    special_case_point.scale,
                    0,
                );
            }
        };

        fn makeCasesPhysical(mem: *VeryPermamentGameStuff, cases: core.MatchCases) !CaseGroup {
            var result = std.ArrayListUnmanaged(CaseState){};
            for (cases.items, 0..) |case, k| {
                try result.append(mem.gpa, .{
                    .fnk_name = case.fnk_name,
                    .pattern = case.pattern,
                    .template = case.template,
                    .next = if (case.next) |next| try makeCasesPhysical(mem, next) else null,
                    .pattern_point_relative_to_parent = .{ .pos = .new(3, 2.5 + 1.5 * tof32(k)), .scale = 0.5 },
                });
            }
            return .{ .cases = result, .unfolded = 0 };
        }

        pub fn getFnk(self: Self) !Fnk {
            return Fnk{
                .name = self.fnk_name,
                .body = .{ .cases = try getMatchCases(self.mem, self.cases) },
            };
        }

        fn getMatchCases(mem: *VeryPermamentGameStuff, group: CaseGroup) !core.MatchCases {
            var result = std.ArrayListUnmanaged(core.MatchCaseDefinition){};
            for (group.cases.items) |case| {
                try result.append(mem.arena_for_cases.allocator(), .{
                    .fnk_name = case.fnk_name,
                    .pattern = case.pattern,
                    .template = case.template,
                    .next = if (case.next) |next|
                        (try getMatchCases(mem, next))
                    else
                        null,
                });
            }
            return result;
        }

        pub fn init(fnk: Fnk, mem: *VeryPermamentGameStuff) !Self {
            const cases = try makeCasesPhysical(mem, fnk.body.cases);
            const sample_input = try mem.storeSexpr(Sexpr.doPair(&Sexpr.nil, &Sexpr.input));
            return .{
                .mem = mem,
                .fnk_name = fnk.name,
                .cases = cases,
                // .sample_input = &Sexpr.true,
                .sample_input = sample_input,
            };
        }

        // TODO: deinit

        fn debugMakeAddress(self: *Self, k: usize) !core.CaseAddress {
            return try debugMakeAddress2(self.mem, k);
        }
        fn debugMakeAddress2(mem: *VeryPermamentGameStuff, k: usize) !core.CaseAddress {
            return try mem.gpa.dupe(usize, &.{k});
        }
        fn childAddress(mem: *VeryPermamentGameStuff, parent_address: core.CaseAddress, k: usize) !core.CaseAddress {
            const new_buf = try mem.gpa.alloc(usize, parent_address.len + 1);
            @memcpy(new_buf[0..parent_address.len], parent_address);
            new_buf[parent_address.len] = k;
            return new_buf;
        }

        pub fn update(self: *Self, delta_seconds: f32) !bool {
            // focus-specific updates
            switch (self.focus) {
                .grabbing_case => |*grabbing| {
                    // grabbing case parent is the nothing!
                    grabbing.case.pattern_point_relative_to_parent.lerp_towards((Point{
                        .pos = platform.getMouse().cur.pos(global_camera),
                        .scale = if (grabbing.address_if_released == null) 0.5 else 1,
                    }).applyToLocalPoint(.{ .pos = .new(3, 0) }), 0.6, delta_seconds);
                },
                .grabbing_sexpr => |*grabbing| {
                    grabbing.point.lerp_towards(if (grabbing.address_if_released) |goal|
                        (try goal.getGlobalPoint(self.*))
                            .applyToLocalPoint(switch (goal) {
                            .full_address => |full| switch (full.which) {
                                .pattern => .{ .turns = 0.02, .pos = .new(-0.5, 0) },
                                .template => .{ .turns = -0.02, .pos = .new(0.5, 0) },
                                .fnk_name => .{ .turns = 0.02, .pos = .new(0.5, 0) },
                            },
                            .sample_input => .{ .turns = -0.02, .pos = .new(0.5, 0) },
                            .toolbar, .main_fnk_name => unreachable,
                        })
                    else
                        Point{
                            .pos = platform.getMouse().cur.pos(global_camera),
                            .scale = 1,
                        }, 0.6, delta_seconds);
                    math.lerp_towards(&grabbing.is_pattern, if (grabbing.address_if_released) |goal|
                        if (goal.isPattern()) 1 else 0
                    else
                        @round(grabbing.is_pattern), 0.6, delta_seconds);
                },
                .nothing => {},
                .hovering_sexpr => |*hovering| {
                    if (std.meta.activeTag(hovering.address) == .full_address) {
                        const unfolded = hovering.address.full_address.case_address;
                        try self.cases.setUnfolded(unfolded);
                    }
                    hovering.global_point.lerp_towards(
                        (try hovering.address.getGlobalPoint(self.*)).applyToLocalPoint(.{ .scale = 1.1 }),
                        0.6,
                        delta_seconds,
                    );
                },
                .hovering_special_case => |*hot| {
                    math.lerp_towards(hot, 1, 0.6, delta_seconds);
                },
                .hovering_case => |unfolded| {
                    try self.cases.setUnfolded(unfolded);
                },
            }

            // update cases & focus
            if (std.meta.activeTag(self.focus) == .grabbing_case) {
                try doGrabbingCaseFirstPass(self.mem, self.focus.grabbing_case.address_if_released, &.{}, self.cases, delta_seconds);
                self.focus.grabbing_case.address_if_released = if (self.cases.cases.items.len == 0) try self.debugMakeAddress(0) else try doGrabbingCaseSecondPass(
                    platform.getMouse().cur.pos(global_camera),
                    self.focus.grabbing_case.address_if_released,
                    self.mem,
                    &.{},
                    &self.cases,
                );
            } else {
                const mouse_pos = platform.getMouse().cur.pos(global_camera);
                const maybe_overlapped: ?union(enum) {
                    special_case,
                    case: core.CaseAddress,
                    sexpr: SexprPlace,
                } = if (try updateCasePositionsAndReturnMouseOverlap(
                    self.mem,
                    &.{},
                    mouse_pos,
                    self.cases,
                    delta_seconds,
                )) |overlap|
                    switch (overlap) {
                        .case => |case| .{ .case = case },
                        .sexpr => |sexpr| .{ .sexpr = .{ .full_address = sexpr.full_address } },
                    }
                else if (toolbar.findOverlap(mouse_pos)) |overlap|
                    .{ .sexpr = .{ .toolbar = overlap.index } }
                else if (try SexprView.overlapsSexpr(self.mem.gpa, self.sample_input, SAMPLE_INPUT_POS, mouse_pos)) |overlap|
                    .{ .sexpr = .{ .sample_input = overlap } }
                else if (try SexprView.overlapsSexpr(self.mem.gpa, self.fnk_name, MAIN_FNK_POS, mouse_pos)) |overlap|
                    .{ .sexpr = .{ .main_fnk_name = overlap } }
                else if (toolbar.overlapsWithSpecialCase(mouse_pos))
                    .special_case
                else
                    null;

                switch (self.focus) {
                    .grabbing_case => unreachable,
                    .grabbing_sexpr => |*grabbing| if (maybe_overlapped) |overlapped|
                        switch (overlapped) {
                            .special_case => grabbing.address_if_released = null,
                            .case => |case| {
                                try self.cases.setUnfolded(case);
                                grabbing.address_if_released = null;
                            },
                            .sexpr => |place| {
                                if (std.meta.activeTag(place) == .full_address) {
                                    try self.cases.setUnfolded(place.full_address.case_address);
                                }
                                grabbing.address_if_released = if (place.acceptsDrop()) place else null;
                            },
                        }
                    else {
                        grabbing.address_if_released = null;
                    },
                    .nothing, .hovering_sexpr, .hovering_case, .hovering_special_case => if (maybe_overlapped) |overlapped| {
                        switch (overlapped) {
                            .special_case => if (!(std.meta.activeTag(self.focus) == .hovering_special_case)) {
                                self.focus = .{ .hovering_special_case = 0 };
                            },
                            .case => |case| self.focus = .{ .hovering_case = case },
                            .sexpr => |place| {
                                if (!(std.meta.activeTag(self.focus) == .hovering_sexpr and self.focus.hovering_sexpr.address.equals(place))) {
                                    self.focus = .{
                                        .hovering_sexpr = .{
                                            .address = place,
                                            .global_point = try place.getGlobalPoint(self.*),
                                        },
                                    };
                                }
                            },
                        }
                    } else {
                        self.focus = .nothing;
                    },
                }
            }

            if (platform.getMouse().wasPressed(.left)) {
                switch (self.focus) {
                    .nothing => {},
                    .grabbing_case => |*grabbing| {
                        if (grabbing.address_if_released) |address| {
                            const global_point = grabbing.case.pattern_point_relative_to_parent;
                            const parent_point = try self.cases.getPatternGlobalPoint(.{}, address[0 .. address.len - 1]);
                            grabbing.case.pattern_point_relative_to_parent = parent_point.inverseApplyGetLocal(global_point);
                            try self.cases.insertAt(self.mem, address, grabbing.case);
                            self.focus = .{ .hovering_case = address };
                        } else {
                            self.focus = .{ .nothing = {} };
                        }
                    },
                    .grabbing_sexpr => |grabbing| {
                        if (grabbing.address_if_released) |address| {
                            try address.setSexpr(self, grabbing.sexpr);
                            self.focus = .{ .hovering_sexpr = .{
                                .address = address,
                                .global_point = grabbing.point,
                            } };
                        } else {
                            self.focus = .{ .nothing = {} };
                        }
                    },
                    .hovering_special_case => {
                        self.focus = .{ .grabbing_case = .{
                            .case = toolbar.special_case_value,
                            .address_if_released = null,
                        } };
                    },
                    .hovering_case => |unfolded| {
                        const global_point = try self.cases.getPatternGlobalPoint(.{}, unfolded);
                        var asdf = try self.cases.removeAt(unfolded);
                        asdf.pattern_point_relative_to_parent = global_point;
                        self.focus = .{ .grabbing_case = .{
                            .case = asdf,
                            .address_if_released = unfolded,
                        } };
                    },
                    .hovering_sexpr => |hovering| {
                        self.focus = .{
                            .grabbing_sexpr = .{
                                .address_if_released = if (hovering.address.acceptsDrop()) hovering.address else null,
                                .is_pattern = if (hovering.address.isPattern()) 1 else 0,
                                .point = hovering.global_point,
                                .sexpr = try hovering.address.getSexpr(self.*),
                            },
                        };

                        if (std.meta.activeTag(hovering.address) == .full_address and hovering.address.full_address.which == .fnk_name) {
                            (try self.cases.caseRefAt(hovering.address.full_address.case_address)).fnk_name = &Sexpr.identity;
                        }
                    },
                }
            }

            if (platform.getMouse().wasPressed(.right)) {
                return true;
            }

            return false;
        }

        fn isPattern(which: @FieldType(core.FullAddress, "which")) f32 {
            return switch (which) {
                .pattern => 1,
                else => 0,
            };
        }

        pub fn draw(self: Self) !void {
            drawer.clear(Color.gray(128));
            {
                artist.drawOffscreenCableTo(global_camera, SAMPLE_INPUT_POS);
                try artist.drawSexpr(
                    global_camera,
                    SAMPLE_INPUT_POS,
                    self.sample_input,
                );
                // TODO: also draw these while executing
                try artist.drawHoldedFnk(global_camera, MAIN_FNK_POS, 1, self.fnk_name);
            }

            try drawCases(true, .{}, self.cases);

            try toolbar.draw();

            switch (self.focus) {
                .nothing => {},
                .hovering_special_case => |hot| {
                    // TODO: cooler
                    drawer.drawCaseHolder(global_camera, toolbar.special_case_point
                        .applyToLocalPoint(.{ .scale = hot }));
                },
                .hovering_case => |unfolded| {
                    const pattern_point = try self.cases.getPatternGlobalPoint(.{}, unfolded);
                    drawer.drawCaseHolder(global_camera, .{
                        .pos = pattern_point.pos.sub(.new(3, 0)),
                        .scale = pattern_point.scale,
                    });
                },
                .grabbing_sexpr => |grabbing| {
                    try artist.drawBothSexpr(
                        global_camera,
                        grabbing.point,
                        grabbing.is_pattern,
                        grabbing.sexpr,
                    );
                },
                .grabbing_case => |grabbing| {
                    // grabbing case parent is the nothing!
                    const pattern_point = grabbing.case.pattern_point_relative_to_parent;
                    try artist.drawPatternSexpr(
                        global_camera,
                        pattern_point,
                        grabbing.case.pattern,
                    );
                    try drawCaseExtra(pattern_point, grabbing.case);
                },
                .hovering_sexpr => |hovering| {
                    try artist.drawBothSexpr(
                        global_camera,
                        hovering.global_point,
                        if (hovering.address.isPattern()) 1 else 0,
                        try hovering.address.getSexpr(self),
                    );
                    // try artist.drawPatternOutline(camera, artist.sexprPatternChildView(
                    //     case.pattern_point,
                    //     full_address.sexpr_address,
                    // ));
                },
            }
        }

        fn drawCases(is_first: bool, parent_point: Point, group: CaseGroup) OoM!void {
            for (group.cases.items) |case| {
                const pattern_point = parent_point.applyToLocalPoint(case.pattern_point_relative_to_parent);
                try artist.drawPatternSexpr(
                    global_camera,
                    pattern_point,
                    case.pattern,
                );
                if (case.pattern_point_relative_to_parent.scale >= 0.9) {
                    try drawCaseExtra(pattern_point, case);
                }

                const pos = pattern_point.applyToLocalPosition(.new(0, 1));
                drawer.drawCable(
                    global_camera,
                    pos.sub(.new(if (is_first) 5 else 3, 0)),
                    pos,
                    1,
                    0,
                );
            }
        }

        fn drawCaseExtra(pattern_point: Point, case: CaseState) !void {
            try artist.drawSexpr(
                global_camera,
                pattern_point.applyToLocalPoint(.{ .pos = .new(DIST_TO_TEMPLATE, 0) }),
                case.template,
            );
            try artist.drawHoldedFnk(global_camera, pattern_point.applyToLocalPoint(FNK_NAME_OFFSET), 0, case.fnk_name);
            drawer.drawCable(
                global_camera,
                pattern_point.applyToLocalPosition(.new(0.5, 0)),
                pattern_point.applyToLocalPosition(.new(DIST_TO_TEMPLATE - 0.5, 0)),
                pattern_point.scale,
                0,
            );
            if (case.next) |next| {
                try drawCases(false, pattern_point, next);
            }
        }

        const OverlapResult = union(enum) {
            case: core.CaseAddress,
            sexpr: struct {
                full_address: core.FullAddress,
            },
        };

        fn updateCasePositionsAndReturnMouseOverlap(mem: *VeryPermamentGameStuff, parent_address: core.CaseAddress, relative_mouse_pos: Vec2, group: CaseGroup, delta_seconds: f32) !?OverlapResult {
            const is_gen0 = parent_address.len == 0;
            var cur_top_line: f32 = 2;
            const unfolded = group.unfolded;

            var overlapped: ?OverlapResult = null;
            for (group.cases.items, 0..) |*case, k| {
                const is_folded: bool = k != unfolded;
                defer cur_top_line += if (is_folded) 1.5 else 2.5;
                const relative_pattern_point = Point{
                    .pos = .new(if (is_gen0) 5 else 4, cur_top_line + if (is_folded) tof32(0.5) else 1.0),
                    .scale = if (is_folded) 0.5 else 1,
                };
                case.pattern_point_relative_to_parent.lerp_towards(relative_pattern_point, 0.6, delta_seconds);

                const local_mouse_pos = relative_pattern_point.inverseApplyGetLocalPosition(relative_mouse_pos);

                const cur_address = try childAddress(mem, parent_address, k);

                if (try SexprView.overlapsPatternSexpr(
                    platform.gpa,
                    case.pattern,
                    relative_pattern_point,
                    relative_mouse_pos,
                )) |local_address| {
                    overlapped = .{ .sexpr = .{ .full_address = .{
                        .case_address = cur_address,
                        .sexpr_address = local_address,
                        .which = .pattern,
                    } } };
                } else if (blk: {
                    if (is_folded) break :blk null;
                    break :blk try SexprView.overlapsSexpr(
                        platform.gpa,
                        case.template,
                        relative_pattern_point.applyToLocalPoint(.{ .pos = .new(DIST_TO_TEMPLATE, 0) }),
                        relative_mouse_pos,
                    );
                }) |local_address| {
                    overlapped = .{ .sexpr = .{ .full_address = .{
                        .case_address = cur_address,
                        .sexpr_address = local_address,
                        .which = .template,
                    } } };
                } else if (blk: {
                    if (is_folded) break :blk null;
                    break :blk try SexprView.overlapsSexpr(
                        platform.gpa,
                        case.fnk_name,
                        relative_pattern_point.applyToLocalPoint(FNK_NAME_OFFSET),
                        relative_mouse_pos,
                    );
                }) |local_address| {
                    overlapped = .{ .sexpr = .{ .full_address = .{
                        .case_address = cur_address,
                        .sexpr_address = local_address,
                        .which = .fnk_name,
                    } } };
                } else if (inRange(local_mouse_pos.y, -1, 1) and
                    inRange(local_mouse_pos.x, -5 / case.pattern_point_relative_to_parent.scale, 0))
                {
                    overlapped = .{ .case = cur_address };
                }

                if (!is_folded) if (case.next) |next| {
                    const child_overlap = try updateCasePositionsAndReturnMouseOverlap(
                        mem,
                        cur_address,
                        local_mouse_pos,
                        next,
                        delta_seconds,
                    );
                    if (child_overlap) |child| {
                        overlapped = child;
                    }
                };
            }
            return overlapped;
        }

        fn getUnfoldedChild(address_if_released: ?core.CaseAddress, parent_address: core.CaseAddress, group_unfolded: usize) union(enum) {
            normal: usize,
            above: usize,
        } {
            if (address_if_released) |k| {
                if (k.len == parent_address.len + 1) {
                    return .{ .above = k[parent_address.len] };
                } else {
                    return .{ .normal = group_unfolded };
                }
            } else {
                return .{ .normal = group_unfolded };
            }
        }

        fn doGrabbingCaseFirstPass(mem: *VeryPermamentGameStuff, address_if_released: ?core.CaseAddress, parent_address: core.CaseAddress, group: CaseGroup, delta_seconds: f32) !void {
            // first pass just to update positions almost as usual
            const is_gen0 = parent_address.len == 0;
            var cur_top_line: f32 = 2;
            const unfolded = getUnfoldedChild(address_if_released, parent_address, group.unfolded);
            for (group.cases.items, 0..) |*case, k| {
                if (std.meta.eql(unfolded, .{ .above = k })) {
                    cur_top_line += 1.5;
                }
                const is_folded = !std.meta.eql(unfolded, .{ .normal = k });
                defer cur_top_line += if (is_folded) 1.5 else 2.5;
                const relative_pattern_point = Point{
                    .pos = .new(if (is_gen0) 5 else 4, cur_top_line + if (is_folded) tof32(0.5) else 1.0),
                    .scale = if (is_folded) 0.5 else 1,
                };
                case.pattern_point_relative_to_parent.lerp_towards(
                    relative_pattern_point,
                    0.6,
                    delta_seconds,
                );

                const cur_address = try childAddress(mem, parent_address, k);
                if (!is_folded) if (case.next) |next| {
                    try doGrabbingCaseFirstPass(
                        mem,
                        address_if_released,
                        cur_address,
                        next,
                        delta_seconds,
                    );
                };
            }
        }

        fn doGrabbingCaseSecondPass(
            mouse_pos_relative_to_parent: Vec2,
            address_if_released: ?core.CaseAddress,
            mem: *VeryPermamentGameStuff,
            parent_address: core.CaseAddress,
            group: *CaseGroup,
        ) !?core.CaseAddress {
            // second pass to update the grabbing state
            for (group.cases.items, 0..) |*case, k| {
                const grabbing_pos_relative_to_cur = Point.inverseApplyGetLocalPosition(
                    case.pattern_point_relative_to_parent,
                    mouse_pos_relative_to_parent,
                );
                if (inRange(
                    grabbing_pos_relative_to_cur.y,
                    -1,
                    1,
                ) and inRange(
                    grabbing_pos_relative_to_cur.x,
                    -5.0 / case.pattern_point_relative_to_parent.scale,
                    0,
                )) {
                    group.unfolded = k;
                    return null;
                }
            } else {
                for (group.cases.items, 0..) |*case, k| {
                    const grabbing_pos_relative_to_cur = Point.inverseApplyGetLocalPosition(
                        case.pattern_point_relative_to_parent,
                        mouse_pos_relative_to_parent,
                    );
                    if (grabbing_pos_relative_to_cur.y < 0 and inRange(
                        grabbing_pos_relative_to_cur.x,
                        -5.0 / case.pattern_point_relative_to_parent.scale,
                        0,
                    )) {
                        return try childAddress(mem, parent_address, k);
                    }
                } else {
                    if (group.cases.items.len > 0) {
                        const last_case = group.cases.items[group.cases.items.len - 1];
                        const grabbing_pos_relative_to_last = Point.inverseApplyGetLocalPosition(
                            last_case.pattern_point_relative_to_parent,
                            mouse_pos_relative_to_parent,
                        );
                        if (grabbing_pos_relative_to_last.y > 0 and inRange(
                            grabbing_pos_relative_to_last.x,
                            -5.0 / last_case.pattern_point_relative_to_parent.scale,
                            0,
                        )) {
                            return try childAddress(mem, parent_address, group.cases.items.len);
                        }
                    }

                    const unfolded = getUnfoldedChild(address_if_released, parent_address, group.unfolded);
                    for (group.cases.items, 0..) |*case, k| {
                        const is_unfolded = std.meta.eql(unfolded, .{ .normal = k });
                        if (is_unfolded) {
                            const cur_address = try childAddress(mem, parent_address, k);
                            const cur_relative_mouse = Point.inverseApplyGetLocalPosition(
                                case.pattern_point_relative_to_parent,
                                mouse_pos_relative_to_parent,
                            );
                            if (case.next) |*next| {
                                const child_thing = try doGrabbingCaseSecondPass(
                                    cur_relative_mouse,
                                    address_if_released,
                                    mem,
                                    cur_address,
                                    next,
                                );
                                if (child_thing) |x| {
                                    return x;
                                }
                            } else if (inRange(cur_relative_mouse.x, 0, 5) and cur_relative_mouse.y > 0) {
                                return try childAddress(mem, cur_address, 0);
                            } else {
                                return null;
                            }
                        }
                    } else {
                        return null;
                    }
                }
            }
        }
    };
}

pub fn ExecutingFnk(platform: Platform, drawer: Drawer) type {
    return struct {
        const Self = @This();
        const artist = Artist(platform, drawer);

        // TODO: draw the variable name on bound values

        // TODO: remove this, probably
        scoring_run: *core.ScoringRun,
        thread: core.ExecutionThread,

        anim_t: f32,

        pub fn init(
            input: *const Sexpr,
            fn_name: *const Sexpr,
            scoring_run: *core.ScoringRun,
        ) !Self {
            var result = Self{
                .thread = try .init(input, fn_name, scoring_run),
                .scoring_run = scoring_run,
                .anim_t = 0.0,
            };

            // for now, skip the "start" anim
            std.debug.assert(null == try result.thread.advanceTinyStep(result.scoring_run));

            // TODO: remove these
            // std.debug.assert(null == try result.thread.advanceTinyStep(result.scoring_run));
            // result.anim_t = 0.3;

            return result;
        }

        pub fn update(self: *Self, delta_seconds: f32) !?*const Sexpr {
            if (platform.getMouse().wasPressed(.right)) self.anim_t = 0.99;

            self.anim_t += delta_seconds / if (platform.getMouse().cur.isDown(.left)) tof32(20.0) else 2.0;
            while (self.anim_t >= 1) {
                self.anim_t -= 1;
                _ = try self.thread.advanceTinyStep(self.scoring_run);
                // if (try self.thread.advanceTinyStep(self.scoring_run)) |x| return x;
            }
            return null;

            // if (platform.getMouse().wasPressed(.left)) {
            //     return try self.thread.advanceTinyStep(self.scoring_run);
            // } else {
            //     return null;
            // }
        }

        pub fn draw(self: Self) !void {
            drawer.clear(Color.gray(128));

            if (false) try artist.drawSexpr(
                global_camera,
                .{
                    .pos = platform.getMouse().cur.pos(global_camera),
                    .scale = 1 - self.anim_t,
                },
                &Sexpr.input,
            );

            var parent_point = Point{};

            // if (self.thread.stack.items.len == 0) {
            //     try artist.drawSexpr(camera, parent_point.applyToLocalPoint(SAMPLE_INPUT_POS), self.thread.active_value);
            //     std.log.debug("TODO: ending?", .{});
            //     return;
            // }

            // std.log.debug("cur state: {s}", .{@tagName(self.thread.last_visual_state)});
            var it = std.mem.reverseIterator(self.thread.stack.items);
            switch (self.thread.last_visual_state) {
                .just_started => {
                    // const active_stack: core.StackThing = it.next().?;
                    // try artist.drawSexpr(camera, parent_point.applyToLocalPoint(SAMPLE_INPUT_POS), self.thread.active_value);
                    // try artist.drawSexpr(camera, parent_point.applyToLocalPoint(MAIN_FNK_POS), active_stack.cur_fnk_name);
                    // try drawCases(true, parent_point, active_stack.cur_cases, true);
                },
                .failed_to_match => |discarded_case| {
                    const active_stack: core.StackThing = it.next().?;
                    artist.drawOffscreenCableTo(global_camera, SAMPLE_INPUT_POS);
                    try artist.drawSexpr(global_camera, parent_point.applyToLocalPoint(SAMPLE_INPUT_POS), self.thread.active_value);
                    try artist.drawHoldedFnk(global_camera, parent_point.applyToLocalPoint(MAIN_FNK_POS), 1, active_stack.cur_fnk_name);
                    if (self.anim_t < 0.5) {
                        const t = clamp01(remap(self.anim_t, 0, 0.4, 0, 1));
                        const t2 = clamp01(remap(self.anim_t, 0.4, 0.5, 0, 1));
                        try drawCases(
                            1,
                            parent_point.applyToLocalPoint(.{ .pos = .new(0, lerp(1.5, 0, t)) }),
                            active_stack.cur_cases,
                            false,
                            0,
                        );
                        try drawCase(
                            1 - t2 * 0.5,
                            parent_point
                                .applyToLocalPoint(.{ .pos = .new(lerp(DIST_TO_TEMPLATE, 4, t2), lerp(3, 0, t)) }),
                            discarded_case,
                            true,
                            true,
                            0,
                        );
                    } else {
                        const t = remap(self.anim_t, 0.5, 1, 0, 1);
                        try drawCase(0.5, parent_point
                            .applyToLocalPoint(Point.lerp(
                            .{ .pos = .new(4, 0) },
                            .{ .pos = .new(12, -4), .scale = 0, .turns = -0.65 },
                            t,
                        )), discarded_case, true, false, 0);
                        try drawCase(
                            1,
                            parent_point
                                .applyToLocalPoint(.{ .pos = .new(5, lerp(3.5, 3, t)), .scale = lerp(0.5, 1, t) }),
                            active_stack.cur_cases[0],
                            true,
                            true,
                            0,
                        );
                        try drawCases(
                            1,
                            parent_point.applyToLocalPoint(.{ .pos = .new(0, 1.5) }),
                            active_stack.cur_cases[1..],
                            false,
                            0,
                        );
                    }
                },
                .matched => |matched| {
                    if (self.anim_t < 0.5) {
                        if (matched.added_new_fnk_to_stack) {
                            _ = it.next().?;
                        }
                        if (!matched.tail_optimized) {
                            _ = it.next().?;
                        }

                        const t = clamp01(remap(self.anim_t, 0, 0.4, 0, 1));
                        const t2 = clamp01(remap(self.anim_t, 0.4, 0.5, 0, 1));

                        artist.drawOffscreenCableTo(global_camera, SAMPLE_INPUT_POS);
                        try artist.drawSexpr(
                            global_camera,
                            parent_point.applyToLocalPoint(SAMPLE_INPUT_POS),
                            matched.old_active_value,
                        );
                        try artist.drawHoldedFnk(global_camera, parent_point.applyToLocalPoint(MAIN_FNK_POS), 1, matched.old_fnk_name);

                        try drawCases(
                            1,
                            parent_point.applyToLocalPoint(.{ .pos = .new(0, lerp(1.5, 0, t)) }),
                            matched.discarded_cases,
                            false,
                            0,
                        );
                        try drawCase(
                            1 - t2 * 0.5,
                            parent_point
                                .applyToLocalPoint(.{ .pos = .new(lerp(DIST_TO_TEMPLATE, 4, t2), lerp(3, 0, t)) }),
                            matched.case,
                            true,
                            true,
                            0,
                        );
                    } else {
                        const t = remap(self.anim_t, 0.5, 1, 0, 1);

                        // TODO: draw centered
                        const t2 = clamp01(remap(self.anim_t, 0.5, 0.8, 0, 1));
                        const dissolving_pattern_point = parent_point
                            .applyToLocalPoint(SAMPLE_INPUT_POS)
                            .applyToLocalPoint(.{ .scale = 1 - t2 });
                        const cable_asdf_pos = dissolving_pattern_point.applyToLocalPosition(.new(3.5, 0));
                        drawer.drawCable(
                            global_camera,
                            .new(-CABLE_OFFSCREEN_DIST, 0),
                            dissolving_pattern_point.applyToLocalPosition(.new(-0.5, 0)),
                            1,
                            t2 * 3,
                        );
                        try artist.drawSexpr(
                            global_camera,
                            dissolving_pattern_point,
                            matched.old_active_value,
                        );
                        try artist.drawPatternSexpr(
                            global_camera,
                            dissolving_pattern_point
                                .applyToLocalPoint(.{ .pos = .new(3, 0) }),
                            matched.case.pattern,
                        );
                        drawer.drawCable(
                            global_camera,
                            dissolving_pattern_point.applyToLocalPosition(.new(-1, 1)),
                            dissolving_pattern_point.applyToLocalPosition(.new(3, 1)),
                            dissolving_pattern_point.scale,
                            0,
                        );

                        const active_value_cur_pos = parent_point.applyToLocalPoint(Point.lerp(
                            .{ .pos = .new(5 + DIST_TO_TEMPLATE - 1, 0) },
                            SAMPLE_INPUT_POS,
                            t,
                        ));
                        artist.drawCableTo(global_camera, cable_asdf_pos, active_value_cur_pos);
                        try artist.drawSexpr(
                            global_camera,
                            active_value_cur_pos,
                            // TODO: smoothly anim this, and also on the child cases
                            if (t > 0.5) self.thread.active_value else matched.case.template,
                        );

                        if (matched.added_new_fnk_to_stack) {
                            const active_stack: core.StackThing = it.next().?;
                            try artist.drawHoldedFnk(global_camera, parent_point
                                .applyToLocalPoint(Point.lerp(
                                (Point{ .pos = .new(DIST_TO_TEMPLATE - 1, 0) })
                                    .applyToLocalPoint(FNK_NAME_OFFSET),
                                MAIN_FNK_POS,
                                t,
                            )), t, active_stack.cur_fnk_name);
                            try drawCases(
                                1,
                                parent_point.applyToLocalPoint(.{
                                    // TODO: this anim
                                    .pos = .new(lerp(DIST_TO_TEMPLATE * 5, 0, t), 0),
                                }),
                                active_stack.cur_cases,
                                true,
                                0,
                            );
                        }

                        // TODO: join the 3 prev_stack cases in 1, maybe
                        if (matched.tail_optimized) {
                            try artist.drawHoldedFnk(
                                global_camera,
                                parent_point
                                    .applyToLocalPoint(Point.lerp(
                                    MAIN_FNK_POS,
                                    .{ .pos = .new(4, -8), .scale = 0, .turns = -0.65 },
                                    t,
                                )),
                                1,
                                matched.old_fnk_name,
                            );

                            // THINKING
                            // const maybe_prev_stack = it.next();
                            // const how_unfolded_is_prev_stack: f32 = if (matched.tail_optimized and !matched.added_new_fnk_to_stack)
                            //     t
                            // else if (!matched.tail_optimized and matched.added_new_fnk_to_stack)
                            //     1 - t2
                            // else if (!matched.tail_optimized and !matched.added_new_fnk_to_stack)
                            //     0.0
                            // else blk: {
                            //     // this case is just nothing
                            //     std.log.debug("hhmmm", .{});
                            //     break :blk 0.0;
                            // };

                            if (!matched.added_new_fnk_to_stack) {
                                defer parent_point = parent_point.applyToLocalPoint(.{ .pos = .new(lerp(-DIST_BETWEEN_QUEUED_FNKS, 0, t), 0) });

                                if (it.next()) |prev_stack| {
                                    try artist.drawHoldedFnk(
                                        global_camera,
                                        parent_point
                                            .applyToLocalPoint(.{ .pos = .new(lerp(-DIST_BETWEEN_QUEUED_FNKS, 0, t), 0) })
                                            .applyToLocalPoint(MAIN_FNK_POS),
                                        1,
                                        prev_stack.cur_fnk_name,
                                    );
                                    try drawCases(
                                        t,
                                        parent_point.applyToLocalPoint(.{
                                            .pos = .new(lerp(-1 - DIST_BETWEEN_QUEUED_FNKS, 0, t), 0),
                                        }),
                                        prev_stack.cur_cases,
                                        true,
                                        1 - t,
                                    );
                                }
                            }
                        } else {
                            const prev_stack: core.StackThing = it.next().?;
                            if (matched.added_new_fnk_to_stack) {
                                defer parent_point = parent_point.applyToLocalPoint(.{ .pos = .new(-DIST_BETWEEN_QUEUED_FNKS * t2, 0) });

                                try artist.drawHoldedFnk(
                                    global_camera,
                                    parent_point
                                        .applyToLocalPoint(.{ .pos = .new(lerp(0, -DIST_BETWEEN_QUEUED_FNKS, t2), 0) })
                                        .applyToLocalPoint(MAIN_FNK_POS),
                                    1,
                                    prev_stack.cur_fnk_name,
                                );
                                // TODO: revise this (waiting cases should be gen0?)
                                try drawCases(
                                    0,
                                    parent_point.applyToLocalPoint(.{
                                        .pos = .new(lerp(DIST_TO_TEMPLATE - 1, -1 - DIST_BETWEEN_QUEUED_FNKS, t2), 0),
                                    }),
                                    prev_stack.cur_cases,
                                    true,
                                    t2,
                                );
                            } else {
                                try artist.drawHoldedFnk(
                                    global_camera,
                                    parent_point
                                        .applyToLocalPoint(MAIN_FNK_POS),
                                    1,
                                    prev_stack.cur_fnk_name,
                                );

                                try drawCases(
                                    t,
                                    parent_point
                                        .applyToLocalPoint(.{ .pos = .new(lerp(DIST_TO_TEMPLATE - 1, 0, t), 0) }),
                                    prev_stack.cur_cases,
                                    true,
                                    0,
                                );
                            }
                        }
                    }
                },
                .ended => |result| {
                    artist.drawOffscreenCableTo(global_camera, SAMPLE_INPUT_POS);
                    try artist.drawSexpr(global_camera, parent_point.applyToLocalPoint(SAMPLE_INPUT_POS), result);
                },
            }
            while (it.next()) |x| {
                parent_point = parent_point.applyToLocalPoint(.{ .pos = .new(-DIST_BETWEEN_QUEUED_FNKS, 0) });
                try artist.drawHoldedFnk(global_camera, parent_point.applyToLocalPoint(MAIN_FNK_POS), 1, x.cur_fnk_name);

                try drawCases(
                    0,
                    parent_point.applyToLocalPoint(.{ .pos = .new(-1, 0) }),
                    x.cur_cases,
                    true,
                    1,
                );
            }
        }

        // TODO: remove this duplication from EditingFnk
        fn drawCases(is_gen0: f32, parent_point: Point, cases: []const core.MatchCaseDefinition, first_unfolded: bool, hiding_children: f32) OoM!void {
            for (cases, 0..) |case, k| {
                const relative_pattern_point = if (first_unfolded and k == 0) Point{
                    .pos = .new(lerp(4, 5, is_gen0), 3),
                    .scale = 1,
                } else Point{
                    .pos = .new(lerp(4, 5, is_gen0), 3.5 + tof32(k) * 1.5),
                    .scale = 0.5,
                };
                const pattern_point = parent_point.applyToLocalPoint(relative_pattern_point);

                // TODO: constant cable should be true except when whooshing away
                // try drawCase(is_gen0, pattern_point, case, first_unfolded and k == 0, true, hiding_children);
                try drawCase(is_gen0, pattern_point, case, first_unfolded and k == 0, is_gen0 > 0.5, hiding_children);
            }
        }

        // TODO: join with_extra and constant_cable into a single struct? so constant cable can have a default value
        fn drawCase(is_gen0: f32, pattern_point: Point, case: core.MatchCaseDefinition, with_extra: bool, constant_cable: bool, hiding_children: f32) OoM!void {
            try artist.drawPatternSexpr(
                global_camera,
                pattern_point,
                case.pattern,
            );
            if (with_extra) {
                try drawCaseExtra(pattern_point.applyToLocalPoint(.{ .scale = 1 - hiding_children }), case);
            }

            const cable_from = pattern_point.applyToLocalPosition(.new((lerp(-3, -5, is_gen0)) / if (constant_cable) pattern_point.scale else 1, 1));
            const cable_to = pattern_point.applyToLocalPosition(.new(0, 1));
            drawer.drawCable(
                global_camera,
                cable_from,
                cable_to,
                if (constant_cable) 1 else pattern_point.scale,
                0,
            );
        }

        fn drawCaseExtra(pattern_point: Point, case: core.MatchCaseDefinition) !void {
            try artist.drawSexpr(
                global_camera,
                pattern_point.applyToLocalPoint(.{ .pos = .new(DIST_TO_TEMPLATE, 0) }),
                case.template,
            );
            try artist.drawHoldedFnk(global_camera, pattern_point.applyToLocalPoint(FNK_NAME_OFFSET), 0, case.fnk_name);
            drawer.drawCable(
                global_camera,
                pattern_point.applyToLocalPosition(.new(0.5, 0)),
                pattern_point.applyToLocalPosition(.new(DIST_TO_TEMPLATE - 0.5, 0)),
                pattern_point.scale,
                0,
            );
            if (case.next) |next| {
                try drawCases(0, pattern_point, next.items, true, 0);
            }
        }
    };
}

test {
    const dummy_platform = Platform{
        .gpa = std.testing.allocator,
        .getMouse = struct {
            pub fn anon() Mouse {
                unreachable;
            }
        }.anon,
        .getPlayerData = undefined,
        .setPlayerData = undefined,
    };
    std.testing.refAllDecls(EditingFnk(dummy_platform, Drawer.dummy));
    std.testing.refAllDecls(ExecutingFnk(dummy_platform, Drawer.dummy));
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
                math.lerp_towards(
                    &button.hot_t,
                    if (self.ui_state.isHot(k)) 1 else 0,
                    0.6,
                    delta_seconds,
                );
                math.lerp_towards(
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
                math.smoothstep(self.t, 2, 6),
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
                const pull = math.smoothstep(self.t, 8.3, 12) * 6.8;
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
