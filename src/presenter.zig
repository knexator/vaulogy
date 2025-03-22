//! This should be unchanged regardless of platform

// TODO: maybe combine Editing & Executing (they both share Camera and some UI)

const std = @import("std");

pub const Mouse = @import("kommon/input.zig").Mouse;
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
const funk = @import("kommon/funktional.zig");

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

pub const DESIGN = @import("DESIGN");

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

pub const Platform = struct {
    gpa: std.mem.Allocator,
    getPlayerData: fn (mem: *VeryPermamentGameStuff) OoM!?PlayerData,
    setPlayerData: fn (player_data: PlayerData, mem: *VeryPermamentGameStuff) OoM!void,
    getMouse: fn () Mouse,
    getKeyboard: fn () Keyboard,
};

// TODO NOW: allow non-ascii sexpr names
pub const PlayerData = struct {
    // TODO: this field should not be here.
    ascii_data: []const u8,

    fnks: FnkCollection,
    custom_samples: std.ArrayHashMap(*const Sexpr, []const Sample, core.SexprContext, true),
    is_builtin_level_solved: [builtin_levels.len]bool,
    first_time: bool = true,

    const no_samples: []const Sample = &.{};

    pub fn allFnkNames(self: PlayerData) []const *const Sexpr {
        return self.fnks.keys();
    }

    pub fn empty(mem: *VeryPermamentGameStuff) !PlayerData {
        var asdf: FnkCollection = .init(mem.gpa);
        // TODO: WITHOUT THIS LINE, IT CRASHES WHEN ADDING A 5th FNK
        try asdf.ensureTotalCapacity(1000);
        return .{
            .ascii_data = "",
            .fnks = asdf,
            // .fnks = FnkCollection.init(mem.gpa),
            .custom_samples = .init(mem.gpa),
            .is_builtin_level_solved = @splat(false),
        };
    }

    pub fn updateSolvedStatusOfAll(self: *PlayerData, mem: *VeryPermamentGameStuff) !void {
        for (0..builtin_levels.len) |k| {
            try self.updateSolvedStatus(k, mem);
        }
    }

    pub fn updateSolvedStatus(self: *PlayerData, level_index: usize, mem: *VeryPermamentGameStuff) !void {
        std.debug.assert(level_index < builtin_levels.len);
        self.is_builtin_level_solved[level_index] = try isSolved(builtin_levels[level_index], self.fnks, mem);
    }

    fn isSolved(level: BuiltinLevel, fnks: FnkCollection, mem: *VeryPermamentGameStuff) !bool {
        var score = try core.ScoringRun.initFromFnks(fnks, mem);
        defer score.deinit(false);

        for (level.manual_samples) |sample| {
            var exec = core.ExecutionThread.init(sample.input, level.fnk_name, &score) catch |err| switch (err) {
                error.FnkNotFound => return false,
                else => return err,
            };
            defer exec.deinit();

            const actual_output = exec.getFinalResult(&score) catch |err| switch (err) {
                error.FnkNotFound, error.NoMatchingCase, error.InvalidMetaFnk, error.UsedUndefinedVariable => return false,
                error.OutOfMemory => return err,
                error.BAD_INPUT => return err,
            };
            if (!actual_output.equals(sample.output.?)) return false;
        } else {
            return true;
        }
    }

    pub fn fromAscii(data: []const u8, mem: *VeryPermamentGameStuff) !PlayerData {
        const ascii_data = try mem.gpa.dupe(u8, data);
        var parser = parsing.Parser{ .remaining_text = ascii_data };
        var fnks = FnkCollection.init(mem.gpa);
        errdefer fnks.deinit();
        // TODO: WITHOUT THIS LINE, IT CRASHES WHEN ADDING A 5th FNK
        try fnks.ensureTotalCapacity(1000);
        try parser.parseFnkCollection(&fnks, &mem.pool_for_sexprs, mem.arena_for_cases.allocator());
        var is_builtin_level_solved: [builtin_levels.len]bool = undefined;
        for (builtin_levels, &is_builtin_level_solved) |level, *target| {
            target.* = try isSolved(level, fnks, mem);
        }
        return PlayerData{
            .fnks = fnks,
            .custom_samples = .init(mem.gpa),
            .ascii_data = ascii_data,
            .is_builtin_level_solved = is_builtin_level_solved,
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
    setTransparency: fn (alpha: f32) void,
    drawLine: fn (camera: Camera, points: []const Vec2, color: Color) void,
    drawRect: fn (camera: Camera, rect: Rect, stroke: ?Color, fill: ?Color) void,
    clipAtomRegion: fn (camera: Camera, point: Point) void,
    endClip: fn () void,
    drawDebugText: fn (camera: Camera, center: Point, text: [:0]const u8, color: Color) void,
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
    // TODO: think how to use the visuals when drawing a variable
    drawVariable: fn (camera: Camera, world_point: Point, visuals: AtomVisuals) void,
    drawPatternVariable: fn (camera: Camera, world_point: Point, visuals: AtomVisuals) void,
    drawWildcardsCable: fn (camera: Camera, points: []const Vec2, visuals: []const AtomVisuals) void,

    pub fn drawCaseHolderExtended(self: Drawer, camera: Camera, world_point: Point, enabled: bool) void {
        if (!enabled) self.setTransparency(0.5);
        self.drawCaseHolder(camera, world_point);
        if (!enabled) self.setTransparency(1);
    }

    pub fn drawArrowForSample(self: Drawer, camera: Camera, center: Point, solved: ?bool) void {
        const color: Color = if (solved) |s|
            if (s) .from01(0.2, 1, 0.5) else .from01(1, 0.2, 0.3)
        else
            .black;
        self.drawLine(camera, &.{
            center.applyToLocalPosition(.new(-1, 0)),
            center.applyToLocalPosition(.new(2, 0)),
        }, color);
        self.drawLine(camera, &.{
            center.applyToLocalPosition(.new(1, -1)),
            center.applyToLocalPosition(.new(2, 0)),
            center.applyToLocalPosition(.new(1, 1)),
        }, color);
    }

    const dummySignatures = struct {
        pub fn nothing() void {
            unreachable;
        }
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
    pub const dummy = Drawer{
        .clear = dummySignatures.color,
        .setTransparency = struct {
            pub fn anon(alpha: f32) void {
                _ = alpha;
                unreachable;
            }
        }.anon,
        .drawLine = struct {
            pub fn anon(camera: Camera, points: []const Vec2, color: Color) void {
                _ = camera;
                _ = points;
                _ = color;
                unreachable;
            }
        }.anon,
        .drawRect = struct {
            pub fn anon(camera: Camera, rect: Rect, stroke: ?Color, fill: ?Color) void {
                _ = camera;
                _ = rect;
                _ = stroke;
                _ = fill;
                unreachable;
            }
        }.anon,
        .clipAtomRegion = dummySignatures.camera_point,
        .endClip = dummySignatures.nothing,
        .drawAtomDebug = dummySignatures.camera_point,
        .drawAtom = dummySignatures.camera_point_visuals,
        .drawVariable = dummySignatures.camera_point_visuals,
        .drawPatternVariable = dummySignatures.camera_point_visuals,
        .drawPatternAtomDebug = dummySignatures.camera_point,
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
        .drawDebugText = struct {
            pub fn anon(camera: Camera, center: Point, text: [:0]const u8, color: Color) void {
                _ = camera;
                _ = center;
                _ = text;
                _ = color;
                unreachable;
            }
        }.anon,
        .drawWildcardsCable = struct {
            pub fn anon(camera: Camera, points: []const Vec2, visuals: []const AtomVisuals) void {
                _ = camera;
                _ = points;
                _ = visuals;
                unreachable;
            }
        }.anon,
    };
};

fn moveCamera(camera: *Camera, delta_seconds: f32, keyboard: Keyboard, mouse: Mouse) void {
    const mouse_pos = mouse.cur.pos(camera.*);
    camera.* = camera.zoom(mouse_pos, camera.height * switch (mouse.cur.scrolled) {
        .none => tof32(1.0),
        .down => 1.1,
        .up => 0.9,
    });

    inline for (.{
        .{ KeyboardButton.left, Vec2.new(-1, 0) },
        .{ KeyboardButton.right, Vec2.new(1, 0) },
        .{ KeyboardButton.up, Vec2.new(0, -1) },
        .{ KeyboardButton.down, Vec2.new(0, 1) },
    }) |key_dir| {
        if (keyboard.isDown(key_dir[0])) {
            camera.center = camera.center.add(key_dir[1].scale(delta_seconds * camera.height));
        }
    }
}

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

fn defaultFnkBody(mem: *VeryPermamentGameStuff) FnkBody {
    const default_fnk =
        \\default {
        \\  @x -> @x;
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
            level_select_to_editing_fnk: LevelSelectToEditingFnk(platform, drawer),
            editing_fnk: EditingFnk(platform, drawer),
            editing_fnk_to_testing: EditingFnkToTesting(platform, drawer),
            executing_fnk: ExecutingFnk(platform, drawer),
        },
        // kinda hacky, could maybe be a stack
        prev_editing_state: ?EditingFnk(platform, drawer),

        pub fn init(result: *Self) !void {
            result.prev_editing_state = null;

            const platform_alloc = platform.gpa;
            result.mem = VeryPermamentGameStuff.init(platform_alloc);
            const player_data = (try platform.getPlayerData(&result.mem)) orelse (try PlayerData.empty(&result.mem));
            if (!player_data.first_time) return error.TODO;

            result.persistence = player_data;

            result.state = .{
                .level_select = try .init(&result.persistence),
            };

            try Artist(platform, drawer).init();
        }

        fn initEditing(self: *Self, fnk_name: *const Sexpr, builtin_samples: ?[]const Sample) !void {
            const res = try self.persistence.fnks.getOrPut(fnk_name);
            if (!res.found_existing) {
                // TODO: change the loop into hashmap?
                for (builtin_levels) |level| {
                    if (!level.fnk_name.equals(fnk_name)) continue;
                    if (level.premade_solution) |raw_fnk| {
                        var parser = parsing.Parser{ .remaining_text = raw_fnk };
                        const fnk = try parser.parseFnkNew(&self.mem.pool_for_sexprs, self.mem.arena_for_cases.allocator());
                        std.debug.assert(fnk.name.equals(level.fnk_name));
                        res.value_ptr.* = fnk.body;
                        break;
                    }
                } else {
                    res.value_ptr.* = defaultFnkBody(&self.mem);
                }
            }
            const fnk_body = res.value_ptr.*;

            // TODO: include both the builtin & the user created samples
            // const samples = self.persistence.custom_samples.get(fnk_name) orelse PlayerData.no_samples;

            self.state = .{
                .editing_fnk = try .init(
                    fnk_name,
                    builtin_samples orelse PlayerData.no_samples,
                    self.persistence.allFnkNames(),
                    fnk_body,
                    &self.mem,
                    &self.persistence,
                ),
            };
            self.scoring_run = undefined;
        }

        pub fn update(self: *Self, delta_seconds: f32) !void {
            if (1.0 / delta_seconds < 40) {
                std.log.info("Low FPS: {d}", .{1.0 / delta_seconds});
            }
            switch (self.state) {
                .level_select => |*ui| if (ui.update(delta_seconds)) |level_index| {
                    self.state = .{
                        .level_select_to_editing_fnk = .init(ui.*, level_index),
                    };
                },
                .level_select_to_editing_fnk => |*anim| if (anim.update(delta_seconds)) {
                    try self.initEditing(anim.level.fnk_name, anim.level.manual_samples);
                },
                .editing_fnk_to_testing => |*anim| if (try anim.update(delta_seconds, &self.mem)) {
                    self.scoring_run = try core.ScoringRun.initFromFnks(
                        self.persistence.fnks,
                        &self.mem,
                    );
                    self.state = .{ .executing_fnk = try .init(
                        if (DESIGN.no_current_data) .{
                            .value = anim.input.value,
                            .is_pattern = 0,
                            .pos = MAIN_INPUT_POS,
                        } else anim.input.value,
                        anim.fnk_name,
                        &self.scoring_run,
                        anim.camera,
                        anim.output.value,
                    ) };
                },
                .editing_fnk => |*editing| switch (try editing.update(delta_seconds)) {
                    .nothing => {},
                    .back_to_level_select => {
                        const fnk = try editing.getFnk();
                        try self.persistence.fnks.put(fnk.name, fnk.body);
                        try self.persistence.updateSolvedStatusOfAll(&self.mem);
                        try platform.setPlayerData(self.persistence, &self.mem);
                        self.state = .{ .level_select = try .init(&self.persistence) };
                    },
                    .launch_test => {
                        const fnk = try editing.getFnk();
                        try self.persistence.fnks.put(fnk.name, fnk.body);
                        try self.persistence.updateSolvedStatusOfAll(&self.mem);
                        try platform.setPlayerData(self.persistence, &self.mem);
                        self.prev_editing_state = editing.*;
                        const sample_index: usize = blk: for (editing.solved_samples, 0..) |value, k| {
                            if (!value) break :blk k;
                        } else @intFromFloat(1.5 + @TypeOf(editing.*).samples_reel.scroll);
                        self.state = .{ .editing_fnk_to_testing = .init(
                            editing.camera,
                            .{
                                .value = editing.samples[sample_index].input,
                                .pos = @TypeOf(editing.*).samples_reel.getPoint(sample_index, .input),
                                .is_pattern = 0,
                            },
                            .{
                                .value = editing.samples[sample_index].output.?,
                                .pos = @TypeOf(editing.*).samples_reel.getPoint(sample_index, .output),
                                .is_pattern = 0,
                            },
                            editing.fnk_name,
                            editing.cases,
                        ) };
                    },
                    .launch_execution => |input| {
                        // todo
                        const fnk = try editing.getFnk();
                        try self.persistence.fnks.put(fnk.name, fnk.body);
                        try platform.setPlayerData(self.persistence, &self.mem);
                        self.prev_editing_state = editing.*;
                        self.scoring_run = try core.ScoringRun.initFromFnks(
                            self.persistence.fnks,
                            &self.mem,
                        );
                        self.state = .{ .executing_fnk = try .init(
                            if (DESIGN.no_current_data) input else editing.main_input,
                            fnk.name,
                            &self.scoring_run,
                            editing.camera,
                            null,
                        ) };
                    },
                    .change_to => |fnk_name| {
                        const fnk = try editing.getFnk();
                        try self.persistence.fnks.put(fnk.name, fnk.body);
                        try platform.setPlayerData(self.persistence, &self.mem);
                        try self.initEditing(fnk_name, if (findBuiltinLevel(fnk_name)) |level|
                            level.manual_samples
                        else
                            null);
                    },
                },
                // TODO
                .executing_fnk => |*executing| switch (try executing.update(delta_seconds)) {
                    .nothing => {},
                    .back_to_editing => self.state = .{ .editing_fnk = self.prev_editing_state.? },
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

const Sample = struct {
    input: *const Sexpr,
    output: ?*const Sexpr,

    const Part = enum { input, output };
    const Address = struct {
        index: usize,
        which: Sample.Part,
        local: core.SexprAddress,
    };
    fn get(self: Sample, which: Part) ?*const Sexpr {
        return switch (which) {
            .input => self.input,
            .output => self.output,
        };
    }
};

const BuiltinLevel = struct {
    // TODO: remove fnk_name, making it a key in a hashmap?
    fnk_name: *const Sexpr,
    // TODO: remove the defaul value
    solution: *const fn (input: *const Sexpr, mem: *VeryPermamentGameStuff) OoM!?*const Sexpr = struct {
        fn anon(input: *const Sexpr, mem: *VeryPermamentGameStuff) OoM!?*const Sexpr {
            _ = input;
            _ = mem;
            @panic("TODO");
        }
    }.anon,
    manual_samples: []const Sample,
    description: [:0]const u8,
    premade_solution: ?[]const u8,

    // TODO: have a comptime pool of Sexprs so this works for solutions that actually use mem
    pub fn init(
        fnk_name: *const Sexpr,
        solution: *const fn (input: *const Sexpr, mem: *VeryPermamentGameStuff) OoM!?*const Sexpr,
        comptime manual_inputs: []const *const Sexpr,
        description: [:0]const u8,
        premade_solution: ?[]const u8,
    ) BuiltinLevel {
        var manual_samples: [manual_inputs.len]Sample = undefined;
        for (manual_inputs, &manual_samples) |input, *sample| {
            sample.input = input;
            sample.output = solution(input, undefined);
        }
        const manual_samples_done = manual_samples;
        return BuiltinLevel{
            .fnk_name = fnk_name,
            .solution = solution,
            .manual_samples = &manual_samples_done,
            .description = description,
            .premade_solution = premade_solution,
        };
    }
};
const Vals = struct {
    pub const Hermes: *const Sexpr = &Sexpr.doLit("Hermes");
    pub const Mercury: *const Sexpr = &Sexpr.doLit("Mercury");
    pub const Aphrodite: *const Sexpr = &Sexpr.doLit("Aphrodite");
    pub const Venus: *const Sexpr = &Sexpr.doLit("Venus");
    pub const Ares: *const Sexpr = &Sexpr.doLit("Ares");
    pub const Mars: *const Sexpr = &Sexpr.doLit("Mars");
    pub const Zeus: *const Sexpr = &Sexpr.doLit("Zeus");
    pub const Jupiter: *const Sexpr = &Sexpr.doLit("Jupiter");

    pub const top: *const Sexpr = &Sexpr.doLit("top");
    pub const bottom: *const Sexpr = &Sexpr.doLit("bottom");

    pub fn wrapped(comptime v: *const Sexpr) *const Sexpr {
        return &Sexpr.doPair(&Sexpr.doPair(Vals.top, v), Vals.bottom);
    }

    pub fn planetFromOlympian(input: *const Sexpr) ?*const Sexpr {
        if (input.equals(Vals.Hermes)) return Vals.Mercury;
        if (input.equals(Vals.Aphrodite)) return Vals.Venus;
        if (input.equals(Vals.Ares)) return Vals.Mars;
        if (input.equals(Vals.Zeus)) return Vals.Jupiter;
        return null;
    }
};

const builtin_levels: []const BuiltinLevel = &.{
    .{ .fnk_name = &Sexpr.doLit("planetFromOlympian"), .manual_samples = &.{
        .{ .input = &Sexpr.doLit("Hermes"), .output = &Sexpr.doLit("Mercury") },
        .{ .input = &Sexpr.doLit("Aphrodite"), .output = &Sexpr.doLit("Venus") },
        .{ .input = &Sexpr.doLit("Ares"), .output = &Sexpr.doLit("Mars") },
        .{ .input = &Sexpr.doLit("Zeus"), .output = &Sexpr.doLit("Jupiter") },
    }, .description = "The simplest Vau: a hardcoded translation", .premade_solution = 
    \\planetFromOlympian {
    \\  Hermes -> Mercury;
    \\  // Aphrodite -> Venus;
    \\  nil -> nil;
    \\  Ares -> Mars;
    \\  Zeus -> Jupiter;
    \\}
    },
    .{ .fnk_name = &Sexpr.doLit("wrapOlympian"), .manual_samples = &.{
        .{ .input = &Sexpr.doLit("Hermes"), .output = &Sexpr.doPair(&Sexpr.doPair(&Sexpr.doLit("top"), &Sexpr.doLit("Hermes")), &Sexpr.doLit("bottom")) },
        .{ .input = &Sexpr.doLit("Aphrodite"), .output = &Sexpr.doPair(&Sexpr.doPair(&Sexpr.doLit("top"), &Sexpr.doLit("Aphrodite")), &Sexpr.doLit("bottom")) },
        .{ .input = &Sexpr.doLit("Ares"), .output = &Sexpr.doPair(&Sexpr.doPair(&Sexpr.doLit("top"), &Sexpr.doLit("Ares")), &Sexpr.doLit("bottom")) },
        .{ .input = &Sexpr.doLit("Zeus"), .output = &Sexpr.doPair(&Sexpr.doPair(&Sexpr.doLit("top"), &Sexpr.doLit("Zeus")), &Sexpr.doLit("bottom")) },
    }, .description = "This Vau takes unstable Data and wraps it safely", .premade_solution = 
    \\wrapOlympian {
    \\  Hermes -> ((top . Hermes) . bottom);
    \\  Aphrodite -> ((top . Aphrodite) . bottom);
    \\  // Ares -> ((top . Ares) . bottom);
    \\  // Zeus -> ((top . Zeus) . bottom);
    \\}
    },
    .{ .fnk_name = &Sexpr.doLit("planetFromWrappedOlympian"), .manual_samples = &.{
        .{ .input = Vals.wrapped(Vals.Hermes), .output = Vals.Mercury },
        .{ .input = Vals.wrapped(Vals.Aphrodite), .output = Vals.Venus },
        .{ .input = Vals.wrapped(Vals.Ares), .output = Vals.Mars },
        .{ .input = Vals.wrapped(Vals.Zeus), .output = Vals.Jupiter },
    }, .description = "Unwrap the unstable Data and then translate it", .premade_solution = 
    \\planetFromWrappedOlympian {
    \\ ((top . @v) . bottom) -> @v;
    \\ // ((top . Aphrodite) . bottom) -> Venus;
    \\ // ((top . Ares) . bottom) -> Mars;
    \\ // ((top . Zeus) . bottom) -> Jupiter;
    \\}
    },
    .{ .fnk_name = &Sexpr.doLit("wrappedPlanetFromOlympian"), .manual_samples = &.{
        .{ .input = Vals.Hermes, .output = Vals.wrapped(Vals.Mercury) },
        .{ .input = Vals.Aphrodite, .output = Vals.wrapped(Vals.Venus) },
        .{ .input = Vals.Ares, .output = Vals.wrapped(Vals.Mars) },
        .{ .input = Vals.Zeus, .output = Vals.wrapped(Vals.Jupiter) },
    }, .description = "Translate the Data and then wrap it", .premade_solution = 
    \\wrappedPlanetFromOlympian {
    \\ @v -> planetFromOlympian: @v {
    \\   Mercury -> ((top . Mercury) . bottom);
    \\   Venus -> ((top . Venus) . bottom);
    \\ }
    \\ // Hermes -> ((top . Mercury) . bottom);
    \\ // Aphrodite -> ((top . Venus) . bottom);
    \\ // Ares -> ((top . Mars) . bottom);
    \\ // Zeus -> ((top . Jupiter) . bottom);
    \\}
    },
    .{ .fnk_name = &Sexpr.doLit("planetPairFromOlympianPair"), .manual_samples = &funk.map(struct {
        pub fn anon(comptime v: *const Sexpr) Sample {
            return .{ .input = v, .output = &Sexpr.doPair(
                Vals.planetFromOlympian(v.pair.left).?,
                Vals.planetFromOlympian(v.pair.right).?,
            ) };
        }
    }.anon, &.{
        &Sexpr.doPair(Vals.Hermes, Vals.Aphrodite),
        &Sexpr.doPair(Vals.Ares, Vals.Zeus),
        &Sexpr.doPair(Vals.Ares, Vals.Ares),
        &Sexpr.doPair(Vals.Zeus, Vals.Hermes),
        &Sexpr.doPair(Vals.Aphrodite, Vals.Hermes),
        &Sexpr.doPair(Vals.Zeus, Vals.Ares),
    }), .description = "Translate two Datas at once", .premade_solution = null },
};

// code smell
fn findBuiltinLevel(fnk_name: *const Sexpr) ?BuiltinLevel {
    for (builtin_levels) |level| {
        if (fnk_name.equals(level.fnk_name)) return level;
    }
    return null;
}

/// Like Drawer, but higher level
fn Artist(platform: Platform, drawer: Drawer) type {
    const AtomVisualCache = struct {
        var visuals_cache: std.StringHashMap(AtomVisuals) = std.StringHashMap(AtomVisuals).init(platform.gpa);

        const HardcodedAtomVisuals = struct {
            profile: ?[]const Vec2,
            color: Color,
        };
        const hardcoded_visuals = .{
            .identity = HardcodedAtomVisuals{
                .color = Color.white,
                .profile = &.{},
            },
            .nil = HardcodedAtomVisuals{
                .color = .from01(0.45, 0.45, 0.45),
                .profile = &.{.new(0.75, -0.25)},
            },
            .input = HardcodedAtomVisuals{
                .color = .from01(0.1, 0.6, 0.6),
                .profile = &.{ .new(0.2, 0.2), .new(0.8, 0.2) },
            },
            .true = HardcodedAtomVisuals{
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
            .false = HardcodedAtomVisuals{
                .color = .from01(0.9, 0.5, 0.5),
                .profile = &.{ .new(1.0 / 6.0, 0.2), .new(0.5, -0.2), .new(5.0 / 6.0, 0.2) },
            },

            //  Zeus -> Jupiter;
            .Hermes = HardcodedAtomVisuals{
                .color = .fromHex("#FA00FF"),
                .profile = null,
            },
            .Mercury = HardcodedAtomVisuals{
                .color = .fromHex("#FF8EEC"),
                .profile = null,
            },
            .Aphrodite = HardcodedAtomVisuals{
                .color = .fromHex("#FFB600"),
                .profile = null,
            },
            .Venus = HardcodedAtomVisuals{
                .color = .fromHex("#FFE18E"),
                .profile = null,
            },
            .Ares = HardcodedAtomVisuals{
                .color = .fromHex("#00E5FF"),
                .profile = null,
            },
            .Mars = HardcodedAtomVisuals{
                .color = .fromHex("#9EFFF2"),
                .profile = null,
            },
            .Zeus = HardcodedAtomVisuals{
                .color = .fromHex("#97F200"),
                .profile = null,
            },
            .Jupiter = HardcodedAtomVisuals{
                .color = .fromHex("#C8ED8F"),
                .profile = null,
            },
        };

        pub fn init() !void {
            inline for (std.meta.fields(@TypeOf(hardcoded_visuals))) |field| {
                const atom_name = field.name;
                const input = @field(hardcoded_visuals, field.name);
                const atom_visuals: AtomVisuals = .{
                    .color = input.color,
                    .profile = input.profile orelse try newAtomProfile(atom_name),
                };
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

        pub fn drawPlacedWildcardsCable(
            camera: Camera,
            pattern_point: Point,
            template_point: Point,
            pattern_value: *const Sexpr,
            template_value: *const Sexpr,
            // TODO: relative address? (maybe pass the casegroup, then)
        ) !void {
            // TODO: avoid memory management here by having a single scratch allocator for the whole frame/drawing
            const asdf = try visualsForCommonWildcards(pattern_value, template_value);
            defer platform.gpa.free(asdf);

            if (asdf.len > 0) {
                drawer.drawWildcardsCable(camera, &.{
                    pattern_point.applyToLocalPosition(.new(0.5, 0)),
                    template_point.applyToLocalPosition(.new(-0.5, 0)),
                }, asdf);
            }
        }

        fn visualsForCommonWildcards(pattern: *const Sexpr, template: *const Sexpr) ![]const AtomVisuals {
            // TODO: better memory management

            const gpa = platform.gpa;

            var pattern_names: std.ArrayList([]const u8) = .init(gpa);
            defer pattern_names.deinit();
            try pattern.getAllVarNames(&pattern_names);

            var template_names: std.ArrayList([]const u8) = .init(gpa);
            defer template_names.deinit();
            try template.getAllVarNames(&template_names);

            var common: std.ArrayList(AtomVisuals) = .init(platform.gpa);

            for (pattern_names.items) |p| {
                for (template_names.items) |t| {
                    if (std.mem.eql(u8, p, t)) {
                        try common.append(try AtomVisualCache.getAtomVisuals(p));
                    }
                }
            }

            return try common.toOwnedSlice();
        }

        // TODO: better memory management
        pub fn drawWildcardLinesToFloating(camera: Camera, parent_cases_point: Point, cases: CaseGroup, grabbing_point: Point, grabbing_wildcards: []const []const u8) !void {
            const case = cases.cases.items[cases.unfolded];
            const pattern_point = parent_cases_point.applyToLocalPoint(case.pattern_point_relative_to_parent);

            var wildcard_names: std.ArrayList([]const u8) = .init(platform.gpa);
            defer wildcard_names.deinit();
            try case.pattern.getAllVarNames(&wildcard_names);

            var common: std.ArrayList(AtomVisuals) = .init(platform.gpa);
            defer common.deinit();

            for (wildcard_names.items) |pattern| {
                for (grabbing_wildcards) |grabbing| {
                    if (std.mem.eql(u8, pattern, grabbing)) {
                        try common.append(try AtomVisualCache.getAtomVisuals(pattern));
                    }
                }
            }

            if (common.items.len > 0) {
                drawer.drawWildcardsCable(camera, &.{
                    pattern_point.applyToLocalPosition(.new(0.5, 0)),
                    grabbing_point.applyToLocalPosition(.new(-0.5, 0)),
                }, common.items);
            }

            // const visuals = try AtomVisualCache.getAtomVisuals(pattern);
            // drawer.drawLine(camera, &.{
            //     pattern_point.applyToLocalPosition(.new(0.5, 0)),
            //     grabbing_point.applyToLocalPosition(.new(-0.5, 0)),
            // }, visuals.color);

            if (case.next) |next| {
                try drawWildcardLinesToFloating(camera, pattern_point, next, grabbing_point, grabbing_wildcards);
            }
        }

        pub fn drawHoldedFnk(camera: Camera, fnk_point: Point, is_main: f32, value: *const Sexpr) !void {
            drawer.drawFnkHolder(camera, fnk_point
                .applyToLocalPoint(.{ .scale = lerp(1, 0.5, is_main) })
                .applyToLocalPoint(.{ .pos = .new(lerp(-1.5, -2.5, is_main), 0), .turns = 0.25 }));
            if (!value.equals(Sexpr.builtin.identity)) {
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

        pub fn drawSexprWithBindings(camera: Camera, world_point: Point, sexpr: *const Sexpr, bindings: BindingsState) !void {
            switch (sexpr.*) {
                .atom_lit => |lit| {
                    try drawAtom(camera, world_point, lit.value);
                },
                .pair => |pair| {
                    try drawSexprWithBindings(camera, world_point.applyToLocalPoint(.{
                        .pos = .new(0.5, -0.5),
                        .scale = 0.5,
                    }), pair.left, bindings);
                    try drawSexprWithBindings(camera, world_point.applyToLocalPoint(.{
                        .pos = .new(0.5, 0.5),
                        .scale = 0.5,
                    }), pair.right, bindings);
                    drawer.drawPairHolder(camera, world_point);
                },
                .atom_var => |x| {
                    // TODO: check that compiler skips the loop if anim_t is null
                    for (bindings.new) |binding| {
                        if (bindings.anim_t) |anim_t| {
                            if (std.mem.eql(u8, binding.name, x.value)) {
                                drawer.clipAtomRegion(camera, world_point);
                                const t = math.smoothstep(anim_t, 0, 0.4);
                                try drawSexpr(camera, world_point.applyToLocalPoint(.{ .pos = .new(remap(t, 0, 1, -2.3, 0), 0) }), binding.value);
                                drawer.endClip();

                                drawer.setTransparency(1 - anim_t);
                                try drawVariable(camera, world_point, x.value);
                                drawer.setTransparency(1);
                                break;
                            }
                        }
                    } else for (bindings.old) |binding| {
                        if (std.mem.eql(u8, binding.name, x.value)) {
                            try drawSexpr(camera, world_point, binding.value);
                            break;
                        }
                    } else {
                        try drawVariable(camera, world_point, x.value);
                    }
                },
            }
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
const BindingsState = struct {
    new: []const core.Binding,
    old: []const core.Binding,
    anim_t: ?f32,
};

const PhysicalSexpr = struct {
    value: *const Sexpr,
    pos: Point,
    is_pattern: f32,
};
const SexprView = struct {
    pub fn overlapsPatternAtom(atom_point: Point, needle_pos: Vec2, kind: enum { atom, pair }) bool {
        const p = atom_point.inverseApplyGetLocal(.{ .pos = needle_pos }).pos;
        return inRange(p.y, -1, 1) and switch (kind) {
            .atom => inRange(p.x, -1, 0.5 * (1 - @abs(p.y))),
            .pair => inRange(p.x, -1 + 0.25 * (1 - @abs(@abs(p.y) - 0.5) / 0.5), 0.5 * (1 - @abs(p.y))),
        };
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
                    if (overlapsPatternAtom(cur_sexpr_pos, needle_pos, .atom)) {
                        return try result.toOwnedSlice();
                    } else {
                        return null;
                    }
                },
                .pair => |pair| {
                    const p = cur_sexpr_pos.inverseApplyGetLocal(.{ .pos = needle_pos }).pos;

                    if (overlapsPatternAtom(cur_sexpr_pos, needle_pos, .pair)) {
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
        return inRange(p.y, -1, 1) and switch (kind) {
            .pair => inRange(p.x, -0.5 * (1 - @abs(p.y)), 0.5 - 0.25 * (1 - @abs(@abs(p.y) - 0.5) / 0.5)),
            .atom => inRange(p.x, -0.5 * (1 - @abs(p.y)), 2),
        };
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

const DEFAULT_CAM: Camera = .{ .center = .new(7, 3), .height = 15.0 };
const DIST_TO_TEMPLATE = 5;
const FNK_NAME_OFFSET = Point{
    .pos = .new(DIST_TO_TEMPLATE - 1, -0.75),
    .turns = -0.25,
    .scale = 0.5,
};
const MAIN_INPUT_POS = Point{ .pos = .new(1, 0) };
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

    pub fn anyWildcardInPlay(self: CaseGroup) bool {
        for (self.cases.items) |asdf| {
            if (!asdf.fnk_name.isFullyResolved()) return true;
            if (!asdf.pattern.isFullyResolved()) return true;
            if (!asdf.template.isFullyResolved()) return true;
            if (asdf.next) |next| {
                if (next.anyWildcardInPlay()) return true;
            }
        }
        return false;
    }

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

    pub fn setAllUnfoldedToZero(self: *CaseGroup) void {
        self.unfolded = 0;
        for (self.cases.items) |*case| {
            if (case.next) |*next| {
                next.setAllUnfoldedToZero();
            }
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

fn EditingFnkToTesting(platform: Platform, drawer: Drawer) type {
    return struct {
        const Self = @This();
        const artist = Artist(platform, drawer);

        input: PhysicalSexpr,
        output: PhysicalSexpr,
        camera: Camera,
        t: f32,
        fnk_name: *const Sexpr,
        // TODO: pointer?
        cases: CaseGroup,

        pub fn init(camera: Camera, input: PhysicalSexpr, output: PhysicalSexpr, fnk_name: *const Sexpr, fnk_cases: CaseGroup) Self {
            var cases = fnk_cases;
            cases.setAllUnfoldedToZero();
            return .{
                .t = 0,
                .camera = camera,
                .input = input,
                .output = output,
                .fnk_name = fnk_name,
                .cases = cases,
            };
        }

        pub fn update(self: *Self, delta_seconds: f32, mem: *VeryPermamentGameStuff) !bool {
            _ = try EditingFnk(platform, drawer).updateCasePositionsAndReturnMouseOverlap(
                mem,
                &.{},
                null,
                self.cases,
                delta_seconds,
            );
            math.towards(&self.t, 1, delta_seconds / 0.5);
            return self.t >= 1;
        }

        pub fn draw(self: Self) !void {
            drawer.clear(Color.gray(128));
            try artist.drawSexpr(self.camera, .lerp(self.input.pos, MAIN_INPUT_POS, self.t), self.input.value);
            try artist.drawSexpr(self.camera, .lerp(self.output.pos, ExecutingFnk(platform, drawer).expected_output_pos, self.t), self.output.value);
            {
                artist.drawOffscreenCableTo(self.camera, MAIN_INPUT_POS);
                try artist.drawHoldedFnk(self.camera, MAIN_FNK_POS, 1, self.fnk_name);
            }
            try EditingFnk(platform, drawer).drawCases(self.camera, true, .{}, self.cases);
            // _ = self;
            // try EditingFnk(platform, drawer).samples_reel.draw(self.camera, &.{self.sample}, &.{true});
        }
    };
}

fn LevelSelectToEditingFnk(platform: Platform, drawer: Drawer) type {
    return struct {
        const Self = @This();
        const artist = Artist(platform, drawer);
        const camera: Camera = DEFAULT_CAM;

        starting_point: Point,
        level: BuiltinLevel,
        t: f32,

        pub fn init(prev: LevelSelect(platform, drawer), level_index: usize) Self {
            return .{
                .level = builtin_levels[level_index],
                .starting_point = Camera.remap(
                    UI.cam,
                    prev.getLevelButtonPoint(level_index),
                    camera,
                ),
                .t = 0,
            };
        }

        pub fn update(self: *Self, delta_seconds: f32) bool {
            math.towards(&self.t, 1, delta_seconds / 0.5);
            return self.t >= 1;
        }

        pub fn draw(self: Self) !void {
            drawer.clear(Color.gray(128));
            try artist.drawHoldedFnk(camera, Point.lerp(self.starting_point, MAIN_FNK_POS, self.t), 1, self.level.fnk_name);
        }
    };
}

pub fn EditingFnk(platform: Platform, drawer: Drawer) type {
    return struct {
        const Self = @This();
        const artist = Artist(platform, drawer);

        const CaseAddressWithPoint = struct {
            address: core.CaseAddress,
            pattern_point_relative_to_parent: Point,
        };
        const CasePlace = union(enum) {
            main_fnk: union(enum) {
                existing: core.CaseAddress,
                ghost: CaseAddressWithPoint,

                // pub fn plainAddress(self: @This()) core.CaseAddress {
                //     return switch (self) {
                //         .existing => |x| x,
                //         .ghost => |x| x,
                //     };
                // }
            },
            toolbar_special_case,
            meta_converter,

            pub fn equals(self: @This(), other: @This()) bool {
                if (std.meta.activeTag(self) != std.meta.activeTag(other)) return false;
                return switch (self) {
                    .main_fnk => |self_case| if (std.meta.activeTag(self_case) != std.meta.activeTag(other.main_fnk))
                        false
                    else switch (self_case) {
                        .existing => |self_existing| std.mem.eql(usize, self_existing, other.main_fnk.existing),
                        .ghost => |self_ghost| std.mem.eql(usize, self_ghost.address, other.main_fnk.ghost.address),
                    },
                    .toolbar_special_case => true,
                    .meta_converter => true,
                };
            }

            pub fn acceptsDrop(place: CasePlace) bool {
                return switch (place) {
                    .main_fnk => true,
                    .toolbar_special_case => false,
                    .meta_converter => true,
                };
            }
        };

        const SexprPlace = union(enum) {
            full_address: core.FullAddress,
            toolbar: usize,
            toolbar_special_var,
            main_input: if (DESIGN.no_current_data) void else core.SexprAddress,
            main_fnk_name: core.SexprAddress,
            sample: Sample.Address,
            external_fnk: fnks_reel.Address,
            fnk_manager,
            meta_converter: core.SexprAddress,

            pub fn equals(self: @This(), other: @This()) bool {
                if (std.meta.activeTag(self) != std.meta.activeTag(other)) return false;
                return switch (self) {
                    .full_address => |self_full| self_full.equals(other.full_address),
                    .toolbar => |self_toolbar| self_toolbar == other.toolbar,
                    .main_input => |self_local| if (DESIGN.no_current_data) true else core.equalSexprAddress(self_local, other.main_input),
                    .main_fnk_name => |self_local| core.equalSexprAddress(self_local, other.main_fnk_name),
                    .sample => |self_sample| self_sample.index == other.sample.index and
                        self_sample.which == other.sample.which and
                        core.equalSexprAddress(self_sample.local, other.sample.local),
                    .external_fnk => |self_fnk| self_fnk.index == other.external_fnk.index and
                        core.equalSexprAddress(self_fnk.local, other.external_fnk.local),
                    .toolbar_special_var => true,
                    .fnk_manager => true,
                    .meta_converter => |self_local| core.equalSexprAddress(self_local, other.meta_converter),
                };
            }

            pub fn getGlobalPoint(address: @This(), self: Self) !Point {
                return switch (address) {
                    .full_address => |full_address| try self.cases.getGlobalPointOf(
                        Point{},
                        full_address,
                    ),
                    .toolbar => |index| toolbar.things[index].point,
                    .main_input => |local| if (DESIGN.no_current_data) MAIN_INPUT_POS else SexprView.sexprChildView(MAIN_INPUT_POS, local),
                    .main_fnk_name => |local| SexprView.sexprChildView(MAIN_FNK_POS, local),
                    .toolbar_special_var => toolbar.special_var_point,
                    .sample => |sample| SexprView.sexprChildView(samples_reel.getPoint(sample.index, sample.which), sample.local),
                    .external_fnk => |fnk| SexprView.sexprChildView(fnks_reel.getPoint(fnk.index), fnk.local),
                    .fnk_manager => fnk_manager.sexpr_point,
                    .meta_converter => |local| SexprView.sexprChildView(meta_converter.sexpr_point, local),
                };
            }

            pub fn getSexpr(address: @This(), self: Self) !?*const Sexpr {
                return switch (address) {
                    .full_address => |full_address| try self.cases.getSexprAt(full_address),
                    .toolbar => |index| toolbar.things[index].value,
                    .main_input => |local| if (DESIGN.no_current_data) null else self.main_input.getAt(local).?,
                    .main_fnk_name => |local| self.fnk_name.getAt(local).?,
                    .toolbar_special_var => toolbar.special_var_state.next_value,
                    .sample => |sample| self.samples[sample.index].get(sample.which).?.getAt(sample.local).?,
                    .external_fnk => |fnk| self.available_fnks[fnk.index].getAt(fnk.local).?,
                    .fnk_manager => null,
                    .meta_converter => |local| if (meta_converter.sexpr) |v| v.getAt(local).? else null,
                    // examples_reel.getPoint(sample.index, sample.which), sample.local),
                    // .main_input => |local| self.main_input.getAt(local).?,
                };
            }

            pub fn setSexpr(address: @This(), self: *Self, value: *const Sexpr) !void {
                switch (address) {
                    .full_address => |full_address| try self.cases.setSexprAt(self.mem, full_address, value),
                    .main_input => |local_address| {
                        if (DESIGN.no_current_data) unreachable;
                        const value_without_variables = try value.changeAllVariablesToNil(self.mem);
                        self.main_input = try self.main_input.setAt(self.mem, local_address, value_without_variables);
                    },
                    .fnk_manager => unreachable,
                    .meta_converter => |local_address| {
                        try meta_converter.setSexpr(self.mem, value, local_address);
                    },
                    .toolbar, .main_fnk_name, .toolbar_special_var, .sample, .external_fnk => unreachable,
                }
            }

            pub fn isPattern(address: @This()) bool {
                return switch (address) {
                    .full_address => |full_address| full_address.which == .pattern,
                    .toolbar_special_var => true,
                    else => false,
                };
            }

            pub fn acceptsDrop(address: @This()) bool {
                return switch (address) {
                    .toolbar => false,
                    .main_fnk_name => false,
                    .toolbar_special_var => false,
                    .sample => false,
                    .external_fnk => false,
                    .full_address => true,
                    .main_input => true,
                    .fnk_manager => true,
                    .meta_converter => true,
                };
            }

            // TODO: some visual feedback
            pub fn acceptsWildcards(address: @This()) bool {
                if (!address.acceptsDrop()) unreachable;
                return switch (address) {
                    .full_address => |x| x.which != .fnk_name,
                    .main_input => false,
                    .fnk_manager => false,
                    .meta_converter => true,
                    else => unreachable,
                };
            }
        };

        const TutorialState = union(enum) {
            none,
            /// hardcoded map
            first_level,
            /// wildcard
            second_level,
            /// apply hardcoded map to unwrapped
            third_level,
            /// nested case
            fourth_level,
            /// free level: map a pair
            fifth_level,

            pub fn allowPickingVaus(self: TutorialState) bool {
                return switch (self) {
                    .first_level, .second_level => false,
                    else => true,
                };
            }

            pub fn allowCreatingVaus(self: TutorialState) bool {
                return self == .none;
            }

            pub fn getToolbar(self: TutorialState) toolbar.Modifier {
                return switch (self) {
                    .first_level => .hidden,
                    .second_level => .only_special_var,
                    .third_level => .all_except_case,
                    else => .normal,
                };
            }

            pub fn getFnksReel(self: TutorialState) fnks_reel.Modifier {
                return switch (self) {
                    .none => .normal,
                    else => .only_first,
                };
            }

            pub fn allowGrabbingCases(self: TutorialState) bool {
                return self != .first_level;
            }

            pub fn allowPickingIdentity(self: TutorialState) bool {
                return self == .none;
            }
        };

        persistence: *PlayerData,
        mem: *VeryPermamentGameStuff,
        camera: Camera = DEFAULT_CAM,
        ui_state: UI.State,
        meta_enabled: bool,
        // TODO: allow user-created Samples
        samples: []const Sample,
        solved_samples: []bool,
        fnk_name: *const Sexpr,
        available_fnks: []const *const Sexpr,
        cases: CaseGroup,
        main_input: if (DESIGN.no_current_data) enum { invalid_field } else *const Sexpr,

        tutorial_state: TutorialState,

        focus: union(enum) {
            nothing,
            hovering_case: struct {
                address: CasePlace,
                hot: f32,
            },
            grabbing_case: struct {
                case: CaseState,
                address_if_released: ?CasePlace,
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
                limitation: enum { none, pattern, template },
            },
        } = .{ .nothing = {} },

        fn overlapsWithTinyCase(mouse_pos: Vec2, case_point: Point) bool {
            const local_point = case_point
                .inverseApplyGetLocalPosition(mouse_pos);

            return local_point.mag() < 2;
        }

        // TODO: cooler, by taking a 'hot' param
        fn drawTinyCase(camera: Camera, case_point: Point, pattern: *const Sexpr, template: *const Sexpr) !void {
            try artist.drawPatternSexpr(camera, case_point
                .applyToLocalPoint(.{ .pos = .new(-1, 0) }), pattern);
            try artist.drawSexpr(camera, case_point
                .applyToLocalPoint(.{ .pos = .new(1, 0) }), template);
            // TODO: artist.drawCableBetween(camera, pattern_pos, template_pos);
            drawer.drawCable(
                camera,
                case_point.applyToLocalPosition(.new(-0.5, 0)),
                case_point.applyToLocalPosition(.new(0.5, 0)),
                case_point.scale,
                0,
            );
        }

        fn drawTinyCaseHolder(camera: Camera, case_point: Point, hot: f32) void {
            // TODO: cooler
            drawer.drawCaseHolder(camera, case_point
                .applyToLocalPoint(.{ .pos = .new(-2, 0) })
                .applyToLocalPoint(.{ .scale = hot }));
        }

        const toolbar = struct {
            const atom_values = [_]*const Sexpr{
                Sexpr.pair_nil_nil,
                Sexpr.builtin.nil,
                Sexpr.builtin.true,
                Sexpr.builtin.false,
            };
            const things = blk: {
                var result: [atom_values.len]struct { value: *const Sexpr, point: Point, index: usize } = undefined;
                for (atom_values, 0..) |atom, k| {
                    result[k] = .{ .value = atom, .point = .{
                        .pos = .new(tof32(k) * 1.6 + 3.5, -2.5),
                        .scale = 0.5,
                    }, .index = k };
                }
                const xx = result;
                break :blk xx;
            };

            const special_var_point = Point{ .pos = .new(2.5, -2.5), .scale = 0.5 };
            var special_var_state: struct {
                random_instance: std.Random.DefaultPrng = std.Random.DefaultPrng.init(0),
                next_value: *const Sexpr = &Sexpr.doVar("first_var"),

                pub fn next(self: *@This(), mem: *VeryPermamentGameStuff) !void {
                    const new_name = try mem.gpa.alloc(u8, 10);
                    Random.init(self.random_instance.random()).alphanumeric_bytes(new_name);
                    self.next_value = try mem.storeSexpr(Sexpr.doVar(new_name));
                }
            } = .{};

            const special_case_point = Point{ .pos = .new(11, -2.5), .scale = 0.5 };
            var special_case_state: struct {
                random_instance: std.Random.DefaultPrng = std.Random.DefaultPrng.init(1),
                next_var: *const Sexpr = Sexpr.builtin.vars.v1,

                pub fn next(self: *@This(), mem: *VeryPermamentGameStuff) !void {
                    const new_name = try mem.gpa.alloc(u8, 10);
                    Random.init(self.random_instance.random()).alphanumeric_bytes(new_name);
                    self.next_var = try mem.storeSexpr(Sexpr.doVar(new_name));
                }

                pub fn value(self: @This()) CaseState {
                    return .{
                        .fnk_name = Sexpr.builtin.identity,
                        .pattern = self.next_var,
                        .template = self.next_var,
                        .next = null,
                        .pattern_point_relative_to_parent = special_case_point,
                    };
                }
            } = .{};

            pub const Modifier = enum {
                normal,
                hidden,
                only_special_var,
                all_except_case,

                pub fn specialVarEnabled(modifier: Modifier, wildcard_in_play: bool) bool {
                    return switch (modifier) {
                        .hidden => false,
                        .normal => true,
                        else => !wildcard_in_play,
                    };
                }

                pub fn specialCaseEnabled(modifier: Modifier) bool {
                    return modifier == .normal;
                }

                pub fn thingsEnabled(modifier: Modifier) bool {
                    return switch (modifier) {
                        .normal, .all_except_case => true,
                        .hidden, .only_special_var => false,
                    };
                }
            };

            pub fn overlapsWithSpecialVar(mouse_pos: Vec2, modifier: Modifier, wildcard_in_play: bool) bool {
                return modifier.specialVarEnabled(wildcard_in_play) and SexprView.overlapsPatternAtom(special_var_point, mouse_pos, .atom);
            }

            pub fn overlapsWithSpecialCase(mouse_pos: Vec2, modifier: Modifier) bool {
                return modifier.specialCaseEnabled() and overlapsWithTinyCase(mouse_pos, special_case_point);
            }

            pub fn findOverlap(mouse_pos: Vec2, modifier: Modifier) ?std.meta.Elem(@TypeOf(things)) {
                if (!modifier.thingsEnabled()) return null;
                for (things) |thing| {
                    if (SexprView.overlapsAtom(thing.point, mouse_pos, .atom)) {
                        return thing;
                    }
                }
                return null;
            }

            pub fn draw(camera: Camera, modifier: Modifier, wildcard_in_play: bool) !void {
                if (modifier == .hidden) return;

                drawer.setTransparency(if (modifier.specialVarEnabled(wildcard_in_play)) 1 else 0.5);
                try artist.drawPatternSexpr(camera, special_var_point, special_var_state.next_value);

                drawer.setTransparency(if (modifier.thingsEnabled()) 1 else 0.5);
                for (things) |thing| {
                    try artist.drawSexpr(camera, thing.point, thing.value);
                }

                drawer.setTransparency(if (modifier.specialCaseEnabled()) 1 else 0.5);
                try drawTinyCase(camera, special_case_point, special_case_state.value().pattern, special_case_state.value().template);

                if (modifier != .normal) drawer.setTransparency(1);
            }
        };

        // TODO: these vars should live on an instance, not the class
        const samples_reel = struct {
            const top_left: Point = .{ .pos = .new(-6, 0.25), .scale = 0.75 };
            // TODO: the -1 is a tutorial hack, make it 0 once the scroll bar is finished
            var scroll: f32 = -1;

            var display_solved_status: bool = false;

            const rect: Rect = .{ .top_left = top_left.pos, .size = Vec2.new(7, 7.5).scale(top_left.scale) };

            const N_VISIBLE_SAMPLES = 3;

            fn getMaxScroll(samples_len: usize) f32 {
                return @max(0, tof32(samples_len) - N_VISIBLE_SAMPLES);
            }

            pub fn updateScroll(main: Self, delta_seconds: f32) void {
                math.lerp_towards_range(&samples_reel.scroll, 0, getMaxScroll(main.samples.len), 0.1, delta_seconds);
            }

            pub fn getPoint(k: usize, which: Sample.Part) Point {
                const index: f32 = tof32(k) - scroll;
                const y = 1.25 + index * 2.5;
                const scale = @min(
                    math.smoothstep(index, -0.5, 0),
                    math.smoothstep(index, 2.5, 2),
                );
                return top_left.applyToLocalPoint(.{ .pos = .new(switch (which) {
                    .input => 0.75,
                    .output => 4.5,
                }, y), .scale = scale });
            }

            pub fn findOverlap(mouse_pos: Vec2, samples: []const Sample) !?Sample.Address {
                for (samples, 0..) |sample, k| {
                    if (try SexprView.overlapsSexpr(
                        platform.gpa,
                        sample.input,
                        getPoint(k, .input),
                        mouse_pos,
                    )) |local| {
                        return Sample.Address{ .index = k, .local = local, .which = .input };
                    }
                    if (sample.output) |output| {
                        if (try SexprView.overlapsSexpr(
                            platform.gpa,
                            output,
                            getPoint(k, .output),
                            mouse_pos,
                        )) |local| {
                            return Sample.Address{ .index = k, .local = local, .which = .output };
                        }
                    }
                }
                return null;
            }

            pub fn draw(camera: Camera, samples: []const Sample, solved_status: []const bool) !void {
                std.debug.assert(samples.len == solved_status.len);
                drawer.drawRect(camera, rect, .black, null);
                for (samples, solved_status, 0..) |sample, solved, k| {
                    drawer.drawArrowForSample(camera, getPoint(k, .output).applyToLocalPoint(.{
                        .pos = .new(-1.25, 0),
                        .scale = 0.25,
                    }), if (display_solved_status) solved else null);
                    try artist.drawSexpr(
                        camera,
                        getPoint(k, .input),
                        sample.input,
                    );
                    if (sample.output) |output| {
                        try artist.drawSexpr(
                            camera,
                            getPoint(k, .output),
                            output,
                        );
                    } else {
                        return error.TODO;
                    }
                }
                drawScrollBar(camera, samples.len);
                drawer.drawDebugText(camera, .{ .pos = rect.get(.top_center).addY(0.2) }, "tests", .black);
            }

            // TODO: mouse-interactable scrollbar
            fn drawScrollBar(camera: Camera, samples_len: usize) void {
                const scroll_perc = scroll / getMaxScroll(samples_len);
                const bar_height = N_VISIBLE_SAMPLES / getMaxScroll(samples_len);
                drawer.drawRect(
                    camera,
                    .{
                        .top_left = rect.get(.top_right).addY((rect.size.y - bar_height) * scroll_perc),
                        .size = .new(0.2, bar_height),
                    },
                    null,
                    .black,
                );
            }
        };

        // TODO: would be nice to classify fnks by name
        const fnks_reel = struct {
            const top_left: Point = .{ .pos = .new(-6, 0.5 + samples_reel.rect.size.y), .scale = 0.75 };
            var scroll: f32 = 0;

            const rect: Rect = .{ .top_left = top_left.pos, .size = Vec2.new(7, 5.5).scale(top_left.scale) };
            const N_FNKS_PER_ROW = 3;
            const N_VISIBLE_FNKS = 2;

            pub const Address = struct {
                index: usize,
                local: core.SexprAddress,
            };

            fn getMaxScroll(fnks_len: usize) f32 {
                return @max(0, tof32(std.math.divCeil(usize, fnks_len, N_FNKS_PER_ROW) catch unreachable) - N_VISIBLE_FNKS);
            }

            pub fn updateScroll(main: Self, delta_seconds: f32) void {
                math.lerp_towards_range(&fnks_reel.scroll, 0, getMaxScroll(main.available_fnks.len), 0.1, delta_seconds);
            }

            fn getPoint(k: usize) Point {
                const v_index: f32 = tof32(k / N_FNKS_PER_ROW) - scroll;
                const y = 2 + v_index * 2.5;
                const x = 1.25 + tof32(k % N_FNKS_PER_ROW) * 2.1;
                const scale = @min(
                    math.smoothstep(v_index, -0.5, 0),
                    math.smoothstep(v_index, 1.5, 1),
                );
                return top_left.applyToLocalPoint(.{
                    .pos = .new(x, y),
                    .scale = scale * 0.75,
                    .turns = -0.25,
                });
            }

            pub const Modifier = enum { normal, only_first };

            pub fn findOverlap(mouse_pos: Vec2, available_fnks: []const *const Sexpr, modifier: Modifier) !?Address {
                for (available_fnks, 0..) |fnk_name, k| {
                    if (try SexprView.overlapsSexpr(
                        platform.gpa,
                        fnk_name,
                        getPoint(k),
                        mouse_pos,
                    )) |local| {
                        return .{ .index = k, .local = local };
                    }
                    if (modifier == .only_first) break;
                }
                return null;
            }

            pub fn draw(camera: Camera, available_fnks: []const *const Sexpr, modifier: Modifier) !void {
                drawer.drawRect(camera, rect, .black, null);
                for (available_fnks, 0..) |fnk_name, k| {
                    try artist.drawSexpr(camera, getPoint(k), fnk_name);
                    if (modifier == .only_first and k == 0) drawer.setTransparency(0.5);
                }
                if (modifier == .only_first) drawer.setTransparency(1);
                drawScrollBar(camera, available_fnks.len);
                drawer.drawDebugText(camera, .{ .pos = rect.get(.top_center).addY(0.2) }, "vaus", .black);
            }

            fn drawScrollBar(camera: Camera, fnks_len: usize) void {
                const scroll_perc = scroll / getMaxScroll(fnks_len);
                const bar_height = N_VISIBLE_FNKS / getMaxScroll(fnks_len);
                drawer.drawRect(
                    camera,
                    .{
                        .top_left = rect.get(.top_right).addY((rect.size.y - bar_height) * scroll_perc),
                        .size = .new(0.2, bar_height),
                    },
                    null,
                    .black,
                );
            }
        };

        /// create/edit/delete fnks
        const fnk_manager = struct {
            const sexpr_point: Point = .{ .pos = .new(-5, -1.5), .scale = 0.5, .turns = -0.25 };

            pub fn findOverlap(mouse_pos: Vec2) bool {
                return SexprView.overlapsAtom(sexpr_point, mouse_pos, .atom);
            }

            pub fn draw(camera: Camera) !void {
                drawer.drawRect(
                    camera,
                    Rect.fromCenterAndSize(sexpr_point.pos, .both(sexpr_point.scale)),
                    .black,
                    null,
                );
                drawer.drawDebugText(camera, sexpr_point.applyToLocalPoint(.{ .pos = .new(-1, 0) }), "change vau", .black);
            }
        };

        /// sexprs to cases and vice versa
        const meta_converter = struct {
            const sexpr_point: Point = .{ .pos = .new(16, -3), .scale = 0.75 };
            const case_point: Point = sexpr_point.applyToLocalPoint(.{ .pos = .new(0, 2.5) });

            pub const Overlap = union(enum) {
                case,
                sexpr: core.SexprAddress,
            };

            /// Don't set this directly
            var sexpr: ?*const Sexpr = null;
            /// Don't set this directly
            var case: ?core.MatchCaseDefinition = null;

            pub fn setSexpr(mem: *VeryPermamentGameStuff, new_sexpr: *const Sexpr, local_address: core.SexprAddress) !void {
                sexpr = if (meta_converter.sexpr) |existing| blk: {
                    // const value_without_variables = try new_sexpr.changeAllVariablesToNil(mem);
                    break :blk try existing.setAt(mem, local_address, new_sexpr);
                } else blk: {
                    std.debug.assert(local_address.len == 0);
                    break :blk new_sexpr;
                };

                case = core.caseFromSexpr(sexpr.?, mem.arena_for_cases.allocator(), &mem.pool_for_sexprs) catch |err| switch (err) {
                    error.InvalidMetaFnk, error.BAD_INPUT => null,
                    else => return err,
                };
            }

            pub fn setCase(mem: *VeryPermamentGameStuff, new_case: core.MatchCaseDefinition) !void {
                case = new_case;
                sexpr = try core.sexprFromCase(case.?, &mem.pool_for_sexprs);
            }

            pub fn findOverlap(mouse_pos: Vec2) !?Overlap {
                if (sexpr) |s| {
                    if (try SexprView.overlapsSexpr(
                        platform.gpa,
                        s,
                        sexpr_point,
                        mouse_pos,
                    )) |local| {
                        return .{ .sexpr = local };
                    }
                } else if (SexprView.overlapsAtom(
                    sexpr_point,
                    mouse_pos,
                    .atom,
                )) {
                    return .{ .sexpr = core.emptySexprAddress };
                }

                if (overlapsWithTinyCase(mouse_pos, case_point)) {
                    return .case;
                }

                return null;
            }

            pub fn draw(camera: Camera) !void {
                if (sexpr) |s| {
                    try artist.drawSexpr(camera, sexpr_point, s);
                } else {
                    drawer.drawRect(
                        camera,
                        Rect.fromCenterAndSize(sexpr_point.pos, .both(sexpr_point.scale)),
                        .black,
                        null,
                    );
                }

                if (case) |c| {
                    // TODO: case fnk_name and "has_next"
                    try drawTinyCase(camera, case_point, c.pattern, c.template);
                } else {
                    drawer.drawRect(
                        camera,
                        Rect.fromCenterAndSize(case_point.pos, .both(case_point.scale)),
                        .black,
                        null,
                    );
                }
            }
        };

        fn makeCasePhysical(mem: *VeryPermamentGameStuff, case: core.MatchCaseDefinition, point: Point) !CaseState {
            return .{
                .fnk_name = case.fnk_name,
                .pattern = case.pattern,
                .template = case.template,
                .next = if (case.next) |next| try makeCasesPhysical(mem, next) else null,
                .pattern_point_relative_to_parent = point,
            };
        }

        fn makeCasesPhysical(mem: *VeryPermamentGameStuff, cases: core.MatchCases) OoM!CaseGroup {
            var result = std.ArrayListUnmanaged(CaseState){};
            for (cases.items, 0..) |case, k| {
                try result.append(mem.gpa, try makeCasePhysical(mem, case, .{ .pos = .new(3, 2.5 + 1.5 * tof32(k)), .scale = 0.5 }));
            }
            return .{ .cases = result, .unfolded = 0 };
        }

        pub fn getFnk(self: Self) !Fnk {
            return Fnk{
                .name = self.fnk_name,
                .body = .{ .cases = try getMatchCases(self.mem, self.cases) },
            };
        }

        fn makeCaseVirtual(mem: *VeryPermamentGameStuff, case: CaseState) !core.MatchCaseDefinition {
            return .{
                .fnk_name = case.fnk_name,
                .pattern = case.pattern,
                .template = case.template,
                .next = if (case.next) |next|
                    (try getMatchCases(mem, next))
                else
                    null,
            };
        }

        fn getMatchCases(mem: *VeryPermamentGameStuff, group: CaseGroup) OoM!core.MatchCases {
            var result = std.ArrayListUnmanaged(core.MatchCaseDefinition){};
            for (group.cases.items) |case| {
                try result.append(mem.arena_for_cases.allocator(), try makeCaseVirtual(mem, case));
            }
            return result;
        }

        pub fn init(fnk_name: *const Sexpr, builtin_samples: []const Sample, available_fnks: []const *const Sexpr, fnk_body: core.FnkBody, mem: *VeryPermamentGameStuff, persistence: *PlayerData) !Self {
            const cases = try makeCasesPhysical(mem, fnk_body.cases);
            const main_input: *const Sexpr = Sexpr.builtin.nil;

            const ui_state = UI.State{ .buttons = try UI.Button.row(platform.gpa, .zero, .one, &(.{
                "Back",
                "Reset\nView",
                "Check",
            } ++ if (DESIGN.no_current_data) .{} else .{
                "⏵",
            })) };

            const solved_samples = try mem.gpa.alloc(bool, builtin_samples.len);
            try updateSolvedSamples(
                .{ .name = fnk_name, .body = fnk_body },
                builtin_samples,
                persistence,
                mem,
                solved_samples,
            );

            return .{
                .fnk_name = fnk_name,
                .samples = builtin_samples,
                .solved_samples = solved_samples,
                .mem = mem,
                .persistence = persistence,
                .cases = cases,
                .main_input = if (DESIGN.no_current_data) .invalid_field else main_input,
                .ui_state = ui_state,
                .available_fnks = available_fnks,
                // TODO: figure out when to enable the meta features
                .meta_enabled = false,
                .tutorial_state = if (fnk_name.equals(builtin_levels[0].fnk_name))
                    .first_level
                else if (fnk_name.equals(builtin_levels[1].fnk_name))
                    .second_level
                else if (fnk_name.equals(builtin_levels[2].fnk_name))
                    .third_level
                else if (fnk_name.equals(builtin_levels[3].fnk_name))
                    .fourth_level
                else if (fnk_name.equals(builtin_levels[4].fnk_name))
                    .fifth_level
                else
                    .none,
            };
        }

        // TODO: don't call this if nothing actually changed
        fn onChangedSomething(self: *Self) !void {
            samples_reel.display_solved_status = false;
            try updateSolvedSamples(try self.getFnk(), self.samples, self.persistence, self.mem, self.solved_samples);
        }

        fn updateSolvedSamples(fnk: Fnk, samples: []const Sample, persistence: *PlayerData, mem: *VeryPermamentGameStuff, buf: []bool) !void {
            std.debug.assert(samples.len == buf.len);

            try persistence.fnks.put(fnk.name, fnk.body);

            var score = try core.ScoringRun.initFromFnks(persistence.fnks, mem);
            defer score.deinit(false);

            for (samples, buf) |sample, *target| {
                target.* = blk: {
                    var exec = core.ExecutionThread.init(sample.input, fnk.name, &score) catch |err| switch (err) {
                        error.FnkNotFound => break :blk false,
                        else => return err,
                    };
                    defer exec.deinit();

                    const actual_output = exec.getFinalResult(&score) catch |err| switch (err) {
                        error.FnkNotFound, error.NoMatchingCase, error.InvalidMetaFnk, error.UsedUndefinedVariable => break :blk false,
                        error.BAD_INPUT => break :blk false,
                        error.OutOfMemory => return err,
                    };
                    break :blk actual_output.equals(sample.output.?);
                };
            }
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

        pub fn update(self: *Self, delta_seconds: f32) !union(enum) {
            nothing,
            back_to_level_select,
            launch_test,
            launch_execution: if (DESIGN.no_current_data) PhysicalSexpr else void,
            change_to: *const Sexpr,
        } {
            var mouse = platform.getMouse();

            if (self.ui_state.update(platform.getMouse(), delta_seconds)) |pressed_button| {
                switch (pressed_button) {
                    0 => return .back_to_level_select,
                    1 => self.camera = DEFAULT_CAM,
                    2 => {
                        samples_reel.display_solved_status = true;
                        return .launch_test;
                    },
                    3 => if (DESIGN.no_current_data) unreachable else return .launch_execution,
                    else => @panic("oops"),
                }
            }

            inline for (.{ samples_reel, fnks_reel }) |x| {
                if (x.rect.contains(mouse.cur.pos(self.camera))) {
                    x.scroll -= delta_seconds * 10 * mouse.cur.scrolled.toNumber();
                    mouse.cur.scrolled = .none;
                }
                x.updateScroll(self.*, delta_seconds);
            }
            moveCamera(&self.camera, delta_seconds, platform.getKeyboard(), mouse);

            const camera = self.camera;

            // focus-specific updates
            switch (self.focus) {
                .grabbing_case => |*grabbing| {
                    // grabbing case parent is the nothing!
                    grabbing.case.pattern_point_relative_to_parent.lerp_towards((Point{
                        .pos = platform.getMouse().cur.pos(camera),
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
                            .main_input => .{ .turns = -0.02, .pos = .new(0.5, 0) },
                            .fnk_manager => .{ .turns = 0.02, .pos = .new(0.5, 0) },
                            .meta_converter => .{ .turns = -0.02, .pos = .new(0.5, 0) },
                            .toolbar, .main_fnk_name, .toolbar_special_var, .sample, .external_fnk => unreachable,
                        })
                    else
                        // TODO: it would be nice to have the scale instantly correct when the camera zooms
                        Point{
                            .pos = platform.getMouse().cur.pos(camera),
                            .scale = camera.height / DEFAULT_CAM.height,
                        }, 0.6, delta_seconds);
                    math.lerp_towards(&grabbing.is_pattern, switch (grabbing.limitation) {
                        .pattern => 1,
                        .template => 0,
                        .none => if (grabbing.address_if_released) |goal|
                            if (goal.isPattern()) 1 else 0
                        else
                            @round(grabbing.is_pattern),
                    }, 0.6, delta_seconds);
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
                .hovering_case => |*hovering| {
                    if (std.meta.activeTag(hovering.address) == .main_fnk) {
                        try self.cases.setUnfolded(hovering.address.main_fnk.existing);
                    }
                    math.lerp_towards(&hovering.hot, 1, 0.6, delta_seconds);
                },
            }

            // update cases & focus
            {
                const mouse_pos = platform.getMouse().cur.pos(camera);
                const Overlapped = union(enum) {
                    case: CasePlace,
                    sexpr: SexprPlace,
                };
                const maybe_overlapped: ?Overlapped = if (blk: {
                    if (try asdfUpdateAndReturnOverlap(
                        self,
                        mouse_pos,
                        delta_seconds,
                    )) |overlap|
                        switch (overlap) {
                            .case => |case| break :blk Overlapped{ .case = .{ .main_fnk = case } },
                            .sexpr => |sexpr| break :blk if (!self.tutorial_state.allowPickingIdentity() and
                                sexpr.which == .fnk_name and
                                (try self.cases.getSexprAt(sexpr)).equals(Sexpr.builtin.identity) and
                                self.focus == .nothing)
                                break :blk null
                            else
                                break :blk Overlapped{ .sexpr = .{ .full_address = sexpr } },
                        }
                    else
                        break :blk null;
                }) |overlap|
                    overlap
                else if (toolbar.findOverlap(mouse_pos, self.tutorial_state.getToolbar())) |overlap|
                    .{ .sexpr = .{ .toolbar = overlap.index } }
                else if (try samples_reel.findOverlap(mouse_pos, self.samples)) |overlap|
                    .{ .sexpr = .{ .sample = overlap } }
                else if (if (self.tutorial_state.allowPickingVaus()) try fnks_reel.findOverlap(mouse_pos, self.available_fnks, self.tutorial_state.getFnksReel()) else null) |overlap|
                    .{ .sexpr = .{ .external_fnk = overlap } }
                else if (self.tutorial_state.allowCreatingVaus() and fnk_manager.findOverlap(mouse_pos))
                    .{ .sexpr = .fnk_manager }
                else if (if (self.meta_enabled) try meta_converter.findOverlap(mouse_pos) else null) |overlap| switch (overlap) {
                    .sexpr => |local| .{ .sexpr = .{ .meta_converter = local } },
                    .case => .{ .case = .meta_converter },
                } else if (toolbar.overlapsWithSpecialVar(mouse_pos, self.tutorial_state.getToolbar(), self.cases.anyWildcardInPlay()))
                    .{ .sexpr = .toolbar_special_var }
                else if (DESIGN.no_current_data and SexprView.overlapsAtom(MAIN_INPUT_POS, mouse_pos, .atom))
                    .{ .sexpr = .main_input }
                else if (if (DESIGN.no_current_data) null else try SexprView.overlapsSexpr(self.mem.gpa, self.main_input, MAIN_INPUT_POS, mouse_pos)) |overlap|
                    .{ .sexpr = .{ .main_input = overlap } }
                else if (try SexprView.overlapsSexpr(self.mem.gpa, self.fnk_name, MAIN_FNK_POS, mouse_pos)) |overlap|
                    .{ .sexpr = .{ .main_fnk_name = overlap } }
                else if (toolbar.overlapsWithSpecialCase(mouse_pos, self.tutorial_state.getToolbar()))
                    .{ .case = .toolbar_special_case }
                else
                    null;

                switch (self.focus) {
                    .grabbing_case => |*grabbing| if (maybe_overlapped) |overlapped|
                        switch (overlapped) {
                            .case => |place| {
                                grabbing.address_if_released = if (place.acceptsDrop()) place else null;
                            },
                            .sexpr => {
                                grabbing.address_if_released = null;
                            },
                        }
                    else {
                        grabbing.address_if_released = null;
                    },
                    .grabbing_sexpr => |*grabbing| if (maybe_overlapped) |overlapped|
                        switch (overlapped) {
                            .case => |place| {
                                if (std.meta.activeTag(place) == .main_fnk) {
                                    try self.cases.setUnfolded(place.main_fnk.existing);
                                }
                                grabbing.address_if_released = null;
                            },
                            .sexpr => |place| {
                                if (std.meta.activeTag(place) == .full_address) {
                                    try self.cases.setUnfolded(place.full_address.case_address);
                                }
                                grabbing.address_if_released = if (!place.acceptsDrop()) null else switch (grabbing.limitation) {
                                    .none => place,
                                    .pattern => if (!place.acceptsWildcards()) null else if (place.isPattern()) place else null,
                                    .template => if (!place.acceptsWildcards()) null else if (place.isPattern()) null else place,
                                };
                            },
                        }
                    else {
                        grabbing.address_if_released = null;
                    },
                    .nothing, .hovering_sexpr, .hovering_case => if (maybe_overlapped) |overlapped| {
                        switch (overlapped) {
                            // .special_case => if (!(std.meta.activeTag(self.focus) == .hovering_special_case)) {
                            //     self.focus = .{ .hovering_special_case = 0 };
                            // },
                            .case => |place| {
                                if (!(std.meta.activeTag(self.focus) == .hovering_case and self.focus.hovering_case.address.equals(place))) {
                                    self.focus = .{ .hovering_case = .{
                                        .address = place,
                                        .hot = 0,
                                    } };
                                }
                            },
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

            // Mouse interaction
            if (platform.getMouse().wasPressed(.left)) {
                switch (self.focus) {
                    .nothing => {},
                    .grabbing_case => |*grabbing| {
                        if (grabbing.address_if_released) |place| {
                            switch (place) {
                                .main_fnk => |case| {
                                    const address = case.ghost.address;
                                    const global_point = grabbing.case.pattern_point_relative_to_parent;
                                    const parent_point = try self.cases.getPatternGlobalPoint(.{}, address[0 .. address.len - 1]);
                                    grabbing.case.pattern_point_relative_to_parent = parent_point.inverseApplyGetLocal(global_point);
                                    try self.cases.insertAt(self.mem, address, grabbing.case);
                                    try self.onChangedSomething();
                                    self.focus = .{ .hovering_case = .{ .address = .{ .main_fnk = .{ .existing = address } }, .hot = 1 } };
                                },
                                .meta_converter => {
                                    try meta_converter.setCase(self.mem, try makeCaseVirtual(self.mem, grabbing.case));
                                    self.focus = .{ .hovering_case = .{ .address = place, .hot = 1 } };
                                },
                                .toolbar_special_case => unreachable,
                            }
                        } else {
                            self.focus = .{ .nothing = {} };
                        }
                    },
                    .grabbing_sexpr => |grabbing| {
                        if (grabbing.address_if_released) |address| {
                            if (address == .fnk_manager) {
                                return .{ .change_to = try grabbing.sexpr.changeAllVariablesToNil(self.mem) };
                            } else if (DESIGN.no_current_data and address == .main_input) {
                                self.focus = .nothing;
                                return .{ .launch_execution = .{ .value = try grabbing.sexpr.changeAllVariablesToNil(self.mem), .pos = grabbing.point, .is_pattern = grabbing.is_pattern } };
                            } else {
                                try address.setSexpr(self, grabbing.sexpr);
                                try self.onChangedSomething();
                                if (DESIGN.autograb_wildcard_template_after_pattern and grabbing.limitation == .pattern) {
                                    self.focus = .{ .grabbing_sexpr = .{
                                        .sexpr = grabbing.sexpr,
                                        .address_if_released = null,
                                        .limitation = .template,
                                        .is_pattern = 1,
                                        .point = grabbing.point,
                                    } };
                                } else {
                                    self.focus = .{ .hovering_sexpr = .{
                                        .address = address,
                                        .global_point = grabbing.point,
                                    } };
                                }
                            }
                        } else {
                            self.focus = .{ .nothing = {} };
                        }
                    },
                    .hovering_case => |hovering| if (self.tutorial_state.allowGrabbingCases()) {
                        switch (hovering.address) {
                            .main_fnk => |unfolded| {
                                const global_point = try self.cases.getPatternGlobalPoint(.{}, unfolded.existing);
                                var asdf = try self.cases.removeAt(unfolded.existing);
                                try self.onChangedSomething();
                                const old_point = asdf.pattern_point_relative_to_parent;
                                asdf.pattern_point_relative_to_parent = global_point;
                                self.focus = .{ .grabbing_case = .{
                                    .case = asdf,
                                    .address_if_released = .{ .main_fnk = .{ .ghost = .{
                                        .address = unfolded.existing,
                                        .pattern_point_relative_to_parent = old_point,
                                    } } },
                                } };
                            },
                            .meta_converter => {
                                if (meta_converter.case) |case| {
                                    self.focus = .{
                                        .grabbing_case = .{
                                            .case = try makeCasePhysical(self.mem, case, meta_converter.case_point),
                                            .address_if_released = hovering.address,
                                        },
                                    };
                                }
                            },
                            .toolbar_special_case => {
                                self.focus = .{ .grabbing_case = .{
                                    .case = toolbar.special_case_state.value(),
                                    .address_if_released = null,
                                } };
                                try toolbar.special_case_state.next(self.mem);
                            },
                        }
                    },
                    .hovering_sexpr => |hovering| {
                        if (try hovering.address.getSexpr(self.*)) |v| {
                            self.focus = .{
                                .grabbing_sexpr = .{
                                    .address_if_released = if (hovering.address.acceptsDrop()) hovering.address else null,
                                    .is_pattern = if (hovering.address.isPattern()) 1 else 0,
                                    .point = hovering.global_point,
                                    .sexpr = v,
                                    .limitation = if (v.isFullyResolved())
                                        .none
                                    else if (hovering.address == .toolbar_special_var)
                                        .pattern
                                    else
                                        .template,
                                },
                            };

                            if (std.meta.activeTag(hovering.address) == .full_address and hovering.address.full_address.which == .fnk_name) {
                                (try self.cases.caseRefAt(hovering.address.full_address.case_address)).fnk_name = Sexpr.builtin.identity;
                                try self.onChangedSomething();
                            } else if (std.meta.activeTag(hovering.address) == .toolbar_special_var) {
                                try toolbar.special_var_state.next(self.mem);
                            }
                        }
                    },
                }
            }

            if (platform.getMouse().wasPressed(.right)) {
                switch (self.focus) {
                    .hovering_sexpr => |hovering| {
                        if (hovering.address.acceptsDrop()) {
                            if (try hovering.address.getSexpr(self.*)) |old_value| {
                                const new_value = try self.mem.storeSexpr(Sexpr.doPair(old_value, Sexpr.builtin.nil));
                                try hovering.address.setSexpr(self, new_value);
                                try self.onChangedSomething();
                            }
                        }
                    },
                    else => {},
                }
            }

            return .nothing;
        }

        fn isPattern(which: @FieldType(core.FullAddress, "which")) f32 {
            return switch (which) {
                .pattern => 1,
                else => 0,
            };
        }

        pub fn draw(self: Self) !void {
            const camera = self.camera;
            drawer.clear(Color.gray(128));
            {
                artist.drawOffscreenCableTo(camera, MAIN_INPUT_POS);
                if (!DESIGN.no_current_data) try artist.drawSexpr(
                    camera,
                    MAIN_INPUT_POS,
                    self.main_input,
                );
                try artist.drawHoldedFnk(camera, MAIN_FNK_POS, 1, self.fnk_name);
            }

            try drawCases(camera, true, .{}, self.cases);
            try toolbar.draw(camera, self.tutorial_state.getToolbar(), self.cases.anyWildcardInPlay());
            // switch (self.tutorial_state.getToolbar()) {
            //     .normal =>
            //     .hidden => {},
            // }

            try samples_reel.draw(camera, self.samples, self.solved_samples);
            if (self.tutorial_state.allowPickingVaus()) try fnks_reel.draw(camera, self.available_fnks, self.tutorial_state.getFnksReel());
            if (self.tutorial_state.allowCreatingVaus()) try fnk_manager.draw(camera);
            if (self.meta_enabled) try meta_converter.draw(camera);

            switch (self.focus) {
                .nothing => {},
                .hovering_case => |hovering| switch (hovering.address) {
                    .main_fnk => |unfolded| {
                        const pattern_point = try self.cases.getPatternGlobalPoint(.{}, unfolded.existing);
                        drawer.drawCaseHolderExtended(camera, .{
                            .pos = pattern_point.pos.sub(.new(3, 0)),
                            .scale = hovering.hot,
                        }, self.tutorial_state != .first_level);
                    },
                    .meta_converter => if (meta_converter.case != null) {
                        drawTinyCaseHolder(camera, meta_converter.case_point, hovering.hot);
                    },
                    .toolbar_special_case => {
                        drawTinyCaseHolder(camera, toolbar.special_case_point, hovering.hot);
                    },
                },
                .grabbing_sexpr => |grabbing| {
                    try artist.drawBothSexpr(
                        camera,
                        grabbing.point,
                        grabbing.is_pattern,
                        grabbing.sexpr,
                    );
                    if (grabbing.limitation == .template) {
                        var wildcard_names: std.ArrayList([]const u8) = .init(platform.gpa);
                        defer wildcard_names.deinit();
                        try grabbing.sexpr.getAllVarNames(&wildcard_names);
                        try artist.drawWildcardLinesToFloating(camera, .{}, self.cases, grabbing.point, wildcard_names.items);
                    }
                },
                .grabbing_case => |grabbing| {
                    if (grabbing.address_if_released) |place| {
                        switch (place) {
                            .main_fnk => |address| {
                                drawer.setTransparency(0.5);
                                const parent_pattern_point = (try self.cases
                                    .getPatternGlobalPoint(.{}, address.ghost.address[0 .. address.ghost.address.len - 1]));
                                const pattern_point = parent_pattern_point
                                    .applyToLocalPoint(address.ghost.pattern_point_relative_to_parent);
                                try artist.drawPatternSexpr(
                                    camera,
                                    pattern_point,
                                    grabbing.case.pattern,
                                );
                                try drawCaseExtra(camera, pattern_point, grabbing.case);
                                const pos = pattern_point.applyToLocalPosition(.new(0, 1));
                                const esquina = pos.sub(.new(if (address.ghost.address.len == 1) 5 else 3, 0));
                                drawer.drawCable(camera, esquina, pos, 1, 0);
                                drawer.drawLine(camera, &.{ esquina, parent_pattern_point.applyToLocalPosition(.new(if (address.ghost.address.len == 1) 0 else 1, 0)) }, .black);
                                drawer.setTransparency(1);
                            },
                            else => {}, // TODO
                        }
                    }
                    // grabbing case parent is the nothing!
                    const pattern_point = grabbing.case.pattern_point_relative_to_parent;
                    try artist.drawPatternSexpr(
                        camera,
                        pattern_point,
                        grabbing.case.pattern,
                    );
                    try drawCaseExtra(camera, pattern_point, grabbing.case);
                },
                .hovering_sexpr => |hovering| {
                    if (try hovering.address.getSexpr(self)) |value| {
                        try artist.drawBothSexpr(
                            camera,
                            hovering.global_point,
                            if (hovering.address.isPattern()) 1 else 0,
                            value,
                        );
                        // try artist.drawPatternOutline(camera, artist.sexprPatternChildView(
                        //     case.pattern_point,
                        //     full_address.sexpr_address,
                        // ));
                    }
                },
            }

            self.ui_state.draw(drawer);

            switch (self.tutorial_state) {
                .none => {},
                .first_level => {
                    drawer.drawDebugText(camera, .{ .pos = .new(-3.55, -2), .scale = 0.75 }, "That's the name of →\nthe Vau you're editing.", .black);
                    if (DESIGN.no_current_data) {
                        drawer.drawDebugText(camera, .{ .pos = .new(9, 0), .scale = 0.75 }, "← Place some Data here to run the Vau on it.", .black);
                    } else {
                        drawer.drawDebugText(camera, .{ .pos = .new(8, 0), .scale = 0.75 }, "← That gray thing is the current Data;\nfeel free to change it by\ndropping some other Data on it.", .black);
                        // drawer.drawDebugText(camera, .{ .pos = .new(6, -1.85), .scale = 0.75 }, "↓ That gray thing is the current Data;\nfeel free to change it by\ndropping some other Data on it.", .black);
                        drawer.drawDebugText(camera, .{ .pos = .new(3.5, -4), .scale = 0.75 }, "← Click Play to see the Vau applied to the current Data.", .black);
                    }
                    drawer.drawDebugText(camera, .{ .pos = .new(-3.25, 7), .scale = 0.75 }, "↑\nThese Tests are the Data\ntransformations your Vau\nmust achieve.", .black);
                    // drawer.drawDebugText(camera, .{ .pos = .new(10, 1), .scale = 0.75 }, "↓ These are the Cases that make up the Vau.", .black);
                    drawer.drawDebugText(camera, .{ .pos = .new(3, 9.5), .scale = 0.75 }, "Once all Tests are green, the Vau is done and you can go to the next one.", .black);
                },
                .second_level => {
                    drawer.drawDebugText(camera, .{ .pos = .new(11, -3.5), .scale = 0.75 }, "↓ This special Data is called a Wildcard, and will match with any other Data.", .black);
                    drawer.drawDebugText(camera, .{ .pos = .new(8, 8), .scale = 0.75 }, "All the Tests for this Vau have the same structure; use a Wildcard to solve them with a single Case.", .black);
                },
                .third_level => {
                    drawer.drawDebugText(camera, .{ .pos = .new(2.5, 7.25), .scale = 0.75 }, "← your collection of Vaus.", .black);
                    drawer.drawDebugText(camera, .{ .pos = .new(14.35, 0.75), .scale = 0.75 }, "↓ Place a Vau name here to call it on the result.", .black);
                    if (!DESIGN.no_current_data) drawer.drawDebugText(camera, .{ .pos = .new(2.5, -4), .scale = 0.75 }, "← Don't forget to hit Play to see the Vau in action!", .black);
                },
                .fourth_level => {
                    drawer.drawDebugText(camera, .{ .pos = .new(3, 6), .scale = 0.75 }, "Nested Cases will\nbe called on the result →", .black);
                },
                .fifth_level => {
                    drawer.drawDebugText(camera, .{ .pos = .new(5, 9.5), .scale = 0.75 }, "You're now on your own. Good luck!", .black);
                },
            }
        }

        fn drawCases(camera: Camera, is_first: bool, parent_point: Point, group: CaseGroup) OoM!void {
            for (group.cases.items) |case| {
                const pattern_point = parent_point.applyToLocalPoint(case.pattern_point_relative_to_parent);
                try artist.drawPatternSexpr(
                    camera,
                    pattern_point,
                    case.pattern,
                );
                if (case.pattern_point_relative_to_parent.scale >= 0.9) {
                    try drawCaseExtra(camera, pattern_point, case);
                }

                const pos = pattern_point.applyToLocalPosition(.new(0, 1));
                drawer.drawCable(
                    camera,
                    pos.sub(.new(parent_point.scale * if (is_first) tof32(5.0) else tof32(3.0), 0)),
                    pos,
                    1,
                    0,
                );
            }

            if (group.cases.getLastOrNull()) |last_case| {
                const lowest_point = parent_point
                    .applyToLocalPoint(last_case.pattern_point_relative_to_parent)
                    .applyToLocalPosition(.new(0, 1))
                    .sub(.new(parent_point.scale * if (is_first) tof32(5.0) else tof32(3.0), 0));
                drawer.drawLine(camera, &.{ parent_point.applyToLocalPosition(if (is_first) .zero else .new(1, 0)), lowest_point }, .black);
            }
        }

        fn drawCaseExtra(camera: Camera, pattern_point: Point, case: CaseState) !void {
            try artist.drawSexpr(
                camera,
                pattern_point.applyToLocalPoint(.{ .pos = .new(DIST_TO_TEMPLATE, 0) }),
                case.template,
            );
            try artist.drawHoldedFnk(camera, pattern_point.applyToLocalPoint(FNK_NAME_OFFSET), 0, case.fnk_name);
            drawer.drawCable(
                camera,
                pattern_point.applyToLocalPosition(.new(0.5, 0)),
                pattern_point.applyToLocalPosition(.new(DIST_TO_TEMPLATE - 0.5, 0)),
                pattern_point.scale,
                0,
            );
            try artist.drawPlacedWildcardsCable(
                camera,
                pattern_point,
                pattern_point.applyToLocalPoint(.{ .pos = .new(DIST_TO_TEMPLATE, 0) }),
                case.pattern,
                case.template,
            );
            if (case.next) |next| {
                try drawCases(camera, false, pattern_point, next);
            }
        }

        fn asdfUpdateAndReturnOverlap(self: *Self, mouse_pos: Vec2, delta_seconds: f32) !?OverlapResult {
            if (std.meta.activeTag(self.focus) == .grabbing_case) {
                const main_fnk_address_if_released = if (self.focus.grabbing_case.address_if_released) |address_if_released|
                    switch (address_if_released) {
                        .main_fnk => |x| x.ghost.address,
                        else => null,
                    }
                else
                    null;
                try doGrabbingCaseFirstPass(self.mem, main_fnk_address_if_released, &.{}, self.cases, delta_seconds);
                const asdf: ?CaseAddressWithPoint = if (self.cases.cases.items.len == 0)
                    .{ .address = try self.debugMakeAddress(0), .pattern_point_relative_to_parent = relativePatternPoint(true, false, 2) }
                else
                    try doGrabbingCaseSecondPass(
                        mouse_pos,
                        main_fnk_address_if_released,
                        self.mem,
                        &.{},
                        &self.cases,
                    );
                if (asdf) |x| {
                    return OverlapResult{ .case = .{ .ghost = x } };
                } else {
                    return null;
                }
            } else {
                return try updateCasePositionsAndReturnMouseOverlap(
                    self.mem,
                    &.{},
                    mouse_pos,
                    self.cases,
                    delta_seconds,
                );
            }
        }

        const OverlapResult = union(enum) {
            case: @FieldType(CasePlace, "main_fnk"),
            // case: union(enum) {
            //     existing: core.CaseAddress,
            //     ghost: core.CaseAddress,
            // },
            sexpr: core.FullAddress,
        };

        fn relativePatternPoint(is_gen0: bool, is_folded: bool, cur_top_line: f32) Point {
            return .{
                .pos = .new(if (is_gen0) 5 else 4, cur_top_line + if (is_folded) tof32(0.5) else 1.0),
                .scale = if (is_folded) 0.5 else 1,
            };
        }

        fn updateCasePositionsAndReturnMouseOverlap(mem: *VeryPermamentGameStuff, parent_address: core.CaseAddress, maybe_relative_mouse_pos: ?Vec2, group: CaseGroup, delta_seconds: f32) !?OverlapResult {
            const is_gen0 = parent_address.len == 0;
            var cur_top_line: f32 = 2;
            const unfolded = group.unfolded;

            var overlapped: ?OverlapResult = null;
            for (group.cases.items, 0..) |*case, k| {
                const is_folded: bool = k != unfolded;
                defer cur_top_line += if (is_folded) 1.5 else 2.5;
                const relative_pattern_point = relativePatternPoint(is_gen0, is_folded, cur_top_line);
                case.pattern_point_relative_to_parent.lerp_towards(relative_pattern_point, 0.6, delta_seconds);

                const cur_address = try childAddress(mem, parent_address, k);

                const maybe_local_mouse_pos = if (maybe_relative_mouse_pos) |relative_mouse_pos|
                    relative_pattern_point.inverseApplyGetLocalPosition(relative_mouse_pos)
                else
                    null;

                if (maybe_relative_mouse_pos) |relative_mouse_pos| {
                    const local_mouse_pos = maybe_local_mouse_pos.?;
                    if (try SexprView.overlapsPatternSexpr(
                        platform.gpa,
                        case.pattern,
                        relative_pattern_point,
                        relative_mouse_pos,
                    )) |local_address| {
                        overlapped = .{ .sexpr = .{
                            .case_address = cur_address,
                            .sexpr_address = local_address,
                            .which = .pattern,
                        } };
                    } else if (blk: {
                        if (is_folded) break :blk null;
                        break :blk try SexprView.overlapsSexpr(
                            platform.gpa,
                            case.template,
                            relative_pattern_point.applyToLocalPoint(.{ .pos = .new(DIST_TO_TEMPLATE, 0) }),
                            relative_mouse_pos,
                        );
                    }) |local_address| {
                        overlapped = .{ .sexpr = .{
                            .case_address = cur_address,
                            .sexpr_address = local_address,
                            .which = .template,
                        } };
                    } else if (blk: {
                        if (is_folded) break :blk null;
                        break :blk try SexprView.overlapsSexpr(
                            platform.gpa,
                            case.fnk_name,
                            relative_pattern_point.applyToLocalPoint(FNK_NAME_OFFSET),
                            relative_mouse_pos,
                        );
                    }) |local_address| {
                        overlapped = .{ .sexpr = .{
                            .case_address = cur_address,
                            .sexpr_address = local_address,
                            .which = .fnk_name,
                        } };
                    } else if (inRange(local_mouse_pos.y, -1, 1) and
                        inRange(local_mouse_pos.x, -5 / case.pattern_point_relative_to_parent.scale, 0))
                    {
                        overlapped = .{ .case = .{ .existing = cur_address } };
                    }
                }

                if (!is_folded) if (case.next) |next| {
                    const child_overlap = try updateCasePositionsAndReturnMouseOverlap(
                        mem,
                        cur_address,
                        maybe_local_mouse_pos,
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
        ) !?CaseAddressWithPoint {
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
                const below_the_cable = mouse_pos_relative_to_parent.y > 0;
                for (group.cases.items, 0..) |*case, k| {
                    const grabbing_pos_relative_to_cur = Point.inverseApplyGetLocalPosition(
                        case.pattern_point_relative_to_parent,
                        mouse_pos_relative_to_parent,
                    );
                    if (below_the_cable and grabbing_pos_relative_to_cur.y < 0 and inRange(
                        grabbing_pos_relative_to_cur.x,
                        -5.0 / case.pattern_point_relative_to_parent.scale,
                        0,
                    )) {
                        return .{
                            .address = try childAddress(mem, parent_address, k),
                            .pattern_point_relative_to_parent = case
                                .pattern_point_relative_to_parent
                                .applyToLocalPoint(.{ .scale = 1.5, .pos = .new(0, -3) }),
                        };
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
                            return .{
                                .address = try childAddress(mem, parent_address, group.cases.items.len),
                                .pattern_point_relative_to_parent = last_case
                                    .pattern_point_relative_to_parent
                                    .applyToLocalPoint(.{ .scale = 1.5, .pos = .new(0, 3) }),
                            };
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
                                return .{
                                    .address = try childAddress(mem, cur_address, 0),
                                    .pattern_point_relative_to_parent = relativePatternPoint(false, false, 2),
                                };
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

        const BASE_SPEED = 1;
        const SKIP_SPEED_MULT = 3;

        const text_pos: Point = (Point{ .pos = DEFAULT_CAM.center, .scale = 3 }).applyToLocalPoint(.{ .pos = .new(0, -1) });
        pub const result_pos: Point = text_pos.applyToLocalPoint(.{ .pos = .new(-1, 2) });
        pub const bad_fnk_pos: Point = text_pos.applyToLocalPoint(.{ .pos = .new(0, 2), .turns = -0.25, .scale = 0.5 });
        pub const expected_output_pos: Point = text_pos.applyToLocalPoint(.{ .scale = 0.25, .pos = .new(3.25, -0.75) });

        // TODO: previous step, speed controls, different step speed
        // TODO: draw the variable name on bound values

        // TODO: remove this, probably
        scoring_run: *core.ScoringRun,
        thread: core.ExecutionThread,
        camera: Camera,
        ui_state: UI.State,

        // TODO: better rewind
        thread_initial_params: struct {
            value: *const Sexpr,
            fn_name: *const Sexpr,

            pub fn toThread(self: @This(), scoring_run: *core.ScoringRun) !core.ExecutionThread {
                return try .init(self.value, self.fn_name, scoring_run);
            }

            pub fn startThreadAndRunItToStep(self: @This(), scoring_run: *core.ScoringRun, step: usize) !core.ExecutionThread {
                var thread = try self.toThread(scoring_run);
                for (0..step) |_| {
                    std.debug.assert(null == try thread.advanceTinyStep(scoring_run));
                }
                return thread;
            }
        },
        done_steps: usize = 0,

        anim_t: f32 = 0,
        anim_state: union(enum) {
            /// speed multiplier
            normal: f32,
            paused,
            /// remaining fast advance
            advancing: f32,
            backwards: ?core.ExecutionThread,
        } = .{ .normal = 1 },

        result: ?core.ExecutionThread.Result = null,
        main_input: if (DESIGN.no_current_data) PhysicalSexpr else enum { invalid_field },
        expected_output: ?*const Sexpr,

        pub fn init(
            input: if (DESIGN.no_current_data) PhysicalSexpr else *const Sexpr,
            fn_name: *const Sexpr,
            scoring_run: *core.ScoringRun,
            camera: Camera,
            expected_output: ?*const Sexpr,
        ) !Self {
            const thread_initial_params: @FieldType(Self, "thread_initial_params") = .{
                .value = if (DESIGN.no_current_data) input.value else input,
                .fn_name = fn_name,
            };
            var result = Self{
                .thread_initial_params = thread_initial_params,
                .thread = try thread_initial_params.toThread(scoring_run),
                .scoring_run = scoring_run,
                .camera = camera,
                .ui_state = .{ .buttons = try UI.Button.row(platform.gpa, .zero, .one, &.{
                    "⏹",
                    "Reset\nView",
                    "⏯",
                    "⏩",
                    "⏮",
                    "⏭",
                }) },
                .main_input = if (DESIGN.no_current_data) input else .invalid_field,
                .expected_output = expected_output,
            };

            // for now, skip the "start" anim
            // std.debug.assert(null == try result.thread.advanceTinyStep(result.scoring_run));
            const asdf = try result.thread.advanceTinyStep(result.scoring_run);
            result.result = asdf;

            return result;
        }

        pub fn update(self: *Self, delta_seconds: f32) OoM!union(enum) { nothing, back_to_editing } {
            if (self.ui_state.update(platform.getMouse(), delta_seconds)) |pressed_button|
                switch (pressed_button) {
                    0 => return .back_to_editing,
                    1 => self.camera = DEFAULT_CAM,
                    2 => self.anim_state = switch (self.anim_state) {
                        .paused => .{ .normal = 1 },
                        else => .paused,
                    },
                    3 => self.anim_state = .{ .normal = 4 },
                    4 => if (self.done_steps > 0) {
                        self.anim_state = .{ .backwards = self.thread_initial_params.startThreadAndRunItToStep(self.scoring_run, self.done_steps) catch |err| switch (err) {
                            error.OutOfMemory => |x| return x,
                            else => unreachable,
                        } };
                    },
                    5 => self.anim_state = .{ .advancing = 1.1 - self.anim_t },
                    else => return error.TODO,
                };

            // move camera
            moveCamera(&self.camera, delta_seconds, platform.getKeyboard(), platform.getMouse());

            std.log.debug("anim_t: {d}", .{self.anim_t});

            if (DESIGN.no_current_data) {
                self.main_input.pos.lerp_towards(MAIN_INPUT_POS, 0.6, delta_seconds);
                math.lerp_towards(&self.main_input.is_pattern, 0, 0.6, delta_seconds);
            }

            switch (self.anim_state) {
                .paused => {},
                .normal => |speed| {
                    self.anim_t += speed * delta_seconds * BASE_SPEED * stepSpeed(self.anim_t, self.thread.last_visual_state, self.thread.stack.items.len);
                    if (self.anim_t >= 1 and self.result != null) {
                        self.anim_t = 1;
                        self.anim_state = .paused;
                    }
                },
                .advancing => |*remaining| {
                    const advance_step_size = delta_seconds * SKIP_SPEED_MULT * BASE_SPEED * stepSpeed(self.anim_t, self.thread.last_visual_state, self.thread.stack.items.len);

                    if (remaining.* > advance_step_size) {
                        remaining.* -= advance_step_size;
                        self.anim_t += advance_step_size;
                    } else {
                        self.anim_t += remaining.*;
                        self.anim_state = .paused;
                    }
                },
                .backwards => |*asdf| {
                    if (asdf.*) |x| {
                        const advance_step_size = delta_seconds * SKIP_SPEED_MULT * BASE_SPEED * stepSpeed(self.anim_t, self.thread.last_visual_state, self.thread.stack.items.len);
                        self.anim_t -= advance_step_size;
                        if (self.anim_t < 0) {
                            self.anim_t += 1;
                            self.done_steps -= 1;
                            self.result = null;
                            self.thread = x;
                            asdf.* = null;
                        }
                    } else {
                        const advance_step_size = delta_seconds * SKIP_SPEED_MULT * BASE_SPEED * stepSpeed(self.anim_t, self.thread.last_visual_state, self.thread.stack.items.len);
                        self.anim_t -= advance_step_size;
                        if (self.anim_t < 0.1) {
                            self.anim_t = @max(0, self.anim_t);
                            self.anim_state = .paused;
                        }
                    }
                },
            }

            while (self.anim_t >= 1 and self.result == null) {
                self.anim_t -= 1;
                self.done_steps += 1;
                self.result = try self.thread.advanceTinyStep(self.scoring_run);
            }
            return .nothing;

            // if (platform.getMouse().wasPressed(.left)) {
            //     return try self.thread.advanceTinyStep(self.scoring_run);
            // } else {
            //     return null;
            // }
        }

        fn stepSpeed(anim_t: f32, state: core.ExecutionThread.VisualState, execution_stack_count: usize) f32 {
            return switch (state) {
                .just_started => @panic("TODO"),
                .ended => lerp(2, 4, anim_t),
                else => 1,
                .matched => |matched| if (matched.added_new_fnk_to_stack and anim_t > 0.5)
                    lerp(0.5, 0.8, math.smoothstep(anim_t, 0.7, 0.9))
                else if (matched.tail_optimized and execution_stack_count > 0 and anim_t > 0.5)
                    lerp(0.5, 0.8, math.smoothstep(anim_t, 0.7, 0.9))
                else
                    1,
            };
        }

        pub fn draw(self: Self) !void {
            drawer.clear(Color.gray(128));

            if (self.expected_output) |expected_result| {
                try drawExpected(expected_result);
            }

            if (self.result) |result| {
                if (self.anim_t >= 1) {
                    const camera = DEFAULT_CAM;

                    switch (result) {
                        .result => |value| {
                            drawer.drawDebugText(camera, text_pos, "Result:", .black);
                            try artist.drawSexpr(camera, result_pos, value);
                        },
                        .no_matching_case => drawer.drawDebugText(camera, text_pos, "Ran out of cases!", .black),
                        .missing_or_uncompilable_fnk => |fnk_name| {
                            drawer.drawDebugText(camera, text_pos, "Could not find\nor compile this vau:", .black);
                            try artist.drawSexpr(camera, bad_fnk_pos, fnk_name);
                        },
                        .used_undefined_variable => |asdf| {
                            drawer.drawDebugText(camera, text_pos, "Could not fill in\nall Wildcards:", .black);
                            try artist.drawSexpr(camera, result_pos, asdf.template);
                        },
                    }

                    self.ui_state.draw(drawer);
                    return;
                }
            }

            const camera = self.camera;
            var parent_point = Point{};

            // TODO: take the is_pattern into account
            const input_pos = if (DESIGN.no_current_data) self.main_input.pos else MAIN_INPUT_POS;

            // std.log.debug("cur state: {s}", .{@tagName(self.thread.last_visual_state)});
            var it = std.mem.reverseIterator(self.thread.stack.items);
            switch (self.thread.last_visual_state) {
                .just_started => {
                    // const active_stack: core.StackThing = it.next().?;
                    // try artist.drawSexpr(camera, parent_point.applyToLocalPoint(MAIN_INPUT_POS), self.thread.active_value);
                    // try artist.drawSexpr(camera, parent_point.applyToLocalPoint(MAIN_FNK_POS), active_stack.cur_fnk_name);
                    // try drawCases(true, parent_point, active_stack.cur_cases, true);
                },
                .failed_to_match => |discarded_case| {
                    const active_stack: core.StackThing = it.next().?;
                    artist.drawOffscreenCableTo(camera, MAIN_INPUT_POS);
                    // TODO: is the parent_point.apply required here?
                    try artist.drawSexpr(camera, parent_point.applyToLocalPoint(input_pos), self.thread.active_value);
                    try artist.drawHoldedFnk(camera, parent_point.applyToLocalPoint(MAIN_FNK_POS), 1, active_stack.cur_fnk_name);
                    if (self.anim_t < 0.5) {
                        const t = clamp01(remap(self.anim_t, 0, 0.4, 0, 1));
                        const t2 = clamp01(remap(self.anim_t, 0.4, 0.5, 0, 1));
                        try drawCases(
                            camera,
                            1,
                            parent_point.applyToLocalPoint(.{ .pos = .new(0, lerp(1.5, 0, t)) }),
                            active_stack.cur_cases,
                            false,
                            0,
                            .{ .anim_t = null, .new = &.{}, .old = active_stack.cur_bindings.items },
                        );
                        try drawCase(
                            camera,
                            1 - t2 * 0.5,
                            parent_point
                                .applyToLocalPoint(.{ .pos = .new(lerp(DIST_TO_TEMPLATE, 4, t2), lerp(3, 0, t)) }),
                            discarded_case,
                            true,
                            true,
                            0,
                            .{ .anim_t = null, .new = &.{}, .old = active_stack.cur_bindings.items },
                        );
                    } else {
                        const t = remap(self.anim_t, 0.5, 1, 0, 1);
                        try drawCase(camera, 0.5, parent_point
                            .applyToLocalPoint(Point.lerp(
                            .{ .pos = .new(4, 0) },
                            .{ .pos = .new(12, -4), .scale = 0, .turns = -0.65 },
                            t,
                        )), discarded_case, true, false, 0, .{ .anim_t = null, .new = &.{}, .old = active_stack.cur_bindings.items });
                        if (active_stack.cur_cases.len > 0) {
                            try drawCase(
                                camera,
                                1,
                                parent_point
                                    .applyToLocalPoint(.{ .pos = .new(5, lerp(3.5, 3, t)), .scale = lerp(0.5, 1, t) }),
                                active_stack.cur_cases[0],
                                true,
                                true,
                                0,
                                .{ .anim_t = null, .new = &.{}, .old = active_stack.cur_bindings.items },
                            );
                            try drawCases(
                                camera,
                                1,
                                parent_point.applyToLocalPoint(.{ .pos = .new(0, 1.5) }),
                                active_stack.cur_cases[1..],
                                false,
                                0,
                                .{ .anim_t = null, .new = &.{}, .old = active_stack.cur_bindings.items },
                            );
                        }
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

                        artist.drawOffscreenCableTo(camera, MAIN_INPUT_POS);
                        try artist.drawSexpr(
                            camera,
                            parent_point.applyToLocalPoint(input_pos),
                            matched.old_active_value,
                        );
                        try artist.drawHoldedFnk(camera, parent_point.applyToLocalPoint(MAIN_FNK_POS), 1, matched.old_fnk_name);

                        try drawCases(
                            camera,
                            1,
                            parent_point.applyToLocalPoint(.{ .pos = .new(0, lerp(1.5, 0, t)) }),
                            matched.discarded_cases,
                            false,
                            0,
                            .{ .anim_t = null, .new = matched.new_bindings, .old = matched.old_bindings },
                        );
                        try drawCase(
                            camera,
                            1 - t2 * 0.5,
                            parent_point
                                .applyToLocalPoint(.{ .pos = .new(lerp(DIST_TO_TEMPLATE, 4, t2), lerp(3, 0, t)) }),
                            matched.case,
                            true,
                            true,
                            0,
                            .{ .anim_t = null, .new = matched.new_bindings, .old = matched.old_bindings },
                        );
                    } else {
                        const t = remap(self.anim_t, 0.5, 1, 0, 1);

                        // TODO: draw centered
                        const t2 = clamp01(remap(self.anim_t, 0.5, 0.8, 0, 1));
                        const hiding_children_t = math.smoothstep(self.anim_t, 0.5, 0.65);
                        const dissolving_pattern_point = parent_point
                            .applyToLocalPoint(MAIN_INPUT_POS)
                            .applyToLocalPoint(.{ .scale = 1 - t2 });
                        const cable_asdf_pos = dissolving_pattern_point.applyToLocalPosition(.new(3.5, 0));
                        drawer.drawCable(
                            camera,
                            .new(-CABLE_OFFSCREEN_DIST, 0),
                            dissolving_pattern_point.applyToLocalPosition(.new(-0.5, 0)),
                            1,
                            t2 * 3,
                        );
                        try artist.drawSexpr(
                            camera,
                            dissolving_pattern_point,
                            matched.old_active_value,
                        );
                        try artist.drawPatternSexpr(
                            camera,
                            dissolving_pattern_point
                                .applyToLocalPoint(.{ .pos = .new(3, 0) }),
                            matched.case.pattern,
                        );
                        drawer.drawCable(
                            camera,
                            dissolving_pattern_point.applyToLocalPosition(.new(-1, 1)),
                            dissolving_pattern_point.applyToLocalPosition(.new(3, 1)),
                            dissolving_pattern_point.scale,
                            0,
                        );

                        const active_value_cur_pos = parent_point.applyToLocalPoint(Point.lerp(
                            .{ .pos = .new(5 + DIST_TO_TEMPLATE - 1, 0) },
                            MAIN_INPUT_POS,
                            t,
                        ));
                        artist.drawCableTo(camera, cable_asdf_pos, active_value_cur_pos);
                        try artist.drawSexprWithBindings(
                            camera,
                            active_value_cur_pos,
                            matched.case.template,
                            .{ .anim_t = t, .new = matched.new_bindings, .old = matched.old_bindings },
                        );

                        if (matched.added_new_fnk_to_stack) {
                            const active_stack: core.StackThing = it.next().?;
                            try artist.drawHoldedFnk(camera, parent_point
                                .applyToLocalPoint(Point.lerp(
                                (Point{ .pos = .new(DIST_TO_TEMPLATE - 1, 0) })
                                    .applyToLocalPoint(FNK_NAME_OFFSET),
                                MAIN_FNK_POS,
                                t,
                            )), t, active_stack.cur_fnk_name);
                            try drawCases(
                                camera,
                                1,
                                parent_point.applyToLocalPoint(.{
                                    // TODO: this anim
                                    .pos = .new(lerp(DIST_TO_TEMPLATE * 5, 0, t), 0),
                                }),
                                active_stack.cur_cases,
                                true,
                                0,
                                .{ .anim_t = null, .new = &.{}, .old = active_stack.cur_bindings.items },
                            );
                        }

                        // TODO: join the 3 prev_stack cases in 1, maybe
                        if (matched.tail_optimized) {
                            try artist.drawHoldedFnk(
                                camera,
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
                                        camera,
                                        parent_point
                                            .applyToLocalPoint(.{ .pos = .new(lerp(-DIST_BETWEEN_QUEUED_FNKS, 0, t), 0) })
                                            .applyToLocalPoint(MAIN_FNK_POS),
                                        1,
                                        prev_stack.cur_fnk_name,
                                    );
                                    try drawCases(
                                        camera,
                                        t,
                                        parent_point.applyToLocalPoint(.{
                                            .pos = .new(lerp(-1 - DIST_BETWEEN_QUEUED_FNKS, 0, t), 0),
                                        }),
                                        prev_stack.cur_cases,
                                        true,
                                        1 - hiding_children_t,
                                        .{ .anim_t = t, .new = &.{}, .old = prev_stack.cur_bindings.items },
                                        // .{ .anim_t = t, .new = matched.new_bindings, .old = matched.old_bindings },
                                    );
                                }
                            }
                        } else {
                            const prev_stack: core.StackThing = it.next().?;
                            if (matched.added_new_fnk_to_stack) {
                                defer parent_point = parent_point.applyToLocalPoint(.{ .pos = .new(-DIST_BETWEEN_QUEUED_FNKS * t2, 0) });

                                try artist.drawHoldedFnk(
                                    camera,
                                    parent_point
                                        .applyToLocalPoint(.{ .pos = .new(lerp(0, -DIST_BETWEEN_QUEUED_FNKS, t2), 0) })
                                        .applyToLocalPoint(MAIN_FNK_POS),
                                    1,
                                    prev_stack.cur_fnk_name,
                                );
                                // TODO: revise this (waiting cases should be gen0?)
                                try drawCases(
                                    camera,
                                    0,
                                    parent_point.applyToLocalPoint(.{
                                        .pos = .new(lerp(DIST_TO_TEMPLATE - 1, -1 - DIST_BETWEEN_QUEUED_FNKS, t2), 0),
                                    }),
                                    prev_stack.cur_cases,
                                    true,
                                    hiding_children_t,
                                    // TODO: revise, might be prev_stack.cur_bindings.items
                                    .{ .anim_t = t, .new = matched.new_bindings, .old = matched.old_bindings },
                                );
                            } else {
                                try artist.drawHoldedFnk(
                                    camera,
                                    parent_point
                                        .applyToLocalPoint(MAIN_FNK_POS),
                                    1,
                                    prev_stack.cur_fnk_name,
                                );

                                try drawCases(
                                    camera,
                                    t,
                                    parent_point
                                        .applyToLocalPoint(.{ .pos = .new(lerp(DIST_TO_TEMPLATE - 1, 0, t), 0) }),
                                    prev_stack.cur_cases,
                                    true,
                                    0,
                                    .{ .anim_t = t, .new = matched.new_bindings, .old = matched.old_bindings },
                                );
                            }
                        }
                    }
                },
                .ran_out_of_cases => {
                    parent_point = parent_point.applyToLocalPoint(.{ .pos = .new(-self.anim_t * 12, 0) });
                    const active_stack: core.StackThing = it.next().?;
                    artist.drawOffscreenCableTo(camera, parent_point.applyToLocalPoint(MAIN_INPUT_POS));
                    try artist.drawSexpr(camera, parent_point.applyToLocalPoint(input_pos), self.thread.active_value);
                    try artist.drawHoldedFnk(camera, parent_point.applyToLocalPoint(MAIN_FNK_POS), 1, active_stack.cur_fnk_name);
                },
                .failed_to_find_or_compile_fnk => |asdf| {
                    if (self.anim_t < 0.5) {
                        const t = clamp01(remap(self.anim_t, 0, 0.4, 0, 1));
                        const t2 = clamp01(remap(self.anim_t, 0.4, 0.5, 0, 1));

                        artist.drawOffscreenCableTo(camera, MAIN_INPUT_POS);
                        try artist.drawSexpr(
                            camera,
                            parent_point.applyToLocalPoint(MAIN_INPUT_POS),
                            asdf.old_active_value,
                        );
                        try artist.drawHoldedFnk(camera, parent_point.applyToLocalPoint(MAIN_FNK_POS), 1, asdf.old_fnk_name);

                        try drawCase(
                            camera,
                            1 - t2 * 0.5,
                            parent_point
                                .applyToLocalPoint(.{ .pos = .new(lerp(DIST_TO_TEMPLATE, 4, t2), lerp(3, 0, t)) }),
                            asdf.case,
                            true,
                            true,
                            0,
                            // TODO NOW
                            .{ .anim_t = null, .new = &.{}, .old = undefined },
                        );
                    } else {
                        const t = remap(self.anim_t, 0.5, 1, 0, 1);
                        const cam = Camera.lerp(camera, DEFAULT_CAM, t);
                        const p = Point.lerp(parent_point
                            .applyToLocalPoint(.{ .pos = .new(4, 0) })
                            .applyToLocalPoint(FNK_NAME_OFFSET), bad_fnk_pos, t);
                        try artist.drawSexpr(cam, p, asdf.case.fnk_name);
                    }
                },
                .undefined_variable => |asdf| {
                    // TODO: revisit
                    _ = it.next();
                    if (self.anim_t < 0.5) {
                        const t = clamp01(remap(self.anim_t, 0, 0.4, 0, 1));
                        const t2 = clamp01(remap(self.anim_t, 0.4, 0.5, 0, 1));

                        artist.drawOffscreenCableTo(camera, MAIN_INPUT_POS);
                        try artist.drawSexpr(
                            camera,
                            parent_point.applyToLocalPoint(MAIN_INPUT_POS),
                            asdf.old_active_value,
                        );
                        try artist.drawHoldedFnk(camera, parent_point.applyToLocalPoint(MAIN_FNK_POS), 1, asdf.old_fnk_name);

                        try drawCase(
                            camera,
                            1 - t2 * 0.5,
                            parent_point
                                .applyToLocalPoint(.{ .pos = .new(lerp(DIST_TO_TEMPLATE, 4, t2), lerp(3, 0, t)) }),
                            asdf.case,
                            true,
                            true,
                            0,
                            // TODO NOW
                            .{ .anim_t = null, .new = &.{}, .old = undefined },
                        );
                    } else {
                        const t = remap(self.anim_t, 0.5, 1, 0, 1);
                        const cam = Camera.lerp(camera, DEFAULT_CAM, t);
                        const p = Point.lerp(parent_point
                            .applyToLocalPoint(.{ .pos = .new(4, 0) })
                            .applyToLocalPoint(.{ .pos = .new(DIST_TO_TEMPLATE, 0) }), result_pos, t);
                        try artist.drawSexprWithBindings(cam, p, asdf.case.template, .{
                            .new = asdf.new_bindings,
                            .old = asdf.old_bindings,
                            .anim_t = t,
                        });
                    }
                },
                .ended => |result| {
                    const cam = Camera.lerp(camera, DEFAULT_CAM, self.anim_t);
                    const p = Point.lerp(MAIN_INPUT_POS, result_pos, self.anim_t);
                    artist.drawOffscreenCableTo(cam, p);
                    try artist.drawSexpr(cam, p, result);
                },
            }
            while (it.next()) |x| {
                parent_point = parent_point.applyToLocalPoint(.{ .pos = .new(-DIST_BETWEEN_QUEUED_FNKS, 0) });
                try artist.drawHoldedFnk(camera, parent_point.applyToLocalPoint(MAIN_FNK_POS), 1, x.cur_fnk_name);

                try drawCases(
                    camera,
                    0,
                    parent_point.applyToLocalPoint(.{ .pos = .new(-1, 0) }),
                    x.cur_cases,
                    true,
                    1,
                    .{ .anim_t = null, .new = &.{}, .old = x.cur_bindings.items },
                );
            }

            self.ui_state.draw(drawer);
        }

        // TODO: remove this duplication from EditingFnk
        fn drawCases(camera: Camera, is_gen0: f32, parent_point: Point, cases: []const core.MatchCaseDefinition, first_unfolded: bool, hiding_children: f32, bindings: BindingsState) OoM!void {
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
                try drawCase(camera, is_gen0, pattern_point, case, first_unfolded and k == 0, is_gen0 > 0.5, hiding_children, bindings);
            }
        }

        // TODO: join with_extra and constant_cable into a single struct? so constant cable can have a default value
        fn drawCase(
            camera: Camera,
            is_gen0: f32,
            pattern_point: Point,
            case: core.MatchCaseDefinition,
            with_extra: bool,
            constant_cable: bool,
            hiding_children: f32,
            bindings: BindingsState,
        ) OoM!void {
            try artist.drawPatternSexpr(
                camera,
                pattern_point,
                case.pattern,
            );
            if (with_extra) {
                try drawCaseExtra(camera, pattern_point.applyToLocalPoint(.{ .scale = 1 - hiding_children }), case, bindings);
            }

            const cable_from = pattern_point.applyToLocalPosition(.new((lerp(-3, -5, is_gen0)) / if (constant_cable) pattern_point.scale else 1, 1));
            const cable_to = pattern_point.applyToLocalPosition(.new(0, 1));
            drawer.drawCable(
                camera,
                cable_from,
                cable_to,
                if (constant_cable) 1 else pattern_point.scale,
                0,
            );
        }

        // TODO: remove duplication with EditingCase
        fn drawCaseExtra(camera: Camera, pattern_point: Point, case: core.MatchCaseDefinition, bindings: BindingsState) !void {
            try artist.drawSexprWithBindings(
                camera,
                pattern_point.applyToLocalPoint(.{ .pos = .new(DIST_TO_TEMPLATE, 0) }),
                case.template,
                bindings,
            );
            try artist.drawHoldedFnk(camera, pattern_point.applyToLocalPoint(FNK_NAME_OFFSET), 0, case.fnk_name);
            drawer.drawCable(
                camera,
                pattern_point.applyToLocalPosition(.new(0.5, 0)),
                pattern_point.applyToLocalPosition(.new(DIST_TO_TEMPLATE - 0.5, 0)),
                pattern_point.scale,
                0,
            );
            // TODO: draw the bound values travelling on the wire
            try artist.drawPlacedWildcardsCable(
                camera,
                pattern_point,
                pattern_point.applyToLocalPoint(.{ .pos = .new(DIST_TO_TEMPLATE, 0) }),
                case.pattern,
                case.template,
            );
            if (case.next) |next| {
                try drawCases(camera, 0, pattern_point, next.items, true, 0, bindings);
            }
        }

        fn drawExpected(expected: *const Sexpr) !void {
            const camera = DEFAULT_CAM;
            drawer.drawDebugText(camera, expected_output_pos.applyToLocalPoint(.{ .pos = .new(1, -2) }), "Expected\nresult:", .black);
            try artist.drawSexpr(camera, expected_output_pos, expected);
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
        .getKeyboard = struct {
            pub fn anon() Keyboard {
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

        pub fn update(self: *State, mouse: Mouse, delta_seconds: f32) ?usize {
            self.hot = null;
            var result: ?usize = null;

            for (self.buttons, 0..) |button, k| {
                if (!button.enabled) continue;

                if (button.pos.contains(mouse.cur.pos(UI.cam))) {
                    self.hot = k;
                    if (self.active == null and mouse.cur.isDown(.left)) {
                        self.active = k;
                    }
                }

                if (self.isActive(k) and self.isHot(k) and !mouse.cur.isDown(.left)) {
                    result = k;
                }
            }
            if (!mouse.cur.isDown(.left)) {
                if (self.active) |_| {
                    self.active = null;
                }
            } else if (self.active == null) {
                // TODO: better
                self.active = self.buttons.len + 999;
            }
            for (self.buttons, 0..) |*button, k| {
                math.lerp_towards(
                    &button.hot_t,
                    if (self.isHot(k)) 1 else 0,
                    0.6,
                    delta_seconds,
                );
                math.lerp_towards(
                    &button.active_t,
                    if (self.isActive(k)) 1 else 0,
                    0.6,
                    delta_seconds,
                );
            }
            return result;
        }

        pub fn draw(self: State, comptime drawer: Drawer) void {
            for (self.buttons) |button| {
                if (!button.enabled) drawer.setTransparency(0.5);
                defer if (!button.enabled) drawer.setTransparency(1);

                drawer.drawRect(UI.cam, button.pos.plusMargin(
                    clamp01(button.hot_t - button.active_t) * 0.1,
                ), .black, .white);
                if (button.text) |text| {
                    drawer.drawDebugText(UI.cam, .{
                        .pos = button.pos.getCenter(),
                        .scale = button.pos.size.y / (1.5 + tof32(std.mem.count(u8, text, "\n"))),
                    }, text, Color.black);
                }
            }
        }
    };

    pub const Button = struct {
        pos: Rect,
        hot_t: f32 = 0,
        active_t: f32 = 0,
        text: ?[:0]const u8 = null,
        enabled: bool = true,

        pub fn row(alloc: std.mem.Allocator, top_left: Vec2, size: Vec2, texts: []const ?[:0]const u8) ![]Button {
            const result: []Button = try alloc.alloc(Button, texts.len);
            for (texts, result, 0..) |text, *target, k| {
                target.* = .{ .pos = Rect{ .top_left = top_left.addX(size.x * tof32(k)), .size = size }, .text = text };
            }
            return result;
        }
    };
};

pub fn LevelSelect(platform: Platform, drawer: Drawer) type {
    const artist = Artist(platform, drawer);
    return struct {
        const Self = @This();

        level_select_buttons: UI.State,
        play_level_button: UI.State,
        selected_level: ?usize = null,
        persistence: *const PlayerData,

        pub fn init(persistence: *const PlayerData) !Self {
            const res = platform.gpa.alloc(UI.Button, builtin_levels.len) catch unreachable;
            for (res, 0..) |*b, k| {
                b.* = .{
                    .pos = Rect{ .top_left = .new(2, 2.5 + 2.5 * @as(f32, @floatFromInt(k))), .size = .one },
                    .enabled = switch (k) {
                        0 => true,
                        1 => persistence.is_builtin_level_solved[0],
                        2 => persistence.is_builtin_level_solved[1],
                        3 => persistence.is_builtin_level_solved[2],
                        4 => persistence.is_builtin_level_solved[3],
                        else => return error.TODO,
                    },
                };
            }
            return Self{
                .level_select_buttons = .{ .buttons = res },
                .play_level_button = .{ .buttons = try platform.gpa.dupe(UI.Button, &.{
                    .{ .pos = Rect{ .top_left = .new(10, 10), .size = .new(2, 1) }, .text = "Play" },
                }) },
                .persistence = persistence,
            };
        }

        pub fn update(self: *Self, delta_seconds: f32) ?usize {
            const mouse = platform.getMouse();
            if (self.level_select_buttons.update(mouse, delta_seconds)) |pressed| {
                self.selected_level = pressed;
            }
            if (self.selected_level) |selected| {
                if (self.play_level_button.update(mouse, delta_seconds) != null) {
                    return selected;
                }
            }
            return null;
        }

        pub fn draw(self: Self) OoM!void {
            drawer.clear(Color.gray(128));
            for (self.level_select_buttons.buttons, 0..) |button, k| {
                if (!button.enabled) drawer.setTransparency(0.5);
                defer if (!button.enabled) drawer.setTransparency(1);

                if (self.persistence.is_builtin_level_solved[k]) {
                    drawer.drawRect(UI.cam, button.pos.plusMargin(0.2), Color.fromHex("#55ff55"), null);
                }
                if (k == self.selected_level) {
                    drawer.drawRect(UI.cam, button.pos.plusMargin(0.4), .black, null);
                } else if (button.hot_t > 0) {
                    drawer.drawRect(UI.cam, button.pos.plusMargin(button.hot_t - 0.5 - 0.1 * button.active_t), .black, null);
                }
                try artist.drawSexpr(UI.cam, .{
                    .pos = button.pos.top_left.add(.new(0.5, 1)),
                    .turns = -0.25,
                    .scale = 0.5,
                }, builtin_levels[k].fnk_name);
            }

            if (self.selected_level) |selected| {
                const level = builtin_levels[selected];
                drawer.drawDebugText(UI.cam, .{ .pos = UI.cam.center }, level.description, .black);
                self.play_level_button.draw(drawer);
            } else {
                drawer.drawDebugText(
                    UI.cam,
                    .{ .pos = UI.cam.center },
                    if (!self.persistence.is_builtin_level_solved[0])
                        \\Welcome to the Vaulogy lab! 
                        \\Vaus are simple machines for transforming Data into Data.
                        \\Your goal is to fill in all these Vaus.
                    else if (!self.persistence.is_builtin_level_solved[1])
                        \\Good job! On to the next one...
                    else if (!self.persistence.is_builtin_level_solved[2])
                        \\Vaus can be combined. For the next one,
                        \\you will reuse the first one.
                    else if (!self.persistence.is_builtin_level_solved[3])
                        \\You can also call a Vau on the result of another one
                    else
                        "",
                    .black,
                );
            }
        }

        pub fn getLevelButtonPoint(self: Self, level_index: usize) Point {
            return .{
                .pos = self.level_select_buttons.buttons[level_index].pos.top_left.add(.new(0.5, 1)),
                .turns = -0.25,
                .scale = 0.5,
            };
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
