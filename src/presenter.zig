//! This should be unchanged regardless of platform

const std = @import("std");

const core = @import("main.zig");
const Atom = core.Atom;
const Pair = core.Pair;
const Sexpr = core.Sexpr;
const Fnk = core.Fnk;
const FnkCollection = core.FnkCollection;
const VeryPermamentGameStuff = core.VeryPermamentGameStuff;
const parsing = @import("parsing.zig");

const OoM = error{OutOfMemory};

pub const Platform = struct {
    gpa: std.mem.Allocator,
    getPlayerData: fn (mem: *VeryPermamentGameStuff) OoM!?PlayerData,
    setPlayerData: fn (player_data: PlayerData, mem: *VeryPermamentGameStuff) OoM!void,
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

    test "inverse apply" {
        const parent: Point = .{ .pos = .zero, .scale = 2, .turns = 0.25 };
        const local: Point = .{ .pos = .e1 };
        const applied = parent.applyToLocalPoint(local);
        try expectApproxEqAbs(.{ .pos = .new(0, 2), .scale = 2, .turns = 0.25 }, applied, 0.0001);
        try expectApproxEqAbs(parent, applied.inverseApplyToLocalPoint(local), 0.0001);
    }
};

pub const Camera = struct {
    center: Vec2,
    /// how many world units fit between the top and bottom of the camera view
    height: f32,
};

pub const Drawer = struct {
    clear: fn (color: Color) void,
    drawAtomDebug: fn (camera: Camera, world_point: Point) void,
    // drawAtomPatternDebug: fn (camera: Camera, world_point: Point) void,
    // drawCable: fn (camera: Camera, world_from: Vec2, world_to: Vec2, world_scale: f32, offset: f32) void,
};

/// The full game, from loading screen to end credits
pub fn Presenter(platform: Platform, drawer: Drawer) type {
    return struct {
        const Self = @This();

        mem: VeryPermamentGameStuff,
        persistence: PlayerData,

        state: union(enum) {
            intro: IntroSequence,
        },

        const IntroSequence = struct {
            t: f32,
        };

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

            const result = Self{
                .mem = mem,
                .persistence = player_data,
                .state = .{
                    .intro = .{
                        .t = 0.0,
                    },
                },
            };
            // result.openLevel();
            // platform.showMenu()
            return result;
        }

        pub fn update(self: *Self, delta_seconds: f32) void {
            switch (self.state) {
                .intro => |*intro| {
                    intro.t += delta_seconds;
                },
            }
        }

        pub fn draw(self: Self) void {
            switch (self.state) {
                .intro => |intro| {
                    drawer.clear(Color.gray(128));
                    drawer.drawAtomDebug(
                        Camera{
                            .center = .zero,
                            .height = std.math.lerp(20, 4, std.math.clamp(intro.t / 2.0, 0.0, 1.0)),
                        },
                        Point{ .pos = .zero, .scale = 1, .turns = 0 },
                    );
                },
            }
        }
    };
}
