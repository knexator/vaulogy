const std = @import("std");

pub const std_options = std.Options{
    // wasm-freestanding has no stderr, so we have to override this function
    .logFn = myLogFn,
};
fn myLogFn(
    comptime message_level: std.log.Level,
    comptime scope: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    const level_txt = comptime message_level.asText();
    const prefix2 = if (scope == .default) ": " else "(" ++ @tagName(scope) ++ "): ";

    var buf: [1000]u8 = undefined;
    const res = std.fmt.bufPrint(&buf, level_txt ++ prefix2 ++ format ++ "\n", args) catch {
        js_better.debug.logString("RAN OUT OF LOG BUFFER! the log started with:\n");
        js_better.debug.logString(&buf);
        return;
    };
    js_better.debug.logString(res);
}

const js = struct {
    pub const debug = struct {
        extern fn logInt(arg: u32) void;
        extern fn logFloat(arg: f32) void;
        extern fn logString(ptr: [*]const u8, len: usize) void;
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

    pub const storage = struct {
        extern fn itemSize(key_ptr: [*]const u8, key_len: usize) usize;
        extern fn getItem(key_ptr: [*]const u8, key_len: usize, dst_ptr: [*]u8) usize;
        extern fn setItem(key_ptr: [*]const u8, key_len: usize, value_ptr: [*]const u8, value_len: usize) void;
    };
};

const js_better = struct {
    pub const debug = struct {
        pub fn logString(s: []const u8) void {
            js.debug.logString(s.ptr, s.len);
        }
    };

    pub const storage = struct {
        pub fn getItemBuf(key: []const u8, dst: []u8) !?[]const u8 {
            const len = js.storage.getItem(key.ptr, key.len, dst.ptr);
            if (len == 0) return null;
            if (len > dst.len) return error.BufferTooSmall;
            return dst[0..len];
        }

        pub fn getItemAlloc(key: []const u8, alloc: std.mem.Allocator) !?[]const u8 {
            const size = itemSize(key);
            const res = try alloc.alloc(u8, size);
            return (getItemBuf(key, res) catch unreachable);
        }

        pub fn itemSize(key: []const u8) usize {
            return js.storage.itemSize(key.ptr, key.len);
        }

        pub fn setItem(key: []const u8, value: []const u8) void {
            js.storage.setItem(key.ptr, key.len, value.ptr, value.len);
        }
    };

    pub const canvas = struct {
        pub fn getSize() Vec2 {
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
            setFillColor(color);
            const size = getSize();
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
};

const model = @import("main.zig");
const presenter = @import("presenter.zig");

const WebPlatform = struct {
    pub fn getPlayerData(mem: *model.VeryPermamentGameStuff) !?presenter.PlayerData {
        const maybe_ascii = try js_better.storage.getItemAlloc("vaulogy_player_data", mem.gpa);
        if (maybe_ascii) |ascii| {
            defer mem.gpa.free(ascii);
            return presenter.PlayerData.fromAscii(ascii, mem) catch |err| switch (err) {
                error.OutOfMemory => |e| return e,
                else => null,
            };
        } else {
            return null;
        }
    }

    pub fn setPlayerData(player_data: presenter.PlayerData, mem: *model.VeryPermamentGameStuff) !void {
        const ascii = try player_data.toAscii(mem.gpa);
        defer mem.gpa.free(ascii);
        js_better.storage.setItem("vaulogy_player_data", ascii);
    }
};

// TODO: delete this
pub const CameraOld = struct {
    // an object at [camera.topleft] will be drawn on the top left of the screen
    // an object at [camera.topleft.addX(1) will be drawn 'asdf' pixels to the right of that

    topleft: Vec2,
    // how many pixels in a world unit
    asdf: f32,

    pub fn fromStuff(screen_side: f32, original_world: Point, target_screen_relative: Point) CameraOld {
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

    pub fn screenFromWorld(this: CameraOld, world_point: Point) Point {
        return .{
            .pos = this.screenFromWorldPosition(world_point.pos),
            .scale = this.screenFromWorldScale(world_point.scale),
            .turns = world_point.turns,
        };
    }

    pub fn screenFromWorldPosition(this: CameraOld, world_pos: Vec2) Vec2 {
        return world_pos.sub(this.topleft).scale(this.asdf);
    }

    pub fn screenFromWorldScale(this: CameraOld, world_scale: f32) f32 {
        return this.asdf * world_scale;
    }

    pub fn worldFromScreen(this: CameraOld, screen_point: Point) Point {
        return .{
            .pos = screen_point.pos.scale(1 / this.asdf).add(this.topleft),
            .scale = screen_point.scale / this.asdf,
            .turns = screen_point.turns,
        };
    }

    test "basic camera" {
        const camera = CameraOld{ .topleft = .new(2, 3), .asdf = 100 };
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

const Camera = presenter.Camera;
const Point = presenter.Point;
const Vec2 = presenter.Vec2;
const Color = presenter.Color;
const WebDrawer = struct {
    fn screenFromWorld(camera: Camera, world_point: Point) Point {
        const camera_old = CameraOld.fromStuff(
            js_better.canvas.getSize().y,
            .{ .pos = camera.center, .scale = 1 },
            .{ .pos = .half, .scale = 1.0 / camera.height },
        );
        return camera_old.screenFromWorld(world_point);
    }

    pub fn drawAtomDebug(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);
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
        js_better.canvas.pathLoop(&screen_positions);
        js.canvas.setLineWidth(1);
        js_better.canvas.setFillColor(Color.white);
        js_better.canvas.setStrokeColor(Color.black);
        js.canvas.fill();
        js.canvas.stroke();
    }
};

var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
const web_platform = presenter.Platform{
    .gpa = gpa.allocator(),
    .getPlayerData = WebPlatform.getPlayerData,
    .setPlayerData = WebPlatform.setPlayerData,
};
const web_drawer = presenter.Drawer{
    .clear = js_better.canvas.clear,
    .drawAtomDebug = WebDrawer.drawAtomDebug,
};

var game: presenter.Presenter(web_platform, web_drawer) = undefined;

export fn init() void {
    game = @TypeOf(game).init() catch {
        std.log.err("bad start", .{});
        programmerError();
    };
}

const KeyCode = @import("./tools/generate_keycodes_js.zig").KeyCode;
export fn keydown(code: KeyCode) void {
    _ = code;
}

export fn frame(delta_seconds: f32) void {
    game.update(delta_seconds);
}

export fn draw() void {
    game.draw();
}

fn programmerError() noreturn {
    js.debug.logInt(666);
    std.debug.panic("programmer error!", .{});
}
