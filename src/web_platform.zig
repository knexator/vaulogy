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

        pub fn pathLoop(all_positions: []const Vec2) void {
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

    pub fn getMouse() presenter.Mouse {
        return mouse;
    }
};

const Camera = presenter.Camera;
const Point = presenter.Point;
const Vec2 = presenter.Vec2;
const Color = presenter.Color;
const Rect = presenter.Rect;
const WebDrawer = struct {
    fn screenFromWorld(camera: Camera, world_point: Point) Point {
        const rect = camera.toRect();
        const local = Point.inverseApplyGetLocal(Point{
            .pos = rect.top_left,
            .scale = rect.size.y,
        }, world_point);
        const screen = Point{ .pos = .zero, .scale = js_better.canvas.getSize().y };
        return screen.applyToLocalPoint(local);
    }

    fn screenFromWorldPosition(camera: Camera, world_position: Vec2) Vec2 {
        return screenFromWorld(camera, .{ .pos = world_position }).pos;
    }

    fn screenFromWorldScale(camera: Camera, world_scale: f32) f32 {
        return screenFromWorld(camera, .{ .scale = world_scale }).scale;
    }

    fn screenFromWorldSize(camera: Camera, world_size: Vec2) Vec2 {
        return Vec2.new(
            screenFromWorldScale(camera, world_size.x),
            screenFromWorldScale(camera, world_size.y),
        );
    }

    pub fn drawRect(camera: Camera, rect: Rect) void {
        const screen_top_left = screenFromWorldPosition(camera, rect.top_left);
        const screen_size = screenFromWorldSize(camera, rect.size);
        const screen_positions = [_]Vec2{
            screen_top_left,
            screen_top_left.addX(screen_size.x),
            screen_top_left.add(screen_size),
            screen_top_left.addY(screen_size.y),
        };
        js_better.canvas.pathLoop(&screen_positions);
        js.canvas.setLineWidth(1);
        js_better.canvas.setFillColor(Color.white);
        js_better.canvas.setStrokeColor(Color.black);
        js.canvas.fill();
        js.canvas.stroke();
    }

    pub fn drawAtomDebug(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);
        const local_positions = [_]Vec2{
            Vec2.new(-0.5, 0),
            Vec2.new(0, 1),
            Vec2.new(2, 1),
            Vec2.new(2.2, 1.0 / 3.0),
            Vec2.new(1.8, -1.0 / 3.0),
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

    pub fn drawAtom(camera: Camera, world_point: Point, visuals: presenter.AtomVisuals) void {
        const profile = visuals.profile;
        const screen_point = screenFromWorld(camera, world_point);
        const local_positions = [_]Vec2{
            Vec2.new(2, -1),
            Vec2.new(0, -1),
            Vec2.new(-0.5, 0),
            Vec2.new(0, 1),
            Vec2.new(2, 1),
        };
        // TODO: no allocations
        var screen_positions: []Vec2 = gpa.allocator().alloc(Vec2, local_positions.len + profile.len * 2) catch @panic("TODO");
        defer gpa.allocator().free(screen_positions);
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        for (profile, 0..) |pos, i| {
            screen_positions[local_positions.len + i] = screen_point.applyToLocalPosition(
                Vec2.new(2.0 - pos.y, 1.0 - pos.x),
            );
            screen_positions[local_positions.len + profile.len * 2 - i - 1] = screen_point.applyToLocalPosition(
                Vec2.new(2.0 + pos.y, -1.0 + pos.x),
            );
        }
        js_better.canvas.pathLoop(screen_positions);
        js.canvas.setLineWidth(1);
        js_better.canvas.setFillColor(visuals.color);
        js_better.canvas.setStrokeColor(Color.black);
        js.canvas.fill();
        js.canvas.stroke();
    }

    pub fn drawAtomPatternDebug(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);
        const local_positions = [_]Vec2{
            Vec2.new(0.5, 0),
            Vec2.new(0, 1),
            Vec2.new(-1, 1),
            Vec2.new(-0.8, 1.0 / 3.0),
            Vec2.new(-1.2, -1.0 / 3.0),
            Vec2.new(-1, -1),
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

    pub fn drawCable(camera: Camera, world_from: Vec2, world_to: Vec2, world_scale: f32, offset: f32) void {
        const screen_from = screenFromWorldPosition(camera, world_from);
        const screen_to = screenFromWorldPosition(camera, world_to);
        const scale = screenFromWorldScale(camera, world_scale);
        js_better.canvas.setStrokeColor(Color.black);
        js.canvas.setLineWidth(1);
        js.canvas.beginPath();
        js_better.canvas.moveTo(screen_from);
        js_better.canvas.lineTo(screen_to);
        js.canvas.stroke();

        js.canvas.setLineWidth(scale * 0.02);
        js.canvas.beginPath();
        const delta = screen_to.sub(screen_from);
        const length = delta.mag();
        const dir = delta.scale(1 / length);
        var done: f32 = 0;
        js_better.canvas.moveTo(screen_from);
        while (done < length) : (done += 1) {
            js_better.canvas.lineTo(screen_from.add(dir.scale(done)).add(dir.perpCW().scale(cableOffset(done + offset * scale, scale))));
        }
        js_better.canvas.lineTo(screen_to);
        js.canvas.stroke();
    }

    fn cableOffset(x: f32, scale: f32) f32 {
        const z = x * 20 / scale;
        const y = @sin(z) + 0.2 * @sin(z * 1.3) + 0.3 * @sin(z * 3.1);
        return y * 0.1 * scale;
    }
};

var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
const web_platform = presenter.Platform{
    .gpa = gpa.allocator(),
    .getPlayerData = WebPlatform.getPlayerData,
    .setPlayerData = WebPlatform.setPlayerData,
    .getMouse = WebPlatform.getMouse,
};
const web_drawer = presenter.Drawer{
    .clear = js_better.canvas.clear,
    .drawRect = WebDrawer.drawRect,
    .drawAtom = WebDrawer.drawAtom,
    .drawAtomDebug = WebDrawer.drawAtomDebug,
    .drawAtomPatternDebug = WebDrawer.drawAtomPatternDebug,
    .drawCable = WebDrawer.drawCable,
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
    if (code == .KeyA) paused = !paused;
}

var paused = false;
export fn frame(delta_seconds: f32) void {
    if (paused) return;
    game.update(delta_seconds);
    mouse.prev = mouse.cur;
}

export fn draw() void {
    game.draw() catch OoM();
}

const MouseState = presenter.MouseState;
var mouse = presenter.Mouse{ .cur = .init, .prev = .init };

export fn pointermove(x: f32, y: f32) void {
    mouse.cur.clientX = x / js_better.canvas.getSize().y;
    mouse.cur.clientY = y / js_better.canvas.getSize().y;
}

const MouseButton = enum(u8) {
    left = 0,
    middle = 1,
    right = 2,
    _,
};

export fn pointerup(button: MouseButton) void {
    switch (button) {
        .left => mouse.cur.buttons.left = false,
        .middle => mouse.cur.buttons.middle = false,
        .right => mouse.cur.buttons.right = false,
        _ => {},
    }
}

export fn pointerdown(button: MouseButton) void {
    switch (button) {
        .left => mouse.cur.buttons.left = true,
        .middle => mouse.cur.buttons.middle = true,
        .right => mouse.cur.buttons.right = true,
        _ => {},
    }
}

fn programmerError() noreturn {
    js.debug.logInt(666);
    std.debug.panic("programmer error!", .{});
}

fn OoM() noreturn {
    js.debug.logInt(321);
    std.debug.panic("OoM!", .{});
}
