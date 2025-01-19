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

var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
const web_platform = presenter.Platform{
    .gpa = gpa.allocator(),
    .getPlayerData = WebPlatform.getPlayerData,
    .setPlayerData = WebPlatform.setPlayerData,
};

var game: presenter.Presenter(web_platform) = undefined;

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
    _ = delta_seconds;
}

export fn draw() void {}

fn programmerError() noreturn {
    js.debug.logInt(666);
    std.debug.panic("programmer error!", .{});
}
