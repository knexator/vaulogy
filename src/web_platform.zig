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

const JsReader = struct {
    file_index: usize,

    pub const Error = error{FileNotReady};
    pub fn read(self: *JsReader, buf: []u8) Error!usize {
        if (buf.len == 0) return 0;
        if (!js.reader.isLoaded(self.file_index)) return error.FileNotReady;
        const bytes_readed = js.reader.readInto(self.file_index, buf.ptr, buf.len);
        return bytes_readed;
    }

    pub fn reader(self: *JsReader) std.io.Reader(*JsReader, Error, read) {
        return .{ .context = self };
    }

    // return reader.reader().readAllAlloc(alloc, std.math.maxInt(usize)) catch |err| switch (err) {
    //     error.OutOfMemory => return error.OutOfMemory,
    //     error.StreamTooLong => unreachable,
    // };
    // pub fn asyncFile(self: JsReader) presenter.Platform.AsyncAnyReader {
    //     // TODO: mem leak

    //     return presenter.Platform.AsyncFile{
    //         .reader = self.reader(),
    //         .file_index = self.file_index,
    //     };
    // }
};

const js = struct {
    pub const debug = struct {
        extern fn logInt(arg: u32) void;
        extern fn logFloat(arg: f32) void;
        extern fn logString(ptr: [*]const u8, len: usize) void;
    };

    pub const reader = struct {
        extern fn isLoaded(file_index: usize) bool;
        /// returns bytes readed
        extern fn readInto(file_index: usize, dst_ptr: [*]u8, dst_len: usize) usize;
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
        extern fn fillText(text_ptr: [*]const u8, text_len: usize, x: f32, y: f32, h: f32) void;
        extern fn getWidth() u32;
        extern fn getHeight() u32;
        extern fn save() void;
        extern fn restore() void;
        extern fn clip() void;
        extern fn translate(x: f32, y: f32) void;
        extern fn resetTransform() void;

        // TODO: save/restore, rotate/scale, rect
    };

    pub const storage = struct {
        extern fn itemSize(key_ptr: [*]const u8, key_len: usize) usize;
        extern fn getItem(key_ptr: [*]const u8, key_len: usize, dst_ptr: [*]u8) usize;
        extern fn setItem(key_ptr: [*]const u8, key_len: usize, value_ptr: [*]const u8, value_len: usize) void;
        extern fn downloadData(filename_ptr: [*]const u8, filename_len: usize, mime_ptr: [*]const u8, mime_len: usize, contents_ptr: [*]const u8, contents_len: usize) void;
        /// returns index of reader
        extern fn uploadData() usize;
    };

    extern fn setCursor(cursor: presenter.Platform.Cursor) void;
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

        pub fn downloadData(filename: []const u8, mime: enum { txt }, contents: []const u8) void {
            const mime_str = switch (mime) {
                .txt => "text/plain",
            };
            js.storage.downloadData(filename.ptr, mime_str.len, mime_str.ptr, contents.len, contents.ptr, contents.len);
        }

        pub fn uploadData() JsReader {
            const asdf = js.storage.uploadData();
            std.log.debug("asdf: {d}", .{asdf});
            return JsReader{ .file_index = asdf };
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

        pub fn path(all_positions: []const Vec2) void {
            if (all_positions.len < 2) programmerError();
            moveTo(all_positions[0]);
            for (all_positions[1..]) |pos| {
                lineTo(pos);
            }
        }

        pub fn circle(center: Vec2, radius: f32) void {
            js.canvas.arc(center.x, center.y, radius, 0, std.math.tau, false);
        }
    };
};

const kommon = @import("kommon");
const funk = kommon.funktional;
const math = kommon.math;
const model = @import("main.zig");
const presenter = @import("presenter.zig");
const DESIGN = presenter.DESIGN;

const WebPlatform = struct {
    pub fn getPlayerData(mem: *model.VeryPermamentGameStuff) !?presenter.PlayerData {
        if (try js_better.storage.getItemAlloc("vaulogy_player_data_full", mem.gpa)) |ascii| {
            defer mem.gpa.free(ascii);
            return presenter.PlayerData.fromAsciiNew(ascii, mem) catch |err| switch (err) {
                error.OutOfMemory => |e| return e,
                else => null,
            };
        } else
        // legacy
        if (try js_better.storage.getItemAlloc("vaulogy_player_data", mem.gpa)) |ascii| {
            defer mem.gpa.free(ascii);
            return presenter.PlayerData.fromAscii(
                ascii,
                try js_better.storage.getItemAlloc("vaulogy_player_data_custom_samples", mem.gpa) orelse "",
                try js_better.storage.getItemAlloc("vaulogy_player_data_fav_fnks", mem.gpa) orelse "",
                mem,
            ) catch |err| switch (err) {
                error.OutOfMemory => |e| return e,
                else => null,
            };
        } else {
            return null;
        }
    }

    pub fn setPlayerData(player_data: presenter.PlayerData, mem: *model.VeryPermamentGameStuff) !void {
        const ascii = try player_data.toAsciiNew(mem.gpa);
        defer mem.gpa.free(ascii);
        js_better.storage.setItem("vaulogy_player_data_full", ascii);
    }

    pub fn downloadPlayerData(player_data: presenter.PlayerData, alloc: std.mem.Allocator) !void {
        const ascii = try player_data.toAsciiNew(alloc);
        defer alloc.free(ascii);
        js_better.storage.downloadData("vaulogy_save.txt", .txt, ascii);
    }

    pub fn uploadPlayerData() std.io.AnyReader {
        // TODO: memory leak
        const leaked_reader = gpa.allocator().create(JsReader) catch @panic("OoM");
        leaked_reader.* = js_better.storage.uploadData();
        return leaked_reader.reader().any();
    }

    pub fn getMouse() presenter.Mouse {
        return mouse;
    }

    pub fn getKeyboard() presenter.Keyboard {
        return keyboard;
    }

    pub fn setCursor(cursor: presenter.Platform.Cursor) void {
        js.setCursor(cursor);
    }
};

const Camera = presenter.Camera;
const Point = presenter.Point;
const Vec2 = presenter.Vec2;
const Color = presenter.Color;
const Rect = presenter.Rect;
const optimization_dont_draw_tiny = true;
const WebDrawer = struct {
    pub fn asdfBackground() void {}

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

    pub fn setTransparency(alpha: f32) void {
        js.canvas.setGlobalAlpha(alpha);
    }

    pub fn clipAtomRegion(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);
        const local_positions = if (DESIGN.round_data)
            funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.75, 0.25, math.tof32(k) / 32)).addX(0.5);
                }
            }.anon) ++ [2]Vec2{ .new(2.3, 1), .new(2.3, -1) }
        else
            [_]Vec2{
                Vec2.new(-0.5, 0),
                Vec2.new(0, 1),
                Vec2.new(2.3, 1),
                Vec2.new(2.3, -1),
                Vec2.new(0, -1),
            };
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        js.canvas.save();
        js_better.canvas.pathLoop(&screen_positions);
        js.canvas.clip();
    }

    pub fn endClip() void {
        js.canvas.restore();
    }

    pub fn drawLine(camera: Camera, points: []const Vec2, color: Color) void {
        const screen_positions = gpa.allocator().alloc(Vec2, points.len) catch @panic("OoM");
        defer gpa.allocator().free(screen_positions);

        for (points, screen_positions) |world_pos, *screen_pos| {
            screen_pos.* = screenFromWorldPosition(camera, world_pos);
        }

        js.canvas.beginPath();
        js_better.canvas.setStrokeColor(color);
        js_better.canvas.path(screen_positions);
        js.canvas.stroke();
    }

    pub fn drawShapeV2(camera: Camera, parent_world_point: Point, local_points: []const Vec2, stroke: ?Color, fill: ?Color) void {
        const screen_positions = gpa.allocator().alloc(Vec2, local_points.len) catch @panic("OoM");
        defer gpa.allocator().free(screen_positions);

        for (local_points, screen_positions) |local_pos, *screen_pos| {
            screen_pos.* = screenFromWorldPosition(camera, parent_world_point.applyToLocalPosition(local_pos));
        }

        js_better.canvas.pathLoop(screen_positions);

        if (fill) |col| {
            js_better.canvas.setFillColor(col);
            js.canvas.fill();
        }
        if (stroke) |col| {
            js.canvas.setLineWidth(1);
            js_better.canvas.setStrokeColor(col);
            js.canvas.stroke();
        }
    }

    pub fn drawShape(camera: Camera, points: []const Vec2, stroke: ?Color, fill: ?Color) void {
        const screen_positions = gpa.allocator().alloc(Vec2, points.len) catch @panic("OoM");
        defer gpa.allocator().free(screen_positions);

        for (points, screen_positions) |world_pos, *screen_pos| {
            screen_pos.* = screenFromWorldPosition(camera, world_pos);
        }

        js_better.canvas.pathLoop(screen_positions);

        if (stroke) |col| {
            js.canvas.setLineWidth(1);
            js_better.canvas.setStrokeColor(col);
            js.canvas.stroke();
        }
        if (fill) |col| {
            js_better.canvas.setFillColor(col);
            js.canvas.fill();
        }
    }

    pub fn drawRect(camera: Camera, rect: Rect, stroke: ?Color, fill: ?Color) void {
        const screen_top_left = screenFromWorldPosition(camera, rect.top_left);
        const screen_size = screenFromWorldSize(camera, rect.size);
        const screen_positions = [_]Vec2{
            screen_top_left,
            screen_top_left.addX(screen_size.x),
            screen_top_left.add(screen_size),
            screen_top_left.addY(screen_size.y),
        };
        js_better.canvas.pathLoop(&screen_positions);
        if (stroke) |col| {
            js.canvas.setLineWidth(1);
            js_better.canvas.setStrokeColor(col);
            js.canvas.stroke();
        }
        if (fill) |col| {
            js_better.canvas.setFillColor(col);
            js.canvas.fill();
        }
    }

    pub fn drawDebugText(camera: Camera, center: Point, text: [:0]const u8, color: Color) void {
        const screen_point = screenFromWorld(camera, center);
        js_better.canvas.setFillColor(color);
        // std.log.debug("scale: {d}", .{screen_point.scale});
        js.canvas.fillText(text.ptr, text.len, screen_point.pos.x, screen_point.pos.y, screen_point.scale * 0.7);
    }

    pub fn drawVariable(camera: Camera, world_point: Point, visuals: presenter.AtomVisuals) void {
        const screen_point = screenFromWorld(camera, world_point);
        const local_positions = if (DESIGN.round_data)
            funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.75, 0.25, math.tof32(k) / 32)).addX(0.5);
                }
            }.anon) ++ funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.25, 0.75, math.tof32(k) / 32)).mul(.new(0.5, 1)).addX(0.5);
                }
            }.anon)
        else
            [_]Vec2{
                Vec2.new(-0.5, 0),
                Vec2.new(0, 1),
                Vec2.new(0.5, 1),
                Vec2.new(0, 0),
                Vec2.new(0.5, -1),
                Vec2.new(0, -1),
            };
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        js_better.canvas.pathLoop(&screen_positions);
        js.canvas.setLineWidth(1);
        js_better.canvas.setFillColor(visuals.color);
        js_better.canvas.setStrokeColor(Color.black);
        js.canvas.fill();
        js.canvas.stroke();
    }

    pub fn drawPatternVariable(camera: Camera, world_point: Point, visuals: presenter.AtomVisuals) void {
        const screen_point = screenFromWorld(camera, world_point);
        const local_positions = if (DESIGN.round_data)
            funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(-0.25, 0.25, math.tof32(k) / 32)).addX(-0.5);
                }
            }.anon) ++ funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.25, -0.25, math.tof32(k) / 32)).mul(.new(0.5, 1)).addX(-0.5);
                }
            }.anon)
        else
            [_]Vec2{
                Vec2.new(0.5, 0),
                Vec2.new(0, 1),
                Vec2.new(-0.5, 1),
                Vec2.new(0, 0),
                Vec2.new(-0.5, -1),
                Vec2.new(0, -1),
            };
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        js_better.canvas.pathLoop(&screen_positions);
        js.canvas.setLineWidth(1);
        js_better.canvas.setFillColor(visuals.color);
        js_better.canvas.setStrokeColor(Color.black);
        js.canvas.fill();
        js.canvas.stroke();
    }

    pub fn drawAtomDebug(camera: Camera, world_point: Point) void {
        // std.debug.assert(!DESIGN.round_data);
        const local_positions = if (DESIGN.round_data)
            funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.75, 0.25, math.tof32(k) / 32)).addX(0.5);
                }
            }.anon) ++ [_]Vec2{
                .new(2, 1),
                .new(2.2, 1.0 / 3.0),
                .new(1.8, -1.0 / 3.0),
                .new(2, -1),
            }
        else
            [_]Vec2{
                .new(-0.5, 0),
                .new(0, 1),
                .new(2, 1),
                .new(2.2, 1.0 / 3.0),
                .new(1.8, -1.0 / 3.0),
                .new(2, -1),
                .new(0, -1),
            };

        drawShapeV2(camera, world_point, &local_positions, .black, .white);
    }

    pub fn drawAtom(camera: Camera, world_point: Point, visuals: presenter.AtomVisuals) void {
        const profile = visuals.profile;
        const screen_point = screenFromWorld(camera, world_point);
        if (optimization_dont_draw_tiny and screen_point.scale < 0.1) return;
        const local_positions = if (DESIGN.round_data)
            [1]Vec2{.new(2, -1)} ++
                funk.fromCount(32, struct {
                    pub fn anon(k: usize) Vec2 {
                        return Vec2.fromTurns(math.lerp(0.75, 0.25, math.tof32(k) / 32)).addX(0.5);
                    }
                }.anon) ++ [1]Vec2{.new(2, 1)}
        else
            [_]Vec2{
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
        if (visuals.display) |d| {
            const p = screen_point.applyToLocalPosition(.new(0.25, 0));
            js_better.canvas.setFillColor(.black);
            js.canvas.fillText(d.ptr, d.len, p.x, p.y, screen_point.scale);
        }
    }

    pub fn drawPatternAtom(camera: Camera, world_point: Point, visuals: presenter.AtomVisuals) void {
        const profile = visuals.profile;
        const screen_point = screenFromWorld(camera, world_point);
        if (optimization_dont_draw_tiny and screen_point.scale < 0.1) return;
        const local_positions = if (DESIGN.round_data)
            [1]Vec2{.new(-1, -1)} ++
                funk.fromCount(32, struct {
                    pub fn anon(k: usize) Vec2 {
                        return Vec2.fromTurns(math.lerp(-0.25, 0.25, math.tof32(k) / 32)).addX(-0.5);
                    }
                }.anon) ++ [1]Vec2{.new(-1, 1)}
        else
            [_]Vec2{
                Vec2.new(-1, -1),
                Vec2.new(0, -1),
                Vec2.new(0.5, 0),
                Vec2.new(0, 1),
                Vec2.new(-1, 1),
            };
        // TODO: no allocations
        var screen_positions: []Vec2 = gpa.allocator().alloc(Vec2, local_positions.len + profile.len * 2) catch @panic("TODO");
        defer gpa.allocator().free(screen_positions);
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        for (profile, 0..) |pos, i| {
            screen_positions[local_positions.len + i] = screen_point.applyToLocalPosition(
                Vec2.new(-1.0 - pos.y, 1.0 - pos.x),
            );
            screen_positions[local_positions.len + profile.len * 2 - i - 1] = screen_point.applyToLocalPosition(
                Vec2.new(-1.0 + pos.y, -1.0 + pos.x),
            );
        }
        js_better.canvas.pathLoop(screen_positions);
        js.canvas.setLineWidth(1);
        js_better.canvas.setFillColor(visuals.color);
        js_better.canvas.setStrokeColor(Color.black);
        js.canvas.fill();
        js.canvas.stroke();
        if (visuals.display) |d| {
            const p = screen_point.applyToLocalPosition(.new(-0.25, 0));
            js_better.canvas.setFillColor(.black);
            js.canvas.fillText(d.ptr, d.len, p.x, p.y, screen_point.scale);
        }
    }

    pub fn drawPairHolder(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);
        if (optimization_dont_draw_tiny and screen_point.scale < 0.1) return;
        const local_positions = if (DESIGN.round_data)
            funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.75, 0.25, math.tof32(k) / 32)).addX(0.5);
                }
            }.anon) ++ funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.25, 0.75, math.tof32(k) / 32)).scale(0.5).add(.new(0.75, 0.5));
                }
            }.anon) ++ funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.25, 0.75, math.tof32(k) / 32)).scale(0.5).add(.new(0.75, -0.5));
                }
            }.anon)
        else
            [_]Vec2{
                Vec2.new(-0.5, 0),
                Vec2.new(0, 1),
                Vec2.new(0.5, 1),
                Vec2.new(0.25, 0.5),
                Vec2.new(0.5, 0),
                Vec2.new(0.25, -0.5),
                Vec2.new(0.5, -1),
                Vec2.new(0, -1),
            };
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        js_better.canvas.pathLoop(&screen_positions);
        js.canvas.setLineWidth(1);
        js_better.canvas.setFillColor(Color.gray(96));
        js_better.canvas.setStrokeColor(Color.black);
        js.canvas.fill();
        js.canvas.stroke();
    }

    pub fn drawPatternPairHolder(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);
        if (optimization_dont_draw_tiny and screen_point.scale < 0.1) return;
        const local_positions = if (DESIGN.round_data)
            funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(-0.25, 0.25, math.tof32(k) / 32)).addX(-0.5);
                }
            }.anon) ++ funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.25, -0.25, math.tof32(k) / 32)).scale(0.5).add(.new(-1.25, 0.5));
                }
            }.anon) ++ funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.25, -0.25, math.tof32(k) / 32)).scale(0.5).add(.new(-1.25, -0.5));
                }
            }.anon)

                // [_]Vec2{ .new(-1.5, 1), .new(-1, 0.5), .new(-1.5, 0), .new(-1, -0.5), .new(-1.5, -1) }
        else
            [_]Vec2{
                Vec2.new(0.5, 0),
                Vec2.new(0, 1),
                Vec2.new(-1, 1),
                Vec2.new(-0.75, 0.5),
                Vec2.new(-1, 0),
                Vec2.new(-0.75, -0.5),
                Vec2.new(-1, -1),
                Vec2.new(0, -1),
            };
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        js_better.canvas.pathLoop(&screen_positions);
        js.canvas.setLineWidth(1);
        js_better.canvas.setFillColor(Color.gray(96));
        js_better.canvas.setStrokeColor(Color.black);
        js.canvas.fill();
        js.canvas.stroke();
    }

    pub fn drawPatternAtomDebug(camera: Camera, world_point: Point) void {
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

    pub fn drawPatternAtomOutline(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);
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
        js_better.canvas.pathLoop(&screen_positions);
        js.canvas.setLineWidth(2);
        js_better.canvas.setStrokeColor(Color.cyan);
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

        // TODO: only draw in visible bounds to avoid arbitrarily big cost on zoom
        if (true) return;
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

    pub fn drawCaseHolder(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);

        js_better.canvas.setStrokeColor(Color.white);
        js.canvas.beginPath();
        js_better.canvas.circle(screen_point.pos, screen_point.scale * 0.5);
        js.canvas.stroke();
    }

    pub fn drawFnkHolder(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);

        js.canvas.setLineWidth(1);
        js_better.canvas.setStrokeColor(Color.black);
        js.canvas.beginPath();
        js_better.canvas.circle(screen_point.pos, screen_point.scale * 0.5);
        js.canvas.stroke();

        js.canvas.beginPath();
        js_better.canvas.path(&.{
            screen_point.applyToLocalPosition(.new(0, -0.5)),
            screen_point.applyToLocalPosition(.new(0, -1.5)),
        });
        js.canvas.stroke();
    }

    pub fn drawWildcardsCable(camera: Camera, points: []const Vec2, visuals: []const presenter.AtomVisuals) void {
        js.canvas.setLineWidth(3);
        for (visuals) |v| {
            drawLine(camera, points, v.color);
            js.canvas.translate(3, 3);
        }
        js.canvas.resetTransform();
        js.canvas.setLineWidth(1);
    }

    pub fn drawAsdfDevice(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);

        js_better.canvas.setStrokeColor(Color.white);
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

        js_better.canvas.setFillColor(Color.white);
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

    fn cableOffset(x: f32, scale: f32) f32 {
        const z = x * 20 / scale;
        const y = @sin(z) + 0.2 * @sin(z * 1.3) + 0.3 * @sin(z * 3.1);
        return y * 0.1 * scale;
    }
};

var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
var mem_frame_arena: std.heap.ArenaAllocator = undefined;
const web_platform = presenter.Platform{
    .gpa = gpa.allocator(),
    .mem_frame = mem_frame_arena.allocator(),
    .getPlayerData = WebPlatform.getPlayerData,
    .setPlayerData = WebPlatform.setPlayerData,
    .downloadPlayerData = WebPlatform.downloadPlayerData,
    .uploadPlayerData = WebPlatform.uploadPlayerData,
    .getMouse = WebPlatform.getMouse,
    .getKeyboard = WebPlatform.getKeyboard,
    .setCursor = WebPlatform.setCursor,
};
const web_drawer = presenter.Drawer{
    .asdfBackground = WebDrawer.asdfBackground,
    .clear = js_better.canvas.clear,
    .setTransparency = WebDrawer.setTransparency,
    .clipAtomRegion = WebDrawer.clipAtomRegion,
    .endClip = WebDrawer.endClip,
    .drawLine = WebDrawer.drawLine,
    .drawRect = WebDrawer.drawRect,
    .drawShape = WebDrawer.drawShape,
    .drawShapeV2 = WebDrawer.drawShapeV2,
    .drawDebugText = WebDrawer.drawDebugText,
    .drawAtom = WebDrawer.drawAtom,
    .drawPatternAtom = WebDrawer.drawPatternAtom,
    .drawVariable = WebDrawer.drawVariable,
    .drawPatternVariable = WebDrawer.drawPatternVariable,
    .drawAtomDebug = WebDrawer.drawAtomDebug,
    .drawPatternAtomOutline = WebDrawer.drawPatternAtomOutline,
    .drawPairHolder = WebDrawer.drawPairHolder,
    .drawPatternPairHolder = WebDrawer.drawPatternPairHolder,
    .drawPatternAtomDebug = WebDrawer.drawPatternAtomDebug,
    .drawCable = WebDrawer.drawCable,
    .drawCaseHolder = WebDrawer.drawCaseHolder,
    .drawFnkHolder = WebDrawer.drawFnkHolder,
    .drawWildcardsCable = WebDrawer.drawWildcardsCable,
};

var game: presenter.Presenter(web_platform, web_drawer) = undefined;

export fn init() void {
    mem_frame_arena = .init(gpa.allocator());

    @TypeOf(game).init(&game) catch {
        std.log.err("bad start", .{});
        programmerError();
    };
}

var paused = false;
export fn frame(delta_seconds: f32) void {
    if (paused) return;
    _ = mem_frame_arena.reset(.retain_capacity);
    game.update(delta_seconds) catch |err| switch (err) {
        error.OutOfMemory => OoM(),
        // TODO: remove this
        else => programmerError(),
    };
    mouse.prev = mouse.cur;
    mouse.cur.scrolled = .none;
    keyboard.prev = keyboard.cur;
}

export fn draw() void {
    game.draw() catch |err| switch (err) {
        error.OutOfMemory => OoM(),
        // TODO: remove this
        else => programmerError(),
    };
}

var keyboard = presenter.Keyboard{ .cur = .init, .prev = .init, .cur_time = undefined };
const KeyCode = @import("./tools/generate_keycodes_js.zig").KeyCode;
export fn keydown(code: KeyCode) void {
    keychanged(code, true);
}
export fn keyup(code: KeyCode) void {
    keychanged(code, false);
}

fn keychanged(key: KeyCode, is_pressed: bool) void {
    switch (key) {
        .KeyD, .ArrowRight => keyboard.cur.keys.right = is_pressed,
        .KeyA, .ArrowLeft => keyboard.cur.keys.left = is_pressed,
        .KeyW, .ArrowUp => keyboard.cur.keys.up = is_pressed,
        .KeyS, .ArrowDown => keyboard.cur.keys.down = is_pressed,
        .Space => keyboard.cur.keys.space = is_pressed,
        .KeyE => keyboard.cur.keys.KeyE = is_pressed,
        .KeyQ => keyboard.cur.keys.KeyQ = is_pressed,
    }
}

var mouse = presenter.Mouse{ .cur = .init, .prev = .init, .cur_time = undefined };
export fn pointermove(x: f32, y: f32) void {
    mouse.cur.client_pos = Vec2.new(x, y).scale(1.0 / js_better.canvas.getSize().y);
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

export fn wheel(delta_y: i32) void {
    mouse.cur.scrolled = if (delta_y == 0)
        .none
    else if (delta_y > 0)
        .down
    else
        .up;
}

fn programmerError() noreturn {
    js.debug.logInt(666);
    std.debug.panic("programmer error!", .{});
}

fn OoM() noreturn {
    js.debug.logInt(321);
    std.debug.panic("OoM!", .{});
}
