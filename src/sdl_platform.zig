const std = @import("std");

const model = @import("main.zig");
const presenter = @import("presenter.zig");

const SdlPlatform = struct {
    pub fn getPlayerData(mem: *model.VeryPermamentGameStuff) !?presenter.PlayerData {
        _ = mem;
        return null;
    }

    pub fn setPlayerData(player_data: presenter.PlayerData, mem: *model.VeryPermamentGameStuff) !void {
        _ = player_data;
        _ = mem;
    }

    pub fn getMouse() presenter.Mouse {
        return mouse;
    }

    pub fn getKeyboard() presenter.Keyboard {
        return keyboard;
    }
};

var sdl_renderer: *c.SDL_Renderer = undefined;

const tof32 = @import("kommon/math.zig").tof32;
const Camera = presenter.Camera;
const Point = presenter.Point;
const Vec2 = presenter.Vec2;
const Color = presenter.Color;
const Rect = presenter.Rect;
const SdlDrawer = struct {
    fn screenFromWorld(camera: Camera, world_point: Point) Point {
        const rect = camera.toRect();
        const local = Point.inverseApplyGetLocal(Point{
            .pos = rect.top_left,
            .scale = rect.size.y,
        }, world_point);
        const screen = Point{ .pos = .zero, .scale = window_size.y };
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

    fn setRenderDrawColor(color: Color) void {
        panickify(c.SDL_SetRenderDrawColor(sdl_renderer, color.r, color.g, color.b, 0xff));
    }

    pub fn clear(color: Color) void {
        setRenderDrawColor(color);
        panickify(c.SDL_RenderClear(sdl_renderer));
    }

    pub fn drawRect(camera: Camera, rect: Rect, stroke: ?Color, fill: ?Color) void {
        const screen_top_left = screenFromWorldPosition(camera, rect.top_left);
        const screen_size = screenFromWorldSize(camera, rect.size);

        const sdl_rect = c.SDL_FRect{
            .x = screen_top_left.x,
            .y = screen_top_left.y,
            .w = screen_size.x,
            .h = screen_size.y,
        };

        if (stroke) |col| {
            setRenderDrawColor(col);
            panickify(c.SDL_RenderRect(sdl_renderer, &sdl_rect));
        }
        if (fill) |col| {
            setRenderDrawColor(col);
            panickify(c.SDL_RenderFillRect(sdl_renderer, &sdl_rect));
        }
    }

    pub fn drawDebugText(camera: Camera, center: Point, text: [:0]const u8, color: Color) void {
        const screen_point = screenFromWorld(camera, center);
        // TODO: scale
        // panickify(c.SDL_SetRenderScale(sdl_renderer, screen_point.scale / 8, screen_point.scale / 8));
        // defer panickify(c.SDL_SetRenderScale(sdl_renderer, 1, 1));
        setRenderDrawColor(color);
        var it = std.mem.splitScalar(u8, text, '\n');
        var y: f32 = -4 - 8 * (tof32(std.mem.count(u8, text, "\n")) / 2);
        while (it.next()) |line| {
            const lineZ = gpa.allocator().dupeZ(u8, line) catch @panic("OoM");
            defer gpa.allocator().free(lineZ);
            panickify(c.SDL_RenderDebugText(sdl_renderer, screen_point.pos.x - tof32(line.len) * 4, screen_point.pos.y + y, lineZ));
            y += 8;
        }
    }

    fn polygon(screen_positions: []const Vec2, triangles: []const [3]usize, outline_points: []const usize, fill: Color, stroke: Color) void {
        const vertices = gpa.allocator().alloc(c.SDL_Vertex, screen_positions.len) catch @panic("OoM");
        defer gpa.allocator().free(vertices);

        for (screen_positions, vertices) |pos, *vertex| {
            vertex.* = c.SDL_Vertex{
                .position = c.SDL_FPoint{ .x = pos.x, .y = pos.y },
                .color = c.SDL_FColor{
                    .r = @as(f32, @floatFromInt(fill.r)) / 255.0,
                    .g = @as(f32, @floatFromInt(fill.g)) / 255.0,
                    .b = @as(f32, @floatFromInt(fill.b)) / 255.0,
                    .a = 1.0,
                },
                .tex_coord = c.SDL_FPoint{ .x = 0, .y = 0 },
            };
        }

        const indices = gpa.allocator().alloc(c_int, 3 * triangles.len) catch @panic("OoM");
        for (triangles, 0..) |triangle, k| {
            indices[k * 3 + 0] = @intCast(triangle[0]);
            indices[k * 3 + 1] = @intCast(triangle[1]);
            indices[k * 3 + 2] = @intCast(triangle[2]);
        }
        defer gpa.allocator().free(indices);

        setRenderDrawColor(fill);
        panickify(c.SDL_RenderGeometry(
            sdl_renderer,
            null,
            vertices.ptr,
            @intCast(vertices.len),
            indices.ptr,
            @intCast(indices.len),
        ));

        setRenderDrawColor(stroke);
        for (0..outline_points.len) |i| {
            const from = screen_positions[outline_points[i]];
            const to = screen_positions[outline_points[(i + 1) % outline_points.len]];
            panickify(c.SDL_RenderLine(sdl_renderer, from.x, from.y, to.x, to.y));
        }
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
        const indices = [_][3]usize{
            .{ 1, 2, 3 },
            .{ 0, 1, 3 },
            .{ 0, 3, 4 },
            .{ 0, 4, 6 },
            .{ 4, 5, 6 },
        };
        const outline = [_]usize{ 0, 1, 2, 3, 4, 5, 6 };
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        polygon(&screen_positions, &indices, &outline, Color.white, Color.black);
    }

    pub fn drawPatternAtom(camera: Camera, world_point: Point, visuals: presenter.AtomVisuals) void {
        const profile = visuals.profile;
        const screen_point = screenFromWorld(camera, world_point);
        if (screen_point.scale < 0.1) return;
        const local_positions = [_]Vec2{
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

        const indices = gpa.allocator().alloc([3]usize, profile.len * 2 + 3) catch @panic("TODO");
        defer gpa.allocator().free(indices);
        @memset(indices, .{ 0, 0, 0 });
        for (0..indices.len) |i| {
            indices[i] = .{ 2, (3 + i) % screen_positions.len, (4 + i) % screen_positions.len };
        }

        const outline = gpa.allocator().alloc(usize, screen_positions.len) catch @panic("TODO");
        defer gpa.allocator().free(outline);
        for (outline, 0..) |*x, k| {
            x.* = k;
        }
        polygon(screen_positions, indices, outline, visuals.color, Color.black);
    }

    pub fn drawVariable(camera: Camera, world_point: Point, visuals: presenter.AtomVisuals) void {
        const screen_point = screenFromWorld(camera, world_point);
        if (screen_point.scale < 0.1) return;
        const local_positions = [_]Vec2{
            Vec2.new(-0.5, 0),
            Vec2.new(0, 1),
            Vec2.new(0.5, 1),
            Vec2.new(0, 0),
            Vec2.new(0.5, -1),
            Vec2.new(0, -1),
        };
        const indices = [_][3]usize{
            .{ 1, 2, 3 },
            .{ 0, 1, 3 },
            .{ 0, 3, 5 },
            .{ 3, 4, 5 },
        };
        const outline = [_]usize{ 0, 1, 2, 3, 4, 5 };
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        polygon(&screen_positions, &indices, &outline, visuals.color, Color.black);
    }

    pub fn drawPatternVariable(camera: Camera, world_point: Point, visuals: presenter.AtomVisuals) void {
        const screen_point = screenFromWorld(camera, world_point);
        if (screen_point.scale < 0.1) return;
        const local_positions = [_]Vec2{
            Vec2.new(0.5, 0),
            Vec2.new(0, 1),
            Vec2.new(-0.5, 1),
            Vec2.new(0, 0),
            Vec2.new(-0.5, -1),
            Vec2.new(0, -1),
        };
        const indices = [_][3]usize{
            .{ 1, 2, 3 },
            .{ 0, 1, 3 },
            .{ 0, 3, 5 },
            .{ 3, 4, 5 },
        };
        const outline = [_]usize{ 0, 1, 2, 3, 4, 5 };
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        polygon(&screen_positions, &indices, &outline, visuals.color, Color.black);
    }

    pub fn drawPairHolder(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);
        if (screen_point.scale < 0.1) return;
        const local_positions = [_]Vec2{
            Vec2.new(-0.5, 0),
            Vec2.new(0, 1),
            Vec2.new(0.5, 1),
            Vec2.new(0.25, 0.5),
            Vec2.new(0.5, 0),
            Vec2.new(0.25, -0.5),
            Vec2.new(0.5, -1),
            Vec2.new(0, -1),
        };
        const indices = [_][3]usize{
            .{ 1, 2, 3 },
            .{ 0, 1, 3 },
            .{ 0, 3, 4 },
            .{ 0, 4, 5 },
            .{ 0, 5, 7 },
            .{ 5, 6, 7 },
        };
        const outline = [_]usize{ 0, 1, 2, 3, 4, 5, 6, 7 };
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        polygon(&screen_positions, &indices, &outline, Color.gray(96), Color.black);
    }

    pub fn drawPatternPairHolder(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);
        if (screen_point.scale < 0.1) return;
        const local_positions = [_]Vec2{
            Vec2.new(0.5, 0),
            Vec2.new(0, 1),
            Vec2.new(-1, 1),
            Vec2.new(-0.75, 0.5),
            Vec2.new(-1, 0),
            Vec2.new(-0.75, -0.5),
            Vec2.new(-1, -1),
            Vec2.new(0, -1),
        };
        const indices = [_][3]usize{
            .{ 1, 2, 3 },
            .{ 0, 1, 3 },
            .{ 0, 3, 4 },
            .{ 0, 4, 5 },
            .{ 0, 5, 7 },
            .{ 5, 6, 7 },
        };
        const outline = [_]usize{ 0, 1, 2, 3, 4, 5, 6, 7 };
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        polygon(&screen_positions, &indices, &outline, Color.gray(96), Color.black);
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
        const indices = [_][3]usize{
            .{ 1, 2, 3 },
            .{ 0, 1, 3 },
            .{ 0, 3, 4 },
            .{ 0, 4, 6 },
            .{ 4, 5, 6 },
        };
        const outline = [_]usize{ 0, 1, 2, 3, 4, 5, 6 };
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        polygon(&screen_positions, &indices, &outline, Color.white, Color.black);
    }

    pub fn drawPatternAtomOutline(camera: Camera, world_point: Point) void {
        _ = camera;
        _ = world_point;
        @panic("not implemented");
    }

    pub fn drawCable(camera: Camera, world_from: Vec2, world_to: Vec2, world_scale: f32, offset: f32) void {
        const screen_from = screenFromWorldPosition(camera, world_from);
        const screen_to = screenFromWorldPosition(camera, world_to);
        const scale = screenFromWorldScale(camera, world_scale);
        _ = scale;
        _ = offset;
        setRenderDrawColor(Color.black);
        panickify(c.SDL_RenderLine(sdl_renderer, screen_from.x, screen_from.y, screen_to.x, screen_to.y));
    }

    pub fn drawFnkHolder(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);
        _ = screen_point;
        std.log.warn("TODO: drawFnkHolder", .{});
    }

    pub fn drawCaseHolder(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);

        const N = 32;
        var screen_positions: [N + 1]c.SDL_FPoint = undefined;
        for (0..N) |k| {
            const radians = std.math.tau * @as(f32, @floatFromInt(k)) / @as(f32, @floatFromInt(N));
            const point = screen_point.applyToLocalPosition(Vec2.new(std.math.cos(radians), std.math.sin(radians)).scale(0.5));
            screen_positions[k] = c.SDL_FPoint{ .x = point.x, .y = point.y };
        }
        screen_positions[N] = screen_positions[0];
        setRenderDrawColor(Color.white);
        panickify(c.SDL_RenderLines(sdl_renderer, &screen_positions, screen_positions.len));
    }

    pub fn drawAtom(camera: Camera, world_point: Point, visuals: presenter.AtomVisuals) void {
        const profile = visuals.profile;
        const screen_point = screenFromWorld(camera, world_point);
        if (screen_point.scale < 0.1) return;
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

        const indices = gpa.allocator().alloc([3]usize, profile.len * 2 + 3) catch @panic("TODO");
        defer gpa.allocator().free(indices);
        @memset(indices, .{ 0, 0, 0 });
        for (0..indices.len) |i| {
            indices[i] = .{ 2, (3 + i) % screen_positions.len, (4 + i) % screen_positions.len };
        }

        const outline = gpa.allocator().alloc(usize, screen_positions.len) catch @panic("TODO");
        defer gpa.allocator().free(outline);
        for (outline, 0..) |*x, k| {
            x.* = k;
        }
        polygon(screen_positions, indices, outline, visuals.color, Color.black);
    }
};

var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
const sdl_platform = presenter.Platform{
    .gpa = gpa.allocator(),
    .getPlayerData = SdlPlatform.getPlayerData,
    .setPlayerData = SdlPlatform.setPlayerData,
    .getMouse = SdlPlatform.getMouse,
    .getKeyboard = SdlPlatform.getKeyboard,
};
const sdl_drawer = presenter.Drawer{
    .clear = SdlDrawer.clear,
    .drawRect = SdlDrawer.drawRect,
    .drawDebugText = SdlDrawer.drawDebugText,
    .drawAtom = SdlDrawer.drawAtom,
    .drawPatternAtom = SdlDrawer.drawPatternAtom,
    .drawVariable = SdlDrawer.drawVariable,
    .drawPatternVariable = SdlDrawer.drawPatternVariable,
    .drawAtomDebug = SdlDrawer.drawAtomDebug,
    .drawPatternAtomOutline = SdlDrawer.drawPatternAtomOutline,
    .drawPairHolder = SdlDrawer.drawPairHolder,
    .drawPatternPairHolder = SdlDrawer.drawPatternPairHolder,
    .drawPatternAtomDebug = SdlDrawer.drawPatternAtomDebug,
    .drawCable = SdlDrawer.drawCable,
    .drawCaseHolder = SdlDrawer.drawCaseHolder,
    .drawFnkHolder = SdlDrawer.drawFnkHolder,
};

const window_size = Vec2.new(1280, 720);
const MouseState = presenter.MouseState;
var mouse = presenter.Mouse{ .cur = .init, .prev = .init };
var keyboard = presenter.Keyboard{ .cur = .init, .prev = .init };
var game: presenter.Presenter(sdl_platform, sdl_drawer) = undefined;

const c = @cImport({
    @cDefine("SDL_DISABLE_OLD_NAMES", {});
    @cInclude("SDL3/SDL.h");
    @cInclude("SDL3/SDL_revision.h");
    // For programs that provide their own entry points instead of relying on SDL's main function
    // macro magic, 'SDL_MAIN_HANDLED' should be defined before including 'SDL_main.h'.
    @cDefine("SDL_MAIN_HANDLED", {});
    @cInclude("SDL3/SDL_main.h");
});

pub fn main() !void {
    errdefer |err| if (err == error.SdlError) std.log.err("SDL error: {s}", .{c.SDL_GetError()});

    std.log.debug("SDL build time version: {d}.{d}.{d}", .{
        c.SDL_MAJOR_VERSION,
        c.SDL_MINOR_VERSION,
        c.SDL_MICRO_VERSION,
    });
    std.log.debug("SDL build time revision: {s}", .{c.SDL_REVISION});
    {
        const version = c.SDL_GetVersion();
        std.log.debug("SDL runtime version: {d}.{d}.{d}", .{
            c.SDL_VERSIONNUM_MAJOR(version),
            c.SDL_VERSIONNUM_MINOR(version),
            c.SDL_VERSIONNUM_MICRO(version),
        });
        const revision: [*:0]const u8 = c.SDL_GetRevision();
        std.log.debug("SDL runtime revision: {s}", .{revision});
    }

    // For programs that provide their own entry points instead of relying on SDL's main function
    // macro magic, 'SDL_SetMainReady' should be called before calling 'SDL_Init'.
    c.SDL_SetMainReady();

    try errify(c.SDL_SetAppMetadata("Vaulogy", "0.0.0", "com.knexator.vaulogy"));

    try errify(c.SDL_Init(c.SDL_INIT_VIDEO));
    defer c.SDL_Quit();

    std.log.debug("SDL video drivers: {}", .{fmtSdlDrivers(
        c.SDL_GetCurrentVideoDriver().?,
        c.SDL_GetNumVideoDrivers(),
        c.SDL_GetVideoDriver,
    )});

    errify(c.SDL_SetHint(c.SDL_HINT_RENDER_VSYNC, "1")) catch {};

    const window: *c.SDL_Window, const renderer: *c.SDL_Renderer = create_window_and_renderer: {
        var window: ?*c.SDL_Window = null;
        var renderer: ?*c.SDL_Renderer = null;
        try errify(c.SDL_CreateWindowAndRenderer("Vaulogy", @intFromFloat(window_size.x), @intFromFloat(window_size.y), 0, &window, &renderer));
        errdefer comptime unreachable;

        break :create_window_and_renderer .{ window.?, renderer.? };
    };
    sdl_renderer = renderer;
    defer c.SDL_DestroyRenderer(renderer);
    defer c.SDL_DestroyWindow(window);

    std.log.debug("SDL render drivers: {}", .{fmtSdlDrivers(
        c.SDL_GetRendererName(renderer).?,
        c.SDL_GetNumRenderDrivers(),
        c.SDL_GetRenderDriver,
    )});

    try @TypeOf(game).init(&game);

    var timekeeper: Timekeeper = .{ .tocks_per_s = c.SDL_GetPerformanceFrequency() };

    main_loop: while (true) {
        // Process SDL events
        {
            var event: c.SDL_Event = undefined;
            while (c.SDL_PollEvent(&event)) {
                switch (event.type) {
                    c.SDL_EVENT_QUIT => {
                        break :main_loop;
                    },
                    c.SDL_EVENT_MOUSE_BUTTON_DOWN, c.SDL_EVENT_MOUSE_BUTTON_UP => {
                        const is_pressed = event.button.down;
                        switch (event.button.button) {
                            c.SDL_BUTTON_LEFT => mouse.cur.buttons.left = is_pressed,
                            c.SDL_BUTTON_RIGHT => mouse.cur.buttons.right = is_pressed,
                            c.SDL_BUTTON_MIDDLE => mouse.cur.buttons.middle = is_pressed,
                            else => {},
                        }
                    },
                    c.SDL_EVENT_MOUSE_MOTION => {
                        mouse.cur.client_pos = Vec2.new(event.motion.x, event.motion.y).scale(1.0 / window_size.y);
                    },
                    c.SDL_EVENT_MOUSE_WHEEL => {
                        mouse.cur.scrolled = if (event.wheel.y == 0)
                            .none
                        else if (event.wheel.y < 0)
                            .down
                        else
                            .up;
                    },
                    c.SDL_EVENT_KEY_DOWN, c.SDL_EVENT_KEY_UP => {
                        const is_pressed = event.type == c.SDL_EVENT_KEY_DOWN;
                        switch (event.key.key) {
                            c.SDLK_D, c.SDLK_RIGHT => keyboard.cur.right = is_pressed,
                            c.SDLK_A, c.SDLK_LEFT => keyboard.cur.left = is_pressed,
                            c.SDLK_W, c.SDLK_UP => keyboard.cur.up = is_pressed,
                            c.SDLK_S, c.SDLK_DOWN => keyboard.cur.down = is_pressed,
                            else => {},
                        }
                    },
                    else => {},
                }
            }
        }

        // Update the game state
        while (timekeeper.consume()) {
            // frame logic
            try game.update(1.0 / 60.0);
            mouse.prev = mouse.cur;
            mouse.cur.scrolled = .none;
            keyboard.prev = keyboard.cur;
        }

        // Draw
        {
            // SdlDrawer.clear(Color.new(0x47, 0x5b, 0x8d));
            // SdlDrawer.drawRect(.{ .center = Vec2.half, .height = 3 }, .{ .size = .new(1, 1), .top_left = .zero });
            try game.draw();
            try errify(c.SDL_RenderPresent(renderer));
        }

        timekeeper.produce(c.SDL_GetPerformanceCounter());
    }
}

/// Facilitates updating the game logic at a fixed rate.
/// Inspired <https://github.com/TylerGlaiel/FrameTimingControl> and the linked article.
const Timekeeper = struct {
    const updates_per_s = 60;
    const max_accumulated_updates = 8;
    const snap_frame_rates = .{ updates_per_s, 30, 120, 144 };
    const ticks_per_tock = 720; // Least common multiple of 'snap_frame_rates'
    const snap_tolerance_us = 200;
    const us_per_s = 1_000_000;

    tocks_per_s: u64,
    accumulated_ticks: u64 = 0,
    previous_timestamp: ?u64 = null,

    fn consume(timekeeper: *Timekeeper) bool {
        const ticks_per_s: u64 = timekeeper.tocks_per_s * ticks_per_tock;
        const ticks_per_update: u64 = @divExact(ticks_per_s, updates_per_s);
        if (timekeeper.accumulated_ticks >= ticks_per_update) {
            timekeeper.accumulated_ticks -= ticks_per_update;
            return true;
        } else {
            return false;
        }
    }

    fn produce(timekeeper: *Timekeeper, current_timestamp: u64) void {
        if (timekeeper.previous_timestamp) |previous_timestamp| {
            const ticks_per_s: u64 = timekeeper.tocks_per_s * ticks_per_tock;
            const elapsed_ticks: u64 = (current_timestamp -% previous_timestamp) *| ticks_per_tock;
            const snapped_elapsed_ticks: u64 = inline for (snap_frame_rates) |snap_frame_rate| {
                const target_ticks: u64 = @divExact(ticks_per_s, snap_frame_rate);
                const abs_diff = @max(elapsed_ticks, target_ticks) - @min(elapsed_ticks, target_ticks);
                if (abs_diff *| us_per_s <= snap_tolerance_us *| ticks_per_s) {
                    break target_ticks;
                }
            } else elapsed_ticks;
            const ticks_per_update: u64 = @divExact(ticks_per_s, updates_per_s);
            const max_accumulated_ticks: u64 = max_accumulated_updates * ticks_per_update;
            timekeeper.accumulated_ticks = @min(timekeeper.accumulated_ticks +| snapped_elapsed_ticks, max_accumulated_ticks);
        }
        timekeeper.previous_timestamp = current_timestamp;
    }
};

fn fmtSdlDrivers(
    current_driver: [*:0]const u8,
    num_drivers: c_int,
    getDriver: *const fn (c_int) callconv(.C) ?[*:0]const u8,
) std.fmt.Formatter(formatSdlDrivers) {
    return .{ .data = .{
        .current_driver = current_driver,
        .num_drivers = num_drivers,
        .getDriver = getDriver,
    } };
}

fn formatSdlDrivers(
    context: struct {
        current_driver: [*:0]const u8,
        num_drivers: c_int,
        getDriver: *const fn (c_int) callconv(.C) ?[*:0]const u8,
    },
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    var i: c_int = 0;
    while (i < context.num_drivers) : (i += 1) {
        if (i != 0) {
            try writer.writeAll(", ");
        }
        const driver = context.getDriver(i).?;
        try writer.writeAll(std.mem.span(driver));
        if (std.mem.orderZ(u8, context.current_driver, driver) == .eq) {
            try writer.writeAll(" (current)");
        }
    }
}

/// Converts the return value of an SDL function to an error union.
inline fn errify(value: anytype) error{SdlError}!switch (@typeInfo(@TypeOf(value))) {
    .bool => void,
    .pointer, .optional => @TypeOf(value.?),
    .int => |info| switch (info.signedness) {
        .signed => @TypeOf(@max(0, value)),
        .unsigned => @TypeOf(value),
    },
    else => @compileError("unerrifiable type: " ++ @typeName(@TypeOf(value))),
} {
    return switch (@typeInfo(@TypeOf(value))) {
        .bool => if (!value) error.SdlError,
        .pointer, .optional => value orelse error.SdlError,
        .int => |info| switch (info.signedness) {
            .signed => if (value >= 0) @max(0, value) else error.SdlError,
            .unsigned => if (value != 0) value else error.SdlError,
        },
        else => comptime unreachable,
    };
}

// same as errify but panics instead of returning an error
inline fn panickify(value: anytype) switch (@typeInfo(@TypeOf(value))) {
    .bool => void,
    .pointer, .optional => @TypeOf(value.?),
    .int => |info| switch (info.signedness) {
        .signed => @TypeOf(@max(0, value)),
        .unsigned => @TypeOf(value),
    },
    else => @compileError("unerrifiable type: " ++ @typeName(@TypeOf(value))),
} {
    return errify(value) catch {
        std.debug.panic("SDL error: {s}", .{c.SDL_GetError()});
    };
}
