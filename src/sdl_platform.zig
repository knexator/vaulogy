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
};

var sdl_renderer: *c.SDL_Renderer = undefined;

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

    pub fn drawRect(camera: Camera, rect: Rect) void {
        const screen_top_left = screenFromWorldPosition(camera, rect.top_left);
        const screen_size = screenFromWorldSize(camera, rect.size);

        const sdl_rect = c.SDL_FRect{
            .x = screen_top_left.x,
            .y = screen_top_left.y,
            .w = screen_size.x,
            .h = screen_size.y,
        };

        setRenderDrawColor(Color.white);
        panickify(c.SDL_RenderFillRect(sdl_renderer, &sdl_rect));
        setRenderDrawColor(Color.black);
        panickify(c.SDL_RenderRect(sdl_renderer, &sdl_rect));
    }

    fn polygon(screen_positions: []Vec2, fill: Color) void {
        const vertices = gpa.allocator().alloc(c.SDL_Vertex, screen_positions.len) catch @panic("OoM");
        defer gpa.allocator().free(vertices);

        for (screen_positions, vertices) |pos, *vertex| {
            vertex.* = c.SDL_Vertex{
                .position = c.SDL_FPoint{ .x = pos.x, .y = pos.y },
                .color = c.SDL_FColor{
                    .r = @floatFromInt(fill.r),
                    .g = @floatFromInt(fill.g),
                    .b = @floatFromInt(fill.b),
                    .a = 1.0,
                },
                .tex_coord = c.SDL_FPoint{ .x = 0, .y = 0 },
            };
        }

        // TODO: proper triangulation
        const indices = gpa.allocator().alloc(c_int, 3) catch @panic("OoM");
        // const indices = gpa.allocator().alloc(c_int, screen_positions.len * 3) catch @panic("OoM");
        indices[0] = 0;
        indices[1] = 1;
        indices[2] = 2;
        defer gpa.allocator().free(indices);

        panickify(c.SDL_RenderGeometry(
            sdl_renderer,
            null,
            vertices.ptr,
            @intCast(vertices.len),
            indices.ptr,
            @intCast(indices.len),
        ));
    }

    pub fn drawPatternAtomDebug(camera: Camera, world_point: Point) void {
        drawRect(camera, Rect{ .top_left = world_point.pos.sub(.new(1, 1)), .size = .new(world_point.scale, world_point.scale * 2) });

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
        polygon(&screen_positions, Color.white);
    }

    pub fn drawPatternAtom(camera: Camera, world_point: Point, visuals: presenter.AtomVisuals) void {
        _ = visuals;
        drawPatternAtomDebug(camera, world_point);
    }

    pub fn drawPairHolder(camera: Camera, world_point: Point) void {
        _ = camera;
        _ = world_point;
    }

    pub fn drawPatternPairHolder(camera: Camera, world_point: Point) void {
        _ = camera;
        _ = world_point;
    }

    pub fn drawAtomDebug(camera: Camera, world_point: Point) void {
        drawRect(camera, Rect{ .top_left = world_point.pos.sub(.new(0, 1)), .size = .new(world_point.scale * 2, world_point.scale * 2) });
    }

    pub fn drawPatternAtomOutline(camera: Camera, world_point: Point) void {
        _ = camera;
        _ = world_point;
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

    pub fn drawCaseHolder(camera: Camera, world_point: Point) void {
        _ = camera;
        _ = world_point;
    }

    pub fn drawAtom(camera: Camera, world_point: Point, visuals: presenter.AtomVisuals) void {
        _ = visuals;
        drawAtomDebug(camera, world_point);
    }
};

var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
const sdl_platform = presenter.Platform{
    .gpa = gpa.allocator(),
    .getPlayerData = SdlPlatform.getPlayerData,
    .setPlayerData = SdlPlatform.setPlayerData,
    .getMouse = SdlPlatform.getMouse,
};
const sdl_drawer = presenter.Drawer{
    .clear = SdlDrawer.clear,
    .drawRect = SdlDrawer.drawRect,
    .drawAtom = SdlDrawer.drawAtom,
    .drawPatternAtom = SdlDrawer.drawPatternAtom,
    .drawAtomDebug = SdlDrawer.drawAtomDebug,
    .drawPatternAtomOutline = SdlDrawer.drawPatternAtomOutline,
    .drawPairHolder = SdlDrawer.drawPairHolder,
    .drawPatternPairHolder = SdlDrawer.drawPatternPairHolder,
    .drawPatternAtomDebug = SdlDrawer.drawPatternAtomDebug,
    .drawCable = SdlDrawer.drawCable,
    .drawCaseHolder = SdlDrawer.drawCaseHolder,
};

const window_size = Vec2.new(1280, 720);
const MouseState = presenter.MouseState;
var mouse = presenter.Mouse{ .cur = .init, .prev = .init };
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

    game = try @TypeOf(game).init();

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
                        switch (event.button.button) {
                            c.SDL_BUTTON_LEFT => mouse.cur.buttons.left = event.button.down,
                            c.SDL_BUTTON_RIGHT => mouse.cur.buttons.right = event.button.down,
                            c.SDL_BUTTON_MIDDLE => mouse.cur.buttons.middle = event.button.down,
                            else => {},
                        }
                    },
                    c.SDL_EVENT_MOUSE_MOTION => {
                        mouse.cur.clientX = event.motion.x / window_size.y;
                        mouse.cur.clientY = event.motion.y / window_size.y;
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
