const kommon = @import("kommon.zig");
const Vec2 = kommon.math.Vec2;
const Camera = kommon.math.Camera;
const Rect = kommon.math.Rect;

pub const MouseButton = enum { left, right, middle };
pub const MouseState = struct {
    /// client_pos is in ([0..aspect_ratio], [0..1])
    client_pos: Vec2,
    /// the new thing
    position: Vec2,
    scrolled: enum {
        up,
        down,
        none,

        pub fn toNumber(self: @This()) f32 {
            return switch (self) {
                .none => 0,
                .up => 1,
                .down => -1,
            };
        }
    },
    buttons: kommon.meta.BoolFlags(MouseButton, false),
    // buttons: std.enums.EnumSet(MouseButton),

    pub const init: MouseState = .{
        .client_pos = .zero,
        .position = .zero,
        .scrolled = .none,
        .buttons = .{
            .left = false,
            .middle = false,
            .right = false,
        },
    };

    pub fn pos(self: MouseState, camera: Camera) Vec2 {
        return camera.worldFromScreenPosition(self.client_pos);
    }

    pub fn posV2(self: MouseState, camera: Rect) Vec2 {
        return camera.top_left.add(self.client_pos.scale(camera.size.y));
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

    pub fn wasReleased(self: Mouse, button: MouseButton) bool {
        return !self.cur.isDown(button) and self.prev.isDown(button);
    }
};

pub const KeyboardButton = enum { left, right, up, down };
pub const KeyboardState = struct {

    // TODO: try setting it to packed
    keys: kommon.meta.BoolFlags(KeyboardButton, false),

    pub const init: KeyboardState = std.mem.zeroes(KeyboardState);

    pub fn isDown(self: KeyboardState, button: KeyboardButton) bool {
        return switch (button) {
            inline else => |x| @field(self.keys, @tagName(x)),
        };
    }
};

pub const Keyboard = struct {
    cur: KeyboardState,
    prev: KeyboardState,

    pub fn wasPressed(self: Keyboard, button: KeyboardButton) bool {
        return self.cur.isDown(button) and !self.prev.isDown(button);
    }
};

const std = @import("std");
