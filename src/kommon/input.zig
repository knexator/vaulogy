const kommon = @import("kommon.zig");
const Vec2 = kommon.math.Vec2;
const Camera = kommon.math.Camera;

pub const MouseButton = enum { left, right, middle };
pub const MouseState = struct {
    /// client_pos is in ([0..aspect_ratio], [0..1])
    client_pos: Vec2,
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
    buttons: struct {
        left: bool,
        middle: bool,
        right: bool,
    },

    pub const init: MouseState = .{
        .client_pos = .zero,
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
