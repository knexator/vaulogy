// These allow running webgame.zig without having to link to the real external functions

export fn logInt(arg: u32) void {
    _ = arg;
    unreachable;
}
export fn logFloat(arg: f32) void {
    _ = arg;
    unreachable;
}
export fn beginPath() void {
    unreachable;
}
export fn moveTo(x: f32, y: f32) void {
    _ = x;
    _ = y;
    unreachable;
}
export fn lineTo(x: f32, y: f32) void {
    _ = x;
    _ = y;
    unreachable;
}
export fn closePath() void {
    unreachable;
}
export fn fill() void {
    unreachable;
}
export fn stroke() void {
    unreachable;
}
export fn setLineWidth(w: f32) void {
    _ = w;
    unreachable;
}
export fn setFillColor(r: u8, g: u8, b: u8) void {
    _ = r;
    _ = g;
    _ = b;
    unreachable;
}
export fn setStrokeColor(r: u8, g: u8, b: u8) void {
    _ = r;
    _ = g;
    _ = b;
    unreachable;
}
export fn setGlobalAlpha(a: f32) void {
    _ = a;
    unreachable;
}
export fn fillRect(x: f32, y: f32, w: f32, h: f32) void {
    _ = x;
    _ = y;
    _ = w;
    _ = h;
    unreachable;
}
export fn arc(x: f32, y: f32, radius: f32, startAngle: f32, endAngle: f32, counterclockwise: bool) void {
    _ = x;
    _ = y;
    _ = radius;
    _ = startAngle;
    _ = endAngle;
    _ = counterclockwise;
    unreachable;
}
export fn ellipse(x: f32, y: f32, radiusX: f32, radiusY: f32, rotation: f32, startAngle: f32, endAngle: f32, counterclockwise: bool) void {
    _ = x;
    _ = y;
    _ = radiusX;
    _ = radiusY;
    _ = rotation;
    _ = startAngle;
    _ = endAngle;
    _ = counterclockwise;
    unreachable;
}
export fn getWidth() u32 {
    unreachable;
}
export fn getHeight() u32 {
    unreachable;
}
