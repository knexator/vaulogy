extern fn clearOutput() void;
extern fn addOutput(output_ptr: [*]const u8, output_len: usize) void;

fn addOutput2(output: []const u8) void {
    addOutput(output.ptr, output.len);
}

const gpa = std.heap.wasm_allocator;

var input_buffer: []u8 = &.{};
var output_buffer: []u8 = &.{};

export fn allocInput(input_len: usize) [*]const u8 {
    if (input_len > input_buffer.len) {
        input_buffer = gpa.realloc(input_buffer, input_len) catch OoM();
    }
    return input_buffer.ptr;
}

export fn setInput(input_ptr: [*]const u8, input_len: usize) void {
    const input: []const u8 = @ptrCast(input_ptr[0..input_len]);

    var output: std.ArrayListUnmanaged(u8) = .initBuffer(output_buffer);

    clearOutput();

    @import("main.zig").textgame(
        gpa,
        input,
        output.writer(gpa).any(),
    ) catch return;

    addOutput2(output.items);
}

fn OoM() noreturn {
    std.debug.panic("OoM", .{});
}

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

    const output = std.fmt.allocPrint(gpa, level_txt ++ prefix2 ++ format ++ "\n", args) catch |err| switch (err) {
        error.OutOfMemory => "Ran out of memory while printing output. Should never happen...",
    };
    defer gpa.free(output);
    addOutput2(output);
}

const std = @import("std");
