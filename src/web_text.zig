extern fn setOutput(output_ptr: [*]const u8, output_len: usize) void;

const gpa = std.heap.wasm_allocator;

var input_buffer: []u8 = &.{};

export fn allocInput(input_len: usize) [*]const u8 {
    if (input_len > input_buffer.len) {
        input_buffer = gpa.realloc(input_buffer, input_len) catch OoM();
    }
    return input_buffer.ptr;
}

export fn setInput(input_ptr: [*]const u8, input_len: usize) void {
    const input: []const u8 = @ptrCast(input_ptr[0..input_len]);
    setOutput(input.ptr, input.len);
}

fn OoM() noreturn {
    std.debug.panic("OoM", .{});
}

const std = @import("std");
