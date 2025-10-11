extern fn setOutput(output_ptr: [*]const u8, output_len: usize) void;

fn setOutput2(output: []const u8) void {
    setOutput(output.ptr, output.len);
}

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

    var output_buffer: std.ArrayList(u8) = .init(gpa);
    defer output_buffer.deinit();

    @import("main.zig").textgame(
        gpa,
        input,
        output_buffer.writer().any(),
    ) catch {
        setOutput2("Unhandled error!");
        return;
    };

    setOutput2(output_buffer.items);
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

    var buf: [1000]u8 = undefined;
    const res = std.fmt.bufPrint(&buf, level_txt ++ prefix2 ++ format ++ "\n", args) catch {
        setOutput2("RAN OUT OF LOG BUFFER! the log started with:\n");
        setOutput2(&buf);
        return;
    };
    setOutput2(res);
}

const std = @import("std");
