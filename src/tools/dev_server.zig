const builtin = @import("builtin");
const std = @import("std");

const mime = @import("mime");

// Stole code from https://github.com/ziglang/zig/blob/master/lib/compiler/std-docs.zig

pub fn main() !void {
    const addr = try std.net.Address.parseIp("127.0.0.1", 3000);
    var http_server = try addr.listen(.{ .reuse_address = true });

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);

    var args = try std.process.argsWithAllocator(gpa.allocator());
    defer args.deinit();
    std.debug.assert(args.skip());
    const static_dir_name = args.next().?;
    std.debug.assert(!args.skip());

    const static_dir = try std.fs.openDirAbsolute(static_dir_name, .{});

    const port = http_server.listen_address.in.getPort();
    const url_with_newline = try std.fmt.allocPrint(gpa.allocator(), "http://127.0.0.1:{d}/\n", .{port});
    defer gpa.allocator().free(url_with_newline);
    try std.io.getStdOut().writeAll(url_with_newline);
    openBrowserTab(gpa.allocator(), url_with_newline[0 .. url_with_newline.len - 1 :'\n']) catch |err| {
        std.log.err("unable to open browser: {s}", .{@errorName(err)});
    };

    while (true) {
        const connection = try http_server.accept();

        _ = std.Thread.spawn(.{}, accept, .{ connection, gpa.allocator(), static_dir }) catch |err| {
            std.log.err("unable to accept connection: {s}", .{@errorName(err)});
            connection.stream.close();
            continue;
        };
    }
}

fn accept(connection: std.net.Server.Connection, allocator: std.mem.Allocator, static_dir: std.fs.Dir) !void {
    defer connection.stream.close();

    var read_buffer: [8000]u8 = undefined;
    var server = std.http.Server.init(connection, &read_buffer);
    while (server.state == .ready) {
        var request: std.http.Server.Request = server.receiveHead() catch |err| switch (err) {
            error.HttpConnectionClosing => return,
            else => {
                std.log.err("closing http connection: {s}", .{@errorName(err)});
                return;
            },
        };

        if (std.mem.startsWith(u8, request.head.target, "/@mtime/")) {
            const file_path = request.head.target["/@mtime/".len..];
            const stat = static_dir.statFile(file_path) catch |err| switch (err) {
                error.FileNotFound => {
                    std.log.err("could not find the request file {s}\n", .{file_path});
                    try request.respond("can't find that file", .{ .status = .not_found });
                    return;
                },
                else => {
                    std.log.err("could not open the request file {s} due to error {s}\n", .{ file_path, @errorName(err) });
                    try request.respond("internal error", .{ .status = .internal_server_error });
                    return;
                },
            };
            try request.respond(std.mem.asBytes(&stat.mtime)[0..4], .{
                .extra_headers = &.{
                    .{ .name = "content-type", .value = "application/octet-stream" },
                    cache_control_header,
                },
            });
        } else {
            const file_path = if (std.mem.eql(u8, request.head.target, "/")) "/index.html" else request.head.target;
            std.debug.assert(file_path[0] == '/');

            const cur_file = static_dir.openFile(file_path[1..], .{}) catch |err| switch (err) {
                error.FileNotFound => {
                    std.log.err("could not find the request file {s}\n", .{file_path});
                    try request.respond("can't find that file", .{ .status = .not_found });
                    return;
                },
                else => {
                    std.log.err("could not open the request file {s} due to error {s}\n", .{ file_path, @errorName(err) });
                    try request.respond("internal error", .{ .status = .internal_server_error });
                    return;
                },
            };
            const contents = try cur_file.readToEndAlloc(allocator, std.math.maxInt(usize));
            defer allocator.free(contents);
            const ext = std.fs.path.extension(std.fs.path.basename(file_path));
            const mime_type = mime.extension_map.get(ext) orelse .@"application/octet-stream";
            try request.respond(contents, .{
                .extra_headers = &.{
                    .{ .name = "content-type", .value = @tagName(mime_type) },
                    cache_control_header,
                },
            });
        }
    }
}

const cache_control_header: std.http.Header = .{
    .name = "cache-control",
    .value = "max-age=0, must-revalidate",
};

fn openBrowserTab(gpa: std.mem.Allocator, url: []const u8) !void {
    // Until https://github.com/ziglang/zig/issues/19205 is implemented, we
    // spawn a thread for this child process.
    _ = try std.Thread.spawn(.{}, openBrowserTabThread, .{ gpa, url });
}

fn openBrowserTabThread(gpa: std.mem.Allocator, url: []const u8) !void {
    const main_exe = switch (builtin.os.tag) {
        .windows => "explorer",
        .macos => "open",
        else => "xdg-open",
    };
    var child = std.process.Child.init(&.{ main_exe, url }, gpa);
    child.stdin_behavior = .Ignore;
    child.stdout_behavior = .Ignore;
    child.stderr_behavior = .Ignore;
    try child.spawn();
    _ = try child.wait();
}
