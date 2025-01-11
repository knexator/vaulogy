const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "vaulogy",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        // .use_llvm = optimize != .Debug,
        // .use_lld = optimize != .Debug,
    });

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(exe);

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const test_filters = b.option([]const []const u8, "test-filter", "Skip tests that do not match any filter") orelse &[0][]const u8{};
    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/tests.zig"),
        .target = target,
        .optimize = optimize,
        .filters = test_filters,
        .use_llvm = optimize != .Debug,
        .use_lld = optimize != .Debug,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    // Building the webgame
    const webgame_install_dir = std.Build.InstallDir{ .custom = "dist" };
    const webgame_wasm = b.addExecutable(
        .{
            .name = "main",
            .root_source_file = b.path("src/webgame.zig"),
            .target = b.resolveTargetQuery(.{
                .cpu_arch = .wasm32,
                .os_tag = .freestanding,
            }),
            .optimize = optimize,
            .use_llvm = optimize != .Debug,
            .use_lld = optimize != .Debug,
        },
    );

    {
        // taken from https://github.com/daneelsan/minimal-zig-wasm-canvas/blob/master/build.zig
        webgame_wasm.global_base = 6560;
        webgame_wasm.entry = .disabled;
        webgame_wasm.rdynamic = true;
        webgame_wasm.export_memory = true;
        webgame_wasm.stack_size = std.wasm.page_size;
    }

    const compile_wasm = b.addInstallArtifact(webgame_wasm, .{
        .dest_dir = .{ .override = webgame_install_dir },
    });
    b.getInstallStep().dependOn(&compile_wasm.step);
    const copy_static_files = b.addInstallDirectory(.{
        .install_dir = webgame_install_dir,
        .install_subdir = "",
        .source_dir = b.path("static"),
    });
    b.getInstallStep().dependOn(&copy_static_files.step);

    const generate_keycodes = b.addExecutable(.{
        .name = "generate_keycodes",
        .root_source_file = b.path("src/tools/generate_keycodes_js.zig"),
        .target = b.graph.host,
    });
    const generate_keycodes_step = b.addRunArtifact(generate_keycodes);
    const output = generate_keycodes_step.addOutputFileArg("keycodes.js");
    b.getInstallStep().dependOn(&b.addInstallFileWithDir(output, webgame_install_dir, "keycodes.js").step);

    // dev server for testing the webgame
    const dev_server_exe = b.addExecutable(.{
        .name = "dev_server",
        .root_source_file = b.path("src/tools/dev_server.zig"),
        .target = target,
        .optimize = optimize,
    });
    dev_server_exe.root_module.addImport("mime", b.dependency("mime", .{
        .target = target,
        .optimize = optimize,
    }).module("mime"));
    const run_dev_server = b.addRunArtifact(dev_server_exe);
    run_dev_server.step.dependOn(b.getInstallStep());
    run_dev_server.addArg(b.getInstallPath(webgame_install_dir, ""));
    const run_dev_server_step = b.step("dev-pure-zig", "Run the dev server");
    run_dev_server_step.dependOn(&run_dev_server.step);

    // dev server for testing the webgame, with WebSockets + hot reloading
    // FUTURE TODO: remove this step if zig gets a fs.watch equivalent
    const run_dev_server_cmd = b.addSystemCommand(&.{"bun"});
    run_dev_server_cmd.step.dependOn(b.getInstallStep());
    run_dev_server_cmd.addFileArg(b.path("src/tools/dev_server.js"));
    run_dev_server_cmd.addArg(b.getInstallPath(webgame_install_dir, ""));
    const run_dev_server_cmd_step = b.step("dev", "Run the dev server");
    run_dev_server_cmd_step.dependOn(&run_dev_server_cmd.step);
}
