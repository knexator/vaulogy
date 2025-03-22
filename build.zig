const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // To use in other projects
    _ = b.addModule("kommon", .{
        .root_source_file = b.path("src/kommon/kommon.zig"),
        .target = target,
        .optimize = optimize,
    });

    const check = b.step("check", "Check if the project compiles");

    const test_filters = b.option([]const []const u8, "test-filter", "Skip tests that do not match any filter") orelse &[0][]const u8{};
    const test_step = b.step("test", "Run unit tests");

    // CLI tool
    {
        const exe = b.addExecutable(.{
            .name = "vaulogy",
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        });
        b.installArtifact(exe);

        const run_cmd = b.addRunArtifact(exe);
        run_cmd.step.dependOn(b.getInstallStep());
        if (b.args) |args| {
            run_cmd.addArgs(args);
        }
        const run_step = b.step("run", "Run the app");
        run_step.dependOn(&run_cmd.step);

        const exe_unit_tests = b.addTest(.{
            .root_source_file = b.path("src/tests.zig"),
            .target = target,
            .optimize = optimize,
            .filters = test_filters,
        });
        const dummy_exports = b.addStaticLibrary(.{
            .name = "dummy_exports",
            .root_source_file = b.path("src/tools/dummy_exports.zig"),
            .target = target,
            .optimize = optimize,
            // FUTURE TODO: put these back once there's no "TODOImplementWritingLibFiles" error
            // .use_llvm = optimize != .Debug,
            // .use_lld = optimize != .Debug,
        });
        exe_unit_tests.linkLibrary(dummy_exports);

        const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
        test_step.dependOn(&run_exe_unit_tests.step);

        // test solutions.txt against puzzles.txt
        const run_cmd_test = b.addRunArtifact(exe);
        run_cmd_test.addArg("score");
        run_cmd_test.addFileArg(b.path("levels/solutions.txt"));
        run_cmd_test.addFileArg(b.path("levels/puzzles.txt"));
        const run_cmd_test_step = b.step("levels", "Check solutions.txt against puzzles.txt");
        run_cmd_test_step.dependOn(&run_cmd_test.step);

        const exe_check = b.addExecutable(.{
            .name = "vaulogy",
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        });
        check.dependOn(&exe_check.step);
    }

    // Build the sdlgame
    {
        const sdlgame_exe = b.addExecutable(.{
            .name = "sdlgame",
            .root_source_file = b.path("src/sdl_platform.zig"),
            .target = target,
            .optimize = optimize,
        });
        const sdl_dep = b.dependency("sdl", .{
            .target = target,
            .optimize = .ReleaseFast, // TODO: hardcoded to avoid sdl's undefined behaviour bugs
        });
        const sdl_lib = sdl_dep.artifact("SDL3");
        sdlgame_exe.linkLibrary(sdl_lib);
        b.installArtifact(sdlgame_exe);

        const run_sdlgame_cmd = b.addRunArtifact(sdlgame_exe);
        run_sdlgame_cmd.step.dependOn(b.getInstallStep());
        const run_sdlgame = b.step("run_sdl", "Run the sdl game");
        run_sdlgame.dependOn(&run_sdlgame_cmd.step);

        const sdlgame_exe_check = b.addExecutable(.{
            .name = "sdlgame",
            .root_source_file = b.path("src/sdl_platform.zig"),
            .target = target,
            .optimize = optimize,
        });
        sdlgame_exe_check.linkLibrary(sdl_lib);
        check.dependOn(&sdlgame_exe_check.step);
    }

    // Building the webgame
    const webgame_install_dir = std.Build.InstallDir{ .custom = "dist" };
    {
        const webgame_wasm = b.addExecutable(
            .{
                .name = "main",
                // .root_source_file = b.path("src/webgame.zig"),
                .root_source_file = b.path("src/web_platform.zig"),
                .target = b.resolveTargetQuery(.{
                    .cpu_arch = .wasm32,
                    .os_tag = .freestanding,
                }),
                .optimize = optimize,
                // FUTURE TODO: put these back once https://github.com/ziglang/zig/issues/22617 is fixed
                // .use_llvm = optimize != .Debug,
                // .use_lld = optimize != .Debug,
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

        const webgame_wasm_check = b.addExecutable(
            .{
                .name = "main",
                .root_source_file = b.path("src/web_platform.zig"),
                .target = b.resolveTargetQuery(.{
                    .cpu_arch = .wasm32,
                    .os_tag = .freestanding,
                }),
                .optimize = optimize,
            },
        );
        check.dependOn(&webgame_wasm_check.step);
    }

    // dev server for testing the webgame
    {
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
}
