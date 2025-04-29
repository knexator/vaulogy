const presenter = @import("presenter.zig");
const funk = @import("kommon/funktional.zig");
const BuiltinLevel = presenter.BuiltinLevel;
const Sample = presenter.Sample;
const core = @import("main.zig");
const Sexpr = core.Sexpr;

const Vals = struct {
    pub const Hermes: *const Sexpr = &Sexpr.doLit("Hermes");
    pub const Mercury: *const Sexpr = &Sexpr.doLit("Mercury");
    pub const Aphrodite: *const Sexpr = &Sexpr.doLit("Aphrodite");
    pub const Venus: *const Sexpr = &Sexpr.doLit("Venus");
    pub const Ares: *const Sexpr = &Sexpr.doLit("Ares");
    pub const Mars: *const Sexpr = &Sexpr.doLit("Mars");
    pub const Zeus: *const Sexpr = &Sexpr.doLit("Zeus");
    pub const Jupiter: *const Sexpr = &Sexpr.doLit("Jupiter");

    pub const top: *const Sexpr = &Sexpr.doLit("top");
    pub const bottom: *const Sexpr = &Sexpr.doLit("bottom");

    pub fn wrapped(comptime v: *const Sexpr) *const Sexpr {
        return &Sexpr.doPair(&Sexpr.doPair(Vals.top, v), Vals.bottom);
    }

    pub fn planetFromOlympian(input: *const Sexpr) ?*const Sexpr {
        if (input.equals(Vals.Hermes)) return Vals.Mercury;
        if (input.equals(Vals.Aphrodite)) return Vals.Venus;
        if (input.equals(Vals.Ares)) return Vals.Mars;
        if (input.equals(Vals.Zeus)) return Vals.Jupiter;
        return null;
    }

    pub fn toList(comptime values: []const *const Sexpr) *const Sexpr {
        @setEvalBranchQuota(10_000 + values.len * 2);
        if (values.len == 0) return Sexpr.builtin.nil;
        return &Sexpr.doPair(values[0], toList(values[1..]));
    }

    pub fn toPeano(comptime n: usize) *const Sexpr {
        @setEvalBranchQuota(1100 + n * 2);
        if (n == 0) return Sexpr.builtin.nil;
        return &Sexpr.doPair(Sexpr.builtin.true, toPeano(n - 1));
    }
};

pub const builtin_levels: []const BuiltinLevel = &.{
    .{ .fnk_name = &Sexpr.doLit("planetFromOlympian"), .manual_samples = &.{
        .{ .input = &Sexpr.doLit("Hermes"), .output = &Sexpr.doLit("Mercury") },
        .{ .input = &Sexpr.doLit("Aphrodite"), .output = &Sexpr.doLit("Venus") },
        .{ .input = &Sexpr.doLit("Ares"), .output = &Sexpr.doLit("Mars") },
        .{ .input = &Sexpr.doLit("Zeus"), .output = &Sexpr.doLit("Jupiter") },
    }, .description = "The simplest Vau: a hardcoded translation", .premade_solution = 
    \\planetFromOlympian {
    \\  Hermes -> Mercury;
    \\  // Aphrodite -> Venus;
    \\  Aphrodite -> nil;
    \\  Ares -> Mars;
    \\  Zeus -> Jupiter;
    \\}
    , .tutorial_state = .first_level },
    .{ .fnk_name = &Sexpr.doLit("wrapOlympian"), .manual_samples = &.{
        .{ .input = &Sexpr.doLit("Hermes"), .output = &Sexpr.doPair(&Sexpr.doPair(&Sexpr.doLit("top"), &Sexpr.doLit("Hermes")), &Sexpr.doLit("bottom")) },
        .{ .input = &Sexpr.doLit("Aphrodite"), .output = &Sexpr.doPair(&Sexpr.doPair(&Sexpr.doLit("top"), &Sexpr.doLit("Aphrodite")), &Sexpr.doLit("bottom")) },
        .{ .input = &Sexpr.doLit("Ares"), .output = &Sexpr.doPair(&Sexpr.doPair(&Sexpr.doLit("top"), &Sexpr.doLit("Ares")), &Sexpr.doLit("bottom")) },
        .{ .input = &Sexpr.doLit("Zeus"), .output = &Sexpr.doPair(&Sexpr.doPair(&Sexpr.doLit("top"), &Sexpr.doLit("Zeus")), &Sexpr.doLit("bottom")) },
    }, .description = "This Vau takes unstable Data and wraps it safely", .premade_solution = 
    \\wrapOlympian {
    \\  @x -> (@x . @x);
    \\  // Hermes -> ((top . Hermes) . bottom);
    \\  // Aphrodite -> ((top . Aphrodite) . bottom);
    \\  // Ares -> ((top . Ares) . bottom);
    \\  // Zeus -> ((top . Zeus) . bottom);
    \\}
    , .tutorial_state = .second_level },
    .{ .fnk_name = &Sexpr.doLit("planetFromWrappedOlympian"), .manual_samples = &.{
        .{ .input = Vals.wrapped(Vals.Hermes), .output = Vals.Mercury },
        .{ .input = Vals.wrapped(Vals.Aphrodite), .output = Vals.Venus },
        .{ .input = Vals.wrapped(Vals.Ares), .output = Vals.Mars },
        .{ .input = Vals.wrapped(Vals.Zeus), .output = Vals.Jupiter },
    }, .description = "Unwrap the unstable Data and then translate it", .premade_solution = 
    \\planetFromWrappedOlympian {
    \\ ((top . @v) . bottom) -> @v;
    \\ // ((top . Aphrodite) . bottom) -> Venus;
    \\ // ((top . Ares) . bottom) -> Mars;
    \\ // ((top . Zeus) . bottom) -> Jupiter;
    \\}
    , .tutorial_state = .third_level },
    .{ .fnk_name = &Sexpr.doLit("wrappedPlanetFromOlympian"), .manual_samples = &.{
        .{ .input = Vals.Hermes, .output = Vals.wrapped(Vals.Mercury) },
        .{ .input = Vals.Aphrodite, .output = Vals.wrapped(Vals.Venus) },
        .{ .input = Vals.Ares, .output = Vals.wrapped(Vals.Mars) },
        .{ .input = Vals.Zeus, .output = Vals.wrapped(Vals.Jupiter) },
    }, .description = "Translate the Data and then wrap it", .premade_solution = 
    \\wrappedPlanetFromOlympian {
    \\ @v -> planetFromOlympian: @v {
    \\   Mercury -> ((top . Mercury) . bottom);
    \\   // Venus -> ((top . Venus) . bottom);
    \\   Ares -> ((top . Ares) . bottom);
    \\ }
    \\ // Hermes -> ((top . Mercury) . bottom);
    \\ // Aphrodite -> ((top . Venus) . bottom);
    \\ // Ares -> ((top . Mars) . bottom);
    \\ // Zeus -> ((top . Jupiter) . bottom);
    \\}
    , .tutorial_state = .fourth_level },
    .{ .fnk_name = &Sexpr.doLit("olympianToBoth"), .manual_samples = &funk.map(struct {
        pub fn anon(comptime v: *const Sexpr) Sample {
            return .{ .input = v, .output = &Sexpr.doPair(
                v,
                Vals.planetFromOlympian(v).?,
            ) };
        }
    }.anon, &.{
        Vals.Hermes,
        Vals.Aphrodite,
        Vals.Ares,
        Vals.Zeus,
    }), .description = "Show both the input & the result", .premade_solution = null, .tutorial_state = .fifth_level },
    .{
        .fnk_name = &Sexpr.doLit("planetPairFromOlympianPair"),
        .manual_samples = &funk.map(struct {
            pub fn anon(comptime v: *const Sexpr) Sample {
                return .{ .input = v, .output = &Sexpr.doPair(
                    Vals.planetFromOlympian(v.pair.left).?,
                    Vals.planetFromOlympian(v.pair.right).?,
                ) };
            }
        }.anon, &.{
            &Sexpr.doPair(Vals.Hermes, Vals.Aphrodite),
            &Sexpr.doPair(Vals.Ares, Vals.Zeus),
            &Sexpr.doPair(Vals.Ares, Vals.Ares),
            &Sexpr.doPair(Vals.Zeus, Vals.Hermes),
            &Sexpr.doPair(Vals.Aphrodite, Vals.Hermes),
            &Sexpr.doPair(Vals.Zeus, Vals.Ares),
        }),
        .description = "Translate two Datas at once",
        .premade_solution = null,
        .tutorial_state = .not_yet_creating_vaus_or_lists,
    },
    .{
        .fnk_name = &Sexpr.doLit("planetListFromOlympianList"),
        .manual_samples = &funk.map(struct {
            pub fn anon(comptime values: []const *const Sexpr) Sample {
                return .{ .input = Vals.toList(values), .output = toMappedList(values) };
            }
            fn toMappedList(comptime values: []const *const Sexpr) *const Sexpr {
                if (values.len == 0) return Sexpr.builtin.nil;
                return &Sexpr.doPair(Vals.planetFromOlympian(values[0]).?, toMappedList(values[1..]));
            }
        }.anon, &.{
            &.{},
            &.{Vals.Hermes},
            &.{Vals.Aphrodite},
            &.{Vals.Ares},
            &.{Vals.Zeus},
            &.{ Vals.Hermes, Vals.Aphrodite },
            &.{ Vals.Ares, Vals.Zeus },
            &.{ Vals.Aphrodite, Vals.Aphrodite },
            &.{ Vals.Zeus, Vals.Ares },
            &.{ Vals.Zeus, Vals.Aphrodite, Vals.Ares },
            &.{ Vals.Hermes, Vals.Zeus, Vals.Hermes },
            &.{ Vals.Zeus, Vals.Zeus, Vals.Zeus, Vals.Zeus, Vals.Zeus },
            &.{ Vals.Hermes, Vals.Aphrodite, Vals.Ares, Vals.Zeus, Vals.Zeus, Vals.Ares, Vals.Aphrodite, Vals.Hermes },
        }),
        .description = "Translate a list of Datas",
        .premade_solution = null,
        .tutorial_state = .not_yet_creating_vaus_or_lists,
    },
    .{
        .fnk_name = &Sexpr.doLit("hasAres?"),
        .manual_samples = &funk.map(struct {
            pub fn anon(comptime values: []const *const Sexpr) Sample {
                const has_ares: bool = for (values) |v| {
                    if (v.equals(Vals.Ares)) break true;
                } else false;
                return .{ .input = Vals.toList(values), .output = Sexpr.fromBool(has_ares) };
            }
        }.anon, &.{
            &.{ Vals.Ares, Vals.Zeus },
            &.{ Vals.Hermes, Vals.Aphrodite, Vals.Zeus },
            &.{ Vals.Hermes, Vals.Ares, Vals.Aphrodite, Vals.Zeus },
            &.{ Vals.Zeus, Vals.Aphrodite, Vals.Aphrodite, Vals.Hermes },
            &.{Vals.Ares},
            &.{ Vals.Hermes, Vals.Zeus, Vals.Zeus },
            &.{},
            &.{ Vals.Hermes, Vals.Aphrodite, Vals.Zeus, Vals.Ares, Vals.Zeus },
            &.{ Vals.Aphrodite, Vals.Zeus, Vals.Zeus, Vals.Hermes, Vals.Aphrodite },
        }),
        .description = "Check if there is a blue value",
        .premade_solution = null,
        .tutorial_state = .intro_to_list_viewer,
    },
    .{
        .fnk_name = &Sexpr.doLit("peanoSum"),
        .manual_samples = &funk.map(struct {
            pub fn anon(comptime vs: [2]usize) Sample {
                const a = vs[0];
                const b = vs[1];
                return .{
                    .input = &Sexpr.doPair(Vals.toPeano(a), Vals.toPeano(b)),
                    .output = Vals.toPeano(a + b),
                };
            }
        }.anon, &.{
            .{ 1, 0 },
            .{ 0, 1 },
            .{ 1, 1 },
            .{ 2, 0 },
            .{ 0, 2 },
            .{ 0, 0 },
            .{ 3, 2 },
            .{ 7, 2 },
            .{ 3, 6 },
        }),
        .description = "Sum two numbers",
        .premade_solution = null,
        .tutorial_state = .not_yet_creating_vaus,
    },
    .{
        .fnk_name = &Sexpr.doLit("peanoMul"),
        .manual_samples = &funk.map(struct {
            pub fn anon(comptime vs: [2]usize) Sample {
                const a = vs[0];
                const b = vs[1];
                return .{
                    .input = &Sexpr.doPair(Vals.toPeano(a), Vals.toPeano(b)),
                    .output = Vals.toPeano(a * b),
                };
            }
        }.anon, &.{
            .{ 1, 1 },
            .{ 3, 1 },
            .{ 1, 3 },
            .{ 3, 2 },
            .{ 2, 3 },
            .{ 0, 3 },
            .{ 0, 5 },
            .{ 7, 0 },
            .{ 9, 2 },
            .{ 3, 6 },
        }),
        .description = "Multiply two numbers",
        .premade_solution = null,
        .tutorial_state = .not_yet_creating_vaus,
    },
    .{
        .fnk_name = &Sexpr.doLit("reverse"),
        .manual_samples = &funk.map(struct {
            pub fn anon(comptime values: []const *const Sexpr) Sample {
                return .{ .input = Vals.toList(values), .output = toReversedList(values) };
            }
            fn toReversedList(comptime values: []const *const Sexpr) *const Sexpr {
                if (values.len == 0) return Sexpr.builtin.nil;
                return &Sexpr.doPair(values[values.len - 1], toReversedList(values[0 .. values.len - 1]));
            }
        }.anon, &.{
            &.{ Vals.Hermes, Vals.Aphrodite },
            &.{ Vals.Ares, Vals.Zeus },
            &.{ Vals.Zeus, Vals.Aphrodite, Vals.Ares },
            &.{ Vals.Hermes, Vals.Zeus, Vals.Zeus },
            &.{ Vals.Zeus, Vals.Zeus, Vals.Zeus, Vals.Aphrodite, Vals.Zeus },
            &.{ Vals.Hermes, Vals.Aphrodite, Vals.Ares, Vals.Zeus },
        }),
        .description = "Reverse a list",
        .premade_solution = null,
        .tutorial_state = .intro_to_create_vaus,
    },
};
