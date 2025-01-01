const std = @import("std");
const MemoryPool = std.heap.MemoryPool;

const Sexpr = @import("main.zig").Sexpr;
const Pair = @import("main.zig").Pair;
const Atom = @import("main.zig").Atom;
const Fnk = @import("main.zig").Fnk;
const MatchCases = @import("main.zig").MatchCases;
const MatchCaseDefinition = @import("main.zig").MatchCaseDefinition;

fn parseSexpr(input: *[]const u8, pool: *MemoryPool(Sexpr)) !*const Sexpr {
    const result = try parseSexprTrue(input.*, pool);
    input.* = result.rest;
    return result.sexpr;
}

pub fn parseSingleSexpr(input: []const u8, pool: *MemoryPool(Sexpr)) !*const Sexpr {
    var result = try parseSexprTrue(input, pool);
    skipWhitespace(&result.rest);
    if (result.rest.len > 0) return error.BAD_INPUT;
    return result.sexpr;
}

fn parseSexprTrue(input: []const u8, pool: *MemoryPool(Sexpr)) error{ OutOfMemory, BAD_INPUT }!struct { sexpr: *const Sexpr, rest: []const u8 } {
    var rest = input;
    skipWhitespace(&rest);
    if (rest[0] == '(') {
        const asdf = try parseSexprInsideParens(rest[1..], pool);
        return .{ .sexpr = asdf.sexpr, .rest = asdf.rest };
    }
    const asdf = try parseAtom(rest);
    const res = try pool.create();
    if (asdf.is_var) {
        res.* = Sexpr{ .atom_var = asdf.atom };
    } else {
        res.* = Sexpr{ .atom_lit = asdf.atom };
    }
    return .{ .sexpr = res, .rest = asdf.rest };
}

fn parseSexprInsideParens(input: []const u8, pool: *MemoryPool(Sexpr)) !struct { sexpr: *const Sexpr, rest: []const u8 } {
    var rest = input;
    skipWhitespace(&rest);
    if (rest[0] == ')') {
        return .{ .sexpr = &Sexpr.nil, .rest = rest[1..] };
    }
    if (rest[0] == '.') {
        const final_asdf = try parseSexprTrue(rest[1..], pool);
        rest = final_asdf.rest;
        skipWhitespace(&rest);
        if (rest[0] != ')') return error.BAD_INPUT;
        return .{ .sexpr = final_asdf.sexpr, .rest = rest[1..] };
    }
    const first_asdf = try parseSexprTrue(rest, pool);
    const rest_asdf = try parseSexprInsideParens(first_asdf.rest, pool);

    const res = try pool.create();
    res.* = Sexpr{ .pair = Pair{ .left = first_asdf.sexpr, .right = rest_asdf.sexpr } };

    return .{ .sexpr = res, .rest = rest_asdf.rest };
}

fn parseAtom(input: []const u8) !struct { atom: Atom, is_var: bool, rest: []const u8 } {
    const word_breaks = .{ '(', ')', ':', '.', ';' } ++ std.ascii.whitespace;
    const rest = std.mem.trimLeft(u8, input, &std.ascii.whitespace);
    const word_end = std.mem.indexOfAnyPos(u8, rest, 0, &word_breaks) orelse rest.len;
    const is_variable = rest[0] == '@';
    return .{
        .atom = Atom{ .value = rest[(if (is_variable) 1 else 0)..word_end] },
        .is_var = is_variable,
        .rest = rest[word_end..],
    };
}

pub fn parseFnk(input: *[]const u8, pool: *MemoryPool(Sexpr), allocator: std.mem.Allocator) !Fnk {
    const result = try parseFnkTrue(input.*, pool, allocator);
    input.* = result.rest;
    return result.fnk;
}

fn parseFnkTrue(input: []const u8, pool: *MemoryPool(Sexpr), allocator: std.mem.Allocator) !struct { fnk: Fnk, rest: []const u8 } {
    var rest = input;
    skipWhitespace(&rest);
    const name = try parseSexpr(&rest, pool);
    skipWhitespace(&rest);
    // try parseChar(&rest, ':');
    // skipWhitespace(&rest);
    try parseChar(&rest, '{');
    const cases = try parseMatchCases(&rest, pool, allocator);
    skipWhitespace(&rest);
    return .{ .fnk = Fnk{ .name = name, .body = .{ .cases = cases } }, .rest = rest };
}

fn parseMatchCases(input: *[]const u8, pool: *MemoryPool(Sexpr), allocator: std.mem.Allocator) !MatchCases {
    var list = std.ArrayListUnmanaged(MatchCaseDefinition){};
    skipWhitespace(input);
    while (!parseCharIfPossible(input, '}')) {
        const pattern = try parseSexpr(input, pool);
        skipWhitespace(input);
        try parseChar(input, '-');
        try parseChar(input, '>');
        skipWhitespace(input);
        const fn_name_or_template = try parseSexpr(input, pool);
        skipWhitespace(input);
        var fn_name: *const Sexpr = undefined;
        var template: *const Sexpr = undefined;
        if (parseCharIfPossible(input, ':')) {
            fn_name = fn_name_or_template;
            template = try parseSexpr(input, pool);
            skipWhitespace(input);
        } else {
            fn_name = &Sexpr.identity;
            template = fn_name_or_template;
        }
        var next: ?MatchCases = undefined;
        if (parseCharIfPossible(input, ';')) {
            next = null;
        } else {
            try parseChar(input, '{');
            next = try parseMatchCases(input, pool, allocator);
        }
        skipWhitespace(input);

        try list.append(allocator, .{
            .pattern = pattern,
            .fn_name = fn_name,
            .template = template,
            .next = next,
        });
    }
    return list;
}

pub fn skipWhitespace(input: *[]const u8) void {
    input.* = std.mem.trimLeft(u8, input.*, &std.ascii.whitespace);
    while (std.mem.startsWith(u8, input.*, "//")) {
        input.* = input.*[(std.mem.indexOfScalar(u8, input.*, '\n').? + 1)..];
        input.* = std.mem.trimLeft(u8, input.*, &std.ascii.whitespace);
    }
}

fn parseChar(input: *[]const u8, comptime expected: u8) !void {
    if (input.*[0] != expected) return error.BAD_INPUT;
    input.* = input.*[1..];
}

fn parseCharIfPossible(input: *[]const u8, comptime expected: u8) bool {
    if (input.*[0] != expected) return false;
    input.* = input.*[1..];
    return true;
}
