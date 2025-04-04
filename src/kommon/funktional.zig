const std = @import("std");

fn indexOfScalar(comptime T: type, slice: []const T, value: T, comptime eql: ?fn (a: T, b: T) bool) ?usize {
    const equal = eql orelse std.meta.eql;
    for (slice, 0..) |c, j| {
        if (equal(c, value)) return j;
    }
    return null;
}

pub fn indexOfString(slice: []const []const u8, value: []const u8) ?usize {
    return indexOfScalar([]const u8, slice, value, struct {
        // TODO: currying helper
        pub fn anon(a: []const u8, b: []const u8) bool {
            return std.mem.eql(u8, a, b);
        }
    }.anon);
}

pub fn single(arr: anytype) std.meta.Elem(@TypeOf(arr)) {
    std.debug.assert(arr.len == 1);
    return arr[0];
}

pub fn map(comptime map_fn: anytype, comptime in: []const SingleInputOf(map_fn)) [in.len]ReturnOf(map_fn) {
    var result: [in.len]ReturnOf(map_fn) = undefined;
    for (in, &result) |v, *target| {
        target.* = map_fn(v);
    }
    return result;
}

fn SingleInputOf(map_fn: anytype) type {
    return single(@typeInfo(@TypeOf(map_fn)).@"fn".params).type.?;
}

pub fn ReturnOf(map_fn: anytype) type {
    return @typeInfo(@TypeOf(map_fn)).@"fn".return_type.?;
}

pub fn mapWithIndex(comptime map_fn: anytype, comptime in: []const FirstInputOf(map_fn)) [in.len]ReturnOf(map_fn) {
    std.debug.assert(@typeInfo(@TypeOf(map_fn)).@"fn".params.len == 2);
    std.debug.assert(@typeInfo(@TypeOf(map_fn)).@"fn".params[1].type.? == usize);
    var result: [in.len]ReturnOf(map_fn) = undefined;
    for (in, &result, 0..) |v, *target, k| {
        target.* = map_fn(v, k);
    }
    return result;
}

fn FirstInputOf(map_fn: anytype) type {
    return @typeInfo(@TypeOf(map_fn)).@"fn".params[0].type.?;
}

fn SecondInputOf(map_fn: anytype) type {
    return @typeInfo(@TypeOf(map_fn)).@"fn".params[1].type.?;
}

fn ParamsLen(map_fn: anytype) usize {
    return @typeInfo(@TypeOf(map_fn)).@"fn".params.len;
}

pub fn fromCount(comptime n: usize, comptime map_fn: anytype) [n]ReturnOf(map_fn) {
    std.debug.assert(SingleInputOf(map_fn) == usize);
    var result: [n]ReturnOf(map_fn) = undefined;
    for (0..n, &result) |k, *target| {
        target.* = map_fn(k);
    }
    return result;
}

pub fn fromCountAndCtx(comptime n: usize, comptime map_fn: anytype, ctx: SecondInputOf(map_fn)) [n]ReturnOf(map_fn) {
    std.debug.assert(FirstInputOf(map_fn) == usize);
    std.debug.assert(ParamsLen(map_fn) == 2);
    var result: [n]ReturnOf(map_fn) = undefined;
    for (0..n, &result) |k, *target| {
        target.* = map_fn(k, ctx);
    }
    return result;
}

pub fn mapWithIndexAndCtx(comptime map_fn: anytype, comptime in: []const FirstInputOf(map_fn), ctx: anytype) [in.len]ReturnOf(map_fn) {
    std.debug.assert(@typeInfo(@TypeOf(map_fn)).@"fn".params.len == 3);
    std.debug.assert(@typeInfo(@TypeOf(map_fn)).@"fn".params[1].type.? == usize);
    std.debug.assert(@typeInfo(@TypeOf(map_fn)).@"fn".params[2].type.? == @TypeOf(ctx));
    var result: [in.len]ReturnOf(map_fn) = undefined;
    for (in, &result, 0..) |v, *target, k| {
        target.* = map_fn(v, k, ctx);
    }
    return result;
}

pub fn mapWithCtx(comptime map_fn: anytype, comptime in: []const SecondInputOf(map_fn), ctx: anytype) [in.len]ReturnOf(map_fn) {
    std.debug.assert(@typeInfo(@TypeOf(map_fn)).@"fn".params.len == 2);
    std.debug.assert(FirstInputOf(map_fn) == @TypeOf(ctx));
    var result: [in.len]ReturnOf(map_fn) = undefined;
    for (in, &result) |v, *target| {
        target.* = map_fn(ctx, v);
    }
    return result;
}

pub fn sum(comptime T: type, values: []const T) T {
    var result: T = 0;
    for (values) |v| {
        result += v;
    }
    return result;
}

// pub fn concatComptime(comptime strs: []const []const u8) [
//     sum(usize, &map(struct {
//         pub fn anon(v: []const u8) usize {
//             return v.len;
//         }
//     }.anon, strs))
// ]u8 {
pub fn concatComptime(comptime strs: []const []const u8) []const u8 {
    var result: []const u8 = "";
    for (strs) |s| {
        result = result ++ s;
    }
    return result;
}
