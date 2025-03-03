const std = @import("std");

fn single(arr: anytype) std.meta.Elem(@TypeOf(arr)) {
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

pub fn fromCount(comptime n: usize, comptime map_fn: anytype) [n]ReturnOf(map_fn) {
    std.debug.assert(SingleInputOf(map_fn) == usize);
    var result: [n]ReturnOf(map_fn) = undefined;
    for (0..n, &result) |k, *target| {
        target.* = map_fn(k);
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
