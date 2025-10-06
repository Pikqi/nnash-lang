const std = @import("std");
const mvzr = @import("mvzr");

const debug = std.debug;
const Allocator = std.mem.Allocator;
const testing = std.testing;
const test_alloc = testing.allocator;

const macro_regex = mvzr.compile("DEF:\\w+:([^:s])+:") orelse unreachable;
const trim_regex = mvzr.compile("[\n\r]+") orelse unreachable;

const Replace = struct {
    start: usize,
    end: usize,
    new_str: []const u8,
};

fn replacePQOrderByStart(ctx: void, a: Replace, b: Replace) std.math.Order {
    _ = ctx;
    return std.math.order(a.start, b.start);
}
const ReplacePQ = std.PriorityQueue(Replace, void, replacePQOrderByStart);

const PreprocessorOpts = struct {
    trim: bool = true,
};

pub fn preprocessor(alloc: Allocator, input: []const u8, opts: PreprocessorOpts) ![]u8 {
    var iter = macro_regex.iterator(input);
    var map = std.StringHashMap([]const u8).init(alloc);
    var replace_list = ReplacePQ.init(alloc, {});
    defer replace_list.clearAndFree();

    defer map.clearAndFree();

    while (iter.next()) |match| {
        var macro_iter = std.mem.tokenizeAny(u8, match.slice, ":");
        // skip "DEF"
        _ = macro_iter.next();
        const macro_name = macro_iter.next() orelse return error.BAD_MACRO_NAME;
        const macro_value = macro_iter.next() orelse return error.BAD_MACRO_VALUE;
        try map.put(macro_name, macro_value);
        try replace_list.add(.{ .start = match.start, .end = match.end, .new_str = "" });
    }
    if (opts.trim) {
        var trim_iter = trim_regex.iterator(input);

        while (trim_iter.next()) |match| {
            try replace_list.add(.{ .start = match.start, .end = match.end, .new_str = "" });
        }
    }
    // dumpMacroMap(map);

    var map_iter = map.iterator();
    while (map_iter.next()) |entry| {
        const replace_value = entry.value_ptr.*;
        const expression = try std.mem.concat(alloc, u8, &.{ "\\$", entry.key_ptr.* });
        defer alloc.free(expression);

        const match_regex = mvzr.compile(expression) orelse return error.FailToCompileEXP;
        var it = match_regex.iterator(input);
        while (it.next()) |match| {
            try replace_list.add(.{ .start = match.start, .end = match.end, .new_str = replace_value });
        }
    }

    const len_diff = calculateDifference(&replace_list);
    const result_len = @as(isize, @intCast(input.len)) + len_diff;
    const result = try alloc.alloc(u8, @intCast(result_len));
    if (replace_list.items.len == 0) {
        std.debug.assert(result.len == input.len);
        @memcpy(result, input);
        return result;
    }

    var out: usize = 0;
    var in: usize = 0;
    while (replace_list.removeOrNull()) |replace| {
        const bytes_2_copy = replace.start - in;

        // Copy all the bytes before the replace
        @memcpy(result[out .. out + bytes_2_copy], input[in .. in + bytes_2_copy]);
        out += bytes_2_copy;
        in += bytes_2_copy;
        //Copy the bytes that need the replace
        in += replace.end - replace.start;
        const l = replace.new_str.len;
        @memcpy(result[out .. out + l], replace.new_str);
        out += l;
    }
    //Copy all leftover bytes after the last replace
    const left_over_bytes = input.len - in;
    if (left_over_bytes > result.len - out) {
        return error.NotEnoughSpaceInResult;
    }
    @memcpy(result[out .. out + left_over_bytes], input[in .. in + left_over_bytes]);
    out += left_over_bytes;
    if (result_len != out) {
        std.log.warn("Result buffer not fully written. out:{d}, result_len:{d}", .{ out, result_len });
    }

    return result;
}

fn calculateDifference(replace_list: *const ReplacePQ) isize {
    var diff: isize = 0;
    for (replace_list.items) |r| {
        diff -= @intCast(r.end - r.start);
        diff += @intCast(r.new_str.len);
    }
    return diff;
}

test "preprocessor" {
    const input_file = @embedFile("./test_files/preprocessor/macros_1.txt");
    const sol_file = @embedFile("./test_files/preprocessor/macros1_sol.txt");
    const sol_slice = sol_file[0 .. sol_file.len - 1];
    const processed = try preprocessor(test_alloc, input_file, .{ .trim = true });
    defer test_alloc.free(processed);

    try testing.expectEqualStrings(sol_slice, processed);
}

test "preprocessor no macros" {
    const input_file = @embedFile("./test_files/preprocessor/no_macros.txt");
    const processed = try preprocessor(test_alloc, input_file, .{ .trim = false });
    defer test_alloc.free(processed);

    try testing.expectEqualStrings(input_file, processed);
}

fn dumpMacroMap(map: std.StringHashMap([]const u8)) void {
    var iter = map.iterator();
    while (iter.next()) |entry| {
        std.debug.print("{s} = {s}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
    }
}
