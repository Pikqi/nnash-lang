const std = @import("std");
const Writer = std.Io.Writer;
const WriterError = Writer.Error;

const ast = @import("ast.zig");

pub fn dump(writer: *Writer, program: ast.Program) !void {
    for (program.top_items.items) |item| {
        try dumpTopItem(writer, item, 1);
    }
    try writer.flush();
}

fn dumpTopItem(writer: *Writer, item: *ast.TopItem, depth: usize) !void {
    try writer.print("TopItem\n", .{});
    try writeIndent(writer, depth);
    switch (item.*) {
        .block => |block| try dumpBlock(writer, block, depth + 1),
        .simple => |simple| try dumpSimple(writer, simple, depth + 1),
    }
}

fn dumpSimple(writer: *Writer, item: *ast.TopSimpleItem, depth: usize) !void {
    try writer.print("TopSimple\n", .{});
    try writeIndent(writer, depth);
    switch (item.*) {
        .varDeclaration => |vd| try dumpVarDeclaration(writer, vd, depth + 1),
        else => {
            unreachable;
        },
    }
}

fn dumpVarDeclaration(writer: *Writer, item: *ast.VarDeclaration, depth: usize) !void {
    try writer.print("VarDeclaration\n", .{});
    try writeIndent(writer, depth);
    try writer.print("name: {s} type: {t} ", .{ item.ident, item.type });
    // try writer.print("name: {s} type: {t} dimensions: {?any}", .{ item.ident, item.type, item.dimensions });
}

fn dumpBlock(writer: *Writer, item: *ast.SomeBlock, depth: usize) !void {
    try writeIndent(writer, depth);
    _ = item; // autofix
    try writer.print("block", .{});
}

fn writeIndent(writer: *Writer, depth: usize) WriterError!void {
    if (depth == 0) return;
    for (0..depth * DEPTH_DISTANCE) |i| {
        if (i % DEPTH_DISTANCE == 0) {
            try writer.writeByte('|');
        } else {
            try writer.writeByte(' ');
        }
    }
}
const DEPTH_DISTANCE = 2;
