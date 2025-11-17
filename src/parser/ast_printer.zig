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
        .varDeclarationAsign => |vd| try dumpVarDeclarationAssign(writer, vd, depth + 1),
        .assignStatement => |vd| try dumpAssignStatement(writer, vd, depth + 1),
        .callExpression => |cex| try dumpCallExpression(writer, cex, depth + 1),
    }
}

fn dumpVarDeclaration(writer: *Writer, item: *ast.VarDeclaration, depth: usize) !void {
    try writer.print("VarDeclaration\n", .{});
    try writeIndent(writer, depth);
    try writer.print("name: {s} type: {t}\n", .{ item.ident, item.type });
    // try writer.print("name: {s} type: {t} dimensions: {?any}", .{ item.ident, item.type, item.dimensions });
}
fn dumpAssignStatement(writer: *Writer, item: *ast.AssignStatement, depth: usize) !void {
    try writer.print("AssignStatement\n", .{});
    try writeIndent(writer, depth);
    try dumpLValue(writer, item.lvalue, depth + 1);
    try writeIndent(writer, depth);
    try dumpExpression(writer, item.expr, depth + 1);
}

fn dumpLValue(writer: *Writer, item: ast.LValue, depth: usize) !void {
    try writer.print("LValue\n", .{});
    try writeIndent(writer, depth);
    try writer.print("Id: {s}\n", .{item.ident});
    if (item.index) |index| {
        try writeIndent(writer, depth);
        try dumpTuple(writer, index, depth + 1);
    }
}

fn dumpVarDeclarationAssign(writer: *Writer, item: *ast.VarDeclarationAsign, depth: usize) !void {
    try writer.print("VarDeclarationAssign\n", .{});
    try writeIndent(writer, depth);
    try dumpVarDeclaration(writer, item.varDeclaration, depth + 1);
    try dumpExpression(writer, item.expr, depth + 1);
    // try writer.print("name: {s} type: {t} dimensions: {?any}", .{ item.ident, item.type, item.dimensions });
}

fn dumpExpression(writer: *Writer, item: *ast.Expression, depth: usize) anyerror!void {
    try writer.print("Expression\n", .{});
    try writeIndent(writer, depth);
    switch (item.*) {
        .aExpression => |aex| {
            try writer.print("AExpression\n", .{});
            const adepth = depth + 1;
            for (aex.muls, 0..) |mul, i| {
                try writeIndent(writer, adepth + 1);
                try dumpMul(writer, mul, adepth + 2);
                if (i < aex.ops.len) {
                    try writeIndent(writer, adepth + 1);
                    try writer.print("{t}\n", .{aex.ops[i]});
                }
            }
        },
        .callExpression => |cex| {
            try dumpCallExpression(writer, cex, depth + 1);
        },

        .tupleExpression => |tuple| {
            try dumpTuple(writer, tuple, depth + 1);
        },
    }
}
fn dumpCallExpression(writer: *Writer, item: *ast.CallExpression, depth: usize) !void {
    try writer.print("CallExpression\n", .{});
    try writeIndent(writer, depth);
    try writer.print("args: \n", .{});
    try writeIndent(writer, depth);
    try dumpTuple(writer, item.args, depth + 1);
    try writeIndent(writer, depth);
    try dumpCallExpressionContinue(writer, item.callExpressionContinue, depth + 1);
}

fn dumpCallExpressionContinue(writer: *Writer, item: *ast.CallExpressionContinue, depth: usize) !void {
    try writer.print("CallExpressionContinue\n", .{});
    try writeIndent(writer, depth);
    try writer.print("Function Name: {s}\n", .{item.name});
    if (item.callExpressionContinue) |piped| {
        try writeIndent(writer, depth);
        try writer.print("Piped: \n", .{});
        try writeIndent(writer, depth + 1);
        try dumpCallExpressionContinue(writer, piped, depth + 2);
    }
}
fn dumpMul(writer: *Writer, item: *ast.Mul, depth: usize) !void {
    try writer.print("Mul\n", .{});
    try writeIndent(writer, depth);
    try dumpUnary(writer, item.leftUnary, depth + 1);

    if (item.operand) |operand| {
        try writeIndent(writer, depth);
        try writer.print("{t}\n", .{operand});
        try writeIndent(writer, depth);
        try dumpUnary(writer, item.rightUnary.?, depth + 1);
    }
}

fn dumpUnary(writer: *Writer, item: *ast.Unary, depth: usize) !void {
    try writer.print("Unary\n", .{});
    try writeIndent(writer, depth);
    if (item.sign) |sign| {
        try writer.print("SIGN: {t}\n", .{sign});
        try writeIndent(writer, depth);
    }
    try dumpPower(writer, item.power, depth + 1);
}

fn dumpPower(writer: *Writer, item: *ast.Power, depth: usize) !void {
    try writer.print("Power\n", .{});
    try writeIndent(writer, depth);
    try dumpPrimary(writer, item.primary, depth + 1);
}

fn dumpPrimary(writer: *Writer, item: *ast.Primary, depth: usize) anyerror!void {
    try writer.print("Primary\n", .{});
    try writeIndent(writer, depth);
    switch (item.*) {
        .expr => |ex| try dumpExpression(writer, ex, depth + 1),
        .primaryToken => |n| {
            try writer.print("Number\n", .{});
            try writeIndent(writer, depth + 1);
            switch (n) {
                .int_lit => try writer.print("int: {d}\n", .{n.int_lit.value.?.int}),
                .float_lit => try writer.print("float: {d}\n", .{n.float_lit.value.?.float}),
                .ident => try writer.print("ident: {s}\n", .{n.ident.str.?}),
                .str_lit => try writer.print("str_lit: {s}\n", .{n.str_lit.str.?}),
            }
            // try writer.print("{d}", : anytype)
        },
    }
}
fn dumpTuple(writer: *Writer, item: *ast.Tuple, depth: usize) !void {
    try writer.print("Tuple\n", .{});

    for (item.exprs) |ex| {
        try writeIndent(writer, depth);
        try dumpExpression(writer, ex, depth + 2);
    }
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
