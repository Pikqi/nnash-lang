const std = @import("std");
const ArrayList = std.ArrayList;
const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const lexer = @import("../Lexical/lexer.zig");
const Lexem = lexer.Lexem;
const TokenType = lexer.TokenType;
const ast_printer = @import("ast_printer.zig");

const ParserError = error{
    SyntaxError,
};

var buff: [1024]u8 = undefined;
pub const Parser = struct {
    tokens: []Lexem,
    current: usize = 0,
    arena: *ArenaAllocator,
    alloc: Allocator,
    program: ?ast.Program = null,
    fn reportError(self: *Self, msg: []const u8) void {
        _ = self; // autofix
        std.debug.print("{s}", .{msg});
    }

    const Self = @This();
    pub fn init(tokens: []Lexem, arena: *ArenaAllocator) Self {
        return Self{
            .tokens = tokens,
            .arena = arena,
            .alloc = arena.allocator(),
        };
    }
    pub fn deinit(self: *Self) void {
        _ = self.arena.reset(.free_all);
    }
    pub fn printAST(self: *const Self, writer: *std.Io.Writer) !void {
        if (self.program == null) {
            try writer.print("Program not parsed", .{});
            return;
        }
        try ast_printer.dump(writer, self.program.?);
    }
    pub fn parse(self: *Self) !void {
        self.program = .{
            // todo
            .top_items = ArrayList(*ast.TopItem).initCapacity(self.alloc, 20) catch unreachable,
        };
        errdefer {
            const stdout = std.fs.File.stdout();
            var buffered_writer = stdout.writer(&buff);
            const writer = &buffered_writer.interface;
            ast_printer.dump(writer, self.program.?) catch unreachable;
        }
        try self.parseProgram();
    }

    fn parseProgram(self: *Self) !void {
        while (!self.isAtEnd()) {
            try self.program.?.top_items.append(self.alloc, try self.topItem());
        }
    }

    fn topItem(self: *Self) !*ast.TopItem {
        const ti = try self.alloc.create(ast.TopItem);
        switch (self.peek().type) {
            .FUN_DEC => {
                // todo function declaration
                return error.NotImplemented;
            },
            .WHILE => {
                // todo while block
                return error.NotImplemented;
            },
            .IF => {
                // todo while block
                return error.NotImplemented;
            },
            else => {
                ti.* = .{ .simple = try self.topSimple() };
            },
        }
        return ti;
    }

    // top_simple = (var_decl | var_decl_assign | assign_stmt | call_expr) STATMENT_END;
    // var_decl       = type COLON (IDENT | IDENT array_dims)
    // var_decl_assign= expr ASSIGN var_decl
    // assign_stmt    = expr ASSIGN lvalue ;
    // call_expr      = args PIPE call_expr_continue
    fn topSimple(self: *Self) !*ast.TopSimpleItem {
        const ts = try self.alloc.create(ast.TopSimpleItem);
        if (self.matchType() != null) {
            ts.* = .{ .varDeclaration = try self.varDeclaration() };
            _ = try self.consume(.STATMENT_END, "Expected !");
            return ts;
        }
        const ex = try self.parseExpression();
        // assign_stmt | var_decl_assign
        if (self.match(.ASSIGN) != null) {
            // var_decl_assign
            if (self.matchType() != null) {
                const vd = try self.varDeclaration();
                ts.* = .{ .varDeclarationAsign = try self.alloc.create(ast.VarDeclarationAsign) };
                ts.varDeclarationAsign.expr = ex;
                ts.varDeclarationAsign.varDeclaration = vd;
            } else if (self.match(.IDENT)) |matched| {
                const as = try self.alloc.create(ast.AssignStatement);
                as.expr = ex;
                as.ident = try self.alloc.dupe(u8, matched.str.?);
                ts.* = .{ .assignStatement = as };
            }
        } else {
            // this must be a call expression without asignment?
            if (ex.* != .callExpression) {
                self.reportError("Non CallExpression hanging without assignment");
            }
            ts.* = .{ .callExpression = ex.callExpression };
        }
        _ = try self.consume(.STATMENT_END, "Expected !");

        return ts;
    }
    // var_decl = type COLON (IDENT | IDENT array_dims)
    fn varDeclaration(self: *Self) !*ast.VarDeclaration {
        const vd = try self.alloc.create(ast.VarDeclaration);
        const type_lexem = self.matchType();
        if (type_lexem == null) {
            self.reportError("Tried to parse type declaration but previous token is not a type");
            // return error.SyntaxError;
            return error.SyntaxError;
        }
        vd.type = switch (type_lexem.?.type) {
            .INT => .INT,
            .FLOAT => .FLOAT,
            .STRING => .STRING,
            .BOOL => .BOOL,
            .VOID_LIT => .VOID,
            else => unreachable,
        };
        _ = self.advance(); // consume type
        _ = try self.consume(.COLON, "Expected : after type");
        const ident_token = try self.consume(.IDENT, "Expected identifier");
        vd.ident = try self.alloc.dupe(u8, ident_token.str.?);

        if (self.match(.LBRACKET) != null) {
            self.reportError("Array dims not implemented");
            return error.SyntaxError;
        }
        return vd;
    }
    // assign_stmt    = expr ASSIGN lvalue ;
    fn asignStatement(self: *Self) !*ast.AssignStatement {
        const as = try self.alloc.create(ast.AssignStatement);
        as.expr = try self.parseExpression();
        as.ident = try self.consume(.IDENT, "Expected ident");
        return as;
    }

    // expr           = aexpr | call_expr ;
    fn parseExpression(self: *Self) anyerror!*ast.Expression {
        const ex = try self.alloc.create(ast.Expression);
        // tuple
        if (self.check(.LBRACKET)) {
            ex.* = .{ .callExpression = try self.callExpression() };
        } else {
            ex.* = .{ .aExpression = try self.parseAExpression() };
        }
        return ex;
    }

    // tuple = LBRACKET expr { COMMA expr }  RBRACKET ;
    fn parseTuple(self: *Self) !*ast.Tuple {
        _ = try self.consume(.LBRACKET, "Expected ");
        const tuple = try self.alloc.create(ast.Tuple);
        var exprs = try ArrayList(*ast.Expression).initCapacity(self.alloc, 5);
        try exprs.append(self.alloc, try self.parseExpression());

        while (self.match(.RBRACKET) == null) {
            _ = try self.consume(.COMMA, "Expected ,");
            try exprs.append(self.alloc, try self.parseExpression());
        }
        tuple.exprs = try exprs.toOwnedSlice(self.alloc);
        return tuple;
    }

    // [1, 2] | @max | @print!

    // call_expr      = tuple PIPE call_expr_continue
    fn callExpression(self: *Self) !*ast.CallExpression {
        const cex = try self.alloc.create(ast.CallExpression);
        const tuple = try self.parseTuple();
        cex.args = tuple;
        _ = try self.consume(.PIPE, "Expected | after tuple");
        cex.callExpressionContinue = try self.callExpressionContinue();
        return cex;
    }

    // call_expr_continue = (IDENT | BUILTIN_FUN) (PIPE call_expr_continue)?;
    fn callExpressionContinue(self: *Self) !*ast.CallExpressionContinue {
        const cexc = try self.alloc.create(ast.CallExpressionContinue);
        cexc.callExpressionContinue = null;

        const matched = self.matchOneOf(&[_]TokenType{ .IDENT, .BUILTIN_FUN });
        if (matched == null) {
            self.reportError("Expected ident or builtin funciton");
            return error.SyntaxError;
        }
        cexc.name = try self.alloc.dupe(u8, matched.?.str.?);
        if (self.match(.PIPE) != null) {
            cexc.callExpressionContinue = try self.callExpressionContinue();
        }
        return cexc;
    }

    // add = mul { ( PLUS | MINUS ) mul };
    fn parseAExpression(self: *Self) !*ast.AExpression {
        const ax = try self.alloc.create(ast.AExpression);
        var muls = try ArrayList(*ast.Mul).initCapacity(self.alloc, 5);
        var operands = try ArrayList(ast.PlusMinus).initCapacity(self.alloc, 5);
        try muls.append(self.alloc, try self.parseMul());
        while (true) {
            const matched = self.matchOneOf(&[_]TokenType{ .ADD, .SUB });
            if (matched == null) break;
            try operands.append(self.alloc, lexemToPlusMinus(matched.?) catch unreachable);
            try muls.append(self.alloc, try self.parseMul());
        }
        ax.muls = try muls.toOwnedSlice(self.alloc);
        ax.ops = try operands.toOwnedSlice(self.alloc);

        return ax;
    }

    // mul = unary { ( TIMES | DIV ) unary }
    fn parseMul(self: *Self) !*ast.Mul {
        const ml = try self.alloc.create(ast.Mul);
        ml.leftUnary = try self.parseUnary();

        ml.rightUnary = null;
        ml.operand = null;

        if (self.matchOneOf(&[_]TokenType{ .TIMES, .DIV })) |matched| {
            ml.operand = switch (matched.type) {
                .TIMES => .TIMES,
                .DIV => .DIV,
                else => unreachable,
            };
            ml.rightUnary = try self.parseUnary();
        }
        return ml;
    }

    // unary = [ PLUS | MINUS ] power
    fn parseUnary(self: *Self) !*ast.Unary {
        const un = try self.alloc.create(ast.Unary);
        un.sign = null;
        const matched = self.matchOneOf(&[_]TokenType{ .ADD, .SUB });
        if (matched) |plus_minus| {
            un.sign = lexemToPlusMinus(plus_minus) catch unreachable;
        }
        un.power = try self.parsePower();
        return un;
    }
    // power = primary [ POW power ];
    fn parsePower(self: *Self) !*ast.Power {
        const po = try self.alloc.create(ast.Power);
        po.primary = try self.parsePrimary();
        po.pow = null;

        // TODO ADD PARSING POW IN LEXER
        if (self.match(.POW) != null) {
            po.pow = try self.parsePower();
        }
        return po;
    }

    fn parsePrimary(self: *Self) !*ast.Primary {
        const pr = try self.alloc.create(ast.Primary);
        const matched = self.matchOneOf(&[_]TokenType{ .INT_LIT, .FLOAT_LIT, .IDENT });
        if (matched == null) {
            _ = try self.consume(.LPAREN, "Expected (");
            pr.* = .{ .expr = try self.parseExpression() };
            _ = try self.consume(.RPAREN, "Expected )");
        } else {
            switch (matched.?.type) {
                .INT_LIT => {
                    pr.* = .{ .number = .{ .int_lit = matched.? } };
                },
                .FLOAT_LIT => {
                    pr.* = .{ .number = .{ .float_lit = matched.? } };
                },
                .IDENT => {
                    pr.* = .{ .number = .{ .ident = matched.? } };
                },
                else => unreachable,
            }
        }
        return pr;
    }

    fn checkLiteral(self: *Self) bool {
        return switch (self.peek().type) {
            .INT_LIT,
            .FLOAT_LIT,
            .STRING_LIT,
            .TRUE_LIT,
            .FALSE_LIT,
            .VOID_LIT,
            => true,

            else => false,
        };
    }

    // Utilities
    fn check(self: Self, token_type: TokenType) bool {
        return self.peek().type == token_type;
    }
    fn checkNext(self: *Parser, expected: TokenType) bool {
        if (self.isAtEnd()) return false;
        return self.tokens[self.current + 1].type == expected;
    }
    fn match(self: *Self, token_type: TokenType) ?Lexem {
        if (self.check(token_type)) {
            _ = self.advance();
            return self.previous();
        }
        return null;
    }

    fn matchOneOf(self: *Self, comptime expecteds: []const TokenType) ?Lexem {
        inline for (expecteds) |t_type| {
            if (self.check(t_type)) return self.advance();
        }
        return null;
    }
    fn matchType(self: *Self) ?Lexem {
        if (self.check(.INT) or self.check(.STRING) or self.check(.BOOL) or self.check(.FLOAT) or self.check(.VOID_LIT)) {
            return self.peek();
        }
        return null;
    }

    fn consume(self: *Parser, expected: TokenType, msg: []const u8) !Lexem {
        if (!self.check(expected)) {
            self.reportError(msg);
            return error.SyntaxError;
        }
        _ = self.advance();
        return self.previous();
    }

    fn advance(self: *Parser) Lexem {
        if (self.isAtEnd()) return self.peek();

        self.current += 1;
        return self.previous();
    }
    fn peek(self: Self) Lexem {
        if (self.current >= self.tokens.len) {
            return self.tokens[self.current - 1];
        }
        return self.tokens[self.current];
    }

    fn previous(self: Self) Lexem {
        return self.tokens[self.current - 1];
    }
    fn isAtEnd(self: Self) bool {
        return self.current == self.tokens.len;
    }
};
fn lexemToPlusMinus(lexem: Lexem) !ast.PlusMinus {
    return switch (lexem.type) {
        .ADD => .PLUS,
        .SUB => .MINUS,
        else => error.NotPlusMinus,
    };
}
