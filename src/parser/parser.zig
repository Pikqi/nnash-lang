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
        std.log.err("{s}\n", .{msg});
        std.log.err("At token {any}", .{self.peek()});
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
            .top_items = ArrayList(ast.TopItem).initCapacity(self.alloc, 20) catch unreachable,
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
            try self.program.?.top_items.append(self.alloc, try self.topItem(false));
        }
    }

    fn returnStatement(self: *Self) !ast.ReturnStatement {
        _ = try self.consume(.RETURN, "Expected <<");
        if (self.check(.VOID_LIT) or self.check(.STATMENT_END)) {
            _ = self.match(.VOID_LIT);
            return .{ .void = {} };
        }
        return .{ .expression = try self.parseExpression() };
    }
    fn topItem(self: *Self, in_block: bool) !ast.TopItem {
        switch (self.peek().type) {
            .RETURN => {
                if (in_block) {
                    const ti: ast.TopItem = .{ .simple = .{ .returnStatement = try self.returnStatement() } };
                    _ = try self.consume(.STATMENT_END, "Exprected ! after return");
                    return ti;
                } else {
                    self.reportError("Cant return from here");
                    return error.SyntaxError;
                }
            },
            .FUN_DEC => {
                return .{ .block = .{ .funBlock = try self.function() } };
            },
            .WHILE => {
                return .{ .block = .{ .whileBlock = try self.whileBlock() } };
            },
            .IF => {
                return .{ .block = .{ .ifBlock = try self.ifBlock() } };
            },
            else => {
                return .{ .simple = try self.topSimple() };
            },
        }
    }

    fn function(self: *Self) anyerror!ast.FunBlock {
        _ = try self.consume(.FUN_DEC, "Expected fun");
        const fn_ident = try self.consume(.IDENT, "Expected identifier after fun");
        _ = try self.consume(.LBRACKET_DOUBLE, "Expected [[");
        var params_list = try ArrayList(ast.VarDeclaration).initCapacity(self.alloc, 5);
        while (self.match(.RBRACKET_DOUBLE) == null) {
            _ = self.match(.COMMA);
            try params_list.append(self.alloc, try self.varDeclaration());
        }

        _ = try self.consume(.COLON, "Expected :");
        const return_type = self.matchType();
        if (return_type == null) {
            self.reportError("Expected type after :");
            return error.SyntaxError;
        }
        _ = self.advance();

        var statements = try ArrayList(ast.TopItem).initCapacity(self.alloc, 10);
        while (self.match(.END_FUN_DEC) == null) {
            try statements.append(self.alloc, try self.topItem(true));
        }

        return .{
            .returnType = try lexemToType(return_type.?),
            .name = try self.alloc.dupe(u8, fn_ident.str.?),
            .params = try params_list.toOwnedSlice(self.alloc),
            .blockStatements = try statements.toOwnedSlice(self.alloc),
        };
    }

    fn whileBlock(self: *Self) anyerror!ast.WhileBlock {
        _ = try self.consume(.WHILE, "Expected while");
        _ = try self.consume(.LBRACKET, "Expected [");

        const condition = try self.cond();
        _ = try self.consume(.RBRACKET, "Expected ]");

        var statements = try ArrayList(ast.TopItem).initCapacity(self.alloc, 10);
        while (!self.check(.ENDWHILE)) {
            try statements.append(self.alloc, try self.topItem(true));
        }
        const blockStatements = try statements.toOwnedSlice(self.alloc);

        _ = self.advance(); // consume endwhile

        return ast.WhileBlock{ .blockStatements = blockStatements, .condition = condition };
    }
    fn ifBlock(self: *Self) anyerror!*ast.IfBlock {
        _ = try self.consume(.IF, "Expected IF");
        _ = try self.consume(.LBRACKET, "Expected [ after if");
        const condition = try self.cond();

        _ = try self.consume(.RBRACKET, "Expected ] after condidition");
        _ = try self.consume(.COLON, "Expected : after ]");

        const ifbl = try self.alloc.create(ast.IfBlock);
        ifbl.condition = condition;

        var statements = try ArrayList(ast.TopItem).initCapacity(self.alloc, 10);

        while (!self.check(.ELSE) and !self.check(.END_IF)) {
            try statements.append(self.alloc, try self.topItem(true));
        }
        ifbl.blockStatements = try statements.toOwnedSlice(self.alloc);
        if (self.match(.END_IF) != null) {
            ifbl.elseBlock = null;
            return ifbl;
        }
        if (self.match(.ELSE) != null) {
            if (self.check(.IF)) {
                ifbl.elseBlock = .{
                    .elif = try self.ifBlock(),
                };
                return ifbl;
            }
            var else_statements = try ArrayList(ast.TopItem).initCapacity(self.alloc, 10);
            while (!self.check(.END_IF)) {
                try else_statements.append(self.alloc, try self.topItem(true));
            }
            _ = self.advance();
            ifbl.elseBlock = .{
                .elseStatements = try else_statements.toOwnedSlice(self.alloc),
            };
            return ifbl;
        }

        return error.SyntaxError;
    }

    fn cond(self: *Self) !ast.Condition {
        if (self.matchOneOf(&[_]TokenType{ .TRUE_LIT, .FALSE_LIT })) |matched| {
            return .{ .literal = matched.type == .TRUE_LIT };
        }
        var expressions = try ArrayList(ast.Expression).initCapacity(self.alloc, 10);
        var operators = try ArrayList(ast.RelOperator).initCapacity(self.alloc, 9);
        try expressions.append(self.alloc, try self.parseExpression());
        while (self.matchOneOf(&[_]TokenType{ .LT, .LE, .GT, .GE, .EQ, .NEQ })) |matched| {
            try operators.append(self.alloc, lexemToRelOperator(matched) catch unreachable);
            try expressions.append(self.alloc, try self.parseExpression());
        }
        return .{ .nested = .{
            .expressions = try expressions.toOwnedSlice(self.alloc),
            .operator = try operators.toOwnedSlice(self.alloc),
        } };
    }

    // top_simple = (var_decl | var_decl_assign | assign_stmt | call_expr) STATMENT_END;
    // var_decl       = type COLON (IDENT | IDENT array_dims)
    // var_decl_assign= expr ASSIGN var_decl
    // assign_stmt    = expr ASSIGN lvalue ;
    // call_expr      = args PIPE call_expr_continue
    fn topSimple(self: *Self) !ast.TopSimpleItem {

        // VarDecl
        if (self.checkType()) {
            const vd = try self.varDeclaration();
            _ = try self.consume(.STATMENT_END, "Exprected !");
            return .{ .varDeclaration = vd };
        }
        const ex = try self.parseExpression();
        if (self.match(.ASSIGN)) |_| {
            // VarDeclAssign
            if (self.checkType()) {
                const vd = try self.varDeclaration();
                _ = try self.consume(.STATMENT_END, "Exprected !");
                return .{ .varDeclarationAsign = .{
                    .expr = ex,
                    .varDeclaration = vd,
                } };
            }
            // Assign
            const lval = try self.lvalue();
            _ = try self.consume(.STATMENT_END, "Exprected !");
            return .{ .assignStatement = .{
                .expr = ex,
                .lvalue = lval,
            } };
        }
        if (self.check(.RETURN)) {
            const ret: ast.TopSimpleItem = .{ .returnStatement = try self.returnStatement() };
            _ = try self.consume(.STATMENT_END, "Exprected !");
            return ret;
        }

        if (ex != .callExpression) {
            return error.SyntaxError;
        }
        _ = try self.consume(.STATMENT_END, "Exprected !");
        return .{ .callExpression = ex.callExpression };
    }
    fn lvalue(self: *Self) !ast.LValue {
        const ident = try self.consume(.IDENT, "Expected Identifier");
        const ident_str = try self.alloc.dupe(u8, ident.str.?);
        return .{
            .ident = ident_str,
            .index = if (self.check(.LBRACKET)) try self.parseTuple() else null,
        };
    }
    // var_decl = type COLON (IDENT | IDENT array_dims)
    fn varDeclaration(self: *Self) !ast.VarDeclaration {
        const type_lexem = self.matchType();
        if (type_lexem == null) {
            self.reportError("Tried to parse type declaration but previous token is not a type");
            // return error.SyntaxError;
            return error.SyntaxError;
        }
        const vd_type = try lexemToType(type_lexem.?);

        _ = self.advance(); // consume type
        _ = try self.consume(.COLON, "Expected : after type");
        const ident_token = try self.consume(.IDENT, "Expected identifier");

        if (self.match(.LBRACKET) != null) {
            self.reportError("Array dims not implemented");
            return error.SyntaxError;
        }
        return .{ .type = vd_type, .ident = try self.alloc.dupe(u8, ident_token.str.?) };
    }
    // assign_stmt    = expr ASSIGN lvalue ;
    fn asignStatement(self: *Self) !*ast.AssignStatement {
        const as = try self.alloc.create(ast.AssignStatement);
        as.expr = try self.parseExpression();
        as.lvalue = try self.lvalue();
        return as;
    }

    // expr           = aexpr | call_expr ;
    fn parseExpression(self: *Self) anyerror!ast.Expression {
        // tuple
        if (self.check(.LBRACKET)) {
            const tuple = try self.parseTuple();
            if (self.check(.PIPE)) {
                return .{ .callExpression = try self.callExpression(tuple) };
            } else {
                return .{ .tupleExpression = tuple };
            }
        }

        return .{ .aExpression = try self.parseAExpression() };
    }

    // tuple = LBRACKET expr { COMMA expr }  RBRACKET ;
    fn parseTuple(self: *Self) !*ast.Tuple {
        _ = try self.consume(.LBRACKET, "Expected ");
        const tuple = try self.alloc.create(ast.Tuple);
        var exprs = try ArrayList(ast.Expression).initCapacity(self.alloc, 5);
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
    fn callExpression(self: *Self, tuple_opt: ?*ast.Tuple) !ast.CallExpression {
        const tuple = if (tuple_opt) |t| t else try self.parseTuple();
        _ = try self.consume(.PIPE, "Expected | after tuple");
        return .{ .args = tuple, .callExpressionContinue = try self.callExpressionContinue() };
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
    fn parseAExpression(self: *Self) !ast.AExpression {
        var muls = try ArrayList(ast.Mul).initCapacity(self.alloc, 5);
        var operands = try ArrayList(ast.PlusMinus).initCapacity(self.alloc, 5);
        try muls.append(self.alloc, try self.parseMul());
        while (true) {
            const matched = self.matchOneOf(&[_]TokenType{ .ADD, .SUB });
            if (matched == null) break;
            try operands.append(self.alloc, lexemToPlusMinus(matched.?) catch unreachable);
            try muls.append(self.alloc, try self.parseMul());
        }
        return .{
            .muls = try muls.toOwnedSlice(self.alloc),
            .ops = try operands.toOwnedSlice(self.alloc),
        };
    }

    // mul = unary { ( TIMES | DIV ) unary }
    fn parseMul(self: *Self) !ast.Mul {
        var ml: ast.Mul = .{
            .leftUnary = try self.parseUnary(),
        };

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
    fn parseUnary(self: *Self) !ast.Unary {
        var un: ast.Unary = undefined;
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

        if (self.match(.POW) != null) {
            po.pow = try self.parsePower();
        }
        return po;
    }

    fn parsePrimary(self: *Self) !ast.Primary {
        var pr: ast.Primary = undefined;
        const matched = self.matchOneOf(&[_]TokenType{ .INT_LIT, .FLOAT_LIT, .IDENT, .STRING_LIT, .TRUE_LIT, .FALSE_LIT });
        if (matched == null) {
            _ = try self.consume(.LPAREN, "Expected (");
            pr = .{ .expr = try self.parseExpression() };
            _ = try self.consume(.RPAREN, "Expected )");
            return pr;
        }
        switch (matched.?.type) {
            .INT_LIT => {
                pr = .{ .primaryToken = .{ .int_lit = matched.? } };
            },
            .FLOAT_LIT => {
                pr = .{ .primaryToken = .{ .float_lit = matched.? } };
            },
            .IDENT => {
                pr = .{ .primaryToken = .{ .ident = matched.? } };
            },
            .STRING_LIT => {
                pr = .{ .primaryToken = .{ .str_lit = matched.? } };
            },
            .TRUE_LIT, .FALSE_LIT => {
                pr = .{ .primaryToken = .{ .bool_lit = matched.? } };
            },

            else => unreachable,
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

    fn checkType(self: *Self) bool {
        return self.check(.INT) or self.check(.STRING) or self.check(.BOOL) or self.check(.FLOAT) or self.check(.VOID_LIT);
    }

    fn matchType(self: *Self) ?Lexem {
        if (self.checkType()) {
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
fn lexemToRelOperator(lexem: Lexem) !ast.RelOperator {
    return switch (lexem.type) {
        .LT => .LT,
        .LE => .LE,
        .GT => .GT,
        .GE => .GE,
        .EQ => .EQ,
        .NEQ => .NEQ,
        else => error.NotRelOperator,
    };
}

fn lexemToPlusMinus(lexem: Lexem) !ast.PlusMinus {
    return switch (lexem.type) {
        .ADD => .PLUS,
        .SUB => .MINUS,
        else => error.NotPlusMinus,
    };
}

fn lexemToType(lexem: Lexem) !ast.Types {
    return switch (lexem.type) {
        .INT => .INT,
        .STRING => .STRING,
        .VOID_LIT => .VOID,
        .FLOAT => .FLOAT,
        .BOOL => .BOOL,
        else => error.NotAType,
    };
}
