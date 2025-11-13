const std = @import("std");
const ArrayList = std.ArrayList;

const Types = enum {
    INT,
    STRING,
    VOID,
    FLOAT,
    BOOL,
};

// program = {top_item} EOF
pub const Program = struct {
    top_items: ArrayList(*TopItem),
    const Self = @This();
    pub fn dump(self: *const Self) void {
        for (self.top_items.items) |i| {
            i.dump();
        }
    }
};

pub const TopItem = union(enum) {
    block: *SomeBlock,
    simple: *TopSimpleItem,
};

pub const SomeBlock = union(enum) {
    ifBlock: IfBlock,
    whileBlock: WhileBlock,
    funBlock: FunBlock,
};
pub const IfBlock = struct {
    condition: Condition,
    blockStatements: ArrayList(BlockUnion),
};
pub const WhileBlock = struct {
    condition: Condition,
    blockStatements: ArrayList(BlockUnion),
};
pub const FunBlock = struct {};

pub const Block = struct {
    stmtOrBlockList: ArrayList(BlockUnion),
};

pub const BlockUnion = union(enum) { statement: Statement, blocks: SomeBlock };

pub const Statement = union(enum) {
    varDeclaration: VarDeclaration,
    varDeclarationAsign: VarDeclarationAsign,
    callMaybeAssign: CallMaybeAssign,
    assignStatement: AssignStatement,
    returnStatement: ReturnStatement,
};

// top_simple = (var_decl | var_decl_assign | call_and_maybe_assign | assign_stmt) STATMENT_END;
pub const TopSimpleItem = union(enum) {
    varDeclaration: *VarDeclaration,
    varDeclarationAsign: VarDeclarationAsign,
    callMaybeAssign: CallMaybeAssign,
    assignStatement: AssignStatement,
};

pub const VarDeclaration = struct { type: Types, ident: []const u8, dimensions: ?[]usize = null };

pub const VarDeclarationAsign = struct {};

pub const CallMaybeAssign = struct {};

pub const AssignStatement = struct {};

pub const ReturnStatement = struct {};

pub const Condition = struct {};
