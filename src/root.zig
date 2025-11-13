//! By convention, root.zig is the root source file when making a library.
const std = @import("std");
pub const preprocessor = @import("preprocessor.zig");
pub const lexer = @import("Lexical/lexer.zig");
pub const parser = @import("parser/root.zig");

test "import" {
    _ = @import("preprocessor.zig");
    _ = @import("Lexical/lexer.zig");
    _ = @import("Lexical/scanner.zig");
    _ = @import("parser/root.zig");
    _ = @import("testing.zig");
}
