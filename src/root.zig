//! By convention, root.zig is the root source file when making a library.
const std = @import("std");
pub const preprocessor = @import("preprocessor.zig");
pub const lexer = @import("lexer.zig");

test "import" {
    _ = @import("preprocessor.zig");
    _ = @import("lexer.zig");
}
