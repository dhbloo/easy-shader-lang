from io import TextIOBase
from .lexer import create_lexer, init_lexer_context
from .parser import create_parser
from .codegen import CodeGenContext, SemanticError
from . import ast

lexer = create_lexer()
parser = create_parser()

class Compiler():
    def __init__(self, ast_only=False):
        self.ast_only : bool = ast_only
        self.codegen_ctx : CodeGenContext = None
        self.ast_root : ast.TranslationUnit = None
        self.error_message : str = None

    def compile(self, code_str, module_name) -> bool:
        self.ast_root = parser.parse(code_str)
        if self.ast_only:
            return True

        self.codegen_ctx = CodeGenContext(module_name)
        try:
            self.ast_root.accept(self.codegen_ctx.visitor)
        except SemanticError as err:
            self.error_message = str(err)
            return False

        return True

    def get_error_message(self):
        return self.error_message

    def dump_ast(self, out_stream : TextIOBase):
        out_stream.write(str(self.ast_root))

    def dump_text(self, out_stream : TextIOBase):
        out_stream.write(str(self.codegen_ctx.get_module()))

        





