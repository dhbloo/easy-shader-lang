from io import TextIOBase
from .lexer import create_lexer, init_lexer_context
from .parser import create_parser
from .codegen import CodeGenContext, SemanticError
from . import ast

lexer = create_lexer()
parser = create_parser()

class Compiler():
    def __init__(self, ast_only=False):
        self.ast_only = ast_only
        self.codegen_ctx = None
        self.error_message = None

    def compile(self, code_str, module_name) -> bool:
        ast_root : ast.TranslationUnit = parser.parse(code_str)
        if self.ast_only:
            return ast_root

        self.codegen_ctx = CodeGenContext(module_name)
        try:
            ast_root.accept(self.codegen_ctx.visitor)
        except SemanticError as err:
            self.error_message = str(err)
            print('error_message', self.error_message)
            return False

        return True

    def dump_text(self, out_stream : TextIOBase) -> None:
        out_stream.write(str(self.codegen_ctx.get_module()))

        





