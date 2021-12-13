from .lexer import create_lexer, init_lexer_context
from .parser import create_parser
from .codegen import CodeGenContext
from . import ast

lexer = create_lexer()
parser = create_parser()

class Compiler():
    def __init__(self, ast_only=False):
        super().__init__()
        self.ast_only = ast_only

    def compile(self, code_str, module_name):
        ast_root : ast.TranslationUnit = parser.parse(code_str)
        if self.ast_only:
            return ast_root

        codegen_ctx = CodeGenContext(module_name)
        ast_root.accept(codegen_ctx.visitor)
        





