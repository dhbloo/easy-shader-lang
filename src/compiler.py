from io import TextIOBase, IOBase
from termcolor import colored
from llvmlite import ir
import llvmlite.binding as llvm

from .error import ParseError, SemanticError
from .lexer import create_lexer, init_lexer_context
from .parser import create_parser
from .codegen import CodeGenContext
from . import ast

lexer = create_lexer()
parser = create_parser()

llvm.initialize()
llvm.initialize_native_target()
llvm.initialize_native_asmprinter()  # yes, even this one

class Compiler():
    def __init__(self, ast_only=False):
        self.ast_only : bool = ast_only
        self.module : ir.Module = None
        self.ast_root : ast.TranslationUnit = None
        self.error_message : str = None

    def compile_to_ir(self, code_str, module_name) -> bool:
        try:
            self.ast_root = parser.parse(code_str)
        except ParseError as err:
            self.error_message = str(err)
            return False

        if self.ast_only:
            return True

        codegen_ctx = CodeGenContext(module_name)
        try:
            self.ast_root.accept(codegen_ctx.visitor)
            self.module = codegen_ctx.get_module()
        except SemanticError as err:
            self.error_message = str(err)
            return False
        
        return True

    def verify(self):
        try:
            mod = llvm.parse_assembly(str(self.module))
            mod.verify()
        except Exception as err:
            self.error_message = str(err)
            return False
        return True

    def get_error_message(self):
        return self.error_message

    def dump_ast(self, out_stream : TextIOBase):
        out_stream.write(str(self.ast_root))

    def dump_text(self, out_stream : TextIOBase):
        out_stream.write(str(self.module))

    def dump_bitcode(self, out_stream : IOBase):
        try:
            mod = llvm.parse_assembly(str(self.module))
            bc = mod.as_bitcode()
            out_stream.write(bc)
        except Exception as err:
            self.error_message = str(err)
            return False
        return True

    def create_execution_engine(self):
        """
        Create an ExecutionEngine suitable for JIT code generation on
        the host CPU.  The engine is reusable for an arbitrary number of
        modules.
        """
        # Create a target machine representing the host
        target = llvm.Target.from_default_triple()
        target_machine = target.create_target_machine()
        # And an execution engine with an empty backing module
        backing_mod = llvm.parse_assembly("")
        return llvm.create_mcjit_compiler(backing_mod, target_machine)

        





