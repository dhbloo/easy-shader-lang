from __future__ import annotations 
from typing import List, Optional

from codegen import CodegenContext
from enums import BasicType, BinaryOp, UnaryOp, IOType

class Indent():
    def __init__(self, ind=0) -> None:
        super().__init__()
        self.ind = ind

    def __str__(self) -> str:
        return '  ' * self.ind

class Node():
    """Base class of AST node"""
    def __init__(self, loc) -> None:
        super().__init__()
        self.location = loc  # Row

    def __str__(self, ind=Indent()) -> str:
        raise NotImplementedError()
    
    def codegen(self, ctx : CodegenContext):
        raise NotImplementedError("Codegen not implemented")


###########################################################
## Sec1. Top-level declaration
###########################################################

class TranslationUnit(Node):
    def __init__(self, loc, decl_list) -> None:
        super().__init__(loc)
        self.declaration_list = decl_list

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}'

class BlockDeclaration(Node):
    pass

class TypeDeclaration(BlockDeclaration):
    pass

class TypeAliasDecl(TypeDeclaration):
    def __init__(self, loc, identifier, type_spec : TypeSpecifier) -> None:
        super().__init__(loc)
        self.identifier = identifier
        self.type_spec = type_spec

class Declarator(Node):
    def __init__(self, loc, identifier, type_spec : Optional[TypeSpecifier] = None, initializer=None) -> None:
        super().__init__(loc)
        self.identifier = identifier
        self.type_spec = type_spec
        self.initializer = initializer
        self.is_const = False

class VariableDecl(BlockDeclaration):
    def __init__(self, loc, declarator_list : List[Declarator], is_const : bool) -> None:
        super().__init__(loc)
        self.declarator_list = declarator_list
        if is_const:
            for decl in self.declarator_list:
                decl.is_const = True

class FunctionDefinition(Node):
    def __init__(self, loc, func_decl : FunctionDecl, block_stmt : BlockStatement) -> None:
        super().__init__(loc)
        self.func_decl = func_decl
        self.block_stmt = block_stmt


###########################################################
## Sec2. Type specifier
###########################################################

class TypeSpecifier(Node):
    pass

class SimpleType(TypeSpecifier):
    def __init__(self, loc, type : BasicType) -> None:
        super().__init__(loc)
        self.type = type

    def __str__(self, ind=Indent()) -> str:
        return f'{self.type}'

class ComplexType(TypeSpecifier):
    def __init__(self, loc, identifier : str, is_interface : bool, generics_spec_list : List[TypeSpecifier] = []) -> None:
        super().__init__(loc)
        self.identifier = identifier
        self.is_interface = is_interface
        self.generics_spec_list = generics_spec_list

    def __str__(self, ind=Indent()) -> str:
        type_class = 'interface' if self.is_interface else 'struct'
        gslist = f' {self.generics_spec_list} ' if self.generics_spec_list else ' '
        return f'{type_class}{gslist}({self.identifier})'

class ArrayType(TypeSpecifier):
    def __init__(self, loc, type : TypeSpecifier, size : Optional[int]) -> None:
        super().__init__(loc)
        self.type = type
        self.size = size

    def __str__(self, ind=Indent()) -> str:
        return f'{self.type}[{self.size}]'

class ReferenceType(TypeSpecifier):
    def __init__(self, loc, type : TypeSpecifier) -> None:
        super().__init__(loc)
        self.type = type

    def __str__(self, ind=Indent()) -> str:
        return f'{self.type} ref'

class FunctionType(TypeSpecifier):
    def __init__(self, loc, func_sign : FunctionSignature) -> None:
        super().__init__(loc)
        self.func_sign = func_sign

    def __str__(self, ind=Indent()) -> str:
        return f'{self.func_sign}'


###########################################################
## Sec3. Function declaration
###########################################################

class FunctionDecl(BlockDeclaration):
    def __init__(self, loc, identifier : str, func_sign : FunctionSignature) -> None:
        super().__init__(loc)
        self.identifier = identifier
        self.func_sign = func_sign

class FunctionSignature(Node):
    def __init__(self, loc, generics_type_list : List[GenericType] = [], parameter_decl_list : List[ParameterDecl] = [], return_type_spec : Optional[TypeSpecifier] = None) -> None:
        super().__init__(loc)
        self.generics_type_list = generics_type_list
        self.parameter_decl_list = parameter_decl_list
        self.return_type_spec = return_type_spec

class ParameterDecl(Node):
    def __init__(self, loc, identifier, type_spec : Optional[TypeSpecifier] = None) -> None:
        super().__init__(loc)
        self.identifier = identifier
        self.type_spec = type_spec


###########################################################
## Sec4. Generic declaration
###########################################################

class GenericType(Node):
    def __init__(self, loc, identifier, type_range : Optional[TypeSpecifier] = None) -> None:
        super().__init__(loc)
        self.identifier = identifier
        self.type_range = type_range


###########################################################
## Sec5. Structure/Interface definition
###########################################################

class StructDecl(TypeDeclaration):
    def __init__(self, loc, identifier : str, member_list : List[StructMember], generics_type_list : List[GenericType] = [], base_type : Optional[ComplexType] = None) -> None:
        super().__init__(loc)
        self.identifier = identifier
        self.member_list = member_list
        self.generics_type_list = generics_type_list
        self.base_type = base_type

class InterfaceDecl(TypeDeclaration):
    def __init__(self, loc, identifier : str, member_list : List[InterfaceMember], generics_type_list : List[GenericType] = []) -> None:
        super().__init__(loc)
        self.identifier = identifier
        self.member_list = member_list
        self.generics_type_list = generics_type_list

class StructMember(Node):
    pass

class InterfaceMember(Node):
    pass

class MemberDecl(StructMember):
    def __init__(self, loc, identifier : str, type_spec : TypeSpecifier) -> None:
        super().__init__(loc)
        self.identifier = identifier
        self.type_spec = type_spec

class MemberFuncDecl(InterfaceMember):
    def __init__(self, loc, func_decl : FunctionDecl) -> None:
        super().__init__(loc)
        self.func_decl = func_decl

class MemberFuncDefinition(StructMember):
    def __init__(self, loc, func_definition : FunctionDefinition) -> None:
        super().__init__(loc)
        self.func_definition = func_definition

class MemberTypeFuncDecl(InterfaceMember):
    def __init__(self, loc, type_spec : TypeSpecifier, func_sign : FunctionSignature) -> None:
        super().__init__(loc)
        self.type_spec = type_spec
        self.func_sign = func_sign

class MemberTypeFuncDefinition(StructMember):
    def __init__(self, loc, type_func_decl : MemberTypeFuncDecl, block_stmt : BlockStatement) -> None:
        super().__init__(loc)
        self.type_func_decl = type_func_decl
        self.block_stmt = block_stmt

###########################################################
## Sec6. Statement
###########################################################

class Statement(Node):
    pass

class DeclarationStatement(Statement):
    def __init__(self, loc, declaration : VariableDecl) -> None:
        super().__init__(loc)
        self.declaration = declaration

class BlockStatement(Statement):
    def __init__(self, loc, statement_list : List[Statement]) -> None:
        super().__init__(loc)
        self.statement_list = statement_list

class ExpressionStatement(Statement):
    def __init__(self, loc, expression : Expression) -> None:
        super().__init__(loc)
        self.expression = expression

class IfStatement(Statement):
    def __init__(self, loc, condition : Expression, true_stmt : Statement, false_stmt : Optional[Statement] = None) -> None:
        super().__init__(loc)
        self.condition = condition
        self.true_stmt = true_stmt
        self.false_stmt = false_stmt

class WhileStatement(Statement):
    def __init__(self, loc, condition : Expression, loop_stmt : Statement) -> None:
        super().__init__(loc)
        self.condition = condition
        self.loop_stmt = loop_stmt

class ForStatement(Statement):
    def __init__(self, loc, init_stmt : Statement, cond_expr : Optional[Expression], iter_expr : Optional[Expression], loop_stmt : Expression) -> None:
        super().__init__(loc)
        self.init_stmt = init_stmt
        self.cond_expr = cond_expr
        self.iter_expr = iter_expr
        self.loop_stmt = loop_stmt

class JumpStatement(Statement):
    pass

class BreakStatement(Statement):
    def __init__(self, loc) -> None:
        super().__init__(loc)

class ContinueStatement(Statement):
    def __init__(self, loc) -> None:
        super().__init__(loc)

class ReturnStatement(Statement):
    def __init__(self, loc, expression : Optional[Expression]) -> None:
        super().__init__(loc)
        self.expression = expression


###########################################################
## Sec7. Expression
###########################################################

class Expression(Node):
    pass

class AssignExpression(Expression):
    def __init__(self, loc, lhs_expr : Expression, rhs_expr : Expression) -> None:
        super().__init__(loc)
        self.lhs_expr = lhs_expr
        self.rhs_expr = rhs_expr

class BinaryExpression(Expression):
    def __init__(self, loc, lhs_expr : Expression, rhs_expr : Expression, operator : BinaryOp) -> None:
        super().__init__(loc)
        self.lhs_expr = lhs_expr
        self.rhs_expr = rhs_expr
        self.operator = operator

class UnaryExpression(Expression):
    def __init__(self, loc, expression : Expression, operator : UnaryOp) -> None:
        super().__init__(loc)
        self.expression = expression
        self.operator = operator

class PrimaryExpression(Expression):
    pass

class Operand(PrimaryExpression):
    pass

class LiteralOperand(Operand):
    def __init__(self, loc, literal) -> None:
        super().__init__(loc)
        self.literal = literal

class IdentifierOperand(Operand):
    def __init__(self, loc, identifier : str) -> None:
        super().__init__(loc)
        self.identifier = identifier

class ExpressionOperand(Operand):
    def __init__(self, loc, expression : Expression) -> None:
        super().__init__(loc)
        self.expression = expression

class MemberExpression(PrimaryExpression):
    def __init__(self, loc, object_expr : PrimaryExpression, member_id : str) -> None:
        super().__init__(loc)
        self.object_expr = object_expr
        self.member_id = member_id

class IndexExpression(PrimaryExpression):
    def __init__(self, loc, array_expr : PrimaryExpression, index_expr : Expression) -> None:
        super().__init__(loc)
        self.array_expr = array_expr
        self.index_expr = index_expr

class CastExpression(PrimaryExpression):
    def __init__(self, loc, type_spec : TypeSpecifier, cast_expr : Expression) -> None:
        super().__init__(loc)
        self.type_spec = type_spec
        self.cast_expr = cast_expr

class NewExpression(PrimaryExpression):
    def __init__(self, loc, type_spec : TypeSpecifier, param_list : List[Expression] = []) -> None:
        super().__init__(loc)
        self.type_spec = type_spec
        self.param_list = param_list

class CallExpression(PrimaryExpression):
    def __init__(self, loc, func_expr : PrimaryExpression, generics_spec_list : List[TypeSpecifier] = [], param_list : List[Expression] = []) -> None:
        super().__init__(loc)
        self.func_expr = func_expr
        self.generics_spec_list = generics_spec_list
        self.param_list = param_list

class IOExpression(PrimaryExpression):
    def __init__(self, loc, io_type : IOType, type_spec : TypeSpecifier, name : str) -> None:
        super().__init__(loc)
        self.io_type = io_type
        self.type_spec = type_spec
        self.name = name

class LambdaExpression(PrimaryExpression):
    def __init__(self, loc, func_sign : FunctionSignature, block_stmt : BlockStatement) -> None:
        super().__init__(loc)
        self.func_sign = func_sign
        self.block_stmt = block_stmt


if __name__ == "__main__":
    print(SimpleType(None, BasicType.BOOL))