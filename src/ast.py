from __future__ import annotations 
from typing import List, Optional

from .enums import BasicType, BinaryOp, UnaryOp, IOType

class Indent():
    def __init__(self, ind=0) -> None:
        super().__init__()
        self.ind = ind

    def __str__(self) -> str:
        return '  ' * self.ind

    def __add__(self, i) -> Indent:
        return Indent(self.ind + i)

class Node():
    """Base class of AST node"""
    def __init__(self, loc) -> None:
        super().__init__()
        self.location = loc  # Row

    def __str__(self, ind=Indent()) -> str:
        raise NotImplementedError()
    
    def accept(self, visitor) -> None:
        visitor.visit(self)


###########################################################
## Sec1. Top-level declaration
###########################################################

class TranslationUnit(Node):
    def __init__(self, loc) -> None:
        super().__init__(loc)
        self.declaration_list : List[Node] = []
        self.block_declaration_list : List[BlockDeclaration] = []
        self.func_definition_list : List[FunctionDefinition] = []
        self.convertion_definition_list : List[ConvertionDefinition] = []

    def add_declaration(self, decl):
        if isinstance(decl, BlockDeclaration):
            self.block_declaration_list.insert(0, decl)
        elif isinstance(decl, FunctionDefinition):
            self.func_definition_list.insert(0, decl)
        elif isinstance(decl, ConvertionDefinition):
            self.convertion_definition_list.insert(0, decl)
        else:
            raise TypeError(f'unknown declaration type {type(decl)}')
        self.declaration_list.insert(0, decl)

    def __str__(self, ind=Indent()) -> str:
        out = 'Translation Unit:\n'
        for decl in self.block_declaration_list:
            out += decl.__str__(ind+1)
        for defi in self.func_definition_list:
            out += defi.__str__(ind+1)
        for defi in self.convertion_definition_list:
            out += defi.__str__(ind+1)
        return out

class BlockDeclaration(Node):
    pass

class TypeDeclaration(BlockDeclaration):
    pass

class TypeAliasDecl(TypeDeclaration):
    def __init__(self, loc, identifier, type_spec : TypeSpecifier) -> None:
        super().__init__(loc)
        self.identifier = identifier
        self.type_spec = type_spec

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Type Alias:\n{ind+1}ID: {self.identifier}\n{ind+1}Type: {self.type_spec.__str__(ind+2)}\n'

class VariableDecl(BlockDeclaration):
    def __init__(self, loc, declarator_list : List[Declarator], is_const : bool) -> None:
        super().__init__(loc)
        self.declarator_list = declarator_list
        if is_const:
            for decl in self.declarator_list:
                decl.is_const = True

    def __str__(self, ind=Indent()) -> str:
        out = f'{ind}Variable Declarations:\n'
        for decl in self.declarator_list:
            out += decl.__str__(ind+1)
        return out

class Declarator(Node):
    def __init__(self, loc, identifier, type_spec : Optional[TypeSpecifier], init_expr : Expression) -> None:
        super().__init__(loc)
        self.identifier = identifier
        self.type_spec = type_spec
        self.init_expr = init_expr
        self.is_const = False

    def __str__(self, ind=Indent()) -> str:
        const = '(const)' if self.is_const else ' '
        return f'{ind}Declarator: {const}\n{ind+1}ID: {self.identifier}\n{ind+1}Type: {self.type_spec.__str__(ind+2) if self.type_spec else "(empty)"}\n{ind+1}Initializer:\n{self.init_expr.__str__(ind+2)}'

class FunctionDefinition(Node):
    def __init__(self, loc, func_decl : FunctionDecl, block_stmt : BlockStatement) -> None:
        super().__init__(loc)
        self.func_decl = func_decl
        self.block_stmt = block_stmt

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Function Definition:\n{self.func_decl.__str__(ind+1)}{self.block_stmt.__str__(ind+1)}'

class ConvertionDecl(BlockDeclaration):
    def __init__(self, loc, func_sign : FunctionSignature) -> None:
        super().__init__(loc)
        self.func_sign = func_sign

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Convertion Declaration:\n{self.func_sign.__str__(ind+1)}'

class ConvertionDefinition(Node):
    def __init__(self, loc, conv_decl : ConvertionDecl, block_stmt : BlockStatement) -> None:
        super().__init__(loc)
        self.conv_decl = conv_decl
        self.block_stmt = block_stmt

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Convertion Definition:\n{self.conv_decl.__str__(ind+1)}{self.block_stmt.__str__(ind+1)}'


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
        return f'{self.type.name}'

class ComplexType(TypeSpecifier):
    def __init__(self, loc, identifier : str, is_interface : bool, generics_spec_list : List[TypeSpecifier] = []) -> None:
        super().__init__(loc)
        self.identifier = identifier
        self.is_interface = is_interface
        self.generics_spec_list = generics_spec_list

    def __str__(self, ind=Indent()) -> str:
        type_class = 'interface' if self.is_interface else 'struct'
        if len(self.generics_spec_list) > 0:
            gs_str = '['
            for i, gs in enumerate(self.generics_spec_list):
                if i > 0: gs_str += ', '
                gs_str += f'{gs.__str__(ind+1)}'
            gs_str += ']'
        else:
            gs_str = ''
        return f'{type_class} {gs_str} ({self.identifier})'

class AliasType(TypeSpecifier):
    def __init__(self, loc, type : str) -> None:
        super().__init__(loc)
        self.type = type

    def __str__(self, ind=Indent()) -> str:
        return f'AliasType({self.type})'

class GenericType(TypeSpecifier):
    def __init__(self, loc, type : str) -> None:
        super().__init__(loc)
        self.type = type

    def __str__(self, ind=Indent()) -> str:
        return f'GenericType({self.type})'

class ArrayType(TypeSpecifier):
    def __init__(self, loc, type : TypeSpecifier, size : Optional[int]) -> None:
        super().__init__(loc)
        self.type = type
        self.size = size

    def __str__(self, ind=Indent()) -> str:
        return f'{self.type.__str__(ind+1)}[{self.size or ""}]'

class ReferenceType(TypeSpecifier):
    def __init__(self, loc, type : TypeSpecifier) -> None:
        super().__init__(loc)
        self.type = type

    def __str__(self, ind=Indent()) -> str:
        return f'{self.type.__str__(ind+1)} ref'

class FunctionType(TypeSpecifier):
    def __init__(self, loc, func_sign : FunctionSignature) -> None:
        super().__init__(loc)
        self.func_sign = func_sign

    def __str__(self, ind=Indent()) -> str:
        return f'(\n{self.func_sign.__str__(ind)}{ind})'


###########################################################
## Sec3. Function declaration
###########################################################

class FunctionDecl(BlockDeclaration):
    def __init__(self, loc, identifier : str, func_sign : FunctionSignature) -> None:
        super().__init__(loc)
        self.identifier = identifier
        self.func_sign = func_sign

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Function Declaration:\n{ind+1}ID: {self.identifier}\n{self.func_sign.__str__(ind+1)}'

class FunctionSignature(Node):
    def __init__(self, loc, generics_type_list : List[GenericsType] = [], parameter_decl_list : List[ParameterDecl] = [], return_type_spec : Optional[TypeSpecifier] = None) -> None:
        super().__init__(loc)
        self.generics_type_list = generics_type_list
        self.parameter_decl_list = parameter_decl_list
        self.return_type_spec = return_type_spec

    def __str__(self, ind=Indent()) -> str:
        out = f'{ind}Function Signature:\n{ind+1}Generic Types:\n'
        for generic_type in self.generics_type_list:
            out += generic_type.__str__(ind+2)
        out += f'{ind+1}Parameters:\n'
        for parameter in self.parameter_decl_list:
            out += parameter.__str__(ind+2)
        out += f'{ind+1}Return Type: {self.return_type_spec.__str__(ind+2) if self.return_type_spec else "(Empty)"}\n'
        return out

class ParameterDecl(Node):
    def __init__(self, loc, identifier, type_spec : Optional[TypeSpecifier] = None) -> None:
        super().__init__(loc)
        self.identifier = identifier
        self.type_spec = type_spec

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Parameter Declaration:\n{ind+1}ID: {self.identifier}\n{ind+1}Type: {self.type_spec.__str__(ind+2) if self.type_spec else "(Empty)"}\n'


###########################################################
## Sec4. Generic declaration
###########################################################

class GenericsType(Node):
    def __init__(self, loc, identifier, type_range : Optional[ComplexType] = None) -> None:
        super().__init__(loc)
        self.identifier = identifier
        self.type_range = type_range

    def __str__(self, ind=Indent()) -> str:
        type_str = f': {self.type_range}' if self.type_range else ''
        return f'{ind}Generic Name: {self.identifier} {type_str}\n'


###########################################################
## Sec5. Structure/Interface definition
###########################################################

class StructDecl(TypeDeclaration):
    def __init__(self, loc, identifier : str, member_list : List[StructMember], generics_type_list : List[GenericsType] = [], base_type : list[ComplexType] = []) -> None:
        super().__init__(loc)
        self.identifier = identifier
        self.generics_type_list = generics_type_list
        self.base_type = base_type
        self.member_decl_list : List[MemberDecl] = []
        self.member_func_definition_list : List[MemberFuncDefinition] = []
        self.constructor_func_definition : ConstructorFuncDefinition = None
        for member in member_list:
            self.add_member(member)

    def add_member(self, member):
        if isinstance(member, MemberDecl):
            self.member_decl_list.append(member)
        elif isinstance(member, MemberFuncDefinition):
            self.member_func_definition_list.append(member)
        elif isinstance(member, ConstructorFuncDefinition):
            self.constructor_func_definition = member
        else:
            raise TypeError(f'unknown member type {type(member)}')

    def __str__(self, ind=Indent()) -> str:
        out = f'{ind}Struct Declaration:\n{ind+1}ID: {self.identifier}\n{ind+1}Generic Types:\n'
        for generic_type in self.generics_type_list:
            out += generic_type.__str__(ind+2)
        out += f'{ind+1}Base Type:\n'
        for i, base in enumerate(self.base_type):
            out += f'{ind+2}[{i}]: {base.__str__(ind+3)}\n'
        out += f'{ind+1}Members:\n'
        for member in self.member_decl_list:
            out += member.__str__(ind+2)
        for member in self.member_func_definition_list:
            out += member.__str__(ind+2)
        if self.constructor_func_definition:
            out += self.constructor_func_definition.__str__(ind+2)
        return out

class InterfaceDecl(TypeDeclaration):
    def __init__(self, loc, identifier : str, member_list : List[InterfaceMember], generics_type_list : List[GenericsType] = []) -> None:
        super().__init__(loc)
        self.identifier = identifier
        self.generics_type_list = generics_type_list
        self.member_decl_list = member_list

    def __str__(self, ind=Indent()) -> str:
        out = f'{ind}Interface Declaration:\n{ind+1}ID: {self.identifier}\n{ind+1}Generic Types:\n'
        for generic_type in self.generics_type_list:
            out += generic_type.__str__(ind+2)
        out += f'{ind+1}Members:\n'
        for member in self.member_decl_list:
            out += member.__str__(ind+2)
        return out

class StructMember(Node):
    pass

class InterfaceMember(Node):
    pass

class MemberDecl(StructMember):
    def __init__(self, loc, identifier : str, type_spec : TypeSpecifier) -> None:
        super().__init__(loc)
        self.identifier = identifier
        self.type_spec = type_spec

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Member Declarator:\n{ind+1}ID: {self.identifier}\n{ind+1}Type: {self.type_spec.__str__(ind+2)}\n'

class MemberFuncDecl(InterfaceMember):
    def __init__(self, loc, func_decl : FunctionDecl) -> None:
        super().__init__(loc)
        self.func_decl = func_decl

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Member Function Declaration:\n{self.func_decl.__str__(ind+1)}'

class MemberFuncDefinition(StructMember):
    def __init__(self, loc, func_definition : FunctionDefinition) -> None:
        super().__init__(loc)
        self.func_definition = func_definition

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Member Function Definition:\n{self.func_definition.__str__(ind+1)}'

class ConstructorFuncDefinition(StructMember):
    def __init__(self, loc, struct_type : ComplexType, func_sign : FunctionSignature, block_stmt : BlockStatement) -> None:
        super().__init__(loc)
        self.struct_type = struct_type
        self.func_sign = func_sign
        self.block_stmt = block_stmt

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Constructor Function Definition:\n{ind+1}{self.struct_type}\n{self.func_sign.__str__(ind+1)}{self.block_stmt.__str__(ind+1)}'

###########################################################
## Sec6. Statement
###########################################################

class Statement(Node):
    pass

class DeclarationStatement(Statement):
    def __init__(self, loc, declaration : VariableDecl) -> None:
        super().__init__(loc)
        self.declaration = declaration

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Declaration Statement:\n{self.declaration.__str__(ind+1)}'

class BlockStatement(Statement):
    def __init__(self, loc, statement_list : List[Statement]) -> None:
        super().__init__(loc)
        self.statement_list = statement_list

    def __str__(self, ind=Indent()) -> str:
        out = f'{ind}Block Statement:\n'
        for stmt in self.statement_list:
            out += stmt.__str__(ind+1)
        return out

class ExpressionStatement(Statement):
    def __init__(self, loc, expression : Optional[Expression]) -> None:
        super().__init__(loc)
        self.expression = expression

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Expression Statement:\n{self.expression.__str__(ind+1) if self.expression else ""}'

class IfStatement(Statement):
    def __init__(self, loc, condition : Expression, true_stmt : Statement, false_stmt : Optional[Statement] = None) -> None:
        super().__init__(loc)
        self.condition = condition
        self.true_stmt = true_stmt
        self.false_stmt = false_stmt

    def __str__(self, ind=Indent()) -> str:
        out = f'{ind}If Statement:\n'
        out += f'{ind+1}Condition:\n{self.condition.__str__(ind+2)}'
        out += f'{ind+1}True Statement:\n{self.true_stmt.__str__(ind+2)}'
        if self.false_stmt:
            out += f'{ind+1}False Statement:\n{self.false_stmt.__str__(ind+2)}'
        return out

class WhileStatement(Statement):
    def __init__(self, loc, cond_expr : Expression, loop_stmt : Statement) -> None:
        super().__init__(loc)
        self.cond_expr = cond_expr
        self.loop_stmt = loop_stmt

    def __str__(self, ind=Indent()) -> str:
        out = f'{ind}While Statement:\n'
        out += f'{ind+1}Condition:\n{self.cond_expr.__str__(ind+2)}'
        out += f'{ind+1}Loop Statement:\n{self.loop_stmt.__str__(ind+2)}'
        return out

class ForStatement(Statement):
    def __init__(self, loc, init_stmt : Statement, cond_expr : Optional[Expression], iter_expr : Optional[Expression], loop_stmt : Expression) -> None:
        super().__init__(loc)
        self.init_stmt = init_stmt
        self.cond_expr = cond_expr
        self.iter_expr = iter_expr
        self.loop_stmt = loop_stmt

    def __str__(self, ind=Indent()) -> str:
        out = f'{ind}For Statement:\n'
        out += f'{ind+1}Init Statement:\n{self.init_stmt.__str__(ind+2)}'
        if self.cond_expr:
            out += f'{ind+1}Condition:\n{self.cond_expr.__str__(ind+2)}'
        if self.iter_expr:
            out += f'{ind+1}Iteration:\n{self.iter_expr.__str__(ind+2)}'
        out += f'{ind+1}Loop Statement:\n{self.loop_stmt.__str__(ind+2)}'
        return out

class JumpStatement(Statement):
    pass

class BreakStatement(Statement):
    def __init__(self, loc) -> None:
        super().__init__(loc)

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Break Statement\n'

class ContinueStatement(Statement):
    def __init__(self, loc) -> None:
        super().__init__(loc)

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Continue Statement\n'

class ReturnStatement(Statement):
    def __init__(self, loc, expression : Optional[Expression]) -> None:
        super().__init__(loc)
        self.expression = expression

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Return Statement\n{self.expression.__str__(ind+1) if self.expression else ""}'


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

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Assign Expression:\n{self.lhs_expr.__str__(ind+1)}{self.rhs_expr.__str__(ind+1)}'

class BinaryExpression(Expression):
    def __init__(self, loc, lhs_expr : Expression, rhs_expr : Expression, operator : BinaryOp) -> None:
        super().__init__(loc)
        self.lhs_expr = lhs_expr
        self.rhs_expr = rhs_expr
        self.operator = operator

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Binary Expression:\n{ind+1}BinaryOp: {self.operator.name}\n{self.lhs_expr.__str__(ind+1)}{self.rhs_expr.__str__(ind+1)}'

class UnaryExpression(Expression):
    def __init__(self, loc, expression : Expression, operator : UnaryOp) -> None:
        super().__init__(loc)
        self.expression = expression
        self.operator = operator

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Unary Expression:\n{ind+1}UnaryOp: {self.operator.name}\n{self.expression.__str__(ind+1)}'

class PrimaryExpression(Expression):
    pass

class Operand(PrimaryExpression):
    pass

class LiteralOperand(Operand):
    def __init__(self, loc, literal, is_float=False) -> None:
        super().__init__(loc)
        self.literal = literal
        self.is_float = is_float

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Literal: ({type(self.literal)}) {self.literal}\n'

class IdentifierOperand(Operand):
    def __init__(self, loc, identifier : str) -> None:
        super().__init__(loc)
        self.identifier = identifier

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Identifier: {self.identifier}\n'

class ExpressionOperand(Operand):
    def __init__(self, loc, expression : Expression) -> None:
        super().__init__(loc)
        self.expression = expression

    def __str__(self, ind=Indent()) -> str:
        return self.expression.__str__(ind)

class MemberExpression(PrimaryExpression):
    def __init__(self, loc, object_expr : PrimaryExpression, member_id : str) -> None:
        super().__init__(loc)
        self.object_expr = object_expr
        self.member_id = member_id

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Member Expression:\n{ind+1}Object Expr:\n{self.object_expr.__str__(ind+2)}{ind+1}Member ID: {self.member_id}\n'

class IndexExpression(PrimaryExpression):
    def __init__(self, loc, array_expr : PrimaryExpression, index_expr : Expression) -> None:
        super().__init__(loc)
        self.array_expr = array_expr
        self.index_expr = index_expr

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Index Expression:\n{ind+1}Array Expr:\n{self.array_expr.__str__(ind+2)}{ind+1}Member ID:\n{self.index_expr.__str__(ind+2)}\n'

class RefExpression(PrimaryExpression):
    def __init__(self, loc, ref_expr : Expression) -> None:
        super().__init__(loc)
        self.ref_expr = ref_expr

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Ref Expression:\n{self.ref_expr.__str__(ind+1)}\n'

class CastExpression(PrimaryExpression):
    def __init__(self, loc, type_spec : TypeSpecifier, cast_expr : Expression) -> None:
        super().__init__(loc)
        self.type_spec = type_spec
        self.cast_expr = cast_expr

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Cast Expression:\n{ind+1}Type: {self.type_spec.__str__(ind+2)}\n{ind+1}Member ID:\n{self.cast_expr.__str__(ind+2)}\n'

class NewExpression(PrimaryExpression):
    def __init__(self, loc, type_spec : TypeSpecifier, param_list : List[Expression] = []) -> None:
        super().__init__(loc)
        self.type_spec = type_spec
        self.param_list = param_list

    def __str__(self, ind=Indent()) -> str:
        out = f'{ind}New Expression:\n{ind+1}Type: {self.type_spec.__str__(ind+2)}\n{ind+1}Parameters:\n'
        for param in self.param_list:
            out += param.__str__(ind+2)
        return out

class CallExpression(PrimaryExpression):
    def __init__(self, loc, func_expr : PrimaryExpression, generics_spec_list : List[TypeSpecifier] = [], param_list : List[Expression] = []) -> None:
        super().__init__(loc)
        self.func_expr = func_expr
        self.generics_spec_list = generics_spec_list
        self.param_list = param_list

    def __str__(self, ind=Indent()) -> str:
        out = f'{ind}Call Expression:\n'
        out += f'{ind+1}Function:\n{self.func_expr.__str__(ind+2)}'
        out += f'{ind+1}Generics:\n'
        for i, generics_spec in enumerate(self.generics_spec_list):
            out += f'{ind+2}[{i}]: {generics_spec.__str__(ind+3)}\n'
        out += f'{ind+1}Parameters:\n'
        for param in self.param_list:
            out += param.__str__(ind+2)
        return out

class IOExpression(PrimaryExpression):
    def __init__(self, loc, io_type : IOType, type_spec : TypeSpecifier, name : str) -> None:
        super().__init__(loc)
        self.io_type = io_type
        self.type_spec = type_spec
        self.name = name

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}IO Expression: ({self.io_type.name})\n{ind+1}Type: {self.type_spec.__str__(ind+2)}\n{ind+1}Name: {self.name}\n'

class LambdaExpression(PrimaryExpression):
    def __init__(self, loc, func_sign : FunctionSignature, block_stmt : BlockStatement) -> None:
        super().__init__(loc)
        self.func_sign = func_sign
        self.block_stmt = block_stmt

    def __str__(self, ind=Indent()) -> str:
        return f'{ind}Lambda Expression:\n{self.func_sign.__str__(ind+1)}{self.block_stmt.__str__(ind+1)}'


