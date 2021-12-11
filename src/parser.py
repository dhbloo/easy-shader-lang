import ply.yacc as yacc
import argparse
import sys
import logging
from termcolor import colored

from lexer import *
import ast
from enums import BasicType, BinaryOp, UnaryOp, IOType

# from src.ast import TranslationUnit

start = 'start'  # 起始符号

# 起始
def p_expression_translationUnit(p):
    '''start : translation_unit'''
    p[0] = p[1]

# translation_unit A-> aA | bA | empty
def p_translationUnit_nest(p):
    '''translation_unit : block_decl translation_unit
                        | function_def translation_unit
                        | empty'''
    if p[1]:
        p[0] = p[2]
        p[0].declaration_list.append(p[1])
    else:
        p[0] = ast.TranslationUnit(p.lineno(1), [])

def p_block_decl(p):
    '''block_decl : type_decl SEMICOLON
                  | variable_decl SEMICOLON
                  | constant_decl SEMICOLON
                  | function_decl SEMICOLON'''
    context["generic_func"].clear()
    p[0] = p[1]

def p_type_decl(p):
    '''type_decl : type_alias_decl
                 | struct_decl
                 | interface_decl'''
    p[0] = p[1]

def p_type_alias_decl(p):
    '''type_alias_decl : TYPE ID ASSIGN type_spec'''
    context["type_alias"].append(p[2])
    p[0] = ast.TypeAliasDecl(p.lineno(1), p[2], p[3])

def p_variable_decl(p):
    '''variable_decl : LET declarator declarator_nest'''
    p[0] = ast.VariableDecl(p.lineno(1), [p[2]] + p[3], False)

def p_declarator_nest(p):
    '''declarator_nest : COMMA declarator declarator_nest
                       | empty'''
    if p[1]:
        p[0] = [p[2]] + p[3]
    else:
        p[0] = []

def p_constant_decl(p):
    '''constant_decl : CONST declarator declarator_nest'''
    p[0] = ast.VariableDecl(p.lineno(1), [p[2]] + p[3], True)

def p_declarator(p):
    '''declarator : ID type_spec_colon_opt ASSIGN expression'''
    p[0] = ast.Declarator(p.lineno(1), p[1], p[2], p[4])

def p_type_spec_colon_opt(p):
    '''type_spec_colon_opt : COLON type_spec
                           | empty'''
    if p[1]:
        p[0] = p[2]
    else:
        p[0] = None

def p_function_decl(p):
    '''function_decl : FUNC ID function_sign '''
    p[0] = ast.FunctionDecl(p.lineno(1), p[2], p[3])

def p_function_def(p):
    '''function_def : function_decl block_statement'''
    context["generic_func"].clear()
    p[0] = ast.FunctionDefinition(p.lineno(1), p[1], p[2])

def p_type_spec(p):
    '''type_spec : simple_type
                 | complex_type
                 | generic_type
                 | alias_type
                 | array_type
                 | reference_type
                 | function_type'''
    p[0] = p[1]

def p_simple_type(p):
    '''simple_type : VOID
                   | BOOL
                   | I8
                   | U8
                   | I16
                   | U16
                   | I32
                   | U32
                   | I64
                   | U64
                   | F16
                   | F32
                   | F64'''
    p[0] = ast.SimpleType(p.lineno(1), BasicType[p.slice[1].type])

def p_complex_type(p):
    '''complex_type : INTERFACEID generics_specialization_list_opt
                    | STRUCTID generics_specialization_list_opt'''
    p[0] = ast.ComplexType(p.lineno(1), p[1], p[2])

def p_generic_type(p):
    '''generic_type : GENERICID'''
    p[0] = ast.GenericType(p.lineno(1), p[1]) ##############################疑问

def p_alias_type(p):
    '''alias_type : TYPEALIASID'''
    p[0] = ast.AliasType(p.lineno(1), p[1])

def p_array_type(p):
    '''array_type : type_spec LBRACKET int_literal_opt RBRACKET'''
    p[0] = ast.ArrayType(p.lineno(1), p[1], p[3])  ####################### 如果没有opt，size为-1？

def p_int_literal_opt(p):
    '''int_literal_opt : INT
                       | empty'''
    if p[1]:
        p[0] = p[1]
    else:
        p[0] = None

def p_reference_type(p):
    '''reference_type : type_spec REF'''
    p[0] = ast.ReferenceType(p.lineno(1), p[1])

def p_function_type(p):
    '''function_type : function_sign'''
    p[0] = ast.FunctionType(p.lineno(1), p[1])

def p_struct_decl(p):
    '''struct_decl : STRUCT ID new_struct generics_type_list_opt complex_type_colon_opt LBRACE member_decl_nest RBRACE'''
    context["generic_top"].clear()
    p[0] = ast.StructDecl(p.lineno(1), p[2], p[7], p[4], p[5])

def p_new_struct(p):
    '''new_struct :'''
    context["struct"].append(p[-1])
    context["last_scope_is_top"] = True


def p_complex_type_colon_opt(p):
    '''complex_type_colon_opt : COLON complex_type
                              | empty'''
    if p[1]:
        p[0] = p[2]
    else:
        p[0] = None
    
def p_interface_decl(p):
    '''interface_decl : INTERFACE ID new_interface generics_type_list_opt LBRACE  interface_member_decl_nest RBRACE'''
    context["generic_top"].clear()
    p[0] = ast.InterfaceDecl(p.lineno(1), p[2], p[6], p[4])

def p_new_interface(p):
    '''new_interface :'''
    context["interface"].append(p[-1])
    context["last_scope_is_top"] = True

def p_generics_type_list_opt(p):
    '''generics_type_list_opt : generics_type_list
                              | empty'''
    context["last_scope_is_top"] = False
    if p[1]:
        p[0] = p[1]
    else:
        p[0] = []

def p_member_decl_nest(p):
    '''member_decl_nest : member_decl member_decl_nest
                        | empty'''
    if p[1]:
        p[0] = [p[1]] + p[2]
    else:
        p[0] = []


def p_interface_member_decl_nest(p):
    '''interface_member_decl_nest : interface_member_decl interface_member_decl_nest
                                  | empty'''
    if p[1]:
        p[0] = [p[1]] + p[2]
    else:
        p[0] = []

def p_member_decl(p):
    '''member_decl : member_declarator
                   | function_def
                   | type_function_def'''
    p[0] = p[1]

def p_type_function_def(p):
    '''type_function_def : type_function_decl block_statement'''
    p[0] = ast.MemberTypeFuncDefinition(p.lineno(1), p[1], p[2])

def p_interface_member_decl(p):
    '''interface_member_decl : function_decl SEMICOLON
                             | type_function_decl SEMICOLON'''
    context["generic_func"].clear()
    if isinstance(p[1], ast.FunctionDecl):
        p[0] = ast.MemberFuncDecl(p.lineno(2), p[1])
    else:
        p[0] = p[1]


def p_type_function_decl(p):
    '''type_function_decl : FUNC type_spec function_sign'''
    p[0] = ast.MemberTypeFuncDecl(p.lineno(1), p[2], p[3])

def p_member_declarator(p):
    '''member_declarator : ID COLON type_spec SEMICOLON'''
    p[0] = ast.MemberDecl(p.lineno(1), p[1], p[3])

def p_function_sign(p):
    '''function_sign : generics_type_list_opt LPAREN parameter_decl_list_opt RPAREN type_spec_assigntype_opt'''
    p[0] = ast.FunctionSignature(p.lineno(2), p[1], p[3], p[5])


def p_parameter_decl_list_opt(p):
    '''parameter_decl_list_opt : parameter_decl parameter_decl_comma_nest
                               | empty'''
    if p[1]:
        p[0] = [p[1]] + p[2]
    else:
        p[0] = []

def p_type_spec_assigntype_opt(p):
    '''type_spec_assigntype_opt : ASSIGNTYPE type_spec
                                | empty'''
    if p[1]:
        p[0] = p[2]
    else:
        p[0] = None

def p_parameter_decl_comma_opt(p):
    '''parameter_decl_comma_nest : COMMA parameter_decl parameter_decl_comma_nest
                                 | empty'''
    if p[1]:
        p[0] = [p[2]] + p[3]
    else:
        p[0] = []

def p_parameter_decl(p):
    '''parameter_decl : ID type_spec_colon_opt'''
    p[0] = ast.ParameterDecl(p.lineno(1), p[1], p[2])

"""
泛型
"""
def p_generics_type_list(p):
    '''generics_type_list : LESS generics_type generics_type_comma_nest GREATER'''
    p[0] = [p[2]] + p[3]

def p_generics_type_comma_nest(p):
    '''generics_type_comma_nest : COMMA generics_type generics_type_comma_nest
                                | empty'''
    if p[1]:
        p[0] = [p[2]] + p[3]
    else:
        p[0] = []


def p_generics_type(p):
    '''generics_type : ID generics_type_range_colon_opt'''
    context["generic_top" if context["last_scope_is_top"] else "generic_func"].append(p[1])
    p[0] = ast.GenericsType(p.lineno(1), p[1], p[2])

def p_generics_type_range_colon_opt(p):
    '''generics_type_range_colon_opt : COLON generics_type_range
                                     | empty'''
    if p[1]:
        p[0] = p[2]
    else:
        p[0] = None


def p_generics_type_range(p):
    '''generics_type_range : complex_type'''
    p[0] = p[1]

"""
6种语句
"""

def p_statement(p):
    '''statement : decl_statement
                 | block_statement
                 | expression_statement
                 | if_statement
                 | iteration_statement
                 | jump_statement'''
    p[0] = p[1]

def p_decl_statement(p):
    '''decl_statement : variable_decl SEMICOLON
                      | constant_decl SEMICOLON'''
    p[0] = ast.DeclarationStatement(p.lineno(1), p[1])

def p_block_statement(p):
    '''block_statement : LBRACE statement_nest RBRACE'''
    p[0] = ast.BlockStatement(p.lineno(1), p[2])

def p_statement_nest(p):
    '''statement_nest : statement statement_nest
                      | empty'''
    if p[1]:
        p[0] = [p[1]] + p[2]
    else:
        p[0] = []

def p_expression_statement(p):
    '''expression_statement : expression_opt SEMICOLON'''
    p[0] = ast.ExpressionStatement(p.lineno(1), p[1])

def p_expression_opt(p):
    '''expression_opt : expression
                      | empty'''  ###################################空的如何处理？
    if p[1]:
        p[0] = p[1]
    else:
        p[0] = None

def p_if_statement(p):
    '''if_statement : IF LPAREN expression RPAREN statement statement_else_opt'''
    p[0] = ast.IfStatement(p.lineno(1), p[3], p[5], p[6])

def p_statement_else_opt(p):
    '''statement_else_opt : ELSE statement
                          | empty'''
    if p[1]:
        p[0] = p[2]
    else:
        p[0] = None

def p_iteration_statement(p):
    '''iteration_statement : while_clause
                             | for_clause'''
    p[0] = p[1]

def p_while_clause(p):
    '''while_clause : WHILE LPAREN expression RPAREN statement'''
    p[0] = ast.WhileStatement(p.lineno(1), p[3], p[5])

def p_for_clause(p):
    '''for_clause : FOR LPAREN for_init_statement expression_opt SEMICOLON expression RPAREN statement'''
    p[0] = ast.ForStatement(p.lineno(1), p[3], p[4], p[6], p[8])

def p_forInit_statement(p):
    '''for_init_statement : expression_statement
                          | decl_statement'''
    p[0] = p[1]

def p_jump_statement(p):
    '''jump_statement : BREAK SEMICOLON
                      | CONTINUE SEMICOLON
                      | RETURN expression_opt SEMICOLON'''

    if p[1] == 'break':
        p[0] = ast.BreakStatement(p.lineno(1))
    elif p[1] == 'continue':
        p[0] = ast.ContinueStatement(p.lineno(1))
    elif p[1] == 'return':
        p[0] = ast.ReturnStatement(p.lineno(1), p[2])
"""
表达式
"""

def p_expression(p):
    '''expression : assign_expr
                  | binary_expr
                  | unary_expr
                  | primary_expr'''
    p[0] = p[1]

def p_assign_expr(p):
    '''assign_expr : expression ASSIGN expression'''
    p[0] = ast.AssignExpression(p.lineno(1), p[1], p[3])


# + - * / & == > >= < <= && || ! << >> != % | ^
def p_binary_expr(p):
    '''binary_expr : expression PLUS expression
                   | expression MINUS expression
                   | expression MUL expression
                   | expression DIV expression
                   | expression AND expression
                   | expression OR expression
                   | expression XOR expression
                   | expression MOD expression
                   | expression LSHIFT expression
                   | expression RSHIFT expression
                   | expression LOGICAL_OR expression
                   | expression LOGICAL_AND expression
                   | expression NOT_EQUAL expression
                   | expression EQUAL expression
                   | expression LESS_EQUAL expression
                   | expression LESS expression
                   | expression GREATER_EQUAL expression
                   | expression GREATER expression'''
    p[0] = ast.BinaryExpression(p.lineno(1), p[1], p[3], BinaryOp[p.slice[2].type])

# ++ --
def p_unary_expr(p):
    '''unary_expr : unary_operation primary_expr '''
    p[0] = ast.UnaryExpression(p.lineno(1), p[2], p[1])

# 单目
def p_unary_opration_opt(p):
    '''unary_operation : NOT
                           | LOGICAL_NOT
                           | PLUS %prec UPLUS
                           | MINUS %prec UMINUS'''
    if p[1]:
        p[0] = p[1]

def p_primary_expr(p):
    '''primary_expr : operand
                    | call_expr
                    | index_expr
                    | ref_expr
                    | cast_expr
                    | new_expr
                    | member_expr
                    | lambda_expr
                    | io_expr'''
    p[0] = p[1]

def p_operand(p):
    '''operand : INT
               | HEXADECIMAL
               | FLOAT
               | DOUBLE
               | STRING
               | ID
               | LPAREN expression RPAREN'''
    # print('operand', p[1])
    # print(p.lineno(1), p.slice[1].type)
    if len(p) == '(':
        p[0] = ast.ExpressionOperand(p.lineno(1), p[2])
    elif p.slice[1].type == 'ID':
        p[0] = ast.IdentifierOperand(p.lineno(1), p[1])
    else:
        p[0] = ast.LiteralOperand(p.lineno(1), p[1])

def p_member_expr(p):
    '''member_expr : primary_expr DOT ID'''
    p[0] = ast.MemberExpression(p.lineno(1), p[1], p[3])

def p_index_expr(p):
    '''index_expr : primary_expr LBRACKET expression RBRACKET'''
    p[0] = ast.IndexExpression(p.lineno(2), p[1], p[3])

def p_ref_expr(p):
    '''ref_expr : REF LPAREN expression RPAREN'''


def p_cast_expr(p):
    '''cast_expr : LPAREN type_spec RPAREN expression'''
    p[0] = ast.CastExpression(p.lineno(1), p[2], p[4])

def p_new_expr(p):
    '''new_expr : type_spec LPAREN parameter_list_opt RPAREN'''
    p[0] = ast.NewExpression(p.lineno(2), p[1], p[3])

def p_parameter_list_opt(p):
    '''parameter_list_opt : parameter_list
                          | empty'''
    if p[1]:
        p[0] = p[1]
    else:
        p[0] = []

def p_call_expr(p):
    '''call_expr : primary_expr LPAREN parameter_list_opt RPAREN
                 | primary_expr GENERICMARK LESS type_spec type_spec_comma_nest GREATER LPAREN parameter_list_opt RPAREN'''
    if len(p) == 5:
        p[0] = ast.CallExpression(p.lineno(3), p[1], [], p[3])
    else:
        p[0] = ast.CallExpression(p.lineno(2), p[1], [p[4]] + p[5], p[8])


def p_generics_specialization_list_opt(p):
    '''generics_specialization_list_opt : LESS type_spec type_spec_comma_nest GREATER
                                        | empty'''
    if p[1]:
        p[0] = [p[2]] + p[3]
    else:
        p[0] = []

def p_type_spec_comma_nest(p):
    '''type_spec_comma_nest : COMMA type_spec type_spec_comma_nest
                            | empty'''
    if p[1]:
        p[0] = [p[2]] + p[3]
    else:
        p[0] = []

def p_parameter_list(p):
    '''parameter_list : expression expression_comma_nest'''
    p[0] = p[2]

def p_expression_comma_nest(p):
    '''expression_comma_nest : COMMA expression expression_comma_nest
                             | empty'''
    if p[1]:
        p[0] = [p[2]] + p[3]
    else:
        p[0] = []

def p_lambda_expr(p):
    '''lambda_expr : FUNC function_sign block_statement'''
    p[0] = ast.LambdaExpression(p.lineno(1), p[2], p[3])

def p_io_expr(p):
    '''io_expr : in_out LESS type_spec GREATER LPAREN STRING RPAREN'''
    p[0] = ast.IOExpression(p.lineno(1), p[1], p[3], p[6])

def p_in_out(p):
    '''in_out : IN
              | OUT'''
    p[0] = IOType[p.slice[1].type]

# 空产生式
def p_empty(p):
    'empty :'
    p[0] = None

# 符号运算优先级
precedence = (
    ('left', 'LOGICAL_OR'),
    ('left', 'LOGICAL_AND'),
    ('left', 'OR'),
    ('left', 'XOR'),
    ('left', 'AND'),
    ('left', 'EQUAL', 'NOT_EQUAL'),
    ('left', 'LESS', 'LESS_EQUAL', 'GREATER', 'GREATER_EQUAL'),
    ('left', 'LSHIFT', 'RSHIFT'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MUL', 'DIV', 'MOD', ),
    ('right', 'UMINUS', 'UPLUS', 'LOGICAL_NOT', 'NOT'),            # Unary minus operator
)

# Error rule for syntax errors
def p_error(p):
    print(colored("Syntax error in input!", color='red'), "Traceback", p)


logging.basicConfig(
    level = logging.DEBUG,
    filename = "parselog.txt",
    filemode = "w",
    format = "%(filename)10s:%(lineno)4d:%(message)s"
)
log = logging.getLogger()
lexer = lex.lex()
parser = yacc.yacc(start='start', debug=log)

if __name__ == "__main__":
    arg_parser = argparse.ArgumentParser()
    arg_parser.add_argument("file", nargs='?', type=argparse.FileType('r'), default=sys.stdin)
    args = arg_parser.parse_args()

    with args.file as f:
        program_str = f.read()
        print(program_str)
        print('=' * 60)
        
    clear_context()
    result = parser.parse(program_str, tracking=True)
    print(result)
