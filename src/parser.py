import ply.yacc as yacc
import argparse
import sys
from lexer import *

from termcolor import colored


start = 'start'  # 起始符号

# 起始
def p_expression_translationUnit(p):
    '''start : translation_unit'''
    # print('translation_unit')
    pass


# translation_unit A-> aA | bA | empty
def p_translationUnit_nest(p):
    '''translation_unit : block_decl translation_unit
                        | function_def translation_unit
                        | empty'''
    # print('block_decl', p[1])
    pass

def p_block_decl(p):
    '''block_decl : type_decl SEMICOLON
                  | variable_decl SEMICOLON
                  | constant_decl SEMICOLON
                  | function_decl SEMICOLON'''
    context["generic_func"].clear()
    print(p[1])

def p_type_decl(p):
    '''type_decl : type_alias_decl
                 | struct_decl
                 | interface_decl'''
    p[0] = f'type_decl {p[1]}'
    # print('type_decl', p[1])
    # pass

def p_type_alias_decl(p):
    '''type_alias_decl : TYPE ID ASSIGN type_spec'''
    context["type_alias"].append(p[2])
    # print('TYPE ID ASSIGN type_spec', p[2])
    pass

def p_variable_decl(p):
    '''variable_decl : LET declarator declarator_nest'''
    # print(f'variable_decl :let {p[2]}')
    p[0] = f'variable_decl {p[2]}'
    pass

def p_declarator_nest(p):
    '''declarator_nest : COMMA declarator declarator_nest
                       | empty'''
    pass

def p_constant_decl(p):
    '''constant_decl : CONST declarator declarator_nest'''
    pass

def p_declarator(p):
    '''declarator : ID type_spec_colon_opt ASSIGN expression'''
    p[0] = f'declarator {p[1]} = expression'
    pass

def p_type_spec_colon_opt(p):
    '''type_spec_colon_opt : COLON type_spec
                           | empty'''

def p_function_decl(p):
    '''function_decl : FUNC ID function_sign '''
    p[0] = f'function_decl {p[2]}'
    pass

def p_function_def(p):
    '''function_def : function_decl block_statement'''
    context["generic_func"].clear()
    p[0] = f'{p[1]}'
    print('function_def', p[1])

def p_type_spec(p):
    '''type_spec : simple_type
                 | complex_type
                 | generic_type
                 | alias_type
                 | array_type
                 | reference_type
                 | function_type'''
    # print('type_spec', p[1])
    p[0] = f'{p[1]}'
    # pass

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
    p[0] = f'({p[1]})'
    # print(f'simple type:', p[1])


def p_complex_type(p):
    '''complex_type : INTERFACEID generics_specialization_list_opt
                    | STRUCTID generics_specialization_list_opt'''
    p[0] = f'{p[1]}'

def p_generic_type(p):
    '''generic_type : GENERICID'''
    p[0] = f'{p[1]}'

def p_alias_type(p):
    '''alias_type : TYPEALIASID'''
    p[0] = f'{p[1]}'

def p_array_type(p):
    '''array_type : type_spec LBRACKET int_literal_opt RBRACKET'''
    p[0] = f'{p[1]}[{p[3]}]'

def p_int_literal_opt(p):
    '''int_literal_opt : INT
                       | empty'''
    p[0] = p[1]
    pass

def p_reference_type(p):
    '''reference_type : type_spec REF'''
    pass

def p_function_type(p):
    '''function_type : function_sign'''
    pass

def p_struct_decl(p):
    '''struct_decl : STRUCT ID new_struct generics_type_list_opt complex_type_colon_opt LBRACE member_decl_nest RBRACE'''
    p[0] = f'struct {p[2]} {p[4]} {p[6]}'
    context["generic_top"].clear()
    # pass

def p_new_struct(p):
    '''new_struct :'''
    context["struct"].append(p[-1])
    context["last_scope_is_top"] = True


def p_complex_type_colon_opt(p):
    '''complex_type_colon_opt : COLON complex_type
                              | empty'''
    if (len(p) == 3):
        p[0] = f'{p[1]} {p[2]}'
    pass
    
    
def p_interface_decl(p):
    '''interface_decl : INTERFACE ID new_interface generics_type_list_opt LBRACE  interface_member_decl_nest RBRACE'''
    p[0] = f'interface {p[2]} {p[3]} {p[5]}'
    context["generic_top"].clear()
    pass

def p_new_interface(p):
    '''new_interface :'''
    context["interface"].append(p[-1])
    context["last_scope_is_top"] = True

def p_generics_type_list_opt(p):
    '''generics_type_list_opt : generics_type_list
                              | empty'''
    p[0] = f'generics_type_list_opt {p[1]}'
    context["last_scope_is_top"] = False


def p_member_decl_nest(p):
    '''member_decl_nest : member_decl member_decl_nest
                        | empty'''
    if (len(p) == 3):
        p[0] = f'{p[1]} {p[2]}'

def p_interface_member_decl_nest(p):
    '''interface_member_decl_nest : interface_member_decl interface_member_decl_nest
                                  | empty'''
    if (len(p) == 3):
        p[0] = f'{p[1]} {p[2]}'

def p_member_decl(p):
    '''member_decl : member_declarator
                   | function_def
                   | type_function_def'''
    p[0] = f'{p[1]}'

def p_type_function_def(p):
    '''type_function_def : type_function_decl block_statement'''
    pass

def p_interface_member_decl(p):
    '''interface_member_decl : function_decl SEMICOLON
                             | type_function_decl SEMICOLON'''
    context["generic_func"].clear()
    p[0] = f'{p[1]}'

def p_type_function_decl(p):
    '''type_function_decl : FUNC type_spec function_sign'''
    pass

def p_member_declarator(p):
    '''member_declarator : ID COLON type_spec SEMICOLON'''
    p[0] = f'member_declarator {p[1]} '
    pass

def p_function_sign(p):
    '''function_sign : generics_type_list_opt LPAREN parameter_decl_list_opt RPAREN type_spec_assigntype_opt '''
    p[0] = f'{p[3]}'
    pass

def p_parameter_decl_list_opt(p):
    '''parameter_decl_list_opt : parameter_decl parameter_decl_comma_nest
                               | empty'''
    if (len(p) == 3):
        p[0] = f'parameter_decl_list_opt {p[1]} {p[2]}'
    pass

def p_type_spec_assigntype_opt(p):
    '''type_spec_assigntype_opt : ASSIGNTYPE type_spec
                                | empty'''
    if (len(p) == 3):
        p[0] = f'-> {p[2]}'
    pass

def p_parameter_decl_comma_opt(p):
    '''parameter_decl_comma_nest : COMMA parameter_decl parameter_decl_comma_nest
                                 | empty'''
    if (len(p) == 4):
        p[0] = f'{p[2]} {p[3]}'
    pass

def p_parameter_decl(p):
    '''parameter_decl : ID type_spec_colon_opt'''
    p[0] = p[1]
    pass

"""
泛型
"""
def p_generics_type_list(p):
    '''generics_type_list : LANGRBRACKET generics_type generics_type_comma_nest RANGRBRACKET'''
    p[0] = f'<{p[2]} {p[3]}>'
    pass

def p_generics_type_comma_nest(p):
    '''generics_type_comma_nest : COMMA generics_type generics_type_comma_nest
                                | empty'''
    if (len(p) == 4):
        p[0] = f'{p[1]} {p[2]} {p[3]}'

def p_generics_type(p):
    '''generics_type : ID generics_type_range_colon_opt'''
    context["generic_top" if context["last_scope_is_top"] else "generic_func"].append(p[1])
    p[0] = f'{p[1]}  {p[2]}'

def p_generics_type_range_colon_opt(p):
    '''generics_type_range_colon_opt : COLON generics_type_range
                                     | empty'''
    if (len(p) == 3):
        p[0] = f'{p[1]} {p[2]}'

def p_generics_type_range(p):
    '''generics_type_range : complex_type'''
    p[0] = f'{p[1]}'

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
    pass

def p_decl_statement(p):
    '''decl_statement : variable_decl SEMICOLON
                      | constant_decl SEMICOLON'''
    pass

def p_block_statement(p):
    '''block_statement : LBRACE statement_nest RBRACE'''
    pass

def p_statement_nest(p):
    '''statement_nest : statement statement_nest
                      | empty'''
    pass


def p_expression_statement(p):
    '''expression_statement : expression_opt SEMICOLON'''
    pass

def p_expression_opt(p):
    '''expression_opt : expression
                      | empty'''
    pass

def p_if_statement(p):
    '''if_statement : IF LPAREN expression RPAREN statement statement_else_opt'''
    pass

def p_statement_else_opt(p):
    '''statement_else_opt : ELSE statement
                          | empty'''
    pass

def p_iteration_statement(p):
    '''iteration_statement : while_clause
                             | for_clause'''
    pass

def p_while_clause(p):
    '''while_clause : WHILE LPAREN expression RPAREN statement'''
    pass

def p_for_clause(p):
    '''for_clause : FOR LPAREN for_init_statement expression_opt SEMICOLON expression RPAREN statement'''
    pass

def p_forInit_statement(p):
    '''for_init_statement : expression_statement
                          | decl_statement'''
    pass

def p_jump_statement(p):
    '''jump_statement : BREAK SEMICOLON
                      | CONTINUE SEMICOLON
                      | RETURN expression_opt SEMICOLON'''
    pass

"""
表达式
"""

def p_expression(p):
    '''expression : assign_expr
                  | binary_expr
                  | unary_expr'''
    print('expression', p[1])
    pass

def p_assign_expr(p):
    '''assign_expr : expression ASSIGN expression'''
    pass

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
    p[0] = f'{p[1]} {p[2]}'
    # print(p[0])

# ++ --
def p_unary_expr(p):
    '''unary_expr : unary_operation_opt primary_expr '''
    p[0] = f'{p[2]}'
    print('primary_expr', p[2])

# 单目
def p_unary_opration_opt(p):
    '''unary_operation_opt : NOT
                           | LOGICAL_NOT
                           | PLUS %prec UPLUS
                           | MINUS %prec UMINUS
                           | empty'''
    pass

def p_primary_expr(p):
    '''primary_expr : operand
                    | call_expr
                    | index_expr
                    | cast_expr
                    | new_expr
                    | member_expr
                    | io_expr'''
    p[0] = f'{p[1]}'

def p_operand(p):
    '''operand : INT
               | HEXADECIMAL
               | FLOAT
               | DOUBLE
               | STRING
               | ID
               | LPAREN expression RPAREN'''
    # print('operand', p[1])
    p[0] = f'operand {p[1]}'


def p_member_expr(p):
    '''member_expr : ID DOT ID'''
    p[0] = f'member_expr {p[1]} {p[3]}'

def p_index_expr(p):
    '''index_expr : primary_expr LBRACKET expression RBRACKET'''
    p[0] = f'index_expr {p[1]} {p[3]}'

def p_cast_expr(p):
    '''cast_expr : LPAREN type_spec RPAREN expression'''
    pass

def p_new_expr(p):
    '''new_expr : type_spec LPAREN parameter_list_opt RPAREN'''
    p[0] = f'new_expr {p[1]}'
    print(f'new_expr {p[1]}')

def p_parameter_list_opt(p):
    '''parameter_list_opt : parameter_list
                          | empty'''
    pass

def p_call_expr(p):
    '''call_expr : primary_expr LPAREN parameter_list_opt RPAREN
                 | primary_expr LANGRBRACKET type_spec type_spec_comma_nest RANGRBRACKET LPAREN parameter_list_opt RPAREN'''
    p[0] = f'call_expr {p[1]}'


def p_generics_specialization_list_opt(p):
    '''generics_specialization_list_opt : LANGRBRACKET type_spec type_spec_comma_nest RANGRBRACKET
                                        | empty'''
    pass

def p_type_spec_comma_nest(p):
    '''type_spec_comma_nest : COMMA type_spec type_spec_comma_nest
                            | empty'''

    pass

def p_parameter_list(p):
    '''parameter_list : expression expression_comma_nest'''
    pass

def p_expression_comma_nest(p):
    '''expression_comma_nest : COMMA expression expression_comma_nest
                             | empty'''
    pass

def p_io_expr(p):
    '''io_expr : in_out LANGRBRACKET type_spec RANGRBRACKET LPAREN STRING RPAREN'''

def p_in_out(p):
    '''in_out : IN
              | OUT'''

# 空产生式
def p_empty(p):
    'empty :'
    pass

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


# def p_expression_translationUnit(p):
#     '''start : translation_unit'''
#     pass
#
# # translation_unit A-> aA | bA | empty
# def p_translationUnit_nest(p):
#     '''translation_unit : block_decl translation_unit
#                         | function_def translation_unit
#                         | empty'''
#     pass

# def p_expression_term(p):
#     '''start : A'''
#     print('start : A', p[1])
#
# # 嵌套 重复0次到多次
# def p_A(p):
#     '''A : INT A
#          | DOUBLE A
#          | empty'''
#     if (p[1] == None):
#         print('empty')
#     else:
#         print('p_A', p[1])
#
# # 可选opt
# def p_B(p):
#     '''B : INT B_opt INT'''
#     print('B', p[1], p[3])
#
# def p_B_opt(p):
#     '''B_opt : DOUBLE
#              | empty'''
#     print('B_opt', p[1])

# def p_expression_term(p):
#     '''start : ID SEMICOLON
#              | factor SEMICOLON'''
#     p[0] = p[1]

# def p_term_factor(p):
#     'term : factor'
#     p[0] = p[1]
#
# def p_factor_expr(p):
#     'factor : LPAREN start RPAREN'
#     p[0] = p[2]
#
#
# def p_expression_plus(p):
#     'start : start PLUS term'
#     p[0] = p[1] + p[3]
#
# def p_expression_minus(p):
#     'start : start MINUS term'
#     p[0] = p[1] - p[3]
#
# def p_term_mul(p):
#     'term : term MUL factor'
#     p[0] = p[1] * p[3]
#
# def p_term_div(p):
#     'term : term DIV factor'
#     p[0] = p[1] / p[3]
#
# # 整型变量
# def p_factor_num(p):
#     'factor : INT'
#     p[0] = p[1]

# 32位浮点变量
# def p_factor_float(p):
#     'factor : FLOAT'
#     p[0] = p[1]
#
# # 64位浮点变量
# def p_factor_double(p):
#     'factor : DOUBLE'
#     p[0] = p[1]


# Error rule for syntax errors
def p_error(p):
    print(colored("Syntax error in input!", color='red'), "Traceback", p)




lexer = lex.lex()
parser = yacc.yacc(start='start')

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
        