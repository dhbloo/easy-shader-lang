import ply.yacc as yacc
from lexer import *


start = 'start'  # 起始符号

# 起始
def p_expression_translationUnit(p):
    '''start : translation_unit'''
    print('translation_unit')
    # pass


# translation_unit A-> aA | bA | empty
def p_translationUnit_nest(p):
    '''translation_unit : block_decl translation_unit
                        | function_def translation_unit
                        | empty'''
    print('block_decl', p[1])
    # pass

# block_declaration
def p_blockDec(p):
    '''block_decl : type_decl SEMICOLON
                  | variable_decl SEMICOLON
                  | constant_decl SEMICOLON
                  | function_decl SEMICOLON'''
    print('type_decl', p[1])
    pass

# type_decl
def p_typeDec(p):
    '''type_decl : type_alias_decl
                 | struct_decl
                 | interface_decl'''
    print('type_alias_decl', p[1])
    # pass

# type_alias_decl
def p_typeAliasDecl(p):
    '''type_alias_decl : TYPE ID ASSIGN type_spec'''
    print('TYPE ID ASSIGN type_spec', p[2])
    # pass

# variable_decl
def p_variableDecl(p):
    '''variable_decl : LET declarator declarator_nest'''
    pass

# variable_decl nest
def p_declarator_nest(p):
    '''declarator_nest : COMMA declarator declarator_nest
                       | empty'''
    pass

# constant_decl
def p_constantDecl(p):
    '''constant_decl : CONST declarator declarator_nest'''

# declarator
def p_declarator(p):
    '''declarator : ID type_spec_colon_opt ASSIGN expression'''
    pass

# declarator_nest
def p_typeSpecColon_opt(p):
    '''type_spec_colon_opt : COLON type_spec
                           | empty'''

# function_decl
def p_functioDecl(p):
    '''function_decl : FUNC ID function_sign'''
    pass

# function_def
def p_functionDef(p):
    '''function_def : function_decl block_statement'''
    pass

# type_spec
def p_typeSpec(p):
    '''type_spec : simple_type
                 | complex_type
                 | array_type
                 | reference_type
                 | function_type'''
    print('simple_type')
    # pass

# simple_type
def p_simpleType(p):
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
    print(p[1])
    # pass

# complex_type
def p_complexType(p):
    '''complex_type : ID'''
    pass

# array_type
def p_arrayType(p):
    '''array_type : type_spec LBRACKET int_literal_opt RBRACKET'''
    pass

def p_intLiteral_opt(p):
    '''int_literal_opt : INT
                       | empty'''
    pass

# reference_type
def p_referenceType(p):
    '''reference_type : type_spec REF'''
    pass

# function_type
def p_functionType(p):
    '''function_type : function_sign'''
    pass

# struct_decl
def p_structDecl(p):
    '''struct_decl : STRUCT generics_type_list_opt LBRACE member_decl_nest RBRACE'''
    pass

# interface_decl
def p_interfaceDecl(p):
    '''interface_decl : INTERFACE generics_type_list_opt LBRACE interface_member_decl_nest RBRACE'''
    pass

# generics_type_list_opt
def p_genericsTypeList_opt(p):
    '''generics_type_list_opt : generics_type_list
                              | empty'''

# member_decl_nest
def p_member_decl_nest(p):
    '''member_decl_nest : member_decl member_decl_nest
                        | empty'''

# member_decl_nest
def p_interface_member_decl_nest(p):
    '''interface_member_decl_nest : interface_member_decl interface_member_decl_nest
                        | empty'''

# member_decl
def p_member_decl(p):
    '''member_decl : declarator
                   | function_def'''

# interface_member_decl
def p_interface_member_decl(p):
    '''interface_member_decl : declarator
                             | function_decl'''

# function_sign
def p_function_sign(p):
    '''function_sign : generics_type_list_opt LPAREN parameter_decl_list_opt RPAREN type_spec_assigntype_opt '''

def p_p_parameterDeclList_opt(p):
    '''parameter_decl_list_opt : parameter_dcel_list
                               | empty'''

def p_typeSpecAssigntype_opt(p):
    '''type_spec_assigntype_opt : ASSIGNTYPE type_spec
                                | empty'''

# parameter_decl_list
def p_parameterDeclList(p):
    '''parameter_dcel_list : parameter_decl parameter_decl_comma_nest'''

# parameter_decl_comma_opt
def p_parameterDeclComma_opt(p):
    '''parameter_decl_comma_nest : COMMA parameter_decl parameter_decl_comma_nest
                                 | empty'''
    pass

# parameter_decl
def p_parameterDecl(p):
    '''parameter_decl : ID type_spec_colon_opt'''
    pass

"""
泛型
"""
# generics_type_list
def p_genericsTypeList(p):
    '''generics_type_list : LANGRBRACKET generics_type generics_type_comma_nest RANGRBRACKET'''
    pass

# generics_type_comma_nest
def p_genericsTypeComma_nest(p):
    '''generics_type_comma_nest : COMMA generics_type generics_type_comma_nest
                                | empty'''
    pass

# generics_type
def p_generics_type(p):
    '''generics_type : ID generics_type_range_colon_opt'''
    pass

# generics_type_range_comma_opt
def p_genericsTypeRangeColon_opt(p):
    '''generics_type_range_colon_opt : COLON generics_type_range
                                     | empty'''
    pass

# generics_type_range
def p_genericsTypeRange(p):
    '''generics_type_range : complex_type'''
    pass

"""
6种语句
"""

# statement
def p_statement(p):
    '''statement : decl_statement
                 | block_statement
                 | expression_statement
                 | if_statement
                 | iteration_statement
                 | jump_statement'''
    pass

# decl_statement
def p_declStatement(p):
    '''decl_statement : variable_decl SEMICOLON
                      | constant_decl SEMICOLON'''

# block_statement
def p_blockStatement(p):
    '''block_statement : LBRACE statement_nest RBRACE'''
    pass

# statement_nest
def p_statement_nest(p):
    '''statement_nest : statement statement_nest
                      | empty'''
    pass


# expression_statement
def p_expressionStatement(p):
    '''expression_statement : expression_opt SEMICOLON'''

def p_expressionOpt(p):
    '''expression_opt : expression
                      | empty'''

# if_statement
def p_ifStatement(p):
    '''if_statement : IF LPAREN expression RPAREN statement statement_else_opt'''
    pass

def p_statementElseOpt(p):
    '''statement_else_opt : ELSE statement
                          | empty'''
    pass


# iteration_statement
def p_iterationStatement(p):
    '''iteration_statement : while_clause
                             | for_clause'''
    pass

def p_whileClause(p):
    '''while_clause : WHILE LPAREN expression RPAREN statement'''
    pass

def p_forClause(p):
    '''for_clause : FOR LPAREN for_init_statement expression_opt SEMICOLON expression RPAREN statement'''
    pass

def p_forInitStatement(p):
    '''for_init_statement : expression_statement
                          | decl_statement'''
    pass

def p_jumpStatement(p):
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
    pass

def p_assignExpr(p):
    '''assign_expr : expression ASSIGN expression'''
    pass

# + - * /
def p_binaryExpr(p):
    '''binary_expr : expression PLUS expression
                   | expression MINUS expression
                   | expression MUL expression
                   | expression DIV expression'''
    pass

# ++ --
def p_unaryExpr(p):
    '''unary_expr : unary_operation_opt primary_expr '''
    pass

# 单目
def p_unaryOpration_opt(p):
    '''unary_operation_opt : NOT
                           | empty'''
    pass

def p_primaryExpr(p):
    '''primary_expr : operand
                    | member_expr
                    | index_expr
                    | cast_expr
                    | new_expr
                    | call_expr'''
    pass

def p_operand(p):
    '''operand : INT
               | FLOAT
               | DOUBLE
               | ID
               | LPAREN expression RPAREN'''
    pass

def p_member_expr(p):
    '''member_expr : ID DOT ID'''
    pass

def p_index_expr(p):
    '''index_expr : primary_expr LBRACKET expression RBRACKET'''
    pass

def p_cast_expr(p):
    '''cast_expr : LPAREN type_spec RPAREN expression'''
    pass

def p_new_expr(p):
    '''new_expr : type_spec LPAREN parameter_list_opt RPAREN'''
    pass

def p_parameter_list_opt(p):
    '''parameter_list_opt : parameter_list
                          | empty'''
    pass

def p_call_expr(p):
    '''call_expr : primary_expr generics_specialization_list_opt LPAREN parameter_list_opt RPAREN'''
    pass

def p_generics_specialization_list_opt(p):
    '''generics_specialization_list_opt : generics_specialization_list
                                        | empty'''
    pass

def p_generics_specialization_list(p):
    '''generics_specialization_list : LANGRBRACKET type_spec type_spec_comma_nest RANGRBRACKET'''
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

# 空产生式
def p_empty(p):
    'empty :'
    pass



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
    print("Syntax error in input!")


lexer = lex.lex()
# Build the parser
parser = yacc.yacc(start='start')

while True:
    try:
        s = input('calc > ')
    except EOFError:
        break
    if not s: continue
    result = parser.parse(s)
    print(result)