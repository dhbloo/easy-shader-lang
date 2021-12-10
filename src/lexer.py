import ply.lex as lex
import argparse
import sys

context = {
    "struct" : [],
    "interface" : [],
    "type_alias" : [],
    "generic_top" : [],
    "generic_func" : [],
    "last_scope_is_top" : False,
}

def clear_context():
    context["struct"].clear()  # strcut名
    context["interface"].clear()  # interface名
    context["type_alias"].clear()  # interface名
    context["generic_top"].clear()  # struct和interface的泛型名
    context["generic_func"].clear()  # function泛型名
    context["last_scope_is_top"] = False

def query_name(name):
    #return None
    if name in context["struct"]:
        return 'STRUCTID'
    elif name in context["interface"]:
        return 'INTERFACEID'
    elif name in context["type_alias"]:
        return 'TYPEALIASID'
    elif name in context["generic_top"] or name in context["generic_func"]:
        return 'GENERICID'
    else:
        return None


# 泛型状态
states = (
    ('generics', 'inclusive'),
)

# token名list
tokens = [
    'ID',               # 标识符
    'STRUCTID',         # Struct 标识符
    'INTERFACEID',      # Interface 标识符
    'TYPEALIASID',      # TypeAlias 标识符
    'GENERICID',        # 泛型 标识符
    'INT',              # 10进制数字
    'HEXADECIMAL',      # 16进制数字
    'FLOAT',            # 32位浮点数
    'DOUBLE',           # 64位浮点数
    'STRING',           # 字符串
    'PLUS',             # +
    'MINUS',            # -
    'OR',               # |
    'XOR',              # ^
    'AND',              # &
    'NOT',              # ~
    'MUL',              # *
    'DIV',              # /
    'MOD',              # %
    'ASSIGNTYPE',       # ->
    'LSHIFT',           # <<
    'RSHIFT',           # >>
    'LOGICAL_OR',       # ||
    'LOGICAL_AND',      # &&
    'LOGICAL_NOT',      # !
    'EQUAL',            # ==
    'NOT_EQUAL',        # !=
    'LESS',             # <
    'LESS_EQUAL',       # <=
    'GREATER',          # >
    'GREATER_EQUAL',    # >=
    'ASSIGN',           # =
    'LPAREN',           # (
    'RPAREN',           # )
    'LBRACKET',         # [
    'RBRACKET',         # ]
    'LANGRBRACKET',     # <
    'RANGRBRACKET',     # >
    'LBRACE',           # {
    'RBRACE',           # }
    'COMMA',            # ,
    'COLON',            # :
    'DOT',              # .
    'SEMICOLON',        # ;
]

# 保留字
reserved = {
    'let'       : 'LET',
    'in'        : 'IN',
    'out'       : 'OUT',
    'ref'       : 'REF',
    'type'      : 'TYPE',
    'void'      : 'VOID',
    'bool'      : 'BOOL',
    'i8'        : 'I8',
    'i16'       : 'I16',
    'i32'       : 'I32',
    'i64'       : 'I64',
    'u8'        : 'U8',
    'u16'       : 'U16',
    'u32'       : 'U32',
    'u64'       : 'U64',
    'f16'       : 'F16',
    'f32'       : 'F32',
    'f64'       : 'F64',
    'if'        : 'IF',
    'else'      : 'ELSE',
    'for'       : 'FOR',
    'while'     : 'WHILE',
    'continue'  : 'CONTINUE',
    'break'     : 'BREAK',
    'struct'    : 'STRUCT',
    'const'     : 'CONST',
    'func'      : 'FUNC',
    'return'    : 'RETURN',
    'interface' : 'INTERFACE',
    'sampler'   : 'SAMPLER'
}

tokens = tokens + list(reserved.values())


# 简单token的匹配规则
t_PLUS          = r'\+'
t_MINUS         = r'-'
t_OR            = r'\|'
t_XOR           = r'\^'
t_AND           = r'&'
t_NOT           = r'~'
t_MUL           = r'\*'
t_DIV           = r'/'
t_MOD           = r'%'
t_ASSIGNTYPE    = r'->'
t_LSHIFT        = r'<<'
t_RSHIFT        = r'>>'
t_LOGICAL_OR    = r'\|\|'
t_LOGICAL_AND   = r'&&'
t_LOGICAL_NOT   = r'!'
t_EQUAL         = r'=='
t_NOT_EQUAL     = r'!='
t_LESS          = r'<'
t_LESS_EQUAL    = r'<='
t_GREATER       = r'>'
t_GREATER_EQUAL = r'>='
t_ASSIGN        = r'='
t_LPAREN        = r'\('
t_RPAREN        = r'\)'
t_LBRACKET      = r'\['
t_RBRACKET      = r'\]'
t_LBRACE        = r'\{'
t_RBRACE        = r'\}'
t_ANY_COMMA     = r','
t_COLON         = r':'
t_DOT           = r'\.'
t_SEMICOLON     = r';'



# 匹配到泛型类型声明的第一个<，进入泛型状态
def t_generics(t):
    r'\<'
    t.lexer.code_start = t.lexer.lexpos  # Record the starting position
    t.lexer.level = 1  # Initial brace level
    t.lexer.begin('generics')  # Enter 'generics' state
    t.type = 'LANGRBRACKET'
    t.value = '<'
    return t

# 泛型状态规则
def t_generics_LANGRBRACKET(t):
    r'\<'
    t.lexer.level += 1
    return t


def t_generics_RANGRBRACKET(t):
    r'\>'
    t.lexer.level -= 1

    # If closing brace, return the code fragment
    if t.lexer.level == 0:
        # t.value = t.lexer.lexdata[t.lexer.code_start-1:t.lexer.lexpos]
        # t.type = "GENERICS"
        t.lexer.lineno += t.value.count('\n')
        t.lexer.begin('INITIAL')
    return t



# 忽略空格
t_generics_ignore = " \t\n"


# 错误跳过
def t_generics_error(t):
    t.lexer.skip(1)


# b不同精度浮点数匹配规则
def t_FLOAT(t):
    r'[-+]?(([0-9]*\.[0-9]+)|([0-9]+\.))(f|F)?'  # 普通浮点计数
    # r'[+-]*\d+\.?\d*[Ee]*[+-]*\d+(f|F)?'  # 原版有问题的合并计数,不能如.123
    # r'[+-]*\d+\.\d+([Ee]-?\d+)?'
    # r'[+-]*\d*\.?\d*[Ee]*[+-]*\d+(f|F)?'
    t.type = 'FLOAT' if t.value[-1] == ('f' or 'F') else 'DOUBLE'
    t.value = float(t.value[:-1] if t.value[-1] == ('f' or 'F') else t.value)
    return t


# 十六进制匹配规则
def t_HEXADECIMAL(t):
    r'(0x|0X)[a-fA-F0-9]+'
    t.value = t.vlaue.hex()
    return t


# 整数匹配规则
def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t


# 字符串匹配规则
def t_ANY_STRING(t):
    r'\"((\\.)|[^"\\\n])*\"'
    t.value = t.value[1:-1]
    return t


# 标识符匹配规则
def t_ANY_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    # Check for reserved words & complex/generic type names
    # print(t.value)
    t.type = reserved.get(t.value) or query_name(t.value) or 'ID'
    return t


# 跟踪行号匹配规则
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


# 保存忽略字符，空格等
t_ignore = ' \t'


# 错误处理规则
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


if __name__ == '__main__':
    arg_parser = argparse.ArgumentParser()
    arg_parser.add_argument("file", nargs='?', type=argparse.FileType('r'), default=sys.stdin)
    args = arg_parser.parse_args()

    lexer = lex.lex()

    with args.file as f:
        program_str = f.read()
        print(program_str)
        print('=' * 60)
        
    clear_context()
    lexer.input(program_str)

    while True:
        tok = lexer.token()
        if not tok:
            break  # No more input
        print(tok)


