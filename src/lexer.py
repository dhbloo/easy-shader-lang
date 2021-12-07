import ply.lex as lex

# 泛型状态
states = (
    ('generics', 'inclusive'),
)

# token名list
tokens = [
    'GENERICS',         # 泛型
    'ID',               # 标识符
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
    'RETURNTYPE',       # ->
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
    'ref'       : 'ref',
    'type'      : 'TYPE',
    'void'      : 'VOID',
    'bool'      : 'BOOL',
    'i8'        : 'I8',
    'i16'       : 'I16',
    'i32'       : 'I32',
    'u8'        : 'U8',
    'u16'       : 'U16',
    'u32'       : 'U32',
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
t_RETURNTYPE    = r'->'
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
    # r'[-+]?(([0-9]*\.[0-9]+)|([0-9]+\.))(f|F)?'  # 普通浮点计数
    # r'[+-]*\d+\.?\d*[Ee]*[+-]*\d+(f|F)?'  # 原版有问题的合并计数
    r'[+-]*(([0-9]*\.[0-9]+)|([0-9]+\.))[Ee]*[+-]*\d+(f|F)?'
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
    t.type = reserved.get(t.value, 'ID')  # Check for reserved words
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

    # Build the lexer
    lexer = lex.lex()

    # Test it out
    data = 'aasd  12.123e2f  0.123e2f'

    # Give the lexer some input
    lexer.input(data)

    # Tokenize
    while True:
        tok = lexer.token()
        if not tok:
            break  # No more input
        print(tok)


