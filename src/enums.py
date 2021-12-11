from enum import Enum

class BasicType(Enum):
    VOID = 0
    BOOL = 1
    I8 = 2
    U8 = 3
    I16 = 4
    U16 = 5
    I32 = 6
    U32 = 7
    I64 = 8
    U64 = 9
    F16 = 10
    F32 = 11
    F64 = 12
    

class UnaryOp(Enum):
    PLUS = 0
    MINUS = 1
    NOT = 2
    LOGICAL_NOT = 3

class BinaryOp(Enum):
    PLUS = 0
    MINUS = 1
    MUL = 2
    DIV = 3
    MOD = 4
    LSHIFT = 5
    RSHIFT = 6
    AND = 7
    OR = 8
    XOR = 9
    LOGICAL_AND = 10
    LOGICAL_OR = 11
    EQUAL = 12
    NOT_EQUAL = 13
    LESS = 14
    LESS_EQUAL = 15
    GREATER = 16
    GREATER_EQUAL = 17


class IOType(Enum):
    IN = 0
    OUT = 1