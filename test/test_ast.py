import argparse
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

from src.ast import *


t = ReferenceType(None,
    ArrayType(None, 
        SimpleType(None, BasicType.I32),
        size=4
    )
)
alias = TypeAliasDecl(None,
    "vec4",
    type_spec=t
)
var_decl = VariableDecl(None,
    [
        Declarator(None,
            "a",
            type_spec=t,
            init_expr=LiteralOperand(None, 1)
        ),
        Declarator(None,
            "b",
            type_spec=SimpleType(None, BasicType.F32),
            init_expr=LiteralOperand(None, 1.0)
        ),
    ],
    is_const=True
)
func_decl = FunctionDecl(None,
    "f",
    FunctionSignature(None,
        [
            GenericsType(None,
                "T",
                type_range=None
            ),
            GenericsType(None,
                "U",
                type_range=ComplexType(None,
                    "IClassA",
                    is_interface=True
                )
            ),
        ],
        [
            ParameterDecl(None,
                "arg1",
                type_spec=SimpleType(None, BasicType.F32),
            ),
            ParameterDecl(None,
                "arg2",
                type_spec=None,
            )
        ],
        SimpleType(None, BasicType.F32)
    )
)
print(func_decl)