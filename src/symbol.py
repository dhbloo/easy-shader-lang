from __future__ import annotations 
from typing import Dict, Optional
from llvmlite import ir

from .error import SemanticError
from .type import Type


class Symbol():
    """单个符号，包含标识符，类型和LLVm IR值"""
    def __init__(self, id : str, type : Type, value : Optional[ir.Value | int]) -> None:
        self.id = id
        self.type = type
        self.value = value


class SymbolTable():
    """符号表，需要记录其父符号表的引用和域类型（函数或结构体）"""
    def __init__(self, parent : Optional[SymbolTable], parent_type : Optional[Type]) -> None:
        self.parent = parent
        self.parent_type = parent_type
        self.type_table : Dict[str, Type] = {}
        self.symbol_table : Dict[str, Symbol] = {}
        self.unnamed_index = 0

    def is_root(self) -> bool:
        return self.parent is None

    def add_symbol(self, name : str, type : Type, value : Optional[ir.Value | int] = None) -> Symbol:
        if name in self.symbol_table:
            raise SemanticError(f'redefine symbol {name}')
    
        new_symbol = Symbol(name, type, value)
        self.symbol_table[name] = new_symbol
        return new_symbol

    def add_type(self, name : str, type : Type) -> None:
        if name in self.type_table:
            raise SemanticError(f'redefine type name {name}')

        self.type_table[name] = type

    def add_unnamed_type(self, type : Type) -> None:
        self.add_type(f'__unnamed_type_{self.unnamed_index}', type)
        self.unnamed_index += 1

    def query_type(self, name : str) -> Type:
        if name in self.type_table:
            return self.type_table[name]

        if self.parent:
            return self.parent.query_type(name)
        else:
            raise SemanticError(f'undefined type {name}')

    def query_symbol(self, name : str) -> Symbol:
        if name in self.symbol_table:
            return self.symbol_table[name]
        
        if self.parent:
            return self.parent.query_symbol(name)
        else:
            raise SemanticError(f'undefined symbol {name}')

    def query_local_symbol(self, name : str) -> Optional[Symbol]:
        if name in self.symbol_table:
            return self.symbol_table[name]
        else:
            return None

    def clone(self) -> SymbolTable:
        new_symbol_table = SymbolTable(self.parent, self.parent_type)
        new_symbol_table.type_table = dict(**self.type_table)
        new_symbol_table.symbol_table = dict(**self.symbol_table)
        return new_symbol_table
