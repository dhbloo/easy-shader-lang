from __future__ import annotations 
from typing import Dict, List, Optional, Tuple
from llvmlite import ir

from .ast import ComplexType, Node
from .enums import BasicType, TypeKind
from .codegen import SemanticError
from .symbol import SymbolTable, Symbol


class Type():
    def __init__(self, 
                 basic_type : Optional[BasicType] = None,
                 generic_name : Optional[str] = None,
                 generic_type_range : Optional[Type] = None,
                 struct_name : Optional[str] = None,
                 is_interface : bool = False) -> None:
        super().__init__()
        assert ((basic_type or generic_name or struct_name) and 
                not (basic_type and generic_name) or 
                not (basic_type and struct_name) or
                not (generic_name and struct_name))
        self.is_const = False

        # Basic type
        self.basic_type : Optional[BasicType] = basic_type

        # Generic type
        self.generic_name : Optional[str] = generic_name
        self.generic_type_range : Optional[Type] = generic_type_range

        # Struct / Interface type
        self.struct_name : Optional[str] = struct_name
        self.is_interface : bool = is_interface
        self.base_type : Optional[Type] = None
        self.member_list : List[Symbol] = []

        # Array type
        self.array_dims : List[int] = []  # 0 for empty size

        # Reference type
        self.reference = False

        # Function type
        self.func_params : Optional[List[Symbol]] = None

        # Struct / Interface / Function type
        self.generics_type_list : List[Type] = []
        self.symbol_table : Optional[SymbolTable] = None
        self.unfinished_node : Optional[Node] = None

    def get_kind(self) -> TypeKind:
        if self.func_params:
            return TypeKind.FUNCTION
        elif self.reference:
            return TypeKind.REFERENCE
        elif len(self.array_dims) > 0:
            return TypeKind.ARRAY
        elif self.struct_name:
            return TypeKind.INTERFACE if self.is_interface else TypeKind.STRUCT
        elif self.generic_name:
            return TypeKind.GENERIC
        elif self.basic_type:
            return TypeKind.BASIC
        else:
            return TypeKind.AUTO

    def has_generics(self) -> bool:
        return len(self.generics_type_list) > 0

    def add_const(self) -> Type:
        self.is_const = True
        return self

    def add_symbol_table(self, parent_symbol_table : SymbolTable) -> Type:
        self.symbol_table = SymbolTable(parent_symbol_table, self)
        return self

    def add_generics_type(self, generics_type : Type) -> Type:
        assert generics_type.get_kind() == TypeKind.GENERIC
        self.generics_type_list.append(generics_type)
        if len(generics_type.generic_name) > 0:
            self.symbol_table.add_type(generics_type.generic_name, generics_type)
        else:
            self.symbol_table.add_unnamed_type(generics_type)
        return self

    def add_struct_base_type(self, base_type : Type) -> Type:
        self.base_type = base_type
        return self

    def add_struct_member(self, member : Symbol) -> Type:
        self.member_list.append(member)
        return self

    def add_array_dim(self, size : int) -> Type:
        if self.reference:
            raise SemanticError('type of array element can not be reference')
        self.array_dims.append(size)
        return self

    def add_ref(self) -> Type:
        if self.reference:
            raise SemanticError('can not create a reference to reference type')
        self.reference = True
        return self

    def add_func_params(self, param_list : List[Symbol]) -> Type:
        self.func_params = param_list
        return self

    def to_element_type(self) -> Type:
        assert self.get_kind() == TypeKind.ARRAY
        self.array_dims.pop()
        return self

    def remove_ref(self) -> Type:
        assert self.get_kind() == TypeKind.REFERENCE
        self.reference = False
        return self

    def to_return_type(self) -> Type:
        assert self.get_kind() == TypeKind.FUNCTION
        self.func_params = None
        self.generics_type_list = []
        self.symbol_table = None
        self.unfinished_node = None
        return self

    def clone(self) -> Type:
        temp_type = Type(basic_type=self.basic_type,
                         generic_name=self.generic_name,
                         generic_type_range=self.generic_type_range,
                         struct_name=self.struct_name,
                         is_interface=self.is_interface)
        temp_type.is_const = self.is_const
        temp_type.base_type = self.base_type
        temp_type.array_dims = [*self.array_dims]
        temp_type.reference = self.reference
        if self.func_params:
            temp_type.func_params = [*self.func_params]
        temp_type.generics_type_list = [*self.generics_type_list]
        temp_type.symbol_table = self.symbol_table
        temp_type.unfinished_node = self.unfinished_node

    # def __eq__(self, type: Type) -> bool:
    #     if not isinstance(type, Type):
    #         return False
    #     kind = self.get_kind()
    #     if kind != type.get_kind():
    #         return False
    #     return True

    def specialize(self, generics_spec_list : List[Type]) -> Type:
        pass

    def to_ir_type(self) -> ir.Type:
        pass