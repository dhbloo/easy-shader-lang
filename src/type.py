from __future__ import annotations 
from typing import Dict, List, Optional, Tuple
from llvmlite import ir

from .ast import Node
from .enums import BasicType, TypeKind
from .error import SemanticError
from .utils.misc import merge_matched
from . import symbol


class Type():
    """表示一个ESL语言类型"""
    def __init__(self, 
                 basic_type : Optional[BasicType] = None,
                 generic_name : Optional[str] = None,
                 generic_type_range : Optional[Type] = None,
                 struct_name : Optional[str] = None,
                 is_interface : bool = False) -> None:
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
        self.base_type_list : List[Type] = []
        self.member_list : List[symbol.Symbol] = []
        self.constructor : Optional[symbol.Symbol] = None

        # Array type
        self.array_dims : List[int] = []  # 0 for empty size

        # Reference type
        self.reference = False

        # Function type
        self.func_ret_type : Optional[Type] = None
        self.func_params : List[symbol.Symbol] = []

        # Struct / Interface / Function type
        self.generics_type_list : List[Type] = []
        self.symbol_table : Optional[symbol.SymbolTable] = None
        self.unfinished_node : Optional[Node] = None

    def get_kind(self) -> TypeKind:
        if self.func_ret_type is not None:
            return TypeKind.FUNCTION
        elif self.reference:
            return TypeKind.REFERENCE
        elif len(self.array_dims) > 0:
            return TypeKind.ARRAY
        elif self.struct_name is not None:
            return TypeKind.INTERFACE if self.is_interface else TypeKind.STRUCT
        elif self.generic_name is not None:
            return TypeKind.GENERIC
        elif self.basic_type is not None:
            return TypeKind.BASIC
        else:
            return TypeKind.AUTO

    def has_generics(self) -> bool:
        return len(self.generics_type_list) > 0

    def is_generics(self) -> bool:
        kind = self.get_kind()

        if kind == TypeKind.AUTO:
            return False
        if kind == TypeKind.BASIC:
            return False
        elif kind == TypeKind.GENERIC:
            return True
        elif kind == TypeKind.STRUCT:
            for generics_type in self.generics_type_list:
                if generics_type.is_generics():
                    return True
            return False
        elif kind == TypeKind.ARRAY:
            element_type = self.clone().to_element_type()
            return element_type.is_generics()
        elif kind == TypeKind.REFERENCE:
            refered_type = self.clone().remove_ref()
            return refered_type.is_generics()
        elif kind == TypeKind.FUNCTION:
            for param_symbol in self.func_params:
                if param_symbol.type.is_generics():
                    return True
            return self.func_ret_type.is_generics()
        else:
            assert False

    def add_const(self) -> Type:
        self.is_const = True
        return self

    def add_symbol_table(self, parent_symbol_table : symbol.SymbolTable) -> Type:
        self.symbol_table = symbol.SymbolTable(parent_symbol_table, self)
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
        assert base_type.get_kind() == TypeKind.INTERFACE
        self.base_type_list.append(base_type)
        return self

    def add_struct_member(self, member : symbol.Symbol) -> Type:
        self.member_list.append(member)
        return self

    def add_array_dim(self, size : int) -> Type:
        kind = self.get_kind()
        if kind == TypeKind.BASIC and self.basic_type == BasicType.VOID:
            raise SemanticError('can not create array of void')
        if kind == TypeKind.REFERENCE:
            raise SemanticError('type of array element can not be reference')
        self.array_dims.insert(0, size)
        return self

    def add_ref(self) -> Type:
        if self.reference:
            raise SemanticError('can not create a reference to reference type')
        self.reference = True
        return self

    def add_func_param(self, param : symbol.Symbol) -> Type:
        self.func_params.append(param)
        return self

    def add_func_ret_type(self, ret_type : Type) -> Type:
        assert self.func_ret_type is None
        self.func_ret_type = ret_type
        return self

    def to_element_type(self) -> Type:
        assert self.get_kind() == TypeKind.ARRAY
        self.array_dims.pop()
        return self

    def get_array_size(self) -> int:
        assert self.get_kind() == TypeKind.ARRAY
        return self.array_dims[-1]

    def remove_ref(self) -> Type:
        assert self.get_kind() == TypeKind.REFERENCE
        self.reference = False
        return self

    def clone(self, clone_symbol_table=False) -> Type:
        temp_type = Type(basic_type=self.basic_type,
                         generic_name=self.generic_name,
                         generic_type_range=self.generic_type_range,
                         struct_name=self.struct_name,
                         is_interface=self.is_interface)
        temp_type.is_const = self.is_const
        temp_type.base_type_list = [*self.base_type_list]
        temp_type.array_dims = [*self.array_dims]
        temp_type.reference = self.reference
        temp_type.func_ret_type = self.func_ret_type
        temp_type.func_params = [*self.func_params]
        temp_type.generics_type_list = [*self.generics_type_list]
        if clone_symbol_table:
            temp_type.symbol_table = self.symbol_table.clone()
            temp_type.symbol_table.parent_type = temp_type
        else:
            temp_type.symbol_table = self.symbol_table
        temp_type.unfinished_node = self.unfinished_node
        return temp_type

    def __eq__(self, type: Type) -> bool:
        if not isinstance(type, Type):
            return False

        kind1, kind2 = self.get_kind(), type.get_kind()
        if kind1 == TypeKind.AUTO and kind2 == TypeKind.AUTO:
            return True
        elif kind1 == TypeKind.BASIC and kind2 == TypeKind.BASIC:
            return self.basic_type == type.basic_type
        elif kind1 == TypeKind.GENERIC and kind2.GENERIC:
            # TODO: better check
            return self.generic_name == type.generic_name
        elif kind1 == TypeKind.STRUCT and kind2 == TypeKind.STRUCT:
            return self.struct_name == type.struct_name
        elif kind1 == TypeKind.ARRAY and kind2 == TypeKind.ARRAY:
            return self.clone().to_element_type() == type.clone().to_element_type()
        elif kind1 == TypeKind.REFERENCE and kind2 == TypeKind.REFERENCE:
            return self.clone().remove_ref() == type.clone().remove_ref()
        elif kind1 == TypeKind.FUNCTION and kind2 == TypeKind.FUNCTION:
            if not (self.func_ret_type == type.func_ret_type):
                return False
            if len(self.func_params) != len(type.func_params):
                return False
            for param1, param2 in zip(self.func_params, type.func_params):
                if not (param1.type == param2.type):
                    return False
            return True

        return False

    def __str__(self) -> str:
        kind = self.get_kind()

        def get_generic_str(self) -> str:
            if not self.has_generics():
                return ''
            generic_str = '<'
            for i, generics_type in enumerate(self.generics_type_list):
                if i > 0:
                    generic_str += ', '
                generic_str += f'{generics_type}'
            generic_str += '>'
            return generic_str

        if kind == TypeKind.AUTO:
            return 'auto'
        if kind == TypeKind.BASIC:
            return self.basic_type.name
        elif kind == TypeKind.GENERIC:
            return self.generic_name
        elif kind == TypeKind.STRUCT:
            return f'struct {self.struct_name}{get_generic_str(self)}'
        elif kind == TypeKind.ARRAY:
            element_type = self.clone().to_element_type()
            return f'[{self.get_array_size()}]{element_type}'
        elif kind == TypeKind.REFERENCE:
            refered_type = self.clone().remove_ref()
            return f'{refered_type} ref'
        elif kind == TypeKind.FUNCTION:
            func_str = f'{get_generic_str(self)}('
            for i, param in enumerate(self.func_params):
                if i > 0:
                    func_str += ', '
                func_str += f'{param.type}'
            func_str += f') -> {self.func_ret_type}'
            return func_str
        else:
            assert False

    def to_ir_type(self) -> ir.Type:
        kind = self.get_kind()
        if kind == TypeKind.AUTO:
            return ir.VoidType()
        if kind == TypeKind.BASIC:
            if self.basic_type == BasicType.VOID:
                return ir.VoidType()
            if self.basic_type == BasicType.BOOL:
                return ir.IntType(1)
            elif self.basic_type == BasicType.F16:
                return ir.HalfType()
            elif self.basic_type == BasicType.F32:
                return ir.FloatType()
            elif self.basic_type == BasicType.F64:
                return ir.DoubleType()
            elif self.basic_type == BasicType.I8 or self.basic_type == BasicType.U8:
                return ir.IntType(8)
            elif self.basic_type == BasicType.I16 or self.basic_type == BasicType.U16:
                return ir.IntType(16)
            elif self.basic_type == BasicType.I32 or self.basic_type == BasicType.U32:
                return ir.IntType(32)
            elif self.basic_type == BasicType.I64 or self.basic_type == BasicType.U64:
                return ir.IntType(64)
            else:
                raise NotImplementedError(f'IR BasicType {self.basic_type} not implemented')
        elif kind == TypeKind.STRUCT:
            return ir.global_context.get_identified_type(self.struct_name)
        elif kind == TypeKind.ARRAY:
            element_type = self.clone().to_element_type()
            element_ir_type = element_type.to_ir_type()
            # if element_type.get_kind() == TypeKind.BASIC:
            #     return ir.VectorType(element_ir_type, self.array_dims[-1])
            # else:
            return ir.ArrayType(element_ir_type, self.array_dims[-1])
        elif kind == TypeKind.REFERENCE:
            refered_ir_type = self.clone().remove_ref().to_ir_type()
            return ir.PointerType(refered_ir_type)
        elif kind == TypeKind.FUNCTION:  # Return Function Pointer IR type
            ret_ir_type = self.func_ret_type.to_ir_type()
            param_ir_types = []
            for param in self.func_params:
                param_ir_types.append(param.type.to_ir_type())
            return ir.PointerType(ir.FunctionType(ret_ir_type, param_ir_types))
        else:
            assert False, "uninstantiabled type!"

    def match_generics(self, spec_type : Type) -> Tuple[bool, Dict[str, Type]]:
        """模版类型与函数实参匹配，推导出模版类型"""
        kind1, kind2 = self.get_kind(), spec_type.get_kind()
        if kind1 == TypeKind.GENERIC and kind2 != TypeKind.GENERIC:
            # Matched generics type with a specializaed type
            return True, { self.generic_name: spec_type }
        elif kind1 == TypeKind.AUTO and kind2 == TypeKind.AUTO:
            return True, {}
        elif kind1 == TypeKind.BASIC and kind2 == TypeKind.BASIC:
            return self.basic_type == spec_type.basic_type, {}
        elif kind1 == TypeKind.STRUCT and kind2 == TypeKind.STRUCT:
            return self.struct_name == spec_type.struct_name, {}
        elif kind1 == TypeKind.ARRAY and kind2 == TypeKind.ARRAY:
            return self.clone().to_element_type().match_generics(spec_type.clone().to_element_type())
        elif kind1 == TypeKind.REFERENCE and kind2 == TypeKind.REFERENCE:
            return self.clone().remove_ref().match_generics(spec_type.clone().remove_ref())
        elif kind1 == TypeKind.FUNCTION and kind2 == TypeKind.FUNCTION:
            if len(self.func_params) != len(spec_type.func_params):
                return False, {}
            ret, all_matched = self.func_ret_type.match_generics(spec_type.func_ret_type)
            if not ret:
                return False, {}
            for param1, param2 in zip(self.func_params, spec_type.func_params):
                ret, matched = param1.type.match_generics(param2.type)
                if not ret:
                    return False, {}
                all_matched = merge_matched(all_matched, matched)
                if all_matched is None:
                    return False, {}
            return True, all_matched
        else:
            return False, {}

    def specialize(self, generic_specialization_list : Dict[str, Type], spec_param_array_dims : Optional[Dict[str, List[int]]] = None) -> Type:
        """将该类型中嵌套的泛型类型按照实例化列表进行替换"""
        kind = self.get_kind()

        if kind == TypeKind.AUTO:
            return self
        if kind == TypeKind.BASIC:
            return self
        elif kind == TypeKind.GENERIC:
            if self.generic_name in generic_specialization_list:
                return generic_specialization_list[self.generic_name]
            else:
                return self
        elif kind == TypeKind.STRUCT:
            return self
        elif kind == TypeKind.ARRAY:
            element_type = self.clone().to_element_type()
            return element_type.specialize(generic_specialization_list).clone().add_array_dim(self.get_array_size())
        elif kind == TypeKind.REFERENCE:
            refered_type = self.clone().remove_ref()
            return refered_type.specialize(generic_specialization_list).clone().add_ref()
        elif kind == TypeKind.FUNCTION:
            func_type = self.clone(clone_symbol_table=True)
            func_type.func_ret_type = self.func_ret_type.specialize(generic_specialization_list)
            for i, param_symbol in enumerate(self.func_params):
                param_type = param_symbol.type.specialize(generic_specialization_list)
                # 实例化数组维数
                if spec_param_array_dims is not None and len(param_type.array_dims) > 0 and param_symbol.id in spec_param_array_dims:
                    param_type = param_type.clone()
                    array_dims = spec_param_array_dims[param_symbol.id]
                    for i, dim in enumerate(param_type.array_dims):
                        if dim == 0:
                            param_type.array_dims[i] = array_dims[i]
                new_param_symbol = func_type.symbol_table.replace_local_symbol_type(param_symbol.id, param_type)
                func_type.func_params[i] = new_param_symbol
            return func_type
        else:
            assert False