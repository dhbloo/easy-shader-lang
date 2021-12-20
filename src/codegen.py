from typing import List, Tuple, Optional

from .utils import visitor
from llvmlite import ir
from .enums import BasicType, BinaryOp, TypeKind, UnaryOp
from .symbol import SymbolTable, Type
from .error import SemanticError
from .utils.misc import merge_matched
from . import ast
     
I_BASICTYPES = [BasicType.I8, BasicType.I16, BasicType.I32, BasicType.I64]
U_BASICTYPES = [BasicType.U8, BasicType.U16, BasicType.U32, BasicType.U64]
F_BASICTYPES = [BasicType.F16, BasicType.F32, BasicType.F64]

class CodeGenContext():
    """代码生成上下文"""
    def __init__(self, module_name) -> None:
        self.module = ir.Module(name=module_name)
        self.module.triple = "spir64-unknown-unknown"
        self.visitor = CodeGenVisitor(self)

        self.scope_stack : List[Tuple[SymbolTable, ir.IRBuilder, Type]] = []
        self.block_stack : List[Tuple[ir.Block, ir.Block]] = []
        self.symbol_table = SymbolTable(None, None)
        self.ir_builder : ir.IRBuilder = None
        self.deduced_ret_type : Optional[Type] = None
        self.current_type : Type = None
        self.current_value : ir.Value = None
        self.current_break_block : ir.Block = None
        self.current_continue_block : ir.Block = None
        self.current_object_value : ir.Value = None
        self.current_generic_spec_type : ir.Type = None
        self.create_global_init_function()

    def create_global_init_function(self):
        """创建全局初始化函数"""
        self.init_func_type = ir.FunctionType(ir.VoidType(), ())
        self.init_func = ir.Function(self.module, self.init_func_type, "__init__")
        block = self.init_func.append_basic_block('entry')
        self.ir_builder = ir.IRBuilder(block)
        
    def push_scope(self, symbol_table : SymbolTable, ir_builder : ir.IRBuilder, deduced_ret_type : Optional[Type] = None) -> None:
        """创建一个新的作用域"""
        self.scope_stack.append((self.symbol_table, self.ir_builder, self.deduced_ret_type))
        self.symbol_table = symbol_table
        self.ir_builder = ir_builder
        self.deduced_ret_type = deduced_ret_type

    def pop_scope(self) -> Tuple[SymbolTable, ir.IRBuilder, Optional[Type]]:
        """恢复上一个作用域"""
        poped_scope = (self.symbol_table, self.ir_builder, self.deduced_ret_type)
        (self.symbol_table, self.ir_builder, self.deduced_ret_type) = self.scope_stack.pop()
        return poped_scope

    def push_loop_block(self, break_block : ir.Block, continue_block : ir.Block) -> None:
        """进入新一层循环体"""
        self.block_stack.append((self.current_break_block, self.current_continue_block))
        self.current_break_block = break_block
        self.current_continue_block = continue_block

    def pop_loop_block(self) -> None:
        """退出当前循环体"""
        (self.current_break_block, self.current_continue_block) = self.block_stack.pop()

    def convert_type(self, src_type : Type, dst_type : Type, src_value : ir.Value) -> Tuple[bool, Optional[ir.Value]]:
        """尝试将src_value进行从src_type到dst_type的自动类型转换"""
        # 如果类型相同，则不需转换
        if src_type == dst_type:
            return True, src_value

        # 左值转换为右值（load）
        if src_type.get_kind() == TypeKind.REFERENCE and dst_type.get_kind() != TypeKind.REFERENCE:
            src_val_type = src_type.clone().remove_ref()
            src_value = self.ir_builder.load(src_value)
            return self.convert_type(src_val_type, dst_type, src_value)

        # 基本类型转换
        # 1. 向上转换: bool -> i8 -> i16 -> i32 -> i64, f16 -> f32 -> f64
        # 2. 整数类型向bool转换: i32 -> bool
        if src_type.get_kind() == TypeKind.BASIC and dst_type.get_kind() == TypeKind.BASIC:
            src_bt, dst_bt = src_type.basic_type, dst_type.basic_type
            is_integer = lambda bt: bt.value >= BasicType.I8.value and bt.value <= BasicType.U64.value
            is_float = lambda bt: bt.value >= BasicType.F16.value
            if (is_integer(src_bt) or src_bt == BasicType.BOOL) and is_integer(dst_bt):
                is_i_src = src_bt in I_BASICTYPES
                is_i_dst = dst_bt in I_BASICTYPES
                # u->i 0扩展 u->i 0扩展 i->i 符号扩展
                if (not is_i_src) and (src_bt.value < dst_bt.value):
                    return True, self.ir_builder.zext(src_value, dst_type.to_ir_type())
                elif is_i_src and is_i_dst and (src_bt.value < dst_bt.value):
                    return True, self.ir_builder.sext(src_value, dst_type.to_ir_type())
            elif is_integer(src_bt) and dst_bt == BasicType.BOOL:
                print('wwwwwwwwwwww')
                return True, self.ir_builder.icmp_signed('!=', src_value, src_value.type(0))
            elif is_float(src_bt) and is_float(dst_bt):
                if src_bt.value <= dst_bt.value:
                    return True, self.ir_builder.fpext(src_value, dst_type.to_ir_type())
            # 向上转 i8 u8转到f8以上
            elif (is_integer(src_bt) or src_bt == BasicType.BOOL) and is_float(dst_bt):
                if src_bt == BasicType.I8:
                    return True, self.ir_builder.sitofp(src_value, dst_type.to_ir_type())
                elif src_bt == BasicType.U8 or src_bt == BasicType.BOOL:  # bool转float
                    print(src_bt)
                    print('asdasdasdasdkkkkk')
                    return True, self.ir_builder.uitofp(src_value, dst_type.to_ir_type())
                elif (src_bt == BasicType.I16) and dst_bt.value >= BasicType.F16.value:
                    return True, self.ir_builder.sitofp(src_value, dst_type.to_ir_type())
                elif (src_bt == BasicType.U16) and dst_bt.value >= BasicType.F16.value:
                    return True, self.ir_builder.uitofp(src_value, dst_type.to_ir_type())
                elif (src_bt == BasicType.I32) and dst_bt.value >= BasicType.F32.value:
                    return True, self.ir_builder.sitofp(src_value, dst_type.to_ir_type())
                elif (src_bt == BasicType.U32) and dst_bt.value >= BasicType.F32.value:
                    return True, self.ir_builder.uitofp(src_value, dst_type.to_ir_type())
                elif (src_bt == BasicType.I64) and dst_bt.value >= BasicType.F64.value:
                    return True, self.ir_builder.sitofp(src_value, dst_type.to_ir_type())
                elif (src_bt == BasicType.U64) and dst_bt.value >= BasicType.F64.value:
                    return True, self.ir_builder.uitofp(src_value, dst_type.to_ir_type())
            return False, None


        return False, None

    def get_current_assignment_value(self, node : ast.Node) -> Tuple[Type, ir.Value]:
        """获得当前需要赋值的值"""
        type, value = self.current_type, self.current_value
        assert type and value

        # 处理非显式引用 (左值转右值)
        if not isinstance(node, ast.RefExpression) and type.get_kind() == TypeKind.REFERENCE:
            dst_type = type.clone().remove_ref()
            _, value = self.convert_type(type, dst_type, value)
        else:
            dst_type = type
        
        return dst_type, value

    def get_module(self) -> ir.Module:
        return self.module
        


class CodeGenVisitor():
    def __init__(self, context : CodeGenContext) -> None:
        self.ctx = context

    @visitor.on('node')
    def visit(self, node):
        raise NotImplementedError(f'Node {type(node)} visitor not implemented')

    @visitor.when(ast.TranslationUnit)
    def visit(self, node : ast.TranslationUnit):
        for decl in node.declaration_list:
            decl.accept(self)
        # init函数返回void
        self.ctx.ir_builder.ret_void()

    @visitor.when(ast.TypeAliasDecl)
    def visit(self, node: ast.TypeAliasDecl):
        node.type_spec.accept(self)
        self.ctx.symbol_table.add_type(node.identifier, self.ctx.current_type)

    @visitor.when(ast.VariableDecl)
    def visit(self, node: ast.VariableDecl):
        for declarator in node.declarator_list:
            declarator.accept(self)

    @visitor.when(ast.Declarator)
    def visit(self, node: ast.Declarator):
        # 检查符号是否在该作用域重复定义
        local_symbol = self.ctx.symbol_table.query_local_symbol(node.identifier)
        if local_symbol is not None:
            raise SemanticError(f'redefined variable {local_symbol.id}')

        # 解析(可选)定义类型
        if node.type_spec:
            node.type_spec.accept(self)
            var_type = self.ctx.current_type
        else:
            var_type = None

        # 解析表达式
        node.init_expr.accept(self)
        var_init_type, var_value = self.ctx.get_current_assignment_value(node.init_expr)
        var_type = var_type or var_init_type

        # 初始化表达式类型转换
        cvt_success, var_value = self.ctx.convert_type(var_init_type, var_type, var_value)
        if not cvt_success:
            raise SemanticError(f'{self.ctx.current_type} is not convertible to {var_type}')

        # 增加 const 标记
        if node.is_const:
            var_type.add_const()
        

        # 添加变量至符号表
        # 1. 对于全局变量，分配全局空间，需要在其initializer中加入初始化表达式
        # 2. 对于局部变量，在栈上分配空间，直接在当前context的ir_builder中加入初始化表达式
        if self.ctx.symbol_table.is_root():
            global_value = ir.GlobalVariable(self.ctx.module, var_type.to_ir_type(), node.identifier)
            # 根据初始化表达式是否为常数选择init方式
            if isinstance(var_value, ir.Constant):
                global_value.initializer = var_value
            else:
                self.ctx.ir_builder.store(var_value, global_value)
            global_value.global_constant = var_type.is_const
            self.ctx.symbol_table.add_symbol(node.identifier, var_type, global_value)
            self.ctx.current_value = global_value
        else:
            local_value = self.ctx.ir_builder.alloca(var_type.to_ir_type(), name=node.identifier)
            self.ctx.ir_builder.store(var_value, local_value)
            self.ctx.symbol_table.add_symbol(node.identifier, var_type, local_value)
            self.ctx.current_value = local_value

    @visitor.when(ast.FunctionDefinition)
    def visit(self, node: ast.FunctionDefinition):
        # 解析函数签名与符号
        node.func_decl.accept(self)
        func_type, func_value = self.ctx.current_type, self.ctx.current_value
        
        # 若当前函数的声明已实例化（值存在），则直接解析函数体
        if func_value:
            assert isinstance(func_value, ir.Function)
            entry_block = func_value.append_basic_block('entry')
            func_ir_builder = ir.IRBuilder(entry_block)

            # 创建函数参数局部变量，并储存函数入参值
            assert len(func_type.func_params) == len(func_value.args)
            for param, arg in zip(func_type.func_params, func_value.args):
                param.value = func_ir_builder.alloca(arg.type, name=param.id)
                func_ir_builder.store(arg, param.value)

            # 解析函数体
            self.ctx.push_scope(func_type.symbol_table, func_ir_builder)
            node.block_stmt.accept(self)

            # 若该函数声明省略了返回值类型，推导其为返回表达式的类型
            ret_type = func_type.func_ret_type
            if ret_type.get_kind() == TypeKind.AUTO:
                # 若没有返回语句，返回类型为VOID
                ret_type = self.ctx.deduced_ret_type or Type(basic_type=BasicType.VOID)
                # 替换返回值类型得到新函数类型
                func_type.func_ret_type = ret_type
                new_func_type = func_value.function_type
                new_func_type.return_type = ret_type.to_ir_type()
                # 重新构建函数值
                new_func_value = ir.Function(self.ctx.module, new_func_type, func_value.name)
                for block in func_value.blocks:
                    new_func_value.basic_blocks.append(block)
                # 替换符号表中的符号
                func_symbol = self.ctx.scope_stack[-1][0].query_symbol(node.func_decl.identifier)
                func_symbol.value = new_func_value
                func_value = new_func_value
            
            # 检查函数指令流结束
            if not self.ctx.ir_builder.basic_block.is_terminated:
                if ret_type.get_kind() == TypeKind.BASIC and ret_type.basic_type == BasicType.VOID:
                    self.ctx.ir_builder.ret_void()
                else:
                    raise SemanticError(f'must return a expression of type {ret_type} at the end of function')

            self.ctx.pop_scope()
            self.ctx.current_value = func_value
        # 否则此函数为泛型函数，记录未解析节点至函数类型中
        else:
            func_type.unfinished_node = node

    @visitor.when(ast.SimpleType)
    def visit(self, node: ast.SimpleType):
        self.ctx.current_type = Type(basic_type=node.type)

    @visitor.when(ast.ComplexType)
    def visit(self, node: ast.ComplexType):
        # 从符号表中查找复杂类型的标识符
        complex_type = self.ctx.symbol_table.query_type(node.identifier)

        # 若该复杂类型声明时有带泛型实例化列表，解析该列表
        generic_specialization_list = []
        for generic_spec in node.generics_spec_list:
            generic_spec.accept(self)
            generic_specialization_list.append(self.ctx.current_type)

        # 对于泛型复杂类型，根据以上列表得到其实例化后的类型
        if complex_type.has_generics():
            num_generics = len(complex_type.generics_type_list)
            if num_generics != len(generic_specialization_list):
                raise SemanticError(f'incorrect number of generics specilization ({num_generics} != {len(generic_specialization_list)})')
            
            # 克隆一个新的复杂类型
            complex_type = complex_type.clone(clone_symbol_table=True)

            # 替换其泛型参数列表的类型
            for i in range(num_generics):
                old_generic_name = complex_type.generics_type_list[i].generic_name
                complex_type.generics_type_list[i] = generic_specialization_list[i]
                complex_type.symbol_table.replace_local_type(old_generic_name, generic_specialization_list[i])
        elif len(generic_specialization_list) > 0:
            raise SemanticError('specify generics to a non-generic struct/interface')

        self.ctx.current_type = complex_type

    @visitor.when(ast.AliasType)
    def visit(self, node: ast.AliasType):
        self.ctx.current_type = self.ctx.symbol_table.query_type(node.type)

    @visitor.when(ast.GenericType)
    def visit(self, node: ast.GenericType):
        self.ctx.current_type = self.ctx.symbol_table.query_type(node.type)

    @visitor.when(ast.ArrayType)
    def visit(self, node: ast.ArrayType):
        node.type.accept(self)
        self.ctx.current_type = self.ctx.current_type.clone().add_array_dim(node.size or 0)

    @visitor.when(ast.ReferenceType)
    def visit(self, node: ast.ReferenceType):
        node.type.accept(self)
        self.ctx.current_type = self.ctx.current_type.clone().add_ref()

    @visitor.when(ast.FunctionType)
    def visit(self, node: ast.FunctionType):
        node.func_sign.accept(self)

    @visitor.when(ast.FunctionDecl)
    def visit(self, node: ast.FunctionDecl):
        # 如果是实例化泛型函数
        if self.ctx.current_generic_spec_type:
            func_type = self.ctx.current_generic_spec_type
            self.ctx.current_type = func_type
            self.ctx.current_generic_spec_type = None
            generic_spec_pass = True
        # 首先解析函数签名得到函数类型
        else:
            node.func_sign.accept(self)
            func_type = self.ctx.current_type
            generic_spec_pass = False

        # 对于非泛型函数，此时可以构建ir.Function
        if not func_type.has_generics():
            # 若当前函数的返回值未指定，该函数值不会加入到模块中
            if func_type.func_ret_type.get_kind() == TypeKind.AUTO:
                module = ir.Module()
            else:
                module = self.ctx.module

            # 对于成员函数，将其结构体或接口的名字加到前面
            if (self.ctx.symbol_table.parent_type is not None and 
                self.ctx.symbol_table.parent_type.get_kind() in [TypeKind.STRUCT, TypeKind.INTERFACE]):
                func_name = f'{self.ctx.symbol_table.parent_type.struct_name}.{node.identifier}'
            else:
                func_name = node.identifier

            # 对于实例化的函数，将其实例化的实际类型加到后边
            if generic_spec_pass:
                func_name += f'<<{str(func_type).replace(" ", "")}>>'
                # 对于实例化的泛型函数，如果相同的实例化已经产生过，直接使用之前的
                try:
                    self.ctx.current_value = self.ctx.module.get_global(func_name)
                    return
                except:
                    pass

            func_value = ir.Function(module, func_type.to_ir_type().pointee, func_name)
        # 否则对于泛型函数，其符号值目前留空，等待实例化时再生成值
        else:
            func_value = None
            func_type.unfinished_node = node

        # 将该函数符号加入到当前符号表中（对于泛型实例化则不需要加入）
        if not generic_spec_pass:
            self.ctx.symbol_table.add_symbol(node.identifier, func_type, func_value)

        # 设置当前的值为函数符号值
        self.ctx.current_value = func_value

    @visitor.when(ast.FunctionSignature)
    def visit(self, node: ast.FunctionSignature):
        # 为函数建立符号表
        # 如果上层符号表是struct或interface的符号表，则跳过
        if (self.ctx.symbol_table.parent_type is not None and 
            self.ctx.symbol_table.parent_type.get_kind() in [TypeKind.STRUCT, TypeKind.INTERFACE]):
            parent_symbol_table = self.ctx.symbol_table.parent
        else:
            parent_symbol_table = self.ctx.symbol_table

        # 创建函数类型
        func_type = Type()
        func_type.add_symbol_table(parent_symbol_table)

        # 处理泛型类型声明列表，加入该函数类型的泛型列表和符号表
        for generics_type_node in node.generics_type_list:
            generics_type_node.accept(self)
            func_type.add_generics_type(self.ctx.current_type)

        # 进入函数局部作用域
        self.ctx.push_scope(func_type.symbol_table, self.ctx.ir_builder, self.ctx.deduced_ret_type)

        if node.return_type_spec:
            # 返回值从return_type_spec得到
            node.return_type_spec.accept(self)
            func_type.add_func_ret_type(self.ctx.current_type.clone())
        else:
            # 返回值是Auto
            func_type.add_func_ret_type(Type())

        # 处理函数的参数列表
        for param_idx, param in enumerate(node.parameter_decl_list):
            # 如果参数声明了具体类型，解析该类型
            if param.type_spec:
                param.type_spec.accept(self)
                param_type = self.ctx.current_type
            # 对于成员函数，第一个类型若未指定，自动推导为该struct/interface的引用类型
            elif param_idx == 0 and self.ctx.symbol_table.parent_type and (
                    self.ctx.symbol_table.parent_type.get_kind() == TypeKind.STRUCT or
                    self.ctx.symbol_table.parent_type.get_kind() == TypeKind.INTERFACE):
                param_type = self.ctx.symbol_table.parent_type.clone().add_ref()
            # 否则将该参数的类型当作一个新的泛型类型，并加入到该函数类型的泛型列表中
            else:
                param_type = Type(generic_name='')  # 匿名泛型类型
                func_type.add_generics_type(param_type)

            # 获得参数的类型后，将符号加入到函数的符号表和参数列表中
            # 此时函数参数符号还没有值，需要等到后续的函数定义时才会产生值
            param_symbol = func_type.symbol_table.add_symbol(param.identifier, param_type)
            func_type.add_func_param(param_symbol)

        self.ctx.pop_scope()

        # 函数签名的声明解析完毕，函数类型构建完成
        self.ctx.current_type = func_type

    @visitor.when(ast.GenericsType)
    def visit(self, node: ast.GenericsType):
        # 若泛型类型声明含有限定范围，解析该限定范围（为一个实例化的Interface）
        if node.type_range:
            node.type_range.accept(self)
            type_range = self.ctx.current_type
            if type_range.get_kind() != TypeKind.INTERFACE:
                raise SemanticError('generics type range must be an Interface type')
            if type_range.has_generics():
                raise SemanticError('generics type range must be an specialized Interface type')
        else:
            type_range = None

        # 构造一个待定泛型类型
        self.ctx.current_type = Type(generic_name=node.identifier, generic_type_range=type_range)

    @visitor.when(ast.StructDecl)
    def visit(self, node: ast.StructDecl):
        # 创建新的结构体类型
        struct_type = Type(struct_name=node.identifier, 
                           is_interface=False)

        # 加入类型到符号表中
        self.ctx.symbol_table.add_type(node.identifier, struct_type)

        # 若结构体有泛型参数列表，解析该列表
        for generics_type in node.generics_type_list:
            generics_type.accept(self)
            struct_type.add_generics_type(self.ctx.current_type)
        # 对于有泛型参数的结构体，现在还无法解析其具体内容，保留到未解析节点中
        if struct_type.has_generics():
            struct_type.unfinished_node = node
            self.ctx.current_type = struct_type
            return

        # 解析继承接口类型的列表
        for base_type in node.base_type:
            base_type.accept(self)
            if self.ctx.current_type.get_kind() != TypeKind.INTERFACE:
                raise SemanticError('base type of struct must be interface type')
            struct_type.add_struct_base_type(self.ctx.current_type)

        # 创建新符号表，进入结构体作用域
        struct_type.add_symbol_table(self.ctx.symbol_table)
        self.ctx.push_scope(struct_type.symbol_table, self.ctx.ir_builder, self.ctx.deduced_ret_type)

        # 解析成员变量与成员函数
        for member_idx, member_decl in enumerate(node.member_decl_list):
            # 解析成员类型
            member_decl.type_spec.accept(self)
            # 添加成员变量至符号表与类型中的成员列表，此处无实际值
            member_symbol = self.ctx.symbol_table.add_symbol(member_decl.identifier, self.ctx.current_type, member_idx)
            struct_type.add_struct_member(member_symbol)

        # 完成ir type构建
        struct_ir_type : ir.IdentifiedStructType = struct_type.to_ir_type()
        struct_ir_type.set_body(*[
            member_symbol.type.to_ir_type() 
            for member_symbol in struct_type.member_list
        ])

        for member_func in node.member_func_definition_list:
            # 解析函数定义
            member_func.func_definition.accept(self)

        # 解析构造函数
        if node.constructor_func_definition is None:
            raise SemanticError(f'missing constructor definition in {struct_type}')
        node.constructor_func_definition.accept(self)

        # 退出结构体作用域
        self.ctx.pop_scope()
        self.ctx.current_type = struct_type

    @visitor.when(ast.InterfaceDecl)
    def visit(self, node: ast.InterfaceDecl):
        pass

    @visitor.when(ast.MemberFuncDecl)
    def visit(self, node: ast.MemberFuncDecl):
        pass

    @visitor.when(ast.ConstructorFuncDefinition)
    def visit(self, node: ast.ConstructorFuncDefinition):
        # 检验构造函数的声明类型应该与当前结构体一样
        node.struct_type.accept(self)
        struct_type = self.ctx.current_type
        if not (struct_type == self.ctx.symbol_table.parent_type):
            raise SemanticError('constructor must be of current struct type')
        constructor_id = f'{struct_type}'.replace(' ', '_')
        
        # 转换到一个普通的成员函数定义进行解析
        func_def_node = ast.FunctionDefinition(node.location, 
            func_decl=ast.FunctionDecl(node.location, constructor_id, node.func_sign),
            block_stmt=node.block_stmt)
        func_def_node.accept(self)

        # 检查返回值为VOID
        func_symbol = self.ctx.symbol_table.query_symbol(constructor_id)
        func_ir_type : ir.FunctionType = func_symbol.value.function_type
        if func_ir_type.return_type != ir.VoidType():
            raise SemanticError('constructor return type must be void')

        # 记录构造函数
        struct_type.constructor = func_symbol

    @visitor.when(ast.DeclarationStatement)
    def visit(self, node: ast.DeclarationStatement):
        node.declaration.accept(self)

    @visitor.when(ast.BlockStatement)
    def visit(self, node: ast.BlockStatement):
        # 进入块语句时，创建一个新的局部符号表
        new_symbol_table = SymbolTable(self.ctx.symbol_table, self.ctx.symbol_table.parent_type)
        self.ctx.push_scope(new_symbol_table, self.ctx.ir_builder, self.ctx.deduced_ret_type)
        for stmt in node.statement_list:
            stmt.accept(self)
            # 如果语句块提前结束，则不再解析之后的语句
            if self.ctx.ir_builder.basic_block.is_terminated:
                break
        _, _, deduced_ret_type = self.ctx.pop_scope()
        self.ctx.deduced_ret_type = self.ctx.deduced_ret_type or deduced_ret_type

    @visitor.when(ast.ExpressionStatement)
    def visit(self, node: ast.ExpressionStatement):
        if node.expression:
            node.expression.accept(self)

    @visitor.when(ast.IfStatement)
    def visit(self, node: ast.IfStatement):
        # 解析IF条件表达式
        node.condition.accept(self)
        # 检验条件结果是否可以转换为bool
        cvt_success, cond_value = self.ctx.convert_type(self.ctx.current_type, 
            Type(basic_type=BasicType.BOOL), self.ctx.current_value)
        if not cvt_success:
            raise SemanticError('condition is not convertible to bool')

        # 生成If-Else基本块
        func : ir.Function = self.ctx.ir_builder.function
        then_block = func.append_basic_block('if.then')
        else_block = func.append_basic_block('if.else')
        end_block = func.append_basic_block('if.endif')
        if not self.ctx.ir_builder.basic_block.is_terminated:
            self.ctx.ir_builder.cbranch(cond_value, then_block, else_block)

        # 解析True时的语句
        self.ctx.ir_builder.position_at_start(then_block)
        node.true_stmt.accept(self)
        if not self.ctx.ir_builder.basic_block.is_terminated:
            self.ctx.ir_builder.branch(end_block)
            then_returned = False
        else:
            then_returned = True

        # 解析False时的语句
        self.ctx.ir_builder.position_at_start(else_block)
        if node.false_stmt:
            node.false_stmt.accept(self)
        if not self.ctx.ir_builder.basic_block.is_terminated:
            self.ctx.ir_builder.branch(end_block)
            else_returned = False
        else:
            else_returned = True

        # 设置插入点至if之后
        self.ctx.ir_builder.position_at_start(end_block)
        # 如果两条分支均已经结束，则说明该分支不可达
        if then_returned and else_returned:
            self.ctx.ir_builder.unreachable()

    @visitor.when(ast.WhileStatement)
    def visit(self, node: ast.WhileStatement):
        # 创建While基本块
        func : ir.Function = self.ctx.ir_builder.function
        cond_block = func.append_basic_block('while.cond')
        loop_block = func.append_basic_block('while.loop')
        end_block = func.append_basic_block('while.end')
        if not self.ctx.ir_builder.basic_block.is_terminated:
            self.ctx.ir_builder.branch(cond_block)

        # 解析WHITE条件表达式
        self.ctx.ir_builder.position_at_start(cond_block)
        node.cond_expr.accept(self)
        # 检验条件结果是否可以转换为bool
        cvt_success, cond_value = self.ctx.convert_type(self.ctx.current_type, 
            Type(basic_type=BasicType.BOOL), self.ctx.current_value)
        if not cvt_success:
            raise SemanticError('condition is not convertible to bool')
        # 插入条件跳转指令
        if not self.ctx.ir_builder.basic_block.is_terminated:
            self.ctx.ir_builder.cbranch(cond_value, loop_block, end_block)

        # 解析循环体
        self.ctx.push_loop_block(end_block, cond_block)
        self.ctx.ir_builder.position_at_start(loop_block)
        node.loop_stmt.accept(self)
        if not self.ctx.ir_builder.basic_block.is_terminated:
            self.ctx.ir_builder.branch(cond_block)
        self.ctx.pop_loop_block()

        # 设置插入点至while之后
        self.ctx.ir_builder.position_at_start(end_block)

    @visitor.when(ast.ForStatement)
    def visit(self, node: ast.ForStatement):
        # 生成For初始化语句
        node.init_stmt.accept(self)

        # 创建For基本块
        func : ir.Function = self.ctx.ir_builder.function
        cond_block = func.append_basic_block('for.cond')
        loop_block = func.append_basic_block('for.loop')
        iter_block = func.append_basic_block('for.iter')
        end_block = func.append_basic_block('for.end')
        if not self.ctx.ir_builder.basic_block.is_terminated:
            self.ctx.ir_builder.branch(cond_block)

        # 解析For条件表达式
        self.ctx.ir_builder.position_at_start(cond_block)
        if node.cond_expr:
            node.cond_expr.accept(self)
            # 检验条件结果是否可以转换为bool
            cvt_success, cond_value = self.ctx.convert_type(self.ctx.current_type, 
                Type(basic_type=BasicType.BOOL), self.ctx.current_value)
            if not cvt_success:
                raise SemanticError('condition is not convertible to bool')
            # 插入条件跳转指令
            if not self.ctx.ir_builder.basic_block.is_terminated:
                self.ctx.ir_builder.cbranch(cond_value, loop_block, end_block)
        # 若没有For条件，则无条件进入循环
        else:
            if not self.ctx.ir_builder.basic_block.is_terminated:
                self.ctx.ir_builder.branch(loop_block)

        # 解析循环条件
        self.ctx.ir_builder.position_at_start(iter_block)
        if node.iter_expr:
            node.iter_expr.accept(self)
        self.ctx.ir_builder.branch(cond_block)

        # 解析循环体
        self.ctx.push_loop_block(end_block, cond_block)
        self.ctx.ir_builder.position_at_start(loop_block)
        node.loop_stmt.accept(self)
        if not self.ctx.ir_builder.basic_block.is_terminated:
            self.ctx.ir_builder.branch(iter_block)
        self.ctx.pop_loop_block()

        # 设置插入点至for之后
        self.ctx.ir_builder.position_at_start(end_block)

    @visitor.when(ast.BreakStatement)
    def visit(self, node: ast.BreakStatement):
        if not self.ctx.ir_builder.basic_block.is_terminated:
            self.ctx.ir_builder.branch(self.ctx.current_break_block)

    @visitor.when(ast.ContinueStatement)
    def visit(self, node: ast.ContinueStatement):
        if not self.ctx.ir_builder.basic_block.is_terminated:
            self.ctx.ir_builder.branch(self.ctx.current_continue_block)

    @visitor.when(ast.ReturnStatement)
    def visit(self, node: ast.ReturnStatement):
        ret_type = self.ctx.symbol_table.parent_type.func_ret_type
        if node.expression:
            # 检查函数返回类型是否为void
            if ret_type.get_kind() == TypeKind.BASIC and ret_type.basic_type == BasicType.VOID:
                raise SemanticError('void function should not return a value')

            # 解析返回值表达式
            node.expression.accept(self)
            ret_value_type, ret_value = self.ctx.get_current_assignment_value(node.expression)

            # 如果未指定返回类型，推导表达式类型为该返回值类型
            if ret_type.get_kind() == TypeKind.AUTO:
                # 若当前是第一个return语句，记录解析的返回值类型
                if self.ctx.deduced_ret_type is None:
                    self.ctx.deduced_ret_type = ret_value_type
                # 否则检查两个return的表达式类型一致
                elif not (ret_value_type == self.ctx.deduced_ret_type):
                    raise SemanticError('multiple return expression with different types')
            # 如果指定了返回类型，检验返回值是否可以转换为返回类型
            else:
                cvt_success, ret_value = self.ctx.convert_type(ret_value_type, ret_type, ret_value)
                if not cvt_success:
                    raise SemanticError(f'can not convert returned type {ret_value_type} to function return type {ret_type}')

            self.ctx.ir_builder.ret(ret_value)
        else:
            # 如果未指定返回类型，推导表达式类型为VOID
            if ret_type.get_kind() == TypeKind.AUTO:
                # 若当前是第一个return语句，记录解析的返回值类型
                if self.ctx.deduced_ret_type is None:
                    self.ctx.deduced_ret_type = Type(basic_type=BasicType.VOID)
                # 否则检查两个return的表达式类型一致
                else:
                    if not (self.ctx.current_type == self.ctx.deduced_ret_type):
                        raise SemanticError('multiple return expression with different types')

            # 检查函数返回类型是否为void
            elif ret_type.get_kind() != TypeKind.BASIC or ret_type.basic_type != BasicType.VOID:
                raise SemanticError('non-void function should return a value')

            self.ctx.ir_builder.ret_void()

    @visitor.when(ast.AssignExpression)
    def visit(self, node: ast.AssignExpression):
        # 解析左表达式
        node.lhs_expr.accept(self)
        lhs_type, lhs_value = self.ctx.current_type, self.ctx.current_value

        # 检测左表达式是可修改的引用类型（左值）
        if lhs_type.is_const:
            raise SemanticError('can not assign to const variable')
        if lhs_type.get_kind() != TypeKind.REFERENCE:
            raise SemanticError('can not assign to a r-value')

        # 解析右表达式
        node.rhs_expr.accept(self)
        rhs_type, rhs_value = self.ctx.get_current_assignment_value(node.rhs_expr)

        # 若右表达式类型为显式引用，要求左表达式也为一个引用类型的标识符
        if rhs_type.get_kind() == TypeKind.REFERENCE:
            if not isinstance(node.lhs_expr, ast.IdentifierOperand):
                raise SemanticError('can not assign reference to non identifier operand')
        
            # 在符号表查找该标识符
            symbol = self.ctx.symbol_table.query_symbol(node.lhs_expr.identifier)
            # 检查符号类型是引用
            if symbol.type.get_kind() != TypeKind.REFERENCE:
                raise SemanticError('can not assign reference to a non reference identifier')

            # 检查左右表达式的类型相同
            if lhs_type != rhs_type:
                raise SemanticError('can not assign reference to a different type')

            # 将引用赋值给左侧的引用符号
            self.ctx.ir_builder.store(rhs_value, symbol.value)
        # 否则进行普通赋值
        else:
            # 检查左右表达式的类型相同
            lhs_value_type = lhs_type.clone().remove_ref()
            cvt_success, rhs_value = self.ctx.convert_type(rhs_type, lhs_value_type, rhs_value)
            if not cvt_success:
                raise SemanticError('can not convert expression type to variable type')

            # 储存值到变量中
            self.ctx.ir_builder.store(rhs_value, lhs_value)

    @visitor.when(ast.BinaryExpression)
    def visit(self, node: ast.BinaryExpression):
        # 解析左表达式
        node.lhs_expr.accept(self)
        lhs_type, lhs_value = self.ctx.get_current_assignment_value(node.lhs_expr)
        # 左边的为什么出来的都是i32的？

        # 解析右表达式
        node.rhs_expr.accept(self)
        rhs_type, rhs_value = self.ctx.get_current_assignment_value(node.rhs_expr)

        # # 检查左右表达式类型一致
        # if not lhs_type == rhs_type:
        #     raise SemanticError(f'type of binary operands are not the same')

        # 检查操作数类型可运算（整形或浮点型）
        if lhs_type.get_kind() != TypeKind.BASIC:
            raise SemanticError(f'unsupported type of binary operands')

        # 生成运算指令
        self.ctx.current_type = lhs_type
        basic_type = lhs_type.basic_type
        is_bool = basic_type == BasicType.BOOL
        is_float = basic_type in [BasicType.F16, BasicType.F32, BasicType.F64]  ######
        is_integer = not is_bool and not is_float
        cmp_instr = self.ctx.ir_builder.fcmp_unordered if is_float else self.ctx.ir_builder.icmp_signed  ######
        irb = self.ctx.ir_builder

        # is_bool_lmd_ = lambda bt: bt.value == BasicType.BOOL.value
        # is_integer_lmd = lambda bt: bt.value >= BasicType.I8.value and bt.value <= BasicType.U64.value
        # is_float_lmd = lambda bt: bt.value >= BasicType.F16.value

        if node.operator == BinaryOp.PLUS:  # +
            if lhs_type.basic_type.value < rhs_type.basic_type.value:
                _, lhs_value = self.ctx.convert_type(lhs_type, rhs_type, lhs_value)
                self.ctx.current_type = rhs_type
                is_float = rhs_type.basic_type in [BasicType.F16, BasicType.F32, BasicType.F64]
            else:
                _, rhs_value = self.ctx.convert_type(rhs_type, lhs_type, rhs_value)
                self.ctx.current_type = lhs_type
                is_float = lhs_type.basic_type in [BasicType.F16, BasicType.F32, BasicType.F64]
            value = (irb.fadd if is_float else irb.add)(lhs_value, rhs_value)

        elif node.operator == BinaryOp.MINUS:  # -
            if lhs_type.basic_type.value < rhs_type.basic_type.value:
                _, lhs_value = self.ctx.convert_type(lhs_type, rhs_type, lhs_value)
                self.ctx.current_type = rhs_type
                is_float = rhs_type.basic_type in [BasicType.F16, BasicType.F32, BasicType.F64]
            else:
                _, rhs_value = self.ctx.convert_type(rhs_type, lhs_type, rhs_value)
                self.ctx.current_type = lhs_type
                is_float = lhs_type.basic_type in [BasicType.F16, BasicType.F32, BasicType.F64]
            value = (irb.fsub if is_float else irb.sub)(lhs_value, rhs_value)
        elif node.operator == BinaryOp.MUL:  # *
            if lhs_type.basic_type.value < rhs_type.basic_type.value:
                _, lhs_value = self.ctx.convert_type(lhs_type, rhs_type, lhs_value)
                self.ctx.current_type = rhs_type
                is_float = rhs_type.basic_type in [BasicType.F16, BasicType.F32, BasicType.F64]
            else:
                _, rhs_value = self.ctx.convert_type(rhs_type, lhs_type, rhs_value)
                self.ctx.current_type = lhs_type
                is_float = lhs_type.basic_type in [BasicType.F16, BasicType.F32, BasicType.F64]
            value = (irb.fmul if is_float else irb.mul)(lhs_value, rhs_value)
        elif node.operator == BinaryOp.DIV:  # /
            if lhs_type.basic_type.value < rhs_type.basic_type.value:
                _, lhs_value = self.ctx.convert_type(lhs_type, rhs_type, lhs_value)
                self.ctx.current_type = rhs_type
                is_float = rhs_type.basic_type in [BasicType.F16, BasicType.F32, BasicType.F64]
            else:
                _, rhs_value = self.ctx.convert_type(rhs_type, lhs_type, rhs_value)
                self.ctx.current_type = lhs_type
                is_float = lhs_type.basic_type in [BasicType.F16, BasicType.F32, BasicType.F64]
            if is_float:
                value = irb.fdiv(lhs_value, rhs_value)
            else:
                if self.ctx.current_type.basic_type in I_BASICTYPES:
                    value = irb.sdiv(lhs_value, rhs_value)
                else:
                    value = irb.udiv(lhs_value, rhs_value)
        elif node.operator == BinaryOp.AND:  # &  只有bool和inter可以，且inter时必须要相同
            if is_integer:
                value = irb.and_(lhs_value, rhs_value)
            else:
                raise SemanticError('unsupported type of AND')
        elif node.operator == BinaryOp.OR:  # |
            if is_integer:
                value = irb.or_(lhs_value, rhs_value)
            else:
                raise SemanticError('unsupported type of OR')
        elif node.operator == BinaryOp.XOR:  # ^
            if is_integer:
                value = irb.xor((lhs_value, rhs_value))
            else:
                raise SemanticError('unsupported type of XOR')
        elif node.operator == BinaryOp.MOD:  # %
            if is_integer:
                if lhs_type in I_BASICTYPES:  ##########
                    value = irb.srem(lhs_value, rhs_value)
                else:
                    value = irb.urem(lhs_value, rhs_value)
            elif is_float:
                value = irb.frem(lhs_value, rhs_value)
            else:
                raise SemanticError('unsupported type of MOD')
        elif node.operator == BinaryOp.LSHIFT:  # <<
            if is_integer:
                value = irb.shl(lhs_value, rhs_value)
            else:
                raise SemanticError('unsupported type of LSHIFT')
        elif node.operator == BinaryOp.RSHIFT:  # >>
            if is_integer:
                value = irb.ashr(lhs_value, rhs_value)
            else:
                raise SemanticError('unsupported type of RSHIFT')
        elif node.operator == BinaryOp.LOGICAL_OR:  # or
            if is_bool or is_integer:
                _, lhs_value = self.ctx.convert_type(lhs_type, Type(basic_type=BasicType.BOOL), lhs_value)
                _, rhs_value = self.ctx.convert_type(rhs_type, Type(basic_type=BasicType.BOOL), rhs_value)
                value = self.ctx.ir_builder.or_(lhs_value, rhs_value)
            else:
                raise SemanticError('unsupported type of LOGICAL_OR')
        elif node.operator == BinaryOp.LOGICAL_AND:  # and
            if is_bool or is_integer:
                _, lhs_value = self.ctx.convert_type(lhs_type, Type(basic_type=BasicType.BOOL), lhs_value)
                _, rhs_value = self.ctx.convert_type(rhs_type, Type(basic_type=BasicType.BOOL), rhs_value)
                value = self.ctx.ir_builder.and_(lhs_value, rhs_value)
            else:
                raise SemanticError('unsupported type of LOGICAL_AND')
        elif node.operator == BinaryOp.NOT_EQUAL:  # !=
            self.ctx.current_type = Type(basic_type=BasicType.BOOL)
            value = cmp_instr('!=', lhs_value, rhs_value)  ######
        elif node.operator == BinaryOp.EQUAL:  # ==
            self.ctx.current_type = Type(basic_type=BasicType.BOOL)
            value = cmp_instr('==', lhs_value, rhs_value)  ######
        elif node.operator == BinaryOp.LESS_EQUAL:  # <=
            self.ctx.current_type = Type(basic_type=BasicType.BOOL)
            value = cmp_instr('<=', lhs_value, rhs_value)  ######
        elif node.operator == BinaryOp.LESS:  # <
            self.ctx.current_type = Type(basic_type=BasicType.BOOL)
            value = cmp_instr('<', lhs_value, rhs_value)  ######
        elif node.operator == BinaryOp.GREATER_EQUAL:  # >=
            self.ctx.current_type = Type(basic_type=BasicType.BOOL)
            value = cmp_instr('>=', lhs_value, rhs_value)  ######
        elif node.operator == BinaryOp.GREATER:  # >
            print('is_float', is_float)
            self.ctx.current_type = Type(basic_type=BasicType.BOOL)
            value = cmp_instr('>', lhs_value, rhs_value)  ######
        else:
            raise NotImplementedError(f'binary op {node.operator} not implemented')
        self.ctx.current_value = value
        
    @visitor.when(ast.UnaryExpression)
    def visit(self, node: ast.UnaryExpression):
        # 解析表达式
        node.expression.accept(self)
        type, value = lhs_type, lhs_value = self.ctx.get_current_assignment_value(node.expression)

        # 检查操作数类型可运算（整形或浮点型）
        if type.get_kind() != TypeKind.BASIC:
            raise SemanticError(f'unsupported type of unary operands')

        # 生成运算指令
        self.ctx.current_type = type
        irb = self.ctx.ir_builder
        basic_type = type.basic_type
        is_bool = basic_type == BasicType.BOOL
        is_float = basic_type in [BasicType.F16, BasicType.F32, BasicType.F64]
        is_integer = not is_bool and not is_float

        if node.operator == UnaryOp.PLUS:
            pass
        elif node.operator == UnaryOp.MINUS:
            if is_integer:
                value = irb.neg(value)
            elif is_float:
                value = irb.fneg(value)
            else:
                raise SemanticError('unsupported type for unary not')
        elif node.operator == UnaryOp.NOT:
            if is_float:
                raise SemanticError('unsupported type for unary not')
            value = irb.not_(value)
        elif node.operator == UnaryOp.LOGICAL_NOT:
            if is_bool or is_integer:
                value = self.ctx.ir_builder.icmp_signed('==', value, value.type(0))
            else:
                raise SemanticError('unsupported type for unary not')
        else:
            raise NotImplementedError(f'binary op {node.operator} not implemented')
        self.ctx.current_value = value

    @visitor.when(ast.LiteralOperand)
    def visit(self, node: ast.LiteralOperand):
        if isinstance(node.literal, int):
            self.ctx.current_value = ir.IntType(32)(node.literal)
            self.ctx.current_type = Type(basic_type=BasicType.I32)
        elif isinstance(node.literal, float):
            self.ctx.current_value = (ir.FloatType() if node.is_float else ir.DoubleType())(node.literal)
            self.ctx.current_type = Type(basic_type=BasicType.F32 if node.is_float else BasicType.F64)
        else:
            raise NotImplementedError(f'literal type {type(node.literal)} not implemented')

    @visitor.when(ast.IdentifierOperand)
    def visit(self, node: ast.IdentifierOperand):
        # 在符号表查找标识符
        symbol = self.ctx.symbol_table.query_symbol(node.identifier)
        # 若符号类型本身是引用，则先获取其指向的变量作为左值
        if symbol.type.get_kind() == TypeKind.REFERENCE:
            self.ctx.current_type = symbol.type
            self.ctx.current_value = self.ctx.ir_builder.load(symbol.value)
        # 否则取出符号的变量值，并将类型修改为引用表示左值（除了函数值与常量）
        else:
            self.ctx.current_value = symbol.value
            is_function = symbol.type.get_kind() == TypeKind.FUNCTION
            if is_function:
                self.ctx.current_type = symbol.type
                # 对于泛型函数，先跳过处理
                if symbol.value is None:
                    self.ctx.current_value = None
                else:
                    # 对于函数类型的符号，如果其为函数值的指针，取出其对应的函数值
                    if isinstance(symbol.value.type.pointee, ir.PointerType):
                        self.ctx.current_value = self.ctx.ir_builder.load(symbol.value)
            else:
                self.ctx.current_type = symbol.type.clone().add_ref()

    @visitor.when(ast.ExpressionOperand)
    def visit(self, node: ast.ExpressionOperand):
        node.expression.accept(self)

    @visitor.when(ast.MemberExpression)
    def visit(self, node: ast.MemberExpression):
        node.object_expr.accept(self)
        obj_type, obj_value = self.ctx.current_type, self.ctx.current_value

        # 获得对象表达式值类型（如有引用则去除）
        if obj_type.get_kind() == TypeKind.REFERENCE:
            obj_value_type = obj_type.clone().remove_ref()
        else:
            obj_value_type = obj_type

        # 检验对象值的类型为结构体
        if obj_value_type.get_kind() != TypeKind.STRUCT:
            raise SemanticError(f'object expression type must be struct (not {obj_value_type})')
        
        # 在结构体符号表中查找其成员
        member_symbol = obj_value_type.symbol_table.query_local_symbol(node.member_id)
        if member_symbol is None:
            raise SemanticError(f'no member named {node.member_id} in {obj_value_type}')

        # 若成员为成员函数
        if member_symbol.type.get_kind() == TypeKind.FUNCTION:
            member_value = member_symbol.value
        # 否则为成员变量
        else:
            # 对于结构体引用类型，产生成员指针
            if obj_type.get_kind() == TypeKind.REFERENCE:
                assert type(member_symbol.value) is int
                member_value = self.ctx.ir_builder.gep(obj_value, [
                    ir.IntType(32)(0), ir.IntType(32)(member_symbol.value),
                ], inbounds=True)
            else:
                raise NotImplementedError()

        # 若符号类型本身是引用，则先获取其指向的变量作为左值
        if member_symbol.type.get_kind() == TypeKind.REFERENCE:
            self.ctx.current_type = member_symbol.type
            self.ctx.current_value = self.ctx.ir_builder.load(member_value)
        # 否则取出符号的变量值，并将类型修改为引用表示左值（除了函数值）
        else:
            if member_symbol.type.get_kind() != TypeKind.FUNCTION:
                self.ctx.current_type = member_symbol.type.clone().add_ref()
            else:
                self.ctx.current_type = member_symbol.type
            self.ctx.current_value = member_value

        # 保存当前的对象值
        self.ctx.current_object_value = obj_value

    @visitor.when(ast.IndexExpression)
    def visit(self, node: ast.IndexExpression):
        # 解析数组表达式
        node.array_expr.accept(self)
        array_type, array_value = self.ctx.current_type, self.ctx.current_value

        # 对数组的引用，先移除引用得到数组的值类型
        if array_type.get_kind() == TypeKind.REFERENCE:
            val_array_type = array_type.clone().remove_ref()
        else:
            val_array_type = array_type

        # 检测类型是否为数组
        if val_array_type.get_kind() != TypeKind.ARRAY:
            raise SemanticError('array is not an array type')

        # 解析索引表达式
        node.index_expr.accept(self)
        index_type, index_value = self.ctx.current_type, self.ctx.current_value

        # 检验索引是否可以被转换到整数类型
        cvt_success, index_value = self.ctx.convert_type(index_type, 
            Type(basic_type=BasicType.I64), index_value)
        if not cvt_success:
            raise SemanticError('array index must be an integer type')

        # 若为数组引用，则从索引构造目标值的引用
        if array_type.get_kind() == TypeKind.REFERENCE:
            self.ctx.current_value = self.ctx.ir_builder.gep(
                array_value, [ir.IntType(64)(0), index_value])
            self.ctx.current_type = val_array_type.clone().to_element_type().add_ref()
        # 否则直接取出数组中元素值
        else:
            raise NotImplementedError()

    @visitor.when(ast.RefExpression)
    def visit(self, node: ast.RefExpression):
        node.ref_expr.accept(self)
        if self.ctx.current_type.get_kind() != TypeKind.REFERENCE:
            raise SemanticError('can not convert r-value to reference')

    @visitor.when(ast.CastExpression)
    def visit(self, node: ast.CastExpression):
        # 解析目标类型
        node.type_spec.accept(self)
        target_type = self.ctx.current_type

        # 解析源表达式
        node.cast_expr.accept(self)
        src_type, value = self.ctx.current_type, self.ctx.current_value

        # 首先尝试能否使用自动转换规则
        cvt_success, cvt_value = self.ctx.convert_type(src_type, target_type, value)
        if cvt_success:
            print('cvt_success')
            self.ctx.current_type = target_type
            print('target_type.basic_type', target_type.basic_type)
            self.ctx.current_value = cvt_value
            print('cvt_value', cvt_value)
            return

        if (src_type.get_kind() == TypeKind.REFERENCE):
            value_type = src_type.clone().remove_ref()
            _, value = self.ctx.convert_type(src_type, value_type, value)
            src_type = value_type

        # 否则使用尝试使用强制转换规则
        # 1. 向下转换: i64 -> i32 -> i16 -> i8, f64 -> f32 -> f16
        # 2. 浮点与(整数,bool)互相转换: iX <-> fX, bool <-> fX
        # 都是基础类型才能转
        if src_type.get_kind() == TypeKind.BASIC and target_type.get_kind() == TypeKind.BASIC:
            src_bt, target_bt = src_type.basic_type, target_type.basic_type
            is_integer = lambda bt: BasicType.I8.value <= bt.value <= BasicType.U64.value
            is_float = lambda bt: bt.value >= BasicType.F16.value
            is_bool = lambda bt: bt.value == BasicType.BOOL.value
            # float -> float, uint, sint, bool
            if is_float(src_bt):
                if target_bt in F_BASICTYPES:  # 转float
                    self.ctx.current_type = target_type
                    self.ctx.current_value = self.ctx.ir_builder.fptrunc(value, target_type.to_ir_type())
                elif target_bt in I_BASICTYPES:  # 转有符号int
                    self.ctx.current_type = target_type
                    self.ctx.current_value = self.ctx.ir_builder.fptosi(value, target_type.to_ir_type())
                elif target_bt in U_BASICTYPES:  # 转无符号的int
                    self.ctx.current_type = target_type
                    self.ctx.current_value = self.ctx.ir_builder.fptoui(value, target_type.to_ir_type())
            # int, bool -> float, uint, sint
            elif is_integer(src_bt) or is_bool(src_bt):
                print('aaa')
                if is_integer(target_bt):
                    print('bbb')
                    self.ctx.current_type = target_type
                    self.ctx.current_value = self.ctx.ir_builder.trunc(value, target_type.to_ir_type())
                elif is_float(target_bt) :
                    print('ccc')
                    if src_bt in I_BASICTYPES:
                        print('ddd')
                        self.ctx.current_type = target_type
                        self.ctx.current_value = self.ctx.ir_builder.sitofp(value, target_type.to_ir_type())
                    else :
                        print('ffff')
                        self.ctx.current_type = target_type
                        self.ctx.current_value = self.ctx.ir_builder.uitofp(value, target_type.to_ir_type())
            return
        raise NotImplementedError()

    @visitor.when(ast.NewExpression)
    def visit(self, node: ast.NewExpression):
        node.type_spec.accept(self)
        new_type = self.ctx.current_type

        # 构造基本类型
        if new_type.get_kind() == TypeKind.BASIC:
            if new_type.basic_type == BasicType.VOID:
                raise SemanticError(f'void type can not be instantiated')
            elif len(node.param_list) == 0:
                value = new_type.to_ir_type()(0)
            elif len(node.param_list) == 1:
                node.param_list[0].accept(self)
                value_type, value = self.ctx.get_current_assignment_value(node.param_list[0])
                cvt_success, value = self.ctx.convert_type(value_type, new_type, value)
                if not cvt_success:
                    raise SemanticError(f'can not convert type {value_type} to type {new_type} in new expression')
            else:
                raise SemanticError(f'more than one parameters in basic type new expression')
        # 构造结构体类型（调用构造函数）
        elif new_type.get_kind() == TypeKind.STRUCT:
            func_type, func_value = new_type.constructor.type, new_type.constructor.value

            # 检查函数参数数量是否符合
            n_args = len(node.param_list) + 1
            if len(func_type.func_params) != n_args:
                raise SemanticError(f'new expression must have the same number of parameters with constructor ({n_args} != {len(func_type.func_params)})')

            # 创建临时对象变量
            value = self.ctx.ir_builder.alloca(new_type.to_ir_type())

            # 处理调用参数
            param_values = [value]
            for idx, (param_symbol, param) in enumerate(zip(func_type.func_params, node.param_list)):
                param.accept(self)
                param_type, param_value = self.ctx.current_type, self.ctx.current_value
                cvt_success, param_value = self.ctx.convert_type(param_type, param_symbol.type, param_value)
                if not cvt_success:
                    raise SemanticError(f'can not convert {idx+1}th parameter type from {param_type} to {param_symbol.type}')
                param_values.append(param_value)

            # 调用构造函数
            self.ctx.ir_builder.call(func_value, param_values)

            # 转换对象到右值
            value = self.ctx.ir_builder.load(value)
        # 构造数组类型
        elif new_type.get_kind() == TypeKind.ARRAY:
            # 检查是否提供了过多的参数
            array_size = new_type.get_array_size()
            if len(node.param_list) > array_size:
                raise SemanticError(f'more than {array_size} element in array new expression')

            # 构造数组初始化值
            element_type = new_type.clone().to_element_type()
            param_values = []
            for param in node.param_list:
                param.accept(self)
                param_type, param_value = self.ctx.get_current_assignment_value(param)
                cvt_success, param_value = self.ctx.convert_type(param_type, element_type, param_value)
                if not cvt_success:
                    raise SemanticError(f'can not convert type {param_type} to type {element_type} in array new expression')
                param_values.append(param_value)
            # 如果值没指定全，对于基本类型剩余的补0，否则报错      
            if len(param_values) < array_size:
                if element_type.get_kind() == TypeKind.BASIC:
                    elem_ir_type = element_type.to_ir_type()
                    for _ in range(len(param_values), array_size):
                        param_values.append(elem_ir_type(0))
                else:
                    raise SemanticError(f'must specify {array_size} elements in array new expression (only {len(param_values)} now)')
                  
            value = new_type.to_ir_type()(param_values)
        elif new_type.get_kind() == TypeKind.INTERFACE:
            raise SemanticError(f'can not instantiate interface {new_type}')
        else:
            raise SemanticError(f'can not instantiate type {new_type}')

        self.ctx.current_type = new_type
        self.ctx.current_value = value

    @visitor.when(ast.CallExpression)
    def visit(self, node: ast.CallExpression):
        node.func_expr.accept(self)
        func_type, func_value = self.ctx.current_type, self.ctx.current_value

        # 检查调用值是否为函数类型
        if func_type.get_kind() != TypeKind.FUNCTION:
            raise SemanticError('call expression must be a function type')

        # 检查函数参数数量是否符合
        n_args = len(node.param_list)
        if isinstance(node.func_expr, ast.MemberExpression):
            n_args += 1
        if len(func_type.func_params) != n_args:
            raise SemanticError(f'call expression must have the same number of parameters with function ({n_args} != {len(func_type.func_params)})')

        # 显式实例化泛型函数的类型参数
        if func_value is None:
            if len(node.generics_spec_list) > len(func_type.generics_type_list):
                raise SemanticError(f'more generics types are specified ({len(node.generics_spec_list)} > {len(func_type.generics_type_list)})')

            # 得到显式实例化参数列表
            func_type = func_type.clone(clone_symbol_table=True)
            generics_spec_types = []
            generics_spec_list = {}
            for generics_spec in node.generics_spec_list:
                generics_spec.accept(self)
                generics_type = func_type.generics_type_list[len(generics_spec_types)]
                generics_spec_list[generics_type.generic_name] = self.ctx.current_type
                generics_spec_types.append(self.ctx.current_type)

            # 替换参数和返回值中的潜在泛型类型
            if len(generics_spec_list) > 0:
                func_type = func_type.specialize(generics_spec_list)

        param_values = []
        # 处理成员函数调用（在第一个参数增加self）
        if isinstance(node.func_expr, ast.MemberExpression):
            assert self.ctx.current_object_value is not None
            param_values.append(self.ctx.current_object_value)
            if func_type.func_params[0].type.is_generics():
                raise NotImplementedError()

        # 处理调用参数
        all_matched = {}
        param_array_dims = {}
        for idx, (param_symbol, param) in enumerate(zip(func_type.func_params[len(param_values):], node.param_list)):
            param.accept(self)
            param_type, param_value = self.ctx.current_type, self.ctx.current_value

            # 处理泛型函数参数
            if param_symbol.type.is_generics():
                # 用实参类型匹配推导泛型类型
                match_success, matched = param_symbol.type.match_generics(param_type)
                if not match_success:
                    raise SemanticError(f'can not deduce type {param_symbol.type} from {param_type}')

                # 合并已有的泛型类型推导结果
                all_matched = merge_matched(all_matched, matched)
                if all_matched is None:
                    raise SemanticError(f'conflicts in generics type deduction')

                # 记录实参数组维数
                if len(param_type.array_dims) > 0:
                    param_array_dims[param_symbol.id] = param_type.array_dims
            # 对于非泛型类型，尝试进行自动类型转换
            else:
                cvt_success, param_value = self.ctx.convert_type(param_type, param_symbol.type, param_value)
                if not cvt_success:
                    raise SemanticError(f'can not convert {idx+1}th parameter type from {param_type} to {param_symbol.type}')

            param_values.append(param_value)

        # 实例化泛型函数
        if func_value is None:
            # 用推导得到的泛型类型填充剩下的泛型参数
            for i in range(len(generics_spec_types), len(func_type.generics_type_list)):
                generics_type = func_type.generics_type_list[i]
                if generics_type.generic_name not in all_matched:
                    raise SemanticError(f'unspecified generics type {generics_type}')
                generics_spec_types.append(all_matched[generics_type.generic_name])

            # 实例化函数类型，并清空函数的泛型参数列表
            generics_spec_list = {}
            for i, generics_spec in enumerate(generics_spec_types):
                generic_name = func_type.generics_type_list[i].generic_name
                generics_spec_list[generic_name] = generics_spec
            func_type = func_type.specialize(generics_spec_list, param_array_dims)
            func_type.generics_type_list.clear()

            # 重新解析AST节点得到实例化参数
            self.ctx.current_generic_spec_type = func_type
            func_type.unfinished_node.accept(self)
            func_value = self.ctx.current_value

        self.ctx.current_value = self.ctx.ir_builder.call(func_value, param_values)
        self.ctx.current_type = func_type.func_ret_type

    @visitor.when(ast.IOExpression)
    def visit(self, node: ast.IOExpression):
        raise NotImplementedError()

    @visitor.when(ast.LambdaExpression)
    def visit(self, node: ast.LambdaExpression):
        raise NotImplementedError()


