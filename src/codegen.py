from typing import List, Tuple, Optional
import utils.visitor as visitor
from llvmlite import ir
from .enums import BasicType, BinaryOp, TypeKind, UnaryOp, IOType
from .symbol import Symbol, SymbolTable, Type
from . import ast


class SemanticError(RuntimeError):
    def __init__(self, *args: object) -> None:
        super().__init__(*args)
        

class CodeGenContext():
    def __init__(self, module_name) -> None:
        super().__init__(self)
        self.module = ir.Module(name=module_name)
        self.visitor = CodeGenVisitor(self)

        self.scope_stack : List[Tuple[SymbolTable, ir.IRBuilder, Type]] = []
        self.block_stack : List[Tuple[ir.Block, ir.Block]] = []
        self.symbol_table = SymbolTable(None)
        self.ir_builder : ir.IRBuilder = None
        self.deduced_ret_type : Optional[Type] = None
        self.current_type : Type = None
        self.current_value : ir.Value = None
        self.current_break_block : ir.Block = None
        self.current_continue_block : ir.Block = None

    def create_global_init_function(self):
        self.init_func_type = ir.FunctionType(ir.VoidType(), ())
        self.init_func = ir.Function(self.module, self.init_func_type, "__init__")
        block = self.init_func.append_basic_block('entry')
        self.init_func_ir_builder = ir.IRBuilder(block)
        
    def push_scope(self, symbol_table : SymbolTable, ir_builder : ir.IRBuilder, deduced_ret_type = None) -> None:
        """创建一个新的作用域"""
        self.scope_stack.append((self.symbol_table, self.ir_builder, self.deduced_ret_type))
        self.symbol_table = symbol_table
        self.ir_builder = ir_builder
        self.deduced_ret_type = deduced_ret_type

    def pop_scope(self) -> Tuple[SymbolTable, ir.IRBuilder]:
        """恢复上一个作用域"""
        poped_scope = (self.symbol_table, self.ir_builder)
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

        # 

        raise NotImplementedError()

    def get_module(self) -> ir.Module:
        return self.module
        


class CodeGenVisitor():
    def __init__(self, context : CodeGenContext) -> None:
        super().__init__(self)
        self.ctx = context

    @visitor.on('node')
    def visit(self, node):
        raise NotImplementedError(f'Node {type(node)} visitor not implemented')

    @visitor.when(ast.TranslationUnit)
    def visit(self, node : ast.TranslationUnit):
        for decl in node.declaration_list:
            decl.accept(self)

    @visitor.when(ast.TypeAliasDecl)
    def visit(self, node: ast.TypeAliasDecl):
        self.ctx.symbol_table.add_type(node.identifier, self.ctx.current_type)

    @visitor.when(ast.VariableDecl)
    def visit(self, node: ast.VariableDecl):
        for declarator in node.declarator_list:
            declarator.accept(declarator)

    @visitor.when(ast.Declarator)
    def visit(self, node: ast.Declarator):
        # 解析(可选)定义类型
        if node.type_spec:
            node.type_spec.accept(self)
            var_type = self.ctx.current_type
        else:
            var_type = None

        # 解析表达式
        node.init_expr.accept(self)
        if var_type is None:
            var_type = self.ctx.current_type

        # 初始化表达式类型转换
        cvt_success, var_value = self.ctx.convert_type(self.ctx.current_type, var_type, self.ctx.current_value)
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
            global_value.initializer = var_value
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
        
        # 若当前函数的声明已实例化（值存在），则直接解析函数体
        if self.ctx.current_value:
            assert isinstance(self.ctx.current_value, ir.Function)
            entry_block = self.ctx.current_value.append_basic_block('entry')
            func_ir_builder = ir.IRBuilder(entry_block)

            # 创建函数参数局部变量，并储存函数入参值
            assert len(self.ctx.current_type.func_params) == len(self.ctx.current_value.args)
            for param, arg in zip(self.ctx.current_type.func_params, self.ctx.current_value.args):
                param.value = func_ir_builder.alloca(arg.type, name=param.id)
                func_ir_builder.store(arg, param.value)

            # 解析函数体
            self.ctx.push_scope(self.ctx.current_type.symbol_table, func_ir_builder)
            node.block_stmt.accept(self)
            self.ctx.pop_scope()
        # 否则此函数为泛型函数，记录未解析节点至函数类型中
        else:
            self.ctx.current_type.unfinished_node = node.block_stmt

    @visitor.when(ast.SimpleType)
    def visit(self, node: ast.SimpleType):
        self.ctx.current_type = Type(basic_type=node.type)
        # if node.type == BasicType.BOOL:
        #     self.ctx.current_type = Type(ir.IntType(1), node.type)
        # elif node.type == BasicType.F16:
        #     self.ctx.current_type = Type(ir.HalfType(), node.type)
        # elif node.type == BasicType.F32:
        #     self.ctx.current_type = Type(ir.FloatType(), node.type)
        # elif node.type == BasicType.F64:
        #     self.ctx.current_type = Type(ir.DoubleType(), node.type)
        # elif node.type == BasicType.I8 or node.type == BasicType.U8:
        #     self.ctx.current_type = Type(ir.IntType(8), node.type)
        # elif node.type == BasicType.I16 or node.type == BasicType.U8:
        #     self.ctx.current_type = Type(ir.IntType(16), node.type)
        # elif node.type == BasicType.I32 or node.type == BasicType.U8:
        #     self.ctx.current_type = Type(ir.IntType(32), node.type)
        # elif node.type == BasicType.I64 or node.type == BasicType.U8:
        #     self.ctx.current_type = Type(ir.IntType(64), node.type)
        # else:
        #     raise NotImplementedError(f'IR BasicType {node.type.name} not implemented')

    @visitor.when(ast.ComplexType)
    def visit(self, node: ast.ComplexType):
        complex_type = self.ctx.symbol_table.query_type(node.identifier)

        if len(node.generics_spec_list) > 0:
            raise NotImplementedError()
            generics_spec_list = []
            for spec_type_node in node.generics_spec_list:
                spec_type_node.accept(self)
                generics_spec_list.append(self.ctx.current_type)

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
        self.ctx.current_type.add_array_dim(node.size or 0)

    @visitor.when(ast.ReferenceType)
    def visit(self, node: ast.ReferenceType):
        node.type.accept(self)
        self.ctx.current_type.add_ref()

    @visitor.when(ast.FunctionType)
    def visit(self, node: ast.FunctionType):
        node.func_sign.accept(self)

    @visitor.when(ast.FunctionDecl)
    def visit(self, node: ast.FunctionDecl):
        # 首先解析函数签名得到函数类型
        node.func_sign.accept(self)

        # 对于非泛型函数，此时可以构建ir.Function
        if not self.ctx.current_type.has_generics():
            func_value = ir.Function(self.ctx.module, self.ctx.current_type.to_ir_type(), node.identifier)
        # 否则对于泛型函数，其符号值目前留空，等待实例化时再生成值
        else:
            func_value = None

        # 将该函数符号加入到当前符号表中
        self.ctx.symbol_table.add_symbol(node.identifier, self.ctx.current_type, func_value)

        # 设置当前的值为函数符号值
        self.ctx.current_value = func_value

    @visitor.when(ast.FunctionSignature)
    def visit(self, node: ast.FunctionSignature):
        if node.return_type_spec:
            # 返回值从return_type_spec得到
            node.return_type_spec.accept(self)
            func_type = self.ctx.current_type
        else:
            # 返回值是Auto
            func_type = Type()

        # 为函数建立符号表
        func_type.add_symbol_table(self.ctx.symbol_table)

        # 处理泛型类型声明列表
        for generics_type in node.generics_type_list:
            generics_type.accept(self)
            # 将泛型类型加入该函数类型的泛型列表中
            func_type.add_generics_type(self.ctx.current_type)

        # 处理函数的参数列表
        param_list = []
        for param in node.parameter_decl_list:
            # 如果参数声明了具体类型，解析该类型
            if param.type_spec:
                param.type_spec.accept(self)
                param_type = self.ctx.current_type
            # 否则将该参数的类型当作一个新的泛型类型，并加入到该函数类型的泛型列表中
            else:
                param_type = Type(generic_name='')  # 匿名泛型类型
                func_type.add_generics_type(param_type)

            # 获得参数的类型后，将符号加入到函数的符号表和参数列表中
            # 此时函数参数符号还没有值，需要等到后续的函数定义时才会产生值
            param_symbol = func_type.symbol_table.add_symbol(param.identifier, param_type)
            param_list.append(param_symbol)

        # 加入参数列表，组成函数类型
        func_type.add_func_params(param_list)

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
        pass

    @visitor.when(ast.InterfaceDecl)
    def visit(self, node: ast.InterfaceDecl):
        pass

    @visitor.when(ast.MemberDecl)
    def visit(self, node: ast.MemberDecl):
        pass

    @visitor.when(ast.MemberFuncDecl)
    def visit(self, node: ast.MemberFuncDecl):
        pass

    @visitor.when(ast.MemberFuncDefinition)
    def visit(self, node: ast.MemberFuncDefinition):
        pass

    @visitor.when(ast.MemberTypeFuncDecl)
    def visit(self, node: ast.MemberTypeFuncDecl):
        pass

    @visitor.when(ast.MemberTypeFuncDefinition)
    def visit(self, node: ast.MemberTypeFuncDefinition):
        pass

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
        self.ctx.pop_scope()

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
        with self.ctx.ir_builder.if_else(cond_value) as (then, otherwise):
            with then:
                node.true_stmt.accept(self)
            with otherwise:
                if node.false_stmt:
                    node.false_stmt.accept(self)

    @visitor.when(ast.WhileStatement)
    def visit(self, node: ast.WhileStatement):
        # 创建While基本块
        func = self.ctx.ir_builder.function
        cond_block = ir.Block(func, 'while.cond')
        loop_block = ir.Block(func, 'while.loop')
        end_block = ir.Block(func, 'while.end')
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
        self.ctx.ir_builder.cbranch(cond_value, loop_block, end_block)

        # 解析循环体
        self.ctx.push_loop_block(end_block, cond_block)
        self.ctx.ir_builder.position_at_start(loop_block)
        node.loop_stmt.accept(self)
        self.ctx.ir_builder.branch(cond_block)
        self.ctx.pop_loop_block()

        # 设置插入点至while之后
        self.ctx.ir_builder.position_at_start(end_block)

    @visitor.when(ast.ForStatement)
    def visit(self, node: ast.ForStatement):
        # 生成For初始化语句
        node.init_stmt.accept(self)

        # 创建For基本块
        func = self.ctx.ir_builder.function
        cond_block = ir.Block(func, 'for.cond')
        loop_block = ir.Block(func, 'for.loop')
        end_block = ir.Block(func, 'for.end')
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
            self.ctx.ir_builder.cbranch(cond_value, loop_block, end_block)
        # 若没有For条件，则无条件进入循环
        else:
            self.ctx.ir_builder.branch(loop_block)

        # 解析循环体
        self.ctx.push_loop_block(end_block, cond_block)
        self.ctx.ir_builder.position_at_start(loop_block)
        node.loop_stmt.accept(self)
        if node.iter_expr:
            node.iter_expr.accept(self)
        self.ctx.ir_builder.branch(cond_block)
        self.ctx.pop_loop_block()

        # 设置插入点至for之后
        self.ctx.ir_builder.position_at_start(end_block)

    @visitor.when(ast.BreakStatement)
    def visit(self, node: ast.BreakStatement):
        self.ctx.ir_builder.branch(self.ctx.current_break_block)

    @visitor.when(ast.ContinueStatement)
    def visit(self, node: ast.ContinueStatement):
        self.ctx.ir_builder.branch(self.ctx.current_continue_block)

    @visitor.when(ast.ReturnStatement)
    def visit(self, node: ast.ReturnStatement):
        ret_type = self.ctx.symbol_table.parent_type.clone().to_return_type()
        if node.expression:
            # 检查函数返回类型是否为void
            if ret_type.get_kind() == TypeKind.BASIC and ret_type.basic_type == BasicType.VOID:
                raise SemanticError('void function should not return a value')

            # 解析返回值表达式
            node.expression.accept(self)

            # 如果未指定返回类型，推导表达式类型为该返回值类型
            if ret_type.get_kind() == TypeKind.AUTO:
                self.ctx.deduced_ret_type = self.ctx.current_type
                ret_value = self.ctx.current_value
            # 如果指定了返回类型，检验返回值是否可以转换为返回类型
            else:
                cvt_success, ret_value = self.ctx.convert_type(
                    self.ctx.current_type, ret_type, self.ctx.current_value)
                if not cvt_success:
                    raise SemanticError('condition is not convertible to bool')

            self.ctx.ir_builder.ret(ret_value)
        else:
            # 检查函数返回类型是否为void
            if ret_type.get_kind() != TypeKind.BASIC or ret_type.basic_type != BasicType.VOID:
                raise SemanticError('non-void function should return a value')

            self.ctx.ir_builder.ret_void()

    @visitor.when(ast.AssignExpression)
    def visit(self, node: ast.AssignExpression):
        # 解析左表达式
        node.lhs_expr.accept(self)
        lhs_type, lhs_value = self.ctx.current_type, self.ctx.current_value

        # 检测左表达式是可修改的引用类型（左值）
        if lhs_type.get_kind() != TypeKind.REFERENCE:
            raise SemanticError('can not assign to a r-value')
        if lhs_type.is_const:
            raise SemanticError('can not assign to const variable')

        # 解析右表达式
        node.rhs_expr.accept(self)
        rhs_type, rhs_value = self.ctx.current_type, self.ctx.current_value

        # 若右表达式类型为显式引用，要求左表达式也为一个引用类型的标识符
        if isinstance(node.rhs_expr, ast.RefExpression):
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
            # 检查右侧类型可以被转换到左侧类型去除引用后的值类型
            lhs_value_type = lhs_type.clone().remove_ref()
            cvt_success, rhs_value = self.ctx.convert_type(rhs_type, lhs_value_type, rhs_value)
            if not cvt_success:
                raise SemanticError('can not assign value to target type')

            # 储存值到变量中
            self.ctx.ir_builder.store(rhs_value, lhs_value)
        pass

    @visitor.when(ast.BinaryExpression)
    def visit(self, node: ast.BinaryExpression):
        # 解析左表达式
        node.lhs_expr.accept(self)
        lhs_type, lhs_value = self.ctx.current_type, self.ctx.current_value

        # 解析右表达式
        node.rhs_expr.accept(self)
        rhs_type, rhs_value = self.ctx.current_type, self.ctx.current_value

        # 检查左右表达式类型一致
        if not lhs_type == rhs_type:
            raise SemanticError(f'type of binary operands are not the same')
        # 检查操作数类型可运算（整形或浮点型）
        if lhs_type.get_kind() != TypeKind.BASIC:
            raise SemanticError(f'unsupported type of binary operands')

        # 生成运算指令
        irb = self.ctx.ir_builder
        basic_type = lhs_type.basic_type
        is_bool = basic_type == BasicType.BOOL
        is_float = basic_type == BasicType.F16 or basic_type == BasicType.F32 or basic_type == BasicType.F64
        is_integer = not is_bool and not is_float

        if node.operator == BinaryOp.PLUS:
            value = (irb.fadd if is_float else irb.add)(lhs_value, rhs_value)
        elif node.operator == BinaryOp.MINUS:
            value = (irb.fsub if is_float else irb.sub)(lhs_value, rhs_value)
        else:
            raise NotImplementedError(f'binary op {node.operator} not implemented')
        self.ctx.current_value = value
        self.ctx.current_type = lhs_type

    @visitor.when(ast.UnaryExpression)
    def visit(self, node: ast.UnaryExpression):
        # 解析表达式
        node.expression.accept(self)
        type, value = self.ctx.current_type, self.ctx.current_value

        # 检查操作数类型可运算（整形或浮点型）
        if type.get_kind() != TypeKind.BASIC:
            raise SemanticError(f'unsupported type of unary operands')

        # 生成运算指令
        irb = self.ctx.ir_builder
        basic_type = type.basic_type
        is_bool = basic_type == BasicType.BOOL
        is_float = basic_type == BasicType.F16 or basic_type == BasicType.F32 or basic_type == BasicType.F64
        is_integer = not is_bool and not is_float

        if node.operator == UnaryOp.PLUS:
            pass
        elif node.operator == UnaryOp.MINUS:
            value = irb.neg(value)
        elif node.operator == UnaryOp.NOT:
            if is_float:
                raise SemanticError('unsupported type for unary not')
            value = irb.not_(value)
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
            if symbol.type.get_kind() == TypeKind.FUNCTION or symbol.type.is_const:
                self.ctx.current_type = symbol.type
            else:
                self.ctx.current_type = symbol.type.clone().add_ref()
            self.ctx.current_value = symbol.value

    @visitor.when(ast.ExpressionOperand)
    def visit(self, node: ast.ExpressionOperand):
        node.expression.accept(self)

    @visitor.when(ast.MemberExpression)
    def visit(self, node: ast.MemberExpression):
        pass

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
        cvt_success, value = self.ctx.convert_type(src_type, target_type, value)
        if cvt_success:
            self.ctx.current_type = target_type
            self.ctx.current_value = value

        # 否则使用尝试使用强制转换规则
        raise NotImplementedError()

    @visitor.when(ast.NewExpression)
    def visit(self, node: ast.NewExpression):
        raise NotImplementedError()

    @visitor.when(ast.IOExpression)
    def visit(self, node: ast.IOExpression):
        raise NotImplementedError()

    @visitor.when(ast.LambdaExpression)
    def visit(self, node: ast.LambdaExpression):
        raise NotImplementedError()


