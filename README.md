# easy-shader-lang

Easy Shader Language（ESL）是一个以易于开发与类型安全为目标的图形着色器语言。

## 使用

使用Python 3.7及以上版本，安装Python安装依赖

```bash
pip install ply
pip install llvmlite
```

之后可以调用编译器编译ESL代码，生成LLVM IR中间代码，如：

```
python esl-compiler.py example/function.esl
```

### 测试部分

#### 测试词法分析

```
python test/test_lexer.py <source_file>
```

#### 测试语法分析

```
python test/test_parser.py <source_file>
```

#### 测试AST构建

```
python test/test_ast.py
```

## TODO

代码生成已实现并测试的部分：

+ 类型声明解析
+ 变量声明、函数定义的解析
+ 结构体结构、成员解析
+ 基本语句结构的解析，包括：块语句、If语句、White语句、For语句、跳转语句等
+ 大部分表达式的解析，包括：操作数、一元表达式、二元表达式、成员访问表达式、类型构建表达式、引用表达式、数组索引表达式、强制类型转换表达式、函数调用表达式等
+ 自动类型转换，如`i8 -> i16 -> i32 -> i64`
+ 类型推导：包括变量定义类型推导与返回值类型推导

代码生成目前暂未完全实现和测试的部分：

+ 泛型实例化代码生成
+ Interface接口继承的语义检查
+ 链式自动类型转换
+ IO表达式与Lambda表达式的解析

以上部分代码将会在2021.12.20之前完全实现并重新上传。

