ESLCompiler

1结构体构造函数

2结构体继承inter

3
func operator f32(self) {
    return self.radius;
}

4结构体和接口的泛型

5泛型函数的使用就是，在调用的时候按照对应的泛型类型传参就好。也就是定义完后，调用的时候不需要手动设置类型，根据传入的值类型自动选择
6
let length : M<f32> = 1;

func <cm> calcArea(length : f32<cm>) -> f32<cm*cm>;
都是type spec的问题，应该要有泛型
7
calcArea(length);
let z = func(x) { return x*x; };
let w : func(_: i32) -> i32 = z;
8
s3.intersect(s2);
关于call_expr和member_expr的冲突







