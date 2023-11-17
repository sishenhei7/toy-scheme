rust 里面的闭包和 js 里面的闭包其实不一样：由于闭包里面捕获的变量其实是一个指向上面作用域的借用，但是无论用 Box、Rc 还是 RefCell 包裹，闭包捕获的变量都会在它的作用域结束的时候被销毁，除非这个变量是 static，但是不现实。而 js 或者 scheme 里面的闭包捕获的变量，在它自己所在的作用域结束的时候不会被销毁，所以不能用 rust 里面的闭包来代替 js 或者 scheme 里面的闭包。

那为什么不用 move 或者 clone 来表示闭包捕获的变量？move 的情况下，这个闭包只能使用一次，不现实；clone 的情况下，捕获的变量和原变量失去了关联。

所以考虑不用闭包捕获变量来表示 continuation，而是使用一个 struct 来表示。那么 continuation 从原来的 cps 结构转化为和 react fiber 类似的链式结构，通过 next 指针来实现控制流，并通过一个 map 来保存每个节点来实现节点的跳转和重复 interpreter（callcc）

由于我们使用 map 保存了每个节点，所以 next 里面只需要存这个节点的 cid 就可以了，这样我们就转化成了使用 cid 来模拟 continuation。

kent 说过，表达式求值需要搞清楚两件事：1.what to evaluate; 2.what to do with the value。第一点说的是给什么求值，第二点说的是求值之后拿来干什么，其实就是continuation。以 if 语句为例，我们首先给 predict 求值，然后使用 predict 的值来判断给 then 求值还是给 else 求值。

continuation 有这三种情况：
1.求完这个表达式之后要倒回去
2.求完第一个表达式之后再求第二个
3.求完这个表达式之后，根据这个表达式的值来决定求值哪一个（控制流）

这就是这个包的主要考虑。
