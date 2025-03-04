# Ошибки в LuNA-программах

Для удобства далее в понятие *ошибка* помимо ошибок в традиционном смысле (ошибки компиляции, ошибки времени исполнения и т. п.) включаются также потенциальные недочеты, не приводящие к ошибкам компиляции или некорректному поведению при запуске, обнаружение которых тем не менее может представлять интерес для программиста.

Ошибки разделяются на *семантические* и *синтаксические*.

*Семантические* ошибки влияют (могут влиять) на поведение запущенной программы.

*Синтаксические* ошибки имеют место в том случае, когда текст программы:
 - не соответствует формальной грамматике языка;
 - содержит фрагменты, смысл которых может не соответствовать ожиданиям программиста (например, ошибки SYN5, SYN6);
 - содержит фрагменты, препятствующие компиляции или компоновке программы по иным причинам (например, SYN10).

# Синтаксические ошибки 

## SYN1 - Несоответствие типов аргументов LuNA типам формальных параметров при вызове ФК (типы аргументов известны на этапе компиляции)

**Обнаруживает компилятор.**

В старой классификации:
[LUNA04](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#4-%D0%BD%D0%B5%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D0%B8%D0%B5-%D1%82%D0%B8%D0%BF%D0%BE%D0%B2-%D0%B0%D1%80%D0%B3%D1%83%D0%BC%D0%B5%D0%BD%D1%82%D0%BE%D0%B2-luna-%D0%BF%D1%80%D0%B8-%D0%B2%D1%8B%D0%B7%D0%BE%D0%B2%D0%B5-%D0%B0%D1%82%D0%BE%D0%BC%D0%B0%D1%80%D0%BD%D0%BE%D0%B3%D0%BE-%D1%84%D0%BA)

Пример:

```
C++ sub string_print(string s) ${{ printf("%s\n"); $}}

sub main() {
    string_print(3.14);
}
```

Вывод luna:
```
luna: fatal error: cpp-generation failed (see below):
err> Traceback (most recent call last):
err>   File "/mnt/c/Users/misha/luna/scripts/fcmp2", line 1279, in <module>
err>     bid, cpp=gen_struct(sub)
err>   File "/mnt/c/Users/misha/luna/scripts/fcmp2", line 1210, in gen_struct
err>     cpp_bodyfork, cpp_bodyitems=gen_body(scope, ja)
err>   File "/mnt/c/Users/misha/luna/scripts/fcmp2", line 1177, in gen_body
err>     bibid, cpp=gen_bi(bi, scope)
err>   File "/mnt/c/Users/misha/luna/scripts/fcmp2", line 1156, in gen_bi
err>     return gen_bi_exec(ja, parent_scope)
err>   File "/mnt/c/Users/misha/luna/scripts/fcmp2", line 1007, in gen_bi_exec
err>     cpp+=gen_exec_extern(scope, ja)
err>   File "/mnt/c/Users/misha/luna/scripts/fcmp2", line 644, in gen_exec_extern
err>     value(arg, scope, param)
err>   File "/mnt/c/Users/misha/luna/scripts/fcmp2", line 456, in value
err>     return value_string(x, scope)
err>   File "/mnt/c/Users/misha/luna/scripts/fcmp2", line 430, in value_string
err>     R(x['type'], x)
err>   File "/mnt/c/Users/misha/luna/scripts/repr_ja.py", line 6, in R
err>     raise Exception(', '.join([json.dumps(obj, indent=4) for obj in objs]))
err> Exception: "rconst", {
err>     "is_expr": true,
err>     "type": "rconst",
err>     "value": 3.14
err> }
err>
```

## SYN2 - Использование необъявленного ФК

В старой классификации: [LUNA02](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#2-%D0%BD%D0%B5%D1%81%D1%83%D1%89%D0%B5%D1%81%D1%82%D0%B2%D1%83%D1%8E%D1%89%D0%B0%D1%8F-luna-%D0%BF%D0%BE%D0%B4%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D0%BC%D0%B0)

**Обнаруживает компилятор.**

Пример:
```
sub main() {
    real_print(3.14);
}
```
Вывод luna:
```
luna: fatal error: recom-generation failed (see below):
Traceback (most recent call last):
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 1713, in <module>
    content=Fa(ja).gen()
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 1679, in __init__
    self.Subs[sub_name]=create_sub(self, sub_name)
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 1668, in create_sub
    return SubStruct(fa, sub_name, None)
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 1619, in __init__
    parse_bi(self.Items, bi, self.Regs, self)
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 833, in parse_bi
    sub=ja[bi['code']]
KeyError: 'real_print'
```

## SYN3 - Несовпадение количества аргументов при объявлении ФК и его вызове

В старой классификации: [LUNA06](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#6-%D0%BD%D0%B5%D1%81%D0%BE%D0%B2%D0%BF%D0%B0%D0%B4%D0%B5%D0%BD%D0%B8%D0%B5-%D0%BA%D0%BE%D0%BB%D0%B8%D1%87%D0%B5%D1%81%D1%82%D0%B2%D0%B0-%D0%B0%D1%80%D0%B3%D1%83%D0%BC%D0%B5%D0%BD%D1%82%D0%BE%D0%B2-%D0%BF%D1%80%D0%B8-%D0%BE%D0%B1%D1%8A%D1%8F%D0%B2%D0%BB%D0%B5%D0%BD%D0%B8%D0%B8-%D1%84%D0%BA-%D0%B8-%D0%B5%D0%B3%D0%BE-%D0%B2%D1%8B%D0%B7%D0%BE%D0%B2%D0%B5)

**Обнаруживает компилятор**     

Пример:
```
C++ sub int_sum(int a, int b, name result) ${{ result = a + b; $}}

sub main() {
    df s;
    int_sum(1, 2, 3, s);
}
```
Вывод luna:
```
luna: fatal error: recom-generation failed (see below):
Traceback (most recent call last):
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 1713, in <module>
    content=Fa(ja).gen()
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 1679, in __init__
    self.Subs[sub_name]=create_sub(self, sub_name)
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 1668, in create_sub
    return SubStruct(fa, sub_name, None)
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 1619, in __init__
    parse_bi(self.Items, bi, self.Regs, self)
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 835, in parse_bi
    items.append(BiExecExtern(bi, regs, parent))
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 1453, in __init__
    BiExec.__init__(self, bi, regs, parent)
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 1430, in __init__
    assert len(self.code['args'])==len(self.j['args'])
AssertionError
```

## SYN4 - Более одного описания фрагментов данных внутри блока

В старой классификации: [LUNA07](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#7-%D0%B4%D0%B2%D0%B0-%D0%B8%D0%BB%D0%B8-%D0%B1%D0%BE%D0%BB%D0%B5%D0%B5-%D0%BE%D0%B1%D1%8A%D1%8F%D0%B2%D0%BB%D0%B5%D0%BD%D0%B8%D0%B9-%D1%84%D0%B4-%D0%B2-%D0%BF%D0%BE%D0%B4%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D0%BC%D0%B5)

**Обнаруживает компилятор.**     

Пример:
```
sub main() {
    df x;
    df y;
}
```
Вывод luna:
```
luna: compile error: syntax error at ./luna4:3
    df y;
      ^--here
```

## SYN5 - Имя встречается только при объявлении 

**Не обнаруживается LuNA.**

Некоторый идентификатор (имя) был объявлен, но больше не фигурирует в коде.

### SYN5.1 - Макрос объявлен, но не используется

Пример:
```
C++ sub empty() ${{$}}

#define M(X) $X

sub main() {
    empty();
}

```

### SYN5.2 - Имя, определенное в операторе let, не используется 

Пример:
```
C++ sub empty() ${{$}}

sub main() {
    let x = 42 {
        empty();
    }
}
```

### SYN5.3 - Базовое имя ФД, определенное в операторе df, не используется

Пример:
```
C++ sub empty() ${{$}}

sub main() {
    df x;
    empty();
}
```

### SYN5.4 - Атомарный ФК не используется

Пример:
```
C++ sub empty() ${{$}}

C++ sub sum(int a, int b, name c) ${{ c = a + b; $}}

sub main() {
    empty();
}
```

### SYN5.5 - Структурированный ФК не используется

Пример:
```
C++ sub empty() ${{$}}

sub f(int x) {
    empty();
}

sub main() {
    empty();
}
```

### SYN5.6 - Параметр структурированного ФК не используется

Пример:
```
C++ sub empty() ${{$}}

sub f(int x) {
//    ^^^^^ here
    empty();
}

sub main() {
    empty();
}
```

### SYN5.7 - Счетчик оператора for не используется

Пример:
```
C++ sub empty() ${{$}}

sub main() {
    for i = 1..10 {
        empty();
    }
}
```

### SYN5.8 - Счетчик оператора while не используется

Пример:
```
C++ sub empty() ${{$}}

sub main() {
    df N;
    while 0, i = 1..out N {
        empty();
    }
}
```

## SYN6 - Объявление нескольких ФК с одним и тем же именем

В старой классификации: [LUNA11](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#11-%D0%B8%D0%BC%D0%BF%D0%BE%D1%80%D1%82-%D0%BD%D0%B5%D1%81%D0%BA%D0%BE%D0%BB%D1%8C%D0%BA%D0%B8%D1%85-%D1%80%D0%B0%D0%B7%D0%BD%D1%8B%D1%85-%D1%84%D0%BA-%D0%BF%D0%BE%D0%B4-%D0%BE%D0%B4%D0%BD%D0%B8%D0%BC-%D0%B0%D0%BB%D0%B8%D0%B0%D1%81%D0%BE%D0%BC), [LUNA16](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#16-%D0%BF%D0%BE%D0%B2%D1%82%D0%BE%D1%80%D0%BD%D0%BE%D0%B5-%D0%BE%D0%BF%D1%80%D0%B5%D0%B4%D0%B5%D0%BB%D0%B5%D0%BD%D0%B8%D0%B5-%D1%84%D0%BA)

**Не обнаруживается LuNA.**     

### SYN6.1 - Импортирование ФК под именем уже существующего ФК

Пример:
```
import printa(real) as print;
import printb(real) as print;

sub main(){
	print(1.0);
}
```
Реакция: программа выполнится, вызовется **printb**.

### SYN6.2 - Объявление структурированного ФК с именем уже существующего ФК

Пример:
```
С++ sub print(int a) ${{ printf("%d\n", a); $}}

sub foo() {
    print(1);
}

sub foo() {
    print(2);
}

sub main() {
    foo();
}
```
Реакция: программа выполнится и напечатает **2**.

## SYN7 - Нет main

В старой классификации: [LUNA12](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#12-%D0%BD%D0%B5%D1%82-main)

**Обнаруживает компилятор.**     

Пример:
```
C++ sub empty() ${{$}}

sub foo() { empty(); }
```
Вывод luna:
```
luna: fatal error: cpp-generation failed (see below):
err> Traceback (most recent call last):
err>   File "/mnt/c/Users/misha/luna/scripts/fcmp2", line 1249, in <module>
err>     gja['main']['name']='main'
err> KeyError: 'main'
err>
```

## SYN8 - Повторное объявление имени

В старой классификации: [LUNA13](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#13-df-%D1%81-%D0%BE%D0%B4%D0%B8%D0%BD%D0%B0%D0%BA%D0%BE%D0%B2%D1%8B%D0%BC%D0%B8-%D0%BD%D0%B0%D0%B7%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F%D0%BC%D0%B8-%D0%B2-%D0%BE%D0%B4%D0%BD%D0%BE%D0%B9-%D0%BE%D0%B1%D0%BB%D0%B0%D1%81%D1%82%D0%B8-%D0%B2%D0%B8%D0%B4%D0%B8%D0%BC%D0%BE%D1%81%D1%82%D0%B8)

**Обнаруживает компилятор.**  

Пример:
```
C++ sub empty() ${{$}}

sub main() {
    df x, y, x;
    empty();
}
```
Вывод luna:
```
luna: compile error: redeclaration of x at ./luna8.fa:4
    df x, y, x;
    ^--here
```

Такая ошибка может быть порождена как оператором df, так и при порождении переменных let и итераторов for и while.

## SYN9 - Попытка использования необъявленного идентификатора

В старой классификации: [LUNA14](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#14-%D0%BF%D0%BE%D0%BF%D1%8B%D1%82%D0%BA%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D0%BD%D0%B5%D0%BE%D0%B1%D1%8A%D1%8F%D0%B2%D0%BB%D0%B5%D0%BD%D0%BD%D0%BE%D0%B3%D0%BE-%D1%84%D0%B4)

**Обнаруживает компилятор.** 

Пример:
```
C++ sub foo(int x) ${{$}}

sub main() {
    foo(x);
}

```
Вывод luna:
```
luna: fatal error: recom-generation failed (see below):
Traceback (most recent call last):
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 1713, in <module>
    content=Fa(ja).gen()
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 1679, in __init__
    self.Subs[sub_name]=create_sub(self, sub_name)
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 1668, in create_sub
    return SubStruct(fa, sub_name, None)
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 1619, in __init__
    parse_bi(self.Items, bi, self.Regs, self)
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 835, in parse_bi
    items.append(BiExecExtern(bi, regs, parent))
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 1464, in __init__
    new_deps.update(self.parse_deps(self.j['args'][i],
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 989, in parse_deps
    elif self.is_requestable(expr):
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 955, in is_requestable
    name_type=self._get_name_type(expr['ref'][0])
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 864, in _get_name_type
    return self.Parent._get_name_type(name)
  File "/mnt/c/Users/misha/luna/scripts/fcmp", line 866, in _get_name_type
    raise NameNotRecognized(name)
__main__.NameNotRecognized: x
```

## SYN10 - Импортирование несуществующей C/C++ функции

В старой классификации: [LUNA17](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#17-%D0%B8%D0%BC%D0%BF%D0%BE%D1%80%D1%82-%D0%BD%D0%B5%D1%81%D1%83%D1%89%D0%B5%D1%81%D1%82%D0%B2%D1%83%D1%8E%D1%89%D0%B5%D0%B9-cc-%D1%84%D1%83%D0%BD%D0%BA%D1%86%D0%B8%D0%B8)

**Обнаруживает RTS.** 

Пример:
```
import foo(int) as foo;

sub main() {
    df x;
    foo(x);
}
```
Вывод luna:
```
luna: fatal error: run-time error: errcode=-6
err> 0 ERROR:  /mnt/c/Users/misha/luna/build/programs/mnt/c/Users/misha/Projects/luna/Extraterrestrial/adapt/errors/demos/new/luna10.fa/libucodes.so: undefined symbol: foo ./src/rts/fp.cpp:26
err> 0 ABORT 
err> terminate called after throwing an instance of 'std::logic_error'
err>   what():  basic_string::_M_construct null not valid
err> [DESKTOP-CPJS18K:22831] *** Process received signal ***
err> [DESKTOP-CPJS18K:22831] Signal: Aborted (6)
err> [DESKTOP-CPJS18K:22831] Signal code:  (-6)
err> [DESKTOP-CPJS18K:22831] [ 0] /lib/x86_64-linux-gnu/libc.so.6(+0x42520)[0x7f7aa0b38520]
err> [DESKTOP-CPJS18K:22831] [ 1] /lib/x86_64-linux-gnu/libc.so.6(pthread_kill+0x12c)[0x7f7aa0b8c9fc]      
err> [DESKTOP-CPJS18K:22831] [ 2] /lib/x86_64-linux-gnu/libc.so.6(raise+0x16)[0x7f7aa0b38476]
err> [DESKTOP-CPJS18K:22831] [ 3] /lib/x86_64-linux-gnu/libc.so.6(abort+0xd3)[0x7f7aa0b1e7f3]
err> [DESKTOP-CPJS18K:22831] [ 4] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xa2b9e)[0x7f7aa0de3b9e]
err> [DESKTOP-CPJS18K:22831] [ 5] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae20c)[0x7f7aa0def20c]
err> [DESKTOP-CPJS18K:22831] [ 6] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae277)[0x7f7aa0def277]
err> [DESKTOP-CPJS18K:22831] [ 7] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae4d8)[0x7f7aa0def4d8]
err> [DESKTOP-CPJS18K:22831] [ 8] /lib/x86_64-linux-gnu/libstdc++.so.6(_ZSt19__throw_logic_errorPKc+0x40)[0x7f7aa0de6344]
err> [DESKTOP-CPJS18K:22831] [ 9] /mnt/c/Users/misha/luna/lib/librts.so(+0x37b33)[0x7f7aa10fbb33]
err> [DESKTOP-CPJS18K:22831] [10] /mnt/c/Users/misha/luna/lib/librts.so(+0x1fc55)[0x7f7aa10e3c55]
err> [DESKTOP-CPJS18K:22831] [11] /mnt/c/Users/misha/luna/bin/rts(main+0x1fb)[0x559c8cd13d0b]
err> [DESKTOP-CPJS18K:22831] [12] /lib/x86_64-linux-gnu/libc.so.6(+0x29d90)[0x7f7aa0b1fd90]
err> [DESKTOP-CPJS18K:22831] [13] /lib/x86_64-linux-gnu/libc.so.6(__libc_start_main+0x80)[0x7f7aa0b1fe40]  
err> [DESKTOP-CPJS18K:22831] [14] /mnt/c/Users/misha/luna/bin/rts(+0xdf15)[0x559c8cd13f15]
err> [DESKTOP-CPJS18K:22831] *** End of error message ***
err>
```

## SYN11 - Попытка индексации выражения, не являющегося именем

В старой классификации: [LUNA36](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#36-%D0%BF%D0%BE%D0%BF%D1%8B%D1%82%D0%BA%D0%B0-%D0%B8%D0%BD%D0%B4%D0%B5%D0%BA%D1%81%D0%B0%D1%86%D0%B8%D0%B8-%D0%BE%D0%B1%D1%8A%D0%B5%D0%BA%D1%82%D0%B0-%D0%BD%D0%B5-%D1%8F%D0%B2%D0%BB%D1%8F%D1%8E%D1%89%D0%B5%D0%B3%D0%BE%D1%81%D1%8F-%D1%84%D0%B4)

**Обнаруживает компилятор.**

Пример:
```
C++ sub empty(int x) ${{$}}

sub main(int a) {
    empty(a[1]);
}
```
Вывод luna:
```
luna: fatal error: cpp-generation failed (see below):
err> Traceback (most recent call last):
err>   File "/mnt/c/Users/misha/luna/scripts/fcmp2", line 1279, in <module>
err>     bid, cpp=gen_struct(sub)
err>   File "/mnt/c/Users/misha/luna/scripts/fcmp2", line 1210, in gen_struct
err>     cpp_bodyfork, cpp_bodyitems=gen_body(scope, ja)
err>   File "/mnt/c/Users/misha/luna/scripts/fcmp2", line 1177, in gen_body
err>     bibid, cpp=gen_bi(bi, scope)
err>   File "/mnt/c/Users/misha/luna/scripts/fcmp2", line 1156, in gen_bi
err>     return gen_bi_exec(ja, parent_scope)
err>   File "/mnt/c/Users/misha/luna/scripts/fcmp2", line 1007, in gen_bi_exec
err>     cpp+=gen_exec_extern(scope, ja)
err>   File "/mnt/c/Users/misha/luna/scripts/fcmp2", line 650, in gen_exec_extern
err>     value(arg, scope, param)
err>   File "/mnt/c/Users/misha/luna/scripts/fcmp2", line 452, in value
err>     return value_int(x, scope)
err>   File "/mnt/c/Users/misha/luna/scripts/fcmp2", line 340, in value_int
err>     return 'self.wait(%s).get_int()' % ref1(x['ref'], scope)
err>   File "/mnt/c/Users/misha/luna/scripts/fcmp2", line 463, in ref1
err>     return resolve_id(r[0], scope) \
err>   File "/mnt/c/Users/misha/luna/scripts/fcmp2", line 139, in resolve_id
err>     res=resolve_id(name, scope['parent'])
err>   File "/mnt/c/Users/misha/luna/scripts/fcmp2", line 136, in resolve_id
err>     raise UnresolvedName('Attempting to access basic type as name',
err> __main__.UnresolvedName: ('Attempting to access basic type as name', 'a')
err>
```

## SYN12 - Сигнатура импортируемого атомарного ФК на LuNA отличается от сигнатуры на C++

В старой классификации:
[LUNA01](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#1-%D0%BD%D0%B5%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D0%B8%D0%B5-%D1%82%D0%B8%D0%BF%D0%BE%D0%B2-%D0%B0%D1%80%D0%B3%D1%83%D0%BC%D0%B5%D0%BD%D1%82%D0%BE%D0%B2-luna-%D1%82%D0%B8%D0%BF%D0%B0%D0%BC-c-%D0%BF%D1%80%D0%B8-%D0%B2%D1%8B%D0%B7%D0%BE%D0%B2%D0%B5-%D0%B0%D1%82%D0%BE%D0%BC%D0%B0%D1%80%D0%BD%D0%BE%D0%B3%D0%BE-%D1%84%D0%BA),

Пример:

**C++**
```C++
#include <stdlib.h>
#include <unistd.h>
#include <ucenv.h>

using namespace luna::ucenv;

extern "C" {
void f(OutputDF &x, int value) {
    x = value;
    printf("%d\n", value);
}
}
```

**LuNA**
```
import f(name, real) as f;

sub main() {
    df x, y;
    f(x, 0);
    f(y, 42);
}
```

Вывод:
```
$ luna tmp.fa
0
0
```

# Семантические ошибки

## SEM1 - Использование ФД неправильного типа в качестве аргумента при вызове ФК

В старой классификации: [LUNA04](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#4-%D0%BD%D0%B5%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D0%B8%D0%B5-%D1%82%D0%B8%D0%BF%D0%BE%D0%B2-%D0%B0%D1%80%D0%B3%D1%83%D0%BC%D0%B5%D0%BD%D1%82%D0%BE%D0%B2-luna-%D0%BF%D1%80%D0%B8-%D0%B2%D1%8B%D0%B7%D0%BE%D0%B2%D0%B5-%D0%B0%D1%82%D0%BE%D0%BC%D0%B0%D1%80%D0%BD%D0%BE%D0%B3%D0%BE-%D1%84%D0%BA)

**Обнаруживает RTS (не для всех типов).** 

Пример 1:
```
C++ sub empty() ${{$}}

C++ sub int_set(name x, int v) ${{ x = v; $}}

sub foo(string s) {
    empty();
}

sub main() {
    df s;
    int_set(s, 42);
    foo(s);
}
```
Вывод luna: нет реакции, программа завершается успешно.

Пример 2:
```
C++ sub empty() ${{$}}

C++ sub int_set(name x, int v) ${{ x = v; $}}

sub foo(real s) {
    empty();
}

sub main() {
    df s;
    int_set(s, 42);
    foo(s);
}
```
Вывод luna:
```
luna: fatal error: run-time error: errcode=-6
err> 0 ERROR:  get_real failed for type int ./src/rts/df.cpp:176
err> 0 ABORT 
err> terminate called after throwing an instance of 'RuntimeError'
err>   what():  std::exception
err> [DESKTOP-CPJS18K:89639] *** Process received signal ***
err> [DESKTOP-CPJS18K:89639] Signal: Aborted (6)
err> [DESKTOP-CPJS18K:89639] Signal code:  (-6)
err> [DESKTOP-CPJS18K:89639] [ 0] /lib/x86_64-linux-gnu/libc.so.6(+0x42520)[0x7f28a09f3520]
err> [DESKTOP-CPJS18K:89639] [ 1] /lib/x86_64-linux-gnu/libc.so.6(pthread_kill+0x12c)[0x7f28a0a479fc]      
err> [DESKTOP-CPJS18K:89639] [ 2] /lib/x86_64-linux-gnu/libc.so.6(raise+0x16)[0x7f28a09f3476]
err> [DESKTOP-CPJS18K:89639] [ 3] /lib/x86_64-linux-gnu/libc.so.6(abort+0xd3)[0x7f28a09d97f3]
err> [DESKTOP-CPJS18K:89639] [ 4] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xa2b9e)[0x7f28a0c9eb9e]
err> [DESKTOP-CPJS18K:89639] [ 5] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae20c)[0x7f28a0caa20c]
err> [DESKTOP-CPJS18K:89639] [ 6] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae277)[0x7f28a0caa277]
err> [DESKTOP-CPJS18K:89639] [ 7] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae4d8)[0x7f28a0caa4d8]
err> [DESKTOP-CPJS18K:89639] [ 8] /mnt/c/Users/misha/luna/lib/librts.so(+0x1dbac)[0x7f28a0f9cbac]
err> [DESKTOP-CPJS18K:89639] [ 9] /mnt/c/Users/misha/luna/build/programs/mnt/c/Users/misha/Projects/luna/Extraterrestrial/adapt/errors/demos/new/luna13.fa/libucodes.so(_Z7block_7R2CF+0x7c)[0x7f289d549c7c]
err> [DESKTOP-CPJS18K:89639] [10] /mnt/c/Users/misha/luna/lib/librts.so(+0x43963)[0x7f28a0fc2963]
err> [DESKTOP-CPJS18K:89639] [11] /mnt/c/Users/misha/luna/lib/librts.so(_ZN10ThreadPool7routineEv+0x17d)[0x7f28a0fd28ed]
err> [DESKTOP-CPJS18K:89639] [12] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xdc253)[0x7f28a0cd8253]
err> [DESKTOP-CPJS18K:89639] [13] /lib/x86_64-linux-gnu/libc.so.6(+0x94ac3)[0x7f28a0a45ac3]
err> [DESKTOP-CPJS18K:89639] [14] /lib/x86_64-linux-gnu/libc.so.6(+0x126850)[0x7f28a0ad7850]
err> [DESKTOP-CPJS18K:89639] *** End of error message ***
err>
```

## SEM2 - Повторная инициализация ФД

В старой классификации: [LUNA03](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#3-%D0%BF%D0%BE%D0%B2%D1%82%D0%BE%D1%80%D0%BD%D0%B0%D1%8F-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D1%8F-%D1%84%D0%B4)

**В некоторых случаях обнаруживает RTS.**

Может быть обнаружено во время работы программы, но можно сделать так, что не будет (например, с помощью delete):

```
df x;
init_after_random_delay(x);
init_after_random_delay(x);
print(x) @ { delete x; };
```
Если между выполнением двух `init_after_random_delay(x)` выполнится `print(x) @ { delete x; };`, повторная инициализация не будет обнаружена. 

### SEM2.1 - Повторная инициализация одиночного ФД

Пример:
```
C++ sub int_set(name x, int v) ${{ x = v; $}}

sub main() {
    df x;
    int_set(x, 1);
    int_set(x, 2);
}
```
Вывод luna:
```
luna: fatal error: run-time error: errcode=-6
err> 0 ERROR:  Duplicate id in post: ID<0, 1, 0> ./src/rts/rts.cpp:661
err> 0 ABORT 
err> terminate called after throwing an instance of 'RuntimeError'
err>   what():  std::exception
err> [DESKTOP-CPJS18K:93672] *** Process received signal ***
err> [DESKTOP-CPJS18K:93672] Signal: Aborted (6)
err> [DESKTOP-CPJS18K:93672] Signal code:  (-6)
err> [DESKTOP-CPJS18K:93672] [ 0] /lib/x86_64-linux-gnu/libc.so.6(+0x42520)[0x7fbd61c80520]
err> [DESKTOP-CPJS18K:93672] [ 1] /lib/x86_64-linux-gnu/libc.so.6(pthread_kill+0x12c)[0x7fbd61cd49fc]      
err> [DESKTOP-CPJS18K:93672] [ 2] /lib/x86_64-linux-gnu/libc.so.6(raise+0x16)[0x7fbd61c80476]
err> [DESKTOP-CPJS18K:93672] [ 3] /lib/x86_64-linux-gnu/libc.so.6(abort+0xd3)[0x7fbd61c667f3]
err> [DESKTOP-CPJS18K:93672] [ 4] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xa2b9e)[0x7fbd61f2bb9e]
err> [DESKTOP-CPJS18K:93672] [ 5] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae20c)[0x7fbd61f3720c]
err> [DESKTOP-CPJS18K:93672] [ 6] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae277)[0x7fbd61f37277]
err> [DESKTOP-CPJS18K:93672] [ 7] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae4d8)[0x7fbd61f374d8]
err> [DESKTOP-CPJS18K:93672] [ 8] /mnt/c/Users/misha/luna/lib/librts.so(+0x21010)[0x7fbd6222d010]
err> [DESKTOP-CPJS18K:93672] [ 9] /mnt/c/Users/misha/luna/build/programs/mnt/c/Users/misha/Projects/luna/Extraterrestrial/adapt/errors/demos/new/tmpfile/libucodes.so(_Z7block_8R2CF+0x1ae)[0x7fbd58bb4eae]
err> [DESKTOP-CPJS18K:93672] [10] /mnt/c/Users/misha/luna/lib/librts.so(+0x43963)[0x7fbd6224f963]
err> [DESKTOP-CPJS18K:93672] [11] /mnt/c/Users/misha/luna/lib/librts.so(_ZN10ThreadPool7routineEv+0x17d)[0x7fbd6225f8ed]
err> [DESKTOP-CPJS18K:93672] [12] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xdc253)[0x7fbd61f65253]
err> [DESKTOP-CPJS18K:93672] [13] /lib/x86_64-linux-gnu/libc.so.6(+0x94ac3)[0x7fbd61cd2ac3]
err> [DESKTOP-CPJS18K:93672] [14] /lib/x86_64-linux-gnu/libc.so.6(+0x126850)[0x7fbd61d64850]
err> [DESKTOP-CPJS18K:93672] *** End of error message ***
err>
```

### SEM2.2 - Пересечение диапазонов инициализируемых индексов

В старой классификации: [LUNA35](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#35-%D0%BF%D0%B5%D1%80%D0%B5%D1%81%D0%B5%D1%87%D0%B5%D0%BD%D0%B8%D0%B5-%D0%B4%D0%B8%D0%B0%D0%BF%D0%B0%D0%B7%D0%BE%D0%BD%D0%BE%D0%B2-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B8%D1%80%D1%83%D0%B5%D0%BC%D1%8B%D1%85-%D0%B8%D0%BD%D0%B4%D0%B5%D0%BA%D1%81%D0%BE%D0%B2)

Пример:
```
C++ sub init(name x, int v) ${{ x = v; $}}

sub init_1(name arr, int ind) {     
    init(arr[ind], 0);                     
}

sub init_2(name arr, int ind) {
    init(arr[ind-1], 1);
}

sub main() {                            
    df x;                               
    for i = 0..1250 {
        init_1(x, 2*i);
        init_2(x, 2*i+1);
    }
}
```
Вывод luna:
```
luna: fatal error: run-time error: errcode=-6
err> 0 ERROR:  Duplicate id in post: ID<0, 0, 4> ./src/rts/rts.cpp:661
err> 0 ABORT 
err> terminate called after throwing an instance of 'RuntimeError'
err>   what():  std::exception
err> [DESKTOP-CPJS18K:94936] *** Process received signal ***
err> [DESKTOP-CPJS18K:94936] Signal: Aborted (6)
err> [DESKTOP-CPJS18K:94936] Signal code:  (-6)
err> [DESKTOP-CPJS18K:94936] [ 0] /lib/x86_64-linux-gnu/libc.so.6(+0x42520)[0x7fa88afe3520]
err> [DESKTOP-CPJS18K:94936] [ 1] /lib/x86_64-linux-gnu/libc.so.6(pthread_kill+0x12c)[0x7fa88b0379fc]
err> [DESKTOP-CPJS18K:94936] [ 2] /lib/x86_64-linux-gnu/libc.so.6(raise+0x16)[0x7fa88afe3476]
err> [DESKTOP-CPJS18K:94936] [ 3] /lib/x86_64-linux-gnu/libc.so.6(abort+0xd3)[0x7fa88afc97f3]
err> [DESKTOP-CPJS18K:94936] [ 4] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xa2b9e)[0x7fa88b28eb9e]
err> [DESKTOP-CPJS18K:94936] [ 5] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae20c)[0x7fa88b29a20c]
err> [DESKTOP-CPJS18K:94936] [ 6] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae277)[0x7fa88b29a277]
err> [DESKTOP-CPJS18K:94936] [ 7] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae4d8)[0x7fa88b29a4d8]
err> [DESKTOP-CPJS18K:94936] [ 8] /mnt/c/Users/misha/luna/lib/librts.so(+0x21010)[0x7fa88b590010]
err> [DESKTOP-CPJS18K:94936] [ 9] /mnt/c/Users/misha/luna/build/programs/mnt/c/Users/misha/Projects/luna/Extraterrestrial/adapt/errors/demos/new/tmpfile/libucodes.so(_Z7block_2R2CF+0x1a7)[0x7fa8880179b7]
err> [DESKTOP-CPJS18K:94936] [10] /mnt/c/Users/misha/luna/lib/librts.so(+0x43963)[0x7fa88b5b2963]
err> [DESKTOP-CPJS18K:94936] [11] /mnt/c/Users/misha/luna/lib/librts.so(_ZN10ThreadPool7routineEv+0x17d)[0x7fa88b5c28ed]
err> [DESKTOP-CPJS18K:94936] [12] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xdc253)[0x7fa88b2c8253]
err> [DESKTOP-CPJS18K:94936] [13] /lib/x86_64-linux-gnu/libc.so.6(+0x94ac3)[0x7fa88b035ac3]
err> [DESKTOP-CPJS18K:94936] [14] /lib/x86_64-linux-gnu/libc.so.6(+0x126850)[0x7fa88b0c7850]
err> [DESKTOP-CPJS18K:94936] *** End of error message ***
err>
```

## SEM3 - Попытка использования неинициализированного ФД

**Не обнаруживается LuNA (в текущей версии), приводит к зависанию во время выполнения.**     

### SEM3.1 - Неинициализированный ФД используется вне цикла

Индексы используемого ФД не зависят от счетчиков циклов.

В старой классификации: [LUNA05](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#5-%D0%BF%D0%BE%D0%BF%D1%8B%D1%82%D0%BA%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D0%BD%D0%B5%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%BD%D0%BE%D0%B3%D0%BE-%D1%84%D0%B4)

Пример:
```
C++ sub empty(int x) ${{$}}

sub main() {
    df x;
    empty(x);
}
```
Вывод luna: программа зависает.

### SEM3.2 - Циклическая зависимость по данным

В старой классификации: [LUNA15](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#15-%D1%86%D0%B8%D0%BA%D0%BB%D0%B8%D1%87%D0%B5%D1%81%D0%BA%D0%B0%D1%8F-%D0%B7%D0%B0%D0%B2%D0%B8%D1%81%D0%B8%D0%BC%D0%BE%D1%81%D1%82%D1%8C-%D0%BF%D0%BE-%D0%B4%D0%B0%D0%BD%D0%BD%D1%8B%D0%BC)

Пример:
```
C++ sub int_set(name x, int v) ${{ x = v; $}}

sub main() {
    df x, y;
    int_set(x, y);
    int_set(y, x);
}
```
Вывод luna: программа зависает.

### SEM3.3 - Диапазон используемых индексов не инициализирован полностью

Рассматриваются диапазоны индексов, задаваемые циклами `for`.

В старой классификации: [LUNA18](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#18-%D0%BD%D0%B5%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D0%B8%D0%B5-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86-%D1%86%D0%B8%D0%BA%D0%BB%D0%BE%D0%B2-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B0%D1%86%D0%B8%D0%B8-%D0%B8-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F---%D0%BD%D0%B8%D0%B6%D0%BD%D1%8F%D1%8F-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D0%B0-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D0%BC%D0%BE%D0%B6%D0%B5%D1%82-%D0%B1%D1%8B%D1%82%D1%8C-%D0%BA%D0%B0%D0%BA-%D0%B1%D0%BE%D0%BB%D1%8C%D1%88%D0%B5-%D1%82%D0%B0%D0%BA-%D0%B8-%D0%BC%D0%B5%D0%BD%D1%8C%D1%88%D0%B5-%D0%BD%D0%B8%D0%B6%D0%BD%D0%B5%D0%B9-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D1%8B-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D0%B8),
[LUNA19](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#19-%D0%BD%D0%B5%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D0%B8%D0%B5-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86-%D1%86%D0%B8%D0%BA%D0%BB%D0%BE%D0%B2-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B0%D1%86%D0%B8%D0%B8-%D0%B8-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F---%D0%BD%D0%B8%D0%B6%D0%BD%D1%8F%D1%8F-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D0%B0-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D1%81%D1%82%D1%80%D0%BE%D0%B3%D0%BE-%D0%BC%D0%B5%D0%BD%D1%8C%D1%88%D0%B5-%D0%BD%D0%B8%D0%B6%D0%BD%D0%B5%D0%B9-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D1%8B-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D0%B8),
[LUNA20](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#20-%D0%BD%D0%B5%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D0%B8%D0%B5-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86-%D1%86%D0%B8%D0%BA%D0%BB%D0%BE%D0%B2-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B0%D1%86%D0%B8%D0%B8-%D0%B8-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F---%D0%B2%D0%B5%D1%80%D1%85%D0%BD%D1%8F%D1%8F-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D0%B0-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D0%BC%D0%BE%D0%B6%D0%B5%D1%82-%D0%B1%D1%8B%D1%82%D1%8C-%D0%BA%D0%B0%D0%BA-%D0%BC%D0%B5%D0%BD%D1%8C%D1%88%D0%B5-%D1%82%D0%B0%D0%BA-%D0%B8-%D0%B1%D0%BE%D0%BB%D1%8C%D1%88%D0%B5-%D0%B2%D0%B5%D1%80%D1%85%D0%BD%D0%B5%D0%B9-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D1%8B-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D0%B8),
[LUNA21](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#21-%D0%BD%D0%B5%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D0%B8%D0%B5-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86-%D1%86%D0%B8%D0%BA%D0%BB%D0%BE%D0%B2-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B0%D1%86%D0%B8%D0%B8-%D0%B8-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F---%D0%B2%D0%B5%D1%80%D1%85%D0%BD%D1%8F%D1%8F-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D0%B0-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D1%81%D1%82%D1%80%D0%BE%D0%B3%D0%BE-%D0%B1%D0%BE%D0%BB%D1%8C%D1%88%D0%B5-%D0%B2%D0%B5%D1%80%D1%85%D0%BD%D0%B5%D0%B9-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D1%8B-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D0%B8),
[LUNA22](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#22-%D1%88%D0%B0%D0%B3-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D0%BD%D0%B5-%D0%BA%D1%80%D0%B0%D1%82%D0%B5%D0%BD-%D1%88%D0%B0%D0%B3%D1%83-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D0%B8),
[LUNA37](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#37-%D0%BD%D0%B5%D1%82-%D0%B3%D0%B0%D1%80%D0%B0%D0%BD%D1%82%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%BD%D0%BE%D0%B3%D0%BE-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D0%B8)

Пример 1 - шаг цикла использования не кратен шагу цикла инициализации:
```
C++ sub init(name x, int v) ${{ x = v; $}}
C++ sub print(int x) ${{$}}

sub main() {                            
    df x;                               
    for i = 0..100 {
        init(x[2*i+1], 2 * i + 1);
    }

    for i = 0..66 {
        print(x[3*i+1]);
    }
}
```

Пример 2 - выход за границу цикла инициализации:
```
C++ sub init(name x, int v) ${{ x = v; $}}
C++ sub print(int x) ${{$}}

sub main() {                            
    df x;                               
    for i = 0..100 {
        init(x[i], i);
    }

    for i = 0..101 {
        print(x[i]);
    }
}
```

Пример 3 - нет цикла инициализации:
```
C++ sub init(name x, int v) ${{ x = v; $}}
C++ sub print(int x) ${{$}}

sub main() {                            
    df x;

    for i = 0..101 {
        print(x[i]);
    }
}
```

Реакция RTS зависит от того, будут ли инициализированы необходимые индексированные ФД. 

Пример, где ошибки нет:
```
C++ sub init(name x, int v) ${{ x = v; $}}
C++ sub print(int x) ${{$}}

sub main() {                            
    df x;
    
    init(x[0], 0);
    init(x[5], 5);
    
    for i = 1..4 {
        init(x[i], i);
    }
    
    for i = 6..10 {
        init(x[i], i);
    }
    
    for i = 0..10 {
        print(x[i]);
    }
}
```

### SEM3.4 - Попытка запросить неинициализированный ФД при помощи request

В старой классификации: [LUNA27](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#27-%D0%BF%D0%BE%D0%BF%D1%8B%D1%82%D0%BA%D0%B0-%D0%B7%D0%B0%D0%BF%D1%80%D0%BE%D1%81%D0%B8%D1%82%D1%8C-%D0%BD%D0%B5%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%BD%D1%8B%D0%B9-%D1%84%D0%B4-%D0%BF%D1%80%D0%B8-%D0%BF%D0%BE%D0%BC%D0%BE%D1%89%D0%B8-request)

**Зависание RTS.**

### SEM3.5 - Попытка использования ФД после превышения допустимого числа запросов

В старой классификации: [LUNA28](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#28-%D0%BF%D0%BE%D0%BF%D1%8B%D1%82%D0%BA%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D1%84%D0%B4-%D0%BF%D0%BE%D1%81%D0%BB%D0%B5-%D0%BF%D1%80%D0%B5%D0%B2%D1%8B%D1%88%D0%B5%D0%BD%D0%B8%D1%8F-%D0%B4%D0%BE%D0%BF%D1%83%D1%81%D1%82%D0%B8%D0%BC%D0%BE%D0%B3%D0%BE-%D1%87%D0%B8%D1%81%D0%BB%D0%B0-%D0%B7%D0%B0%D0%BF%D1%80%D0%BE%D1%81%D0%BE%D0%B2)

**Зависание RTS.**

Пример:
```
sub main() {
    df x;
    init(1, x) @ {
        req_count x = 1;
    };
    print(x);
    print(x);
}
```

Реакция: программа зависнет в ожидании инициализации ФД, 
поскольку он был удален сборщиком мусора после первого использования.

### SEM3.6 - Использование ФД после его удаления

В старой классификации: [LUNA29](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#29-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5-%D1%84%D0%B4-%D0%BF%D0%BE%D1%81%D0%BB%D0%B5-%D0%B5%D0%B3%D0%BE-%D1%83%D0%B4%D0%B0%D0%BB%D0%B5%D0%BD%D0%B8%D1%8F-%D0%BF%D1%80%D0%B8-%D0%BF%D0%BE%D0%BC%D0%BE%D1%89%D0%B8-%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D1%83%D1%8E%D1%89%D0%B5%D0%B3%D0%BE-%D0%BE%D0%BF%D0%B5%D1%80%D0%B0%D1%82%D0%BE%D1%80%D0%B0), [LUNA9](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#9-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5-%D1%84%D0%B4-%D0%BF%D0%BE%D1%81%D0%BB%D0%B5-%D0%B5%D0%B3%D0%BE-%D1%83%D0%B4%D0%B0%D0%BB%D0%B5%D0%BD%D0%B8%D1%8F)

**Зависание RTS / Ошибка времени выполнения.**

Пример 1 - оператор `delete`:
```
C++ sub init(name x, int v) ${{ x = v; $}}
C++ sub print(int x) ${{$}}

sub main() {
    df x;
    init(x, 1) @ { delete x; };
    print(x);
}
```

Вывод luna:
```
luna: fatal error: run-time error: errcode=-6
err> 0 ERROR:  Request(s) present at destroy ./src/rts/rts.cpp:708
err> 0 ABORT 
err> terminate called after throwing an instance of 'RuntimeError'
err>   what():  std::exception
err> [DESKTOP-CPJS18K:103293] *** Process received signal ***
err> [DESKTOP-CPJS18K:103293] Signal: Aborted (6)
err> [DESKTOP-CPJS18K:103293] Signal code:  (-6)
err> [DESKTOP-CPJS18K:103293] [ 0] /lib/x86_64-linux-gnu/libc.so.6(+0x42520)[0x7fbb5a8ed520]
err> [DESKTOP-CPJS18K:103293] [ 1] /lib/x86_64-linux-gnu/libc.so.6(pthread_kill+0x12c)[0x7fbb5a9419fc]
err> [DESKTOP-CPJS18K:103293] [ 2] /lib/x86_64-linux-gnu/libc.so.6(raise+0x16)[0x7fbb5a8ed476]
err> [DESKTOP-CPJS18K:103293] [ 3] /lib/x86_64-linux-gnu/libc.so.6(abort+0xd3)[0x7fbb5a8d37f3]
err> [DESKTOP-CPJS18K:103293] [ 4] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xa2b9e)[0x7fbb5ab98b9e]
err> [DESKTOP-CPJS18K:103293] [ 5] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae20c)[0x7fbb5aba420c]
err> [DESKTOP-CPJS18K:103293] [ 6] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae277)[0x7fbb5aba4277]
err> [DESKTOP-CPJS18K:103293] [ 7] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae4d8)[0x7fbb5aba44d8]
err> [DESKTOP-CPJS18K:103293] [ 8] /mnt/c/Users/misha/luna/lib/librts.so(+0x20ae3)[0x7fbb5ae99ae3]
err> [DESKTOP-CPJS18K:103293] [ 9] /mnt/c/Users/misha/luna/build/programs/mnt/c/Users/misha/Projects/luna/Extraterrestrial/adapt/errors/demos/new/tmpfile/libucodes.so(_Z7block_2R2CF+0xd5)[0x7fbb518217a5]
err> [DESKTOP-CPJS18K:103293] [10] /mnt/c/Users/misha/luna/lib/librts.so(+0x43963)[0x7fbb5aebc963]
err> [DESKTOP-CPJS18K:103293] [11] /mnt/c/Users/misha/luna/lib/librts.so(_ZN10ThreadPool7routineEv+0x17d)[0x7fbb5aecc8ed]
err> [DESKTOP-CPJS18K:103293] [12] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xdc253)[0x7fbb5abd2253]
err> [DESKTOP-CPJS18K:103293] [13] /lib/x86_64-linux-gnu/libc.so.6(+0x94ac3)[0x7fbb5a93fac3]
err> [DESKTOP-CPJS18K:103293] [14] /lib/x86_64-linux-gnu/libc.so.6(+0x126850)[0x7fbb5a9d1850]
err> [DESKTOP-CPJS18K:103293] *** End of error message ***
err>
```

Пример 2 - оператор `-->`:
```
C++ sub init(name x, int v) ${{ x = v; $}}
C++ sub print(int x) ${{$}}

sub main() {
    df x;
    init(x, 1) --> (x);
    print(x);
}
```

Вывод luna:
```
luna: fatal error: run-time error: errcode=-6
err> 0 ERROR:  Request(s) present at destroy ./src/rts/rts.cpp:708
err> 0 ABORT 
err> terminate called after throwing an instance of 'RuntimeError'
err>   what():  std::exception
err> [DESKTOP-CPJS18K:103821] *** Process received signal ***
err> [DESKTOP-CPJS18K:103821] Signal: Aborted (6)
err> [DESKTOP-CPJS18K:103821] Signal code:  (-6)
err> [DESKTOP-CPJS18K:103821] [ 0] /lib/x86_64-linux-gnu/libc.so.6(+0x42520)[0x7f996ff94520]
err> [DESKTOP-CPJS18K:103821] [ 1] /lib/x86_64-linux-gnu/libc.so.6(pthread_kill+0x12c)[0x7f996ffe89fc]
err> [DESKTOP-CPJS18K:103821] [ 2] /lib/x86_64-linux-gnu/libc.so.6(raise+0x16)[0x7f996ff94476]
err> [DESKTOP-CPJS18K:103821] [ 3] /lib/x86_64-linux-gnu/libc.so.6(abort+0xd3)[0x7f996ff7a7f3]
err> [DESKTOP-CPJS18K:103821] [ 4] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xa2b9e)[0x7f997023fb9e]
err> [DESKTOP-CPJS18K:103821] [ 5] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae20c)[0x7f997024b20c]
err> [DESKTOP-CPJS18K:103821] [ 6] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae277)[0x7f997024b277]
err> [DESKTOP-CPJS18K:103821] [ 7] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae4d8)[0x7f997024b4d8]
err> [DESKTOP-CPJS18K:103821] [ 8] /mnt/c/Users/misha/luna/lib/librts.so(+0x20ae3)[0x7f9970540ae3]
err> [DESKTOP-CPJS18K:103821] [ 9] /mnt/c/Users/misha/luna/build/programs/mnt/c/Users/misha/Projects/luna/Extraterrestrial/adapt/errors/demos/new/tmpfile/libucodes.so(_Z7block_2R2CF+0xd5)[0x7f996caea7a5]
err> [DESKTOP-CPJS18K:103821] [10] /mnt/c/Users/misha/luna/lib/librts.so(+0x43963)[0x7f9970563963]
err> [DESKTOP-CPJS18K:103821] [11] /mnt/c/Users/misha/luna/lib/librts.so(_ZN10ThreadPool7routineEv+0x17d)[0x7f99705738ed]
err> [DESKTOP-CPJS18K:103821] [12] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xdc253)[0x7f9970279253]
err> [DESKTOP-CPJS18K:103821] [13] /lib/x86_64-linux-gnu/libc.so.6(+0x94ac3)[0x7f996ffe6ac3]
err> [DESKTOP-CPJS18K:103821] [14] /lib/x86_64-linux-gnu/libc.so.6(+0x126850)[0x7f9970078850]
err> [DESKTOP-CPJS18K:103821] *** End of error message ***
err>
```

## SEM4 - Неиспользуемый ФД

В старой классификации: [LUNA10](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#10-%D0%BD%D0%B5%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D1%83%D0%B5%D0%BC%D1%8B%D0%B9-%D1%84%D0%B4)

**Не обнаруживается LuNA.** 

ФД инициализируется, но не используется как входной.

Пример:
```
C++ sub init(name x, int v) ${{ x = v; $}}

sub main() {
    df x;
    init(x, 1);
}
```

## SEM5 - Формула в if/while тождественно истинна/ложна

В старой классификации: [LUNA23](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#23-%D1%84%D0%BE%D1%80%D0%BC%D1%83%D0%BB%D0%B0-%D0%B2-if-%D1%82%D0%BE%D0%B6%D0%B4%D0%B5%D1%81%D1%82%D0%B2%D0%B5%D0%BD%D0%BD%D0%BE-%D0%B8%D1%81%D1%82%D0%B8%D0%BD%D0%BD%D0%B0%D0%BB%D0%BE%D0%B6%D0%BD%D0%B0)

**Не обнаруживается LuNA.** 

Пример:
```
C++ sub real_set(name x, real v) ${{ x = v; $}}

sub main(){
    df a, x, y;
    real_set(x[0], 1.0);
    real_set(y, 2.0);
    if x[0] <= y || x[0] > y {
        real_set(a, 3.0);
    }
}
```

## SEM6 - Формула в if/while истинна/ложна во всех путях выполнения

В старой классификации: [LUNA24](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#24-%D1%84%D0%BE%D1%80%D0%BC%D1%83%D0%BB%D0%B0-%D0%B2-if-%D0%B8%D1%81%D1%82%D0%B8%D0%BD%D0%BD%D0%B0%D0%BB%D0%BE%D0%B6%D0%BD%D0%B0-%D0%B2%D0%BE-%D0%B2%D1%81%D0%B5%D1%85-%D0%BF%D1%83%D1%82%D1%8F%D1%85-%D0%B2%D1%8B%D0%BF%D0%BE%D0%BB%D0%BD%D0%B5%D0%BD%D0%B8%D1%8F)

**Не обнаруживается LuNA.**  

Пример:
```
C++ sub print(int x) ${{$}}

sub print_if_gt_5(int n) {
    if n > 5 {
        print(n);
    }
}

sub main() {
    print_if_gt_5(7); // -> if 7 > 5
    print_if_gt_5(8); // -> if 8 > 5
}
```


## SEM7 - Операторы, возвращающие булево значение, используются в целочисленном контексте

В старой классификации: [LUNA25](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#25-%D0%BE%D0%BF%D0%B5%D1%80%D0%B0%D1%82%D0%BE%D1%80%D1%8B-%D0%B2%D0%BE%D0%B7%D0%B2%D1%80%D0%B0%D1%89%D0%B0%D1%8E%D1%89%D0%B8%D0%B5-%D0%B1%D1%83%D0%BB%D0%B5%D0%B2%D0%BE-%D0%B7%D0%BD%D0%B0%D1%87%D0%B5%D0%BD%D0%B8%D0%B5-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D1%83%D1%8E%D1%82%D1%81%D1%8F-%D0%B2-%D1%86%D0%B5%D0%BB%D0%BE%D1%87%D0%B8%D1%81%D0%BB%D0%B5%D0%BD%D0%BD%D0%BE%D0%BC-%D0%BA%D0%BE%D0%BD%D1%82%D0%B5%D0%BA%D1%81%D1%82%D0%B5)

**Не обнаруживается LuNA.**   

Пример:
```
import c_print(int) as print;
import c_init(name, int) as init;

sub main() {
    df x, N;

    init(x, 42);
    init(N, 10);

    if 0 < x < N {
        print(x);
    }
}
```
Реакция: Программа напечатает 42, хотя интуиция подсказывает, что 42 не меньше 10. Причина в том, что в LuNA не поддерживаются цепочки сравнений, и выражение 0 < x < N будет эквивалентно не 0 < x && x < N, а (0 < x) < N.


## SEM8 - Бесконечная рекурсия

В старой классификации: [LUNA34](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#34-%D0%B1%D0%B5%D0%B7%D1%83%D1%81%D0%BB%D0%BE%D0%B2%D0%BD%D0%B0%D1%8F-%D1%80%D0%B5%D0%BA%D1%83%D1%80%D1%81%D0%B8%D1%8F)

**Зависание RTS.**

Рассматриваются только рекурсивные вызовы вида:
```
sub f(...) {
    f(...);
}
```

В примере, приведенном выше, RTS просто зависнет:

    1. Создается ФВ f
    2. Выполняется ФВ f
    2.1. Выполняется f(...)
    2.2. Создается ФВ f (внутренний)
    2.3. Удаляется ФВ f (внешний)
    3. Возврат к п. 2

Таким образом, бесконечная рекурсия работает за O(1) по памяти. Эксперимент показал, что использование памяти процессом luna за 6+ минут не изменилось.

Может быть вызвано как отсутствием условных операторов между вызовами одного структурированного ФК, так и безусловным выполнением условных операторов, или же их выполнением во всех потоках выполнения.

## SEM9 - Использование оператора информационной зависимости для структурированного ФК

В старой классификации: [LUNA30](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#30-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5-%D0%BE%D0%BF%D0%B5%D1%80%D0%B0%D1%82%D0%BE%D1%80%D0%B0-%D0%B8%D0%BD%D1%84%D0%BE%D1%80%D0%BC%D0%B0%D1%86%D0%B8%D0%BE%D0%BD%D0%BD%D0%BE%D0%B9-%D0%B7%D0%B0%D0%B2%D0%B8%D1%81%D0%B8%D0%BC%D0%BE%D1%81%D1%82%D0%B8-%D0%B4%D0%BB%D1%8F-%D1%81%D1%82%D1%80%D1%83%D0%BA%D1%82%D1%83%D1%80%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%BD%D0%BE%D0%B3%D0%BE-%D1%84%D0%BA)

**Зависание RTS.**

Пример:
```
C++ sub init(name x, int v) ${{ x = v; $}}
C++ sub print(int x) ${{$}}

sub foo(name x, int v) {
    init(x, v);
}

sub main() {
    df x, y;
    foo(x, 1) >> (y);
    if y {
        print(x);
    }
}
```

## SEM10 - Не передать значение для nfparam

В старой классификации: [LUNA31](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#31-%D0%BD%D0%B5-%D0%BF%D0%B5%D1%80%D0%B5%D0%B4%D0%B0%D1%82%D1%8C-%D0%B7%D0%BD%D0%B0%D1%87%D0%B5%D0%BD%D0%B8%D0%B5-%D0%B4%D0%BB%D1%8F-nfparam)

**Обнаруживает RTS.**  

Пример:
```
C++ sub init(name x, int v) ${{ x = v; $}}
C++ sub print(int x) ${{$}}

sub foo(name x, int v) {
    init(x, v);
} @ {
    nfparam n;
    locator_cyclic x => n;
}

sub main() {
    df x, y;
    foo(x, 1);
}
```
Вывод luna:
```
luna: fatal error: run-time error: errcode=-6
err> 0 ERROR:  get_int failed for type (unset) ./src/rts/df.cpp:165
err> 0 ABORT 
err> terminate called after throwing an instance of 'RuntimeError'
err>   what():  std::exception
err> [DESKTOP-CPJS18K:107485] *** Process received signal ***
err> [DESKTOP-CPJS18K:107485] Signal: Aborted (6)
err> [DESKTOP-CPJS18K:107485] Signal code:  (-6)
err> [DESKTOP-CPJS18K:107485] [ 0] /lib/x86_64-linux-gnu/libc.so.6(+0x42520)[0x7f1473b9f520]
err> [DESKTOP-CPJS18K:107485] [ 1] /lib/x86_64-linux-gnu/libc.so.6(pthread_kill+0x12c)[0x7f1473bf39fc]
err> [DESKTOP-CPJS18K:107485] [ 2] /lib/x86_64-linux-gnu/libc.so.6(raise+0x16)[0x7f1473b9f476]
err> [DESKTOP-CPJS18K:107485] [ 3] /lib/x86_64-linux-gnu/libc.so.6(abort+0xd3)[0x7f1473b857f3]
err> [DESKTOP-CPJS18K:107485] [ 4] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xa2b9e)[0x7f1473e4ab9e]
err> [DESKTOP-CPJS18K:107485] [ 5] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae20c)[0x7f1473e5620c]
err> [DESKTOP-CPJS18K:107485] [ 6] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae277)[0x7f1473e56277]
err> [DESKTOP-CPJS18K:107485] [ 7] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae4d8)[0x7f1473e564d8]
err> [DESKTOP-CPJS18K:107485] [ 8] /mnt/c/Users/misha/luna/lib/librts.so(+0x1db48)[0x7f1474148b48]
err> [DESKTOP-CPJS18K:107485] [ 9] /mnt/c/Users/misha/luna/build/programs/mnt/c/Users/misha/Projects/luna/Extraterrestrial/adapt/errors/demos/new/tmpfile/libucodes.so(_Z7block_1R2CF+0x94)[0x7f14708a7b94]
err> [DESKTOP-CPJS18K:107485] [10] /mnt/c/Users/misha/luna/lib/librts.so(+0x43963)[0x7f147416e963]
err> [DESKTOP-CPJS18K:107485] [11] /mnt/c/Users/misha/luna/lib/librts.so(_ZN10ThreadPool7routineEv+0x17d)[0x7f147417e8ed]
err> [DESKTOP-CPJS18K:107485] [12] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xdc253)[0x7f1473e84253]
err> [DESKTOP-CPJS18K:107485] [13] /lib/x86_64-linux-gnu/libc.so.6(+0x94ac3)[0x7f1473bf1ac3]
err> [DESKTOP-CPJS18K:107485] [14] /lib/x86_64-linux-gnu/libc.so.6(+0x126850)[0x7f1473c83850]
err> [DESKTOP-CPJS18K:107485] *** End of error message ***
err>
```

## SEM11 - Попытка запросить ФД из узла, где его нет

В старой классификации: [LUNA32](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#32-%D0%BF%D0%BE%D0%BF%D1%8B%D1%82%D0%BA%D0%B0-%D0%B7%D0%B0%D0%BF%D1%80%D0%BE%D1%81%D0%B8%D1%82%D1%8C-%D1%84%D0%B4-%D0%B8%D0%B7-%D1%83%D0%B7%D0%BB%D0%B0-%D0%B3%D0%B4%D0%B5-%D0%B5%D0%B3%D0%BE-%D0%BD%D0%B5%D1%82),
[LUNA33](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#33-%D0%BD%D0%B5%D0%BF%D1%80%D0%B0%D0%B2%D0%B8%D0%BB%D1%8C%D0%BD%D1%8B%D0%B9-%D0%BF%D0%B0%D1%80%D0%B0%D0%BC%D0%B5%D1%82%D1%80-%D0%B4%D0%BB%D1%8F-nfparam)

**Зависание RTS.**

Ошибка характерна только для приложений с числом узлов больше 1. Стоит отметить, что значение ФД будет корректно установлено, в чём можно убедиться, выполнив ФВ, где значение номера узла будет верным.

Пример:
```
C++ sub init(name x, int v) ${{ x = v; $}}
C++ sub print(int x) ${{ printf("%d\n", x); $}}

sub main() {
    df x;
    init(x, 1) @ { locator_cyclic x => 1; };
    print(x) @ { locator_cyclic x => 0; };
}
```
Вывод luna: при запуске 2 или более MPI-процессов программа зависнет.
