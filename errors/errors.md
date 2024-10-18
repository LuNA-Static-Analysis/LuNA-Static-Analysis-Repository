# Ошибки в LuNA-программах

Для удобства далее в понятие *ошибка* помимо ошибок в традиционном смысле (ошибки компиляции, ошибки времени исполнения и т. п.) включаются также потенциальные недочеты, не приводящие к ошибкам компиляции или некорректному поведению при запуске, обнаружение которых тем не менее может представлять интерес для программиста.

Ошибки разделяются на *семантические* и *синтаксические*.

*Семантические* ошибки влияют (могут влиять) на поведение запущенной программы.

*Синтаксические* ошибки имеют место в том случае, когда текст программы:
 - не соответствует формальной грамматике языка;
 - содержит фрагменты, смысл которых может не соответствовать ожиданиям программиста;
 - содержит фрагменты, перпятствующие компиляции программы по иным причинам.

# Синтаксические ошибки 

## 1. Несоответствие типов аргументов LuNA типам C++ при вызове атомарного ФК (типы аргументов известны на этапе компиляции)

**Обнаруживает компилятор.**

В старой классификации:
[LUNA01](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#1-%D0%BD%D0%B5%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D0%B8%D0%B5-%D1%82%D0%B8%D0%BF%D0%BE%D0%B2-%D0%B0%D1%80%D0%B3%D1%83%D0%BC%D0%B5%D0%BD%D1%82%D0%BE%D0%B2-luna-%D1%82%D0%B8%D0%BF%D0%B0%D0%BC-c-%D0%BF%D1%80%D0%B8-%D0%B2%D1%8B%D0%B7%D0%BE%D0%B2%D0%B5-%D0%B0%D1%82%D0%BE%D0%BC%D0%B0%D1%80%D0%BD%D0%BE%D0%B3%D0%BE-%D1%84%D0%BA),
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

## 2. Использование необъявленного ФК

[В старой классификации](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#2-%D0%BD%D0%B5%D1%81%D1%83%D1%89%D0%B5%D1%81%D1%82%D0%B2%D1%83%D1%8E%D1%89%D0%B0%D1%8F-luna-%D0%BF%D0%BE%D0%B4%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D0%BC%D0%B0)

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

## 3. Несовпадение количества аргументов при объявлении ФК и его вызове

[В старой классификации](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#6-%D0%BD%D0%B5%D1%81%D0%BE%D0%B2%D0%BF%D0%B0%D0%B4%D0%B5%D0%BD%D0%B8%D0%B5-%D0%BA%D0%BE%D0%BB%D0%B8%D1%87%D0%B5%D1%81%D1%82%D0%B2%D0%B0-%D0%B0%D1%80%D0%B3%D1%83%D0%BC%D0%B5%D0%BD%D1%82%D0%BE%D0%B2-%D0%BF%D1%80%D0%B8-%D0%BE%D0%B1%D1%8A%D1%8F%D0%B2%D0%BB%D0%B5%D0%BD%D0%B8%D0%B8-%D1%84%D0%BA-%D0%B8-%D0%B5%D0%B3%D0%BE-%D0%B2%D1%8B%D0%B7%D0%BE%D0%B2%D0%B5)

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

## 4. Два или более объявлений ФД в блоке

[В старой классификации](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#7-%D0%B4%D0%B2%D0%B0-%D0%B8%D0%BB%D0%B8-%D0%B1%D0%BE%D0%BB%D0%B5%D0%B5-%D0%BE%D0%B1%D1%8A%D1%8F%D0%B2%D0%BB%D0%B5%D0%BD%D0%B8%D0%B9-%D1%84%D0%B4-%D0%B2-%D0%BF%D0%BE%D0%B4%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D0%BC%D0%B5)

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

## 5. Имя встречается только при объявлении 

**Не обнаруживается LuNA.**

Некоторый идентификатор (имя) был объявлен, но больше не фигурирует в коде.

### 5.1 Макрос     

Пример:
```
C++ sub empty() ${{$}}

#define M(X) $X

sub main() {
    empty();
}

```

### 5.2 Имя, определенное в операторе let     

Пример:
```
C++ sub empty() ${{$}}

sub main() {
    let x = 42 {
        empty();
    }
}
```

### 5.3 Базовое имя ФД, определенное в операторе df     

Пример:
```
C++ sub empty() ${{$}}

sub main() {
    df x;
    empty();
}
```

### 5.4 Атомарный ФК     

Пример:
```
C++ sub empty() ${{$}}

C++ sub sum(int a, int b, name c) ${{ c = a + b; $}}

sub main() {
    empty();
}
```

### 5.5 Структурированный ФК     

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

### 5.6 Имя параметра структурированного ФК     

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

### 5.7 Счетчик оператора for     

Пример:
```
C++ sub empty() ${{$}}

sub main() {
    for i = 1..10 {
        empty();
    }
}
```

### 5.8 Счетчик оператора while     

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

## 6. Объявление нескольких ФК с одним и тем же именем

В старой классификации: [LUNA11](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#11-%D0%B8%D0%BC%D0%BF%D0%BE%D1%80%D1%82-%D0%BD%D0%B5%D1%81%D0%BA%D0%BE%D0%BB%D1%8C%D0%BA%D0%B8%D1%85-%D1%80%D0%B0%D0%B7%D0%BD%D1%8B%D1%85-%D1%84%D0%BA-%D0%BF%D0%BE%D0%B4-%D0%BE%D0%B4%D0%BD%D0%B8%D0%BC-%D0%B0%D0%BB%D0%B8%D0%B0%D1%81%D0%BE%D0%BC), [LUNA16](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#16-%D0%BF%D0%BE%D0%B2%D1%82%D0%BE%D1%80%D0%BD%D0%BE%D0%B5-%D0%BE%D0%BF%D1%80%D0%B5%D0%B4%D0%B5%D0%BB%D0%B5%D0%BD%D0%B8%D0%B5-%D1%84%D0%BA)

**Не обнаруживается LuNA.**     

### 6.1 Импортирование нескольких ФК под одним именем

Пример:
```
import printa(real) as print;
import printb(real) as print;

sub main(){
	print(1.0);
}
```
Реакция: программа выполнится, вызовется **printb**.

### 6.2 Объявление нескольких структурированных ФК с одним именем

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

## 7. Нет main

[В старой классификации](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#12-%D0%BD%D0%B5%D1%82-main)

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

## 8. Повторное объявление имени в операторе df

[В старой классификации](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#13-df-%D1%81-%D0%BE%D0%B4%D0%B8%D0%BD%D0%B0%D0%BA%D0%BE%D0%B2%D1%8B%D0%BC%D0%B8-%D0%BD%D0%B0%D0%B7%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F%D0%BC%D0%B8-%D0%B2-%D0%BE%D0%B4%D0%BD%D0%BE%D0%B9-%D0%BE%D0%B1%D0%BB%D0%B0%D1%81%D1%82%D0%B8-%D0%B2%D0%B8%D0%B4%D0%B8%D0%BC%D0%BE%D1%81%D1%82%D0%B8)

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

## 9. Попытка использования необъявленного идентификатора

[В старой классификации](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#14-%D0%BF%D0%BE%D0%BF%D1%8B%D1%82%D0%BA%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D0%BD%D0%B5%D0%BE%D0%B1%D1%8A%D1%8F%D0%B2%D0%BB%D0%B5%D0%BD%D0%BD%D0%BE%D0%B3%D0%BE-%D1%84%D0%B4)

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

## 10. Импортирование несуществующей C/C++ функции

[В старой классификации](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#17-%D0%B8%D0%BC%D0%BF%D0%BE%D1%80%D1%82-%D0%BD%D0%B5%D1%81%D1%83%D1%89%D0%B5%D1%81%D1%82%D0%B2%D1%83%D1%8E%D1%89%D0%B5%D0%B9-cc-%D1%84%D1%83%D0%BD%D0%BA%D1%86%D0%B8%D0%B8)

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

## 11. Операторы, возвращающие булево значение, используются в целочисленном контексте

[В старой классификации](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#25-%D0%BE%D0%BF%D0%B5%D1%80%D0%B0%D1%82%D0%BE%D1%80%D1%8B-%D0%B2%D0%BE%D0%B7%D0%B2%D1%80%D0%B0%D1%89%D0%B0%D1%8E%D1%89%D0%B8%D0%B5-%D0%B1%D1%83%D0%BB%D0%B5%D0%B2%D0%BE-%D0%B7%D0%BD%D0%B0%D1%87%D0%B5%D0%BD%D0%B8%D0%B5-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D1%83%D1%8E%D1%82%D1%81%D1%8F-%D0%B2-%D1%86%D0%B5%D0%BB%D0%BE%D1%87%D0%B8%D1%81%D0%BB%D0%B5%D0%BD%D0%BD%D0%BE%D0%BC-%D0%BA%D0%BE%D0%BD%D1%82%D0%B5%D0%BA%D1%81%D1%82%D0%B5)

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

## 12. Попытка индексации выражения, не являющегося именем

[В старой классификации](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#36-%D0%BF%D0%BE%D0%BF%D1%8B%D1%82%D0%BA%D0%B0-%D0%B8%D0%BD%D0%B4%D0%B5%D0%BA%D1%81%D0%B0%D1%86%D0%B8%D0%B8-%D0%BE%D0%B1%D1%8A%D0%B5%D0%BA%D1%82%D0%B0-%D0%BD%D0%B5-%D1%8F%D0%B2%D0%BB%D1%8F%D1%8E%D1%89%D0%B5%D0%B3%D0%BE%D1%81%D1%8F-%D1%84%D0%B4)

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

# Семантические ошибки

## 13. Использование ФД неправильного типа в качестве аргумента при вызове ФК - **обнаруживает RTS**

[В старой классификации]()

**Обнаруживает компилятор.**     

Пример:
```

```
Вывод luna:
```

```

В старой базе:

[(1) Несоответствие типов аргументов LuNA типам C++ при вызове атомарного ФК (типы аргуаментов известны)](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#1-%D0%BD%D0%B5%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D0%B8%D0%B5-%D1%82%D0%B8%D0%BF%D0%BE%D0%B2-%D0%B0%D1%80%D0%B3%D1%83%D0%BC%D0%B5%D0%BD%D1%82%D0%BE%D0%B2-luna-%D1%82%D0%B8%D0%BF%D0%B0%D0%BC-c-%D0%BF%D1%80%D0%B8-%D0%B2%D1%8B%D0%B7%D0%BE%D0%B2%D0%B5-%D0%B0%D1%82%D0%BE%D0%BC%D0%B0%D1%80%D0%BD%D0%BE%D0%B3%D0%BE-%D1%84%D0%BA)

[(4) Несоответствие типов аргументов LuNA при вызове атомарного ФК](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#4-%D0%BD%D0%B5%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D0%B8%D0%B5-%D1%82%D0%B8%D0%BF%D0%BE%D0%B2-%D0%B0%D1%80%D0%B3%D1%83%D0%BC%D0%B5%D0%BD%D1%82%D0%BE%D0%B2-luna-%D0%BF%D1%80%D0%B8-%D0%B2%D1%8B%D0%B7%D0%BE%D0%B2%D0%B5-%D0%B0%D1%82%D0%BE%D0%BC%D0%B0%D1%80%D0%BD%D0%BE%D0%B3%D0%BE-%D1%84%D0%BA)

Примеры программ:

[1.2-err.fa](errors/demos/1.2-err.fa) - ошибка в рантайме

    luna: fatal error: run-time error: errcode=-6
    err> 0 ERROR:  get_int failed for type real ./src/rts/df.cpp:165
    err> 0 ABORT 
    err> terminate called after throwing an instance of 'RuntimeError'
    err>   what():  std::exception
    err> [DESKTOP-CPJS18K:129901] *** Process received signal ***
    err> [DESKTOP-CPJS18K:129901] Signal: Aborted (6)
    err> [DESKTOP-CPJS18K:129901] Signal code:  (-6)
    err> [DESKTOP-CPJS18K:129901] [ 0] /lib/x86_64-linux-gnu/libc.so.6(+0x42520)[0x7f8dcfbf4520]
    err> [DESKTOP-CPJS18K:129901] [ 1] /lib/x86_64-linux-gnu/libc.so.6(pthread_kill+0x12c)[0x7f8dcfc489fc]
    err> [DESKTOP-CPJS18K:129901] [ 2] /lib/x86_64-linux-gnu/libc.so.6(raise+0x16)[0x7f8dcfbf4476]
    err> [DESKTOP-CPJS18K:129901] [ 3] /lib/x86_64-linux-gnu/libc.so.6(abort+0xd3)[0x7f8dcfbda7f3]
    err> [DESKTOP-CPJS18K:129901] [ 4] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xa2b9e)[0x7f8dcfe9fb9e]
    err> [DESKTOP-CPJS18K:129901] [ 5] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae20c)[0x7f8dcfeab20c]
    err> [DESKTOP-CPJS18K:129901] [ 6] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae277)[0x7f8dcfeab277]
    err> [DESKTOP-CPJS18K:129901] [ 7] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xae4d8)[0x7f8dcfeab4d8]
    err> [DESKTOP-CPJS18K:129901] [ 8] /home/reworu/luna/lib/librts.so(+0x1db48)[0x7f8dd019db48]
    err> [DESKTOP-CPJS18K:129901] [ 9] /home/reworu/luna/build/programs/mnt/c/Users/misha/Projects/luna/Extraterrestrial/adapt/errors/demos/1.2-err.fa/libucodes.so(_Z7block_5R2CF+0x3e)[0x7f8dcc8fc89e]
    err> [DESKTOP-CPJS18K:129901] [10] /home/reworu/luna/lib/librts.so(+0x43963)[0x7f8dd01c3963]
    err> [DESKTOP-CPJS18K:129901] [11] /home/reworu/luna/lib/librts.so(_ZN10ThreadPool7routineEv+0x17d)[0x7f8dd01d38ed]
    err> [DESKTOP-CPJS18K:129901] [12] /lib/x86_64-linux-gnu/libstdc++.so.6(+0xdc253)[0x7f8dcfed9253]
    err> [DESKTOP-CPJS18K:129901] [13] /lib/x86_64-linux-gnu/libc.so.6(+0x94ac3)[0x7f8dcfc46ac3]
    err> [DESKTOP-CPJS18K:129901] [14] /lib/x86_64-linux-gnu/libc.so.6(+0x126850)[0x7f8dcfcd8850]
    err> [DESKTOP-CPJS18K:129901] *** End of error message ***
    err>

## 14. [(3) Повторная инициализация ФД](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#3-%D0%BF%D0%BE%D0%B2%D1%82%D0%BE%D1%80%D0%BD%D0%B0%D1%8F-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D1%8F-%D1%84%D0%B4) - **В некоторых случаях обнаруживает RTS**

Может быть обнаружено в рантайме, но можно сделать так, что не будет (например, с помощью delete):

```
df x;
init_after_random_delay(x);
init_after_random_delay(x);
print(x) @ { delete x; };
```
Если между выполнением двух `init_after_random_delay(x)` выполнится `print(x) @ { delete x; };`, повторная инициализация не будет обнаружена. 

### 14.1. [(35) Пересечение диапазонов инициализируемых индексов](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#35-%D0%BF%D0%B5%D1%80%D0%B5%D1%81%D0%B5%D1%87%D0%B5%D0%BD%D0%B8%D0%B5-%D0%B4%D0%B8%D0%B0%D0%BF%D0%B0%D0%B7%D0%BE%D0%BD%D0%BE%D0%B2-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B8%D1%80%D1%83%D0%B5%D0%BC%D1%8B%D1%85-%D0%B8%D0%BD%D0%B4%D0%B5%D0%BA%D1%81%D0%BE%D0%B2)

[В старой классификации]()

**Обнаруживает компилятор.**     

Пример:
```

```
Вывод luna:
```

```

## 15. [(5) Попытка использования неинициализированного ФД](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#5-%D0%BF%D0%BE%D0%BF%D1%8B%D1%82%D0%BA%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D0%BD%D0%B5%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%BD%D0%BE%D0%B3%D0%BE-%D1%84%D0%B4)

[В старой классификации]()

**Обнаруживает компилятор.**     

Пример:
```

```
Вывод luna:
```

```

### 15.1. [(15) Циклическая зависимость по данным](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#15-%D1%86%D0%B8%D0%BA%D0%BB%D0%B8%D1%87%D0%B5%D1%81%D0%BA%D0%B0%D1%8F-%D0%B7%D0%B0%D0%B2%D0%B8%D1%81%D0%B8%D0%BC%D0%BE%D1%81%D1%82%D1%8C-%D0%BF%D0%BE-%D0%B4%D0%B0%D0%BD%D0%BD%D1%8B%D0%BC) - **зависание RTS**

### 15.2. [(18) Несоответствие границ циклов инициалиации и использования - Нижняя граница цикла использования может быть как больше, так и меньше нижней границы цикла инициализации](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#18-%D0%BD%D0%B5%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D0%B8%D0%B5-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86-%D1%86%D0%B8%D0%BA%D0%BB%D0%BE%D0%B2-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B0%D1%86%D0%B8%D0%B8-%D0%B8-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F---%D0%BD%D0%B8%D0%B6%D0%BD%D1%8F%D1%8F-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D0%B0-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D0%BC%D0%BE%D0%B6%D0%B5%D1%82-%D0%B1%D1%8B%D1%82%D1%8C-%D0%BA%D0%B0%D0%BA-%D0%B1%D0%BE%D0%BB%D1%8C%D1%88%D0%B5-%D1%82%D0%B0%D0%BA-%D0%B8-%D0%BC%D0%B5%D0%BD%D1%8C%D1%88%D0%B5-%D0%BD%D0%B8%D0%B6%D0%BD%D0%B5%D0%B9-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D1%8B-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D0%B8) - **возможно зависание RTS**

### 15.3. [(19) Несоответствие границ циклов инициалиации и использования - Нижняя граница цикла использования строго меньше нижней границы цикла инициализации](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#19-%D0%BD%D0%B5%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D0%B8%D0%B5-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86-%D1%86%D0%B8%D0%BA%D0%BB%D0%BE%D0%B2-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B0%D1%86%D0%B8%D0%B8-%D0%B8-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F---%D0%BD%D0%B8%D0%B6%D0%BD%D1%8F%D1%8F-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D0%B0-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D1%81%D1%82%D1%80%D0%BE%D0%B3%D0%BE-%D0%BC%D0%B5%D0%BD%D1%8C%D1%88%D0%B5-%D0%BD%D0%B8%D0%B6%D0%BD%D0%B5%D0%B9-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D1%8B-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D0%B8) - **зависание RTS**

### 15.4. [(20) Несоответствие границ циклов инициалиации и использования - Верхняя граница цикла использования может быть как меньше, так и больше верхней границы цикла инициализации](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#20-%D0%BD%D0%B5%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D0%B8%D0%B5-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86-%D1%86%D0%B8%D0%BA%D0%BB%D0%BE%D0%B2-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B0%D1%86%D0%B8%D0%B8-%D0%B8-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F---%D0%B2%D0%B5%D1%80%D1%85%D0%BD%D1%8F%D1%8F-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D0%B0-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D0%BC%D0%BE%D0%B6%D0%B5%D1%82-%D0%B1%D1%8B%D1%82%D1%8C-%D0%BA%D0%B0%D0%BA-%D0%BC%D0%B5%D0%BD%D1%8C%D1%88%D0%B5-%D1%82%D0%B0%D0%BA-%D0%B8-%D0%B1%D0%BE%D0%BB%D1%8C%D1%88%D0%B5-%D0%B2%D0%B5%D1%80%D1%85%D0%BD%D0%B5%D0%B9-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D1%8B-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D0%B8) - **возможно зависание RTS**

### 15.5. [(21) Несоответствие границ циклов инициалиации и использования - Верхняя граница цикла использования строго больше верхней границы цикла инициализации](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#21-%D0%BD%D0%B5%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D0%B8%D0%B5-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86-%D1%86%D0%B8%D0%BA%D0%BB%D0%BE%D0%B2-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B0%D1%86%D0%B8%D0%B8-%D0%B8-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F---%D0%B2%D0%B5%D1%80%D1%85%D0%BD%D1%8F%D1%8F-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D0%B0-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D1%81%D1%82%D1%80%D0%BE%D0%B3%D0%BE-%D0%B1%D0%BE%D0%BB%D1%8C%D1%88%D0%B5-%D0%B2%D0%B5%D1%80%D1%85%D0%BD%D0%B5%D0%B9-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D1%8B-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D0%B8) - **зависание RTS**

### 15.6. [(22) Шаг цикла использования не кратен шагу цикла инициализации](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#22-%D1%88%D0%B0%D0%B3-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D0%BD%D0%B5-%D0%BA%D1%80%D0%B0%D1%82%D0%B5%D0%BD-%D1%88%D0%B0%D0%B3%D1%83-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D0%B8) - **зависание RTS**

### 15.7. [(37) Нет (гарантированного) цикла инициализации](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#37-%D0%BD%D0%B5%D1%82-%D0%B3%D0%B0%D1%80%D0%B0%D0%BD%D1%82%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%BD%D0%BE%D0%B3%D0%BE-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D0%B8) - **возможно зависание RTS**

Реакция RTS зависит от того, будут ли инициализированы необходимые индексированные ФД. 

### 15.8. [(27) Попытка запросить неинициализированный ФД при помощи request](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#27-%D0%BF%D0%BE%D0%BF%D1%8B%D1%82%D0%BA%D0%B0-%D0%B7%D0%B0%D0%BF%D1%80%D0%BE%D1%81%D0%B8%D1%82%D1%8C-%D0%BD%D0%B5%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%BD%D1%8B%D0%B9-%D1%84%D0%B4-%D0%BF%D1%80%D0%B8-%D0%BF%D0%BE%D0%BC%D0%BE%D1%89%D0%B8-request) - **зависание RTS**

### 15.9. [(28) Попытка использования ФД после превышения допустимого числа запросов](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#28-%D0%BF%D0%BE%D0%BF%D1%8B%D1%82%D0%BA%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D1%84%D0%B4-%D0%BF%D0%BE%D1%81%D0%BB%D0%B5-%D0%BF%D1%80%D0%B5%D0%B2%D1%8B%D1%88%D0%B5%D0%BD%D0%B8%D1%8F-%D0%B4%D0%BE%D0%BF%D1%83%D1%81%D1%82%D0%B8%D0%BC%D0%BE%D0%B3%D0%BE-%D1%87%D0%B8%D1%81%D0%BB%D0%B0-%D0%B7%D0%B0%D0%BF%D1%80%D0%BE%D1%81%D0%BE%D0%B2) - **зависание RTS**

### 15.10. [(29) Использование ФД после его удаления при помощи соответствующего оператора](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#29-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5-%D1%84%D0%B4-%D0%BF%D0%BE%D1%81%D0%BB%D0%B5-%D0%B5%D0%B3%D0%BE-%D1%83%D0%B4%D0%B0%D0%BB%D0%B5%D0%BD%D0%B8%D1%8F-%D0%BF%D1%80%D0%B8-%D0%BF%D0%BE%D0%BC%D0%BE%D1%89%D0%B8-%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D1%83%D1%8E%D1%89%D0%B5%D0%B3%D0%BE-%D0%BE%D0%BF%D0%B5%D1%80%D0%B0%D1%82%D0%BE%D1%80%D0%B0) - **зависание RTS**

## 16. [(9) Использование ФД после его удаления](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#9-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5-%D1%84%D0%B4-%D0%BF%D0%BE%D1%81%D0%BB%D0%B5-%D0%B5%D0%B3%D0%BE-%D1%83%D0%B4%D0%B0%D0%BB%D0%B5%D0%BD%D0%B8%D1%8F) - **?**

[В старой классификации]()

**Обнаруживает компилятор.**     

Пример:
```

```
Вывод luna:
```

```

## 17. [(10) Неиспользуемый ФД](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#10-%D0%BD%D0%B5%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D1%83%D0%B5%D0%BC%D1%8B%D0%B9-%D1%84%D0%B4) - **Не обнаруживается LuNA**

ФД инициализируется, но не используется как входной

[8-ok-initialized-never-consumed.fa](errors/demos/8.3-ok-initialized-never-consumed.fa) - ошибки нет


[В старой классификации]()

**Обнаруживает компилятор.**     

Пример:
```

```
Вывод luna:
```

```


## 18. [(23) Формула в if тождественно истинна/ложна](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#23-%D1%84%D0%BE%D1%80%D0%BC%D1%83%D0%BB%D0%B0-%D0%B2-if-%D1%82%D0%BE%D0%B6%D0%B4%D0%B5%D1%81%D1%82%D0%B2%D0%B5%D0%BD%D0%BD%D0%BE-%D0%B8%D1%81%D1%82%D0%B8%D0%BD%D0%BD%D0%B0%D0%BB%D0%BE%D0%B6%D0%BD%D0%B0) - **Не обнаруживается LuNA**

[В старой классификации]()

**Обнаруживает компилятор.**     

Пример:
```

```
Вывод luna:
```

```

## 19. [(24) Формула в if истинна/ложна во всех путях выполнения](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#24-%D1%84%D0%BE%D1%80%D0%BC%D1%83%D0%BB%D0%B0-%D0%B2-if-%D0%B8%D1%81%D1%82%D0%B8%D0%BD%D0%BD%D0%B0%D0%BB%D0%BE%D0%B6%D0%BD%D0%B0-%D0%B2%D0%BE-%D0%B2%D1%81%D0%B5%D1%85-%D0%BF%D1%83%D1%82%D1%8F%D1%85-%D0%B2%D1%8B%D0%BF%D0%BE%D0%BB%D0%BD%D0%B5%D0%BD%D0%B8%D1%8F) - **Не обнаруживается LuNA**

[В старой классификации]()

**Обнаруживает компилятор.**     

Пример:
```

```
Вывод luna:
```

```

## 20. [(34) Безусловная рекурсия](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#34-%D0%B1%D0%B5%D0%B7%D1%83%D1%81%D0%BB%D0%BE%D0%B2%D0%BD%D0%B0%D1%8F-%D1%80%D0%B5%D0%BA%D1%83%D1%80%D1%81%D0%B8%D1%8F) - **зависание RTS**

Рассматриваются только рекурсивные вызовы вида:
```
sub f(...) {
    f(...);
}
```

В примере, приведенном выше, RTS просто завсинет:

    1. Создается ФВ f
    2. Выполняется ФВ f
    2.1. Выполняется f(...)
    2.2. Создается ФВ f (внутренний)
    2.3. Удаляется ФВ f (внешний)
    3. Возврат к п. 2

Таким образом, бесконенчная рекурсия работает за O(1) по памяти. Эксперимент показал, что использование памяти процессом luna за 6+ минут не изменилось.

Случаи, когда рекурсивный вызов находится под усливием, которое 1) тождественно истинно или 2) истинно для всех рекурсивных вызовов при использованных начальных параметрах, рассматриваются как ошибки 14 и 15 соответственно. 

## 21. TODO непонятно [(30) Использование оператора информационной зависимости для структурированного ФК](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#30-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5-%D0%BE%D0%BF%D0%B5%D1%80%D0%B0%D1%82%D0%BE%D1%80%D0%B0-%D0%B8%D0%BD%D1%84%D0%BE%D1%80%D0%BC%D0%B0%D1%86%D0%B8%D0%BE%D0%BD%D0%BD%D0%BE%D0%B9-%D0%B7%D0%B0%D0%B2%D0%B8%D1%81%D0%B8%D0%BC%D0%BE%D1%81%D1%82%D0%B8-%D0%B4%D0%BB%D1%8F-%D1%81%D1%82%D1%80%D1%83%D0%BA%D1%82%D1%83%D1%80%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%BD%D0%BE%D0%B3%D0%BE-%D1%84%D0%BA) - **зависание RTS**

[В старой классификации]()

**Обнаруживает компилятор.**     

Пример:
```

```
Вывод luna:
```

```

## 22. [(31) Не передать значение для nfparam](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#31-%D0%BD%D0%B5-%D0%BF%D0%B5%D1%80%D0%B5%D0%B4%D0%B0%D1%82%D1%8C-%D0%B7%D0%BD%D0%B0%D1%87%D0%B5%D0%BD%D0%B8%D0%B5-%D0%B4%D0%BB%D1%8F-nfparam) - **обнаруживает RTS**

[В старой классификации]()

**Обнаруживает компилятор.**     

Пример:
```

```
Вывод luna:
```

```

## 23. TODO непонятно [(32) Попытка запросить ФД из узла, где его нет](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#32-%D0%BF%D0%BE%D0%BF%D1%8B%D1%82%D0%BA%D0%B0-%D0%B7%D0%B0%D0%BF%D1%80%D0%BE%D1%81%D0%B8%D1%82%D1%8C-%D1%84%D0%B4-%D0%B8%D0%B7-%D1%83%D0%B7%D0%BB%D0%B0-%D0%B3%D0%B4%D0%B5-%D0%B5%D0%B3%D0%BE-%D0%BD%D0%B5%D1%82) - **зависание RTS**

[В старой классификации]()

**Обнаруживает компилятор.**     

Пример:
```

```
Вывод luna:
```

```

## 24. TODO непонятно [(33) Неправильный параметр для nfparam](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#33-%D0%BD%D0%B5%D0%BF%D1%80%D0%B0%D0%B2%D0%B8%D0%BB%D1%8C%D0%BD%D1%8B%D0%B9-%D0%BF%D0%B0%D1%80%D0%B0%D0%BC%D0%B5%D1%82%D1%80-%D0%B4%D0%BB%D1%8F-nfparam) - **зависание RTS**

[В старой классификации]()

**Обнаруживает компилятор.**     

Пример:
```

```
Вывод luna:
```

```
