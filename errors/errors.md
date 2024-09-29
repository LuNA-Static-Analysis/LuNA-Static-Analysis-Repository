## 1. [(1) Несоответствие типов аргументов LuNA типам C++ при вызове атомарного ФК](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#1-%D0%BD%D0%B5%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D0%B8%D0%B5-%D1%82%D0%B8%D0%BF%D0%BE%D0%B2-%D0%B0%D1%80%D0%B3%D1%83%D0%BC%D0%B5%D0%BD%D1%82%D0%BE%D0%B2-luna-%D1%82%D0%B8%D0%BF%D0%B0%D0%BC-c-%D0%BF%D1%80%D0%B8-%D0%B2%D1%8B%D0%B7%D0%BE%D0%B2%D0%B5-%D0%B0%D1%82%D0%BE%D0%BC%D0%B0%D1%80%D0%BD%D0%BE%D0%B3%D0%BE-%D1%84%D0%BA) + [(4) Несоответствие типов аргументов LuNA при вызове атомарного ФК](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#4-%D0%BD%D0%B5%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D0%B8%D0%B5-%D1%82%D0%B8%D0%BF%D0%BE%D0%B2-%D0%B0%D1%80%D0%B3%D1%83%D0%BC%D0%B5%D0%BD%D1%82%D0%BE%D0%B2-luna-%D0%BF%D1%80%D0%B8-%D0%B2%D1%8B%D0%B7%D0%BE%D0%B2%D0%B5-%D0%B0%D1%82%D0%BE%D0%BC%D0%B0%D1%80%D0%BD%D0%BE%D0%B3%D0%BE-%D1%84%D0%BA)

### 1.1. Использование константы неправильного типа - **обнаруживает компилятор**

Примеры программ:

[1.1-err-luna-compiler-bug.fa](errors/demos/1.1-err-luna-compiler-bug.fa) - возникает баг в компиляторе

    luna: fatal error: cpp-generation failed (see below):
    err> Traceback (most recent call last):
    err>   File "/home/reworu/luna/scripts/fcmp2", line 1279, in <module>
    err>     bid, cpp=gen_struct(sub)
    err>   File "/home/reworu/luna/scripts/fcmp2", line 1210, in gen_struct
    err>     cpp_bodyfork, cpp_bodyitems=gen_body(scope, ja)
    err>   File "/home/reworu/luna/scripts/fcmp2", line 1177, in gen_body
    err>     bibid, cpp=gen_bi(bi, scope)
    err>   File "/home/reworu/luna/scripts/fcmp2", line 1156, in gen_bi
    err>     return gen_bi_exec(ja, parent_scope)
    err>   File "/home/reworu/luna/scripts/fcmp2", line 1007, in gen_bi_exec
    err>     cpp+=gen_exec_extern(scope, ja)
    err>   File "/home/reworu/luna/scripts/fcmp2", line 650, in gen_exec_extern
    err>     value(arg, scope, param)
    err>   File "/home/reworu/luna/scripts/fcmp2", line 452, in value
    err>     return value_int(x, scope)
    err>   File "/home/reworu/luna/scripts/fcmp2", line 328, in value_int
    err>     raise NotImplementedError("Need quotation", x['value'], c)
    err> NameError: name 'c' is not defined
    err>

[1.1-err-so-build-failed.fa](errors/demos/1.1-err-so-build-failed.fa) - сгенерированный C++-код не компилируется

    luna: fatal error: so-build failed: 2 (see below):
    /home/reworu/luna/build/programs/mnt/c/Users/misha/Projects/luna/Extraterrestrial/adapt/errors/demos/1.1-err-so-build-failed.fa/test.cpp: In function ‘BlockRetStatus block_2(CF&)’:
    /home/reworu/luna/build/programs/mnt/c/Users/misha/Projects/luna/Extraterrestrial/adapt/errors/demos/1.1-err-so-build-failed.fa/test.cpp:36:25: error: invalid ‘static_cast’ from type ‘std::__cxx11::basic_string<char>’ to type ‘int’
       36 |                         static_cast<int>((std::string(std::string("Hello"))+std::string(std::string("1")))));
          |                         ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    make: *** [/home/reworu/luna/build/programs/mnt/c/Users/misha/Projects/luna/Extraterrestrial/adapt/errors/demos/1.1-err-so-build-failed.fa/Makefile.libucodes:5: /home/reworu/luna/build/programs/mnt/c/Users/misha/Projects/luna/Extraterrestrial/adapt/errors/demos/1.1-err-so-build-failed.fa/test.o] Error 1
    


[1.1-ok-cast.fa](errors/demos/1.1-ok-cast.fa) - ошибки нет, работает приведение real к int

### 1.2. Использование ФД неправильного типа - **обнаруживает RTS**

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


## 2. [(2) Несуществующая LuNA-подпрограмма](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#2-%D0%BD%D0%B5%D1%81%D1%83%D1%89%D0%B5%D1%81%D1%82%D0%B2%D1%83%D1%8E%D1%89%D0%B0%D1%8F-luna-%D0%BF%D0%BE%D0%B4%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D0%BC%D0%B0) - **обнаруживает компилятор**

## 3. [(3) Повторная инициализация ФД](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#3-%D0%BF%D0%BE%D0%B2%D1%82%D0%BE%D1%80%D0%BD%D0%B0%D1%8F-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D1%8F-%D1%84%D0%B4) - **В некоторых случаях обнаруживает RTS**

Может быть обнаружено в рантайме, но можно сделать так, что не будет (например, с помощью delete):

```
df x;
init_after_random_delay(x);
init_after_random_delay(x);
print(x) @ { delete x; };
```
Если между выполнением двух `init_after_random_delay(x)` выполнится `print(x) @ { delete x; };`, повторная инициализация не будет обнаружена. 

### 3.1. [(35) Пересечение диапазонов инициализируемых индексов](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#35-%D0%BF%D0%B5%D1%80%D0%B5%D1%81%D0%B5%D1%87%D0%B5%D0%BD%D0%B8%D0%B5-%D0%B4%D0%B8%D0%B0%D0%BF%D0%B0%D0%B7%D0%BE%D0%BD%D0%BE%D0%B2-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B8%D1%80%D1%83%D0%B5%D0%BC%D1%8B%D1%85-%D0%B8%D0%BD%D0%B4%D0%B5%D0%BA%D1%81%D0%BE%D0%B2)

## 4. [(5) Попытка использования неинициализированного ФД](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#5-%D0%BF%D0%BE%D0%BF%D1%8B%D1%82%D0%BA%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D0%BD%D0%B5%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%BD%D0%BE%D0%B3%D0%BE-%D1%84%D0%B4)

### 4.1. [(15) Циклическая зависимость по данным](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#15-%D1%86%D0%B8%D0%BA%D0%BB%D0%B8%D1%87%D0%B5%D1%81%D0%BA%D0%B0%D1%8F-%D0%B7%D0%B0%D0%B2%D0%B8%D1%81%D0%B8%D0%BC%D0%BE%D1%81%D1%82%D1%8C-%D0%BF%D0%BE-%D0%B4%D0%B0%D0%BD%D0%BD%D1%8B%D0%BC) - **зависание RTS**

### 4.2. [(18) Несоответствие границ циклов инициалиации и использования - Нижняя граница цикла использования может быть как больше, так и меньше нижней границы цикла инициализации](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#18-%D0%BD%D0%B5%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D0%B8%D0%B5-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86-%D1%86%D0%B8%D0%BA%D0%BB%D0%BE%D0%B2-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B0%D1%86%D0%B8%D0%B8-%D0%B8-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F---%D0%BD%D0%B8%D0%B6%D0%BD%D1%8F%D1%8F-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D0%B0-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D0%BC%D0%BE%D0%B6%D0%B5%D1%82-%D0%B1%D1%8B%D1%82%D1%8C-%D0%BA%D0%B0%D0%BA-%D0%B1%D0%BE%D0%BB%D1%8C%D1%88%D0%B5-%D1%82%D0%B0%D0%BA-%D0%B8-%D0%BC%D0%B5%D0%BD%D1%8C%D1%88%D0%B5-%D0%BD%D0%B8%D0%B6%D0%BD%D0%B5%D0%B9-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D1%8B-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D0%B8) - **возможно зависание RTS**

### 4.3. [(19) Несоответствие границ циклов инициалиации и использования - Нижняя граница цикла использования строго меньше нижней границы цикла инициализации](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#19-%D0%BD%D0%B5%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D0%B8%D0%B5-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86-%D1%86%D0%B8%D0%BA%D0%BB%D0%BE%D0%B2-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B0%D1%86%D0%B8%D0%B8-%D0%B8-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F---%D0%BD%D0%B8%D0%B6%D0%BD%D1%8F%D1%8F-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D0%B0-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D1%81%D1%82%D1%80%D0%BE%D0%B3%D0%BE-%D0%BC%D0%B5%D0%BD%D1%8C%D1%88%D0%B5-%D0%BD%D0%B8%D0%B6%D0%BD%D0%B5%D0%B9-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D1%8B-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D0%B8) - **зависание RTS**

### 4.4. [(20) Несоответствие границ циклов инициалиации и использования - Верхняя граница цикла использования может быть как меньше, так и больше верхней границы цикла инициализации](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#20-%D0%BD%D0%B5%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D0%B8%D0%B5-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86-%D1%86%D0%B8%D0%BA%D0%BB%D0%BE%D0%B2-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B0%D1%86%D0%B8%D0%B8-%D0%B8-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F---%D0%B2%D0%B5%D1%80%D1%85%D0%BD%D1%8F%D1%8F-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D0%B0-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D0%BC%D0%BE%D0%B6%D0%B5%D1%82-%D0%B1%D1%8B%D1%82%D1%8C-%D0%BA%D0%B0%D0%BA-%D0%BC%D0%B5%D0%BD%D1%8C%D1%88%D0%B5-%D1%82%D0%B0%D0%BA-%D0%B8-%D0%B1%D0%BE%D0%BB%D1%8C%D1%88%D0%B5-%D0%B2%D0%B5%D1%80%D1%85%D0%BD%D0%B5%D0%B9-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D1%8B-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D0%B8) - **возможно зависание RTS**

### 4.5. [(21) Несоответствие границ циклов инициалиации и использования - Верхняя граница цикла использования строго больше верхней границы цикла инициализации](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#21-%D0%BD%D0%B5%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D0%B8%D0%B5-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86-%D1%86%D0%B8%D0%BA%D0%BB%D0%BE%D0%B2-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B0%D1%86%D0%B8%D0%B8-%D0%B8-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F---%D0%B2%D0%B5%D1%80%D1%85%D0%BD%D1%8F%D1%8F-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D0%B0-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D1%81%D1%82%D1%80%D0%BE%D0%B3%D0%BE-%D0%B1%D0%BE%D0%BB%D1%8C%D1%88%D0%B5-%D0%B2%D0%B5%D1%80%D1%85%D0%BD%D0%B5%D0%B9-%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D1%8B-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D0%B8) - **зависание RTS**

### 4.6. [(22) Шаг цикла использования не кратен шагу цикла инициализации](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#22-%D1%88%D0%B0%D0%B3-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D0%BD%D0%B5-%D0%BA%D1%80%D0%B0%D1%82%D0%B5%D0%BD-%D1%88%D0%B0%D0%B3%D1%83-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D0%B8) - **зависание RTS**

### 4.7. [(37) Нет (гарантированного) цикла инициализации](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#37-%D0%BD%D0%B5%D1%82-%D0%B3%D0%B0%D1%80%D0%B0%D0%BD%D1%82%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%BD%D0%BE%D0%B3%D0%BE-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D0%B8) - **возможно зависание RTS**

Реакция RTS зависит от того, будут ли инициализированы необходимые индексированные ФД. 

### 4.8. [(27) Попытка запросить неинициализированный ФД при помощи request](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#27-%D0%BF%D0%BE%D0%BF%D1%8B%D1%82%D0%BA%D0%B0-%D0%B7%D0%B0%D0%BF%D1%80%D0%BE%D1%81%D0%B8%D1%82%D1%8C-%D0%BD%D0%B5%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%BD%D1%8B%D0%B9-%D1%84%D0%B4-%D0%BF%D1%80%D0%B8-%D0%BF%D0%BE%D0%BC%D0%BE%D1%89%D0%B8-request) - **зависание RTS**

### 4.9. [(28) Попытка использования ФД после превышения допустимого числа запросов](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#28-%D0%BF%D0%BE%D0%BF%D1%8B%D1%82%D0%BA%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D1%84%D0%B4-%D0%BF%D0%BE%D1%81%D0%BB%D0%B5-%D0%BF%D1%80%D0%B5%D0%B2%D1%8B%D1%88%D0%B5%D0%BD%D0%B8%D1%8F-%D0%B4%D0%BE%D0%BF%D1%83%D1%81%D1%82%D0%B8%D0%BC%D0%BE%D0%B3%D0%BE-%D1%87%D0%B8%D1%81%D0%BB%D0%B0-%D0%B7%D0%B0%D0%BF%D1%80%D0%BE%D1%81%D0%BE%D0%B2) - **зависание RTS**

### 4.10. [(29) Использование ФД после его удаления при помощи соответствующего оператора](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#29-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5-%D1%84%D0%B4-%D0%BF%D0%BE%D1%81%D0%BB%D0%B5-%D0%B5%D0%B3%D0%BE-%D1%83%D0%B4%D0%B0%D0%BB%D0%B5%D0%BD%D0%B8%D1%8F-%D0%BF%D1%80%D0%B8-%D0%BF%D0%BE%D0%BC%D0%BE%D1%89%D0%B8-%D1%81%D0%BE%D0%BE%D1%82%D0%B2%D0%B5%D1%82%D1%81%D1%82%D0%B2%D1%83%D1%8E%D1%89%D0%B5%D0%B3%D0%BE-%D0%BE%D0%BF%D0%B5%D1%80%D0%B0%D1%82%D0%BE%D1%80%D0%B0) - **зависание RTS**

## 5. [(6) Несовпадение количества аргументов при объявлении ФК и его вызове](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#6-%D0%BD%D0%B5%D1%81%D0%BE%D0%B2%D0%BF%D0%B0%D0%B4%D0%B5%D0%BD%D0%B8%D0%B5-%D0%BA%D0%BE%D0%BB%D0%B8%D1%87%D0%B5%D1%81%D1%82%D0%B2%D0%B0-%D0%B0%D1%80%D0%B3%D1%83%D0%BC%D0%B5%D0%BD%D1%82%D0%BE%D0%B2-%D0%BF%D1%80%D0%B8-%D0%BE%D0%B1%D1%8A%D1%8F%D0%B2%D0%BB%D0%B5%D0%BD%D0%B8%D0%B8-%D1%84%D0%BA-%D0%B8-%D0%B5%D0%B3%D0%BE-%D0%B2%D1%8B%D0%B7%D0%BE%D0%B2%D0%B5) - **обнаруживает компилятор**

## 6. [(7) Два или более объявлений ФД в подпрограмме](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#7-%D0%B4%D0%B2%D0%B0-%D0%B8%D0%BB%D0%B8-%D0%B1%D0%BE%D0%BB%D0%B5%D0%B5-%D0%BE%D0%B1%D1%8A%D1%8F%D0%B2%D0%BB%D0%B5%D0%BD%D0%B8%D0%B9-%D1%84%D0%B4-%D0%B2-%D0%BF%D0%BE%D0%B4%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D0%BC%D0%B5) - **обнаруживает компилятор**

## 7. [(9) Использование ФД после его удаления](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#9-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5-%D1%84%D0%B4-%D0%BF%D0%BE%D1%81%D0%BB%D0%B5-%D0%B5%D0%B3%D0%BE-%D1%83%D0%B4%D0%B0%D0%BB%D0%B5%D0%BD%D0%B8%D1%8F) - **?**

## 8. [(10) Неиспользуемый ФД](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#10-%D0%BD%D0%B5%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D1%83%D0%B5%D0%BC%D1%8B%D0%B9-%D1%84%D0%B4)

ФД инициализируется, но не используется как входной

[8-ok-initialized-never-consumed.fa](errors/demos/8.3-ok-initialized-never-consumed.fa) - ошибки нет

## 9. Имя встречается только при объявлении

Некоторый идентификатор (имя) был объявлен, но больше не фигурирует в коде.

### 9.1 Макрос
### 9.2 Имя, определенное в операторе let
### 9.3 Базовое имя ФД, определенное в операторе df
### 9.4 Атомарный ФК
### 9.5 Структурированный ФК
### 9.6 Имя параметра структурированного ФК
### 9.7 Счетчик оператора for
### 9.8 Счетчик оператора while

## 10. [(11) Импорт нескольких разных функций под одним алиасом](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#11-%D0%B8%D0%BC%D0%BF%D0%BE%D1%80%D1%82-%D0%BD%D0%B5%D1%81%D0%BA%D0%BE%D0%BB%D1%8C%D0%BA%D0%B8%D1%85-%D1%80%D0%B0%D0%B7%D0%BD%D1%8B%D1%85-%D1%84%D1%83%D0%BD%D0%BA%D1%86%D0%B8%D0%B9-%D0%BF%D0%BE%D0%B4-%D0%BE%D0%B4%D0%BD%D0%B8%D0%BC-%D0%B0%D0%BB%D0%B8%D0%B0%D1%81%D0%BE%D0%BC) + [(16) Повторное определение ФК](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#16-%D0%BF%D0%BE%D0%B2%D1%82%D0%BE%D1%80%D0%BD%D0%BE%D0%B5-%D0%BE%D0%BF%D1%80%D0%B5%D0%B4%D0%B5%D0%BB%D0%B5%D0%BD%D0%B8%D0%B5-%D1%84%D0%BA) - **нет реакции**

## 11. [(12) Нет main](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#12-%D0%BD%D0%B5%D1%82-main) - **обнаруживает компилятор**

## 12. [(13) DF с одинаковыми названиями в одной области видимости](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#13-df-%D1%81-%D0%BE%D0%B4%D0%B8%D0%BD%D0%B0%D0%BA%D0%BE%D0%B2%D1%8B%D0%BC%D0%B8-%D0%BD%D0%B0%D0%B7%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F%D0%BC%D0%B8-%D0%B2-%D0%BE%D0%B4%D0%BD%D0%BE%D0%B9-%D0%BE%D0%B1%D0%BB%D0%B0%D1%81%D1%82%D0%B8-%D0%B2%D0%B8%D0%B4%D0%B8%D0%BC%D0%BE%D1%81%D1%82%D0%B8) - **обнаруживает компилятор**

## 13. [(14) Попытка использования необъявленного ФД](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#14-%D0%BF%D0%BE%D0%BF%D1%8B%D1%82%D0%BA%D0%B0-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D0%BD%D0%B5%D0%BE%D0%B1%D1%8A%D1%8F%D0%B2%D0%BB%D0%B5%D0%BD%D0%BD%D0%BE%D0%B3%D0%BE-%D1%84%D0%B4) - **обнаруживает компилятор**

## 14. [(17) Импорт несуществующей C/C++ функции](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#17-%D0%B8%D0%BC%D0%BF%D0%BE%D1%80%D1%82-%D0%BD%D0%B5%D1%81%D1%83%D1%89%D0%B5%D1%81%D1%82%D0%B2%D1%83%D1%8E%D1%89%D0%B5%D0%B9-cc-%D1%84%D1%83%D0%BD%D0%BA%D1%86%D0%B8%D0%B8) - **обнаруживает RTS**

## 15. [(23) Формула в if тождественно истинна/ложна](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#23-%D1%84%D0%BE%D1%80%D0%BC%D1%83%D0%BB%D0%B0-%D0%B2-if-%D1%82%D0%BE%D0%B6%D0%B4%D0%B5%D1%81%D1%82%D0%B2%D0%B5%D0%BD%D0%BD%D0%BE-%D0%B8%D1%81%D1%82%D0%B8%D0%BD%D0%BD%D0%B0%D0%BB%D0%BE%D0%B6%D0%BD%D0%B0) - **нет реакции**

## 16. [(24) Формула в if истинна/ложна во всех путях выполнения](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#24-%D1%84%D0%BE%D1%80%D0%BC%D1%83%D0%BB%D0%B0-%D0%B2-if-%D0%B8%D1%81%D1%82%D0%B8%D0%BD%D0%BD%D0%B0%D0%BB%D0%BE%D0%B6%D0%BD%D0%B0-%D0%B2%D0%BE-%D0%B2%D1%81%D0%B5%D1%85-%D0%BF%D1%83%D1%82%D1%8F%D1%85-%D0%B2%D1%8B%D0%BF%D0%BE%D0%BB%D0%BD%D0%B5%D0%BD%D0%B8%D1%8F)

## 17. [(25) Операторы, возвращающие булево значение, используются в целочисленном контексте](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#25-%D0%BE%D0%BF%D0%B5%D1%80%D0%B0%D1%82%D0%BE%D1%80%D1%8B-%D0%B2%D0%BE%D0%B7%D0%B2%D1%80%D0%B0%D1%89%D0%B0%D1%8E%D1%89%D0%B8%D0%B5-%D0%B1%D1%83%D0%BB%D0%B5%D0%B2%D0%BE-%D0%B7%D0%BD%D0%B0%D1%87%D0%B5%D0%BD%D0%B8%D0%B5-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D1%83%D1%8E%D1%82%D1%81%D1%8F-%D0%B2-%D1%86%D0%B5%D0%BB%D0%BE%D1%87%D0%B8%D1%81%D0%BB%D0%B5%D0%BD%D0%BD%D0%BE%D0%BC-%D0%BA%D0%BE%D0%BD%D1%82%D0%B5%D0%BA%D1%81%D1%82%D0%B5)

## 18. [(34) Безусловная рекурсия](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#34-%D0%B1%D0%B5%D0%B7%D1%83%D1%81%D0%BB%D0%BE%D0%B2%D0%BD%D0%B0%D1%8F-%D1%80%D0%B5%D0%BA%D1%83%D1%80%D1%81%D0%B8%D1%8F) - **Зависание RTS**

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

## 19. [(35) Попытка индексации объекта, не являющегося ФД](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#36-%D0%BF%D0%BE%D0%BF%D1%8B%D1%82%D0%BA%D0%B0-%D0%B8%D0%BD%D0%B4%D0%B5%D0%BA%D1%81%D0%B0%D1%86%D0%B8%D0%B8-%D0%BE%D0%B1%D1%8A%D0%B5%D0%BA%D1%82%D0%B0-%D0%BD%D0%B5-%D1%8F%D0%B2%D0%BB%D1%8F%D1%8E%D1%89%D0%B5%D0%B3%D0%BE%D1%81%D1%8F-%D1%84%D0%B4) - **обнаруживает компилятор**

## 20. TODO непонятно [(30) Использование оператора информационной зависимости для структурированного ФК](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#30-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5-%D0%BE%D0%BF%D0%B5%D1%80%D0%B0%D1%82%D0%BE%D1%80%D0%B0-%D0%B8%D0%BD%D1%84%D0%BE%D1%80%D0%BC%D0%B0%D1%86%D0%B8%D0%BE%D0%BD%D0%BD%D0%BE%D0%B9-%D0%B7%D0%B0%D0%B2%D0%B8%D1%81%D0%B8%D0%BC%D0%BE%D1%81%D1%82%D0%B8-%D0%B4%D0%BB%D1%8F-%D1%81%D1%82%D1%80%D1%83%D0%BA%D1%82%D1%83%D1%80%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%BD%D0%BE%D0%B3%D0%BE-%D1%84%D0%BA) - **зависание RTS**

## 21. [(31) Не передать значение для nfparam](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#31-%D0%BD%D0%B5-%D0%BF%D0%B5%D1%80%D0%B5%D0%B4%D0%B0%D1%82%D1%8C-%D0%B7%D0%BD%D0%B0%D1%87%D0%B5%D0%BD%D0%B8%D0%B5-%D0%B4%D0%BB%D1%8F-nfparam) - **обнаруживает RTS**

## 22. TODO непонятно [(32) Попытка запросить ФД из узла, где его нет](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#32-%D0%BF%D0%BE%D0%BF%D1%8B%D1%82%D0%BA%D0%B0-%D0%B7%D0%B0%D0%BF%D1%80%D0%BE%D1%81%D0%B8%D1%82%D1%8C-%D1%84%D0%B4-%D0%B8%D0%B7-%D1%83%D0%B7%D0%BB%D0%B0-%D0%B3%D0%B4%D0%B5-%D0%B5%D0%B3%D0%BE-%D0%BD%D0%B5%D1%82) - **зависание RTS**

## 23. TODO непонятно [(33) Неправильный параметр для nfparam](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#33-%D0%BD%D0%B5%D0%BF%D1%80%D0%B0%D0%B2%D0%B8%D0%BB%D1%8C%D0%BD%D1%8B%D0%B9-%D0%BF%D0%B0%D1%80%D0%B0%D0%BC%D0%B5%D1%82%D1%80-%D0%B4%D0%BB%D1%8F-nfparam) - **зависание RTS**