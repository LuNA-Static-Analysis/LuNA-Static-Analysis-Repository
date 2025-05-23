Файл JSON представляет собой массив объектов одинакового типа.

----
[
    {
        "error_code": "LUNAXX", // тут код ошибки согласно базе
        "details": {
            ... // произвольный объект, свой для каждой ошибки и состоящий либо из примитивных данных, либо из описанных ниже типичных объектов
        }
    },
    {
        "error_code": "LUNAXX", // тут код ошибки согласно базе
        "details": {
            ... // произвольный объект, свой для каждой ошибки и состоящий либо из примитивных данных, либо из описанных ниже типичных объектов
        }
    },
    ....
]
----

У всех ошибок один и тот же общий формат в JSON:
----
{
    "error_code": "LUNA01", // тут код ошибки согласно базе
    "details": {
        ... // произвольный объект, свой для каждой ошибки и состоящий либо из примитивных данных, либо из описанных ниже типичных объектов
    }
}
----

По коду ошибки можно получить:

`error_code`: `строка`

- ее описание
- шаблон строки с ее описанием, в который можно подставить дополнительную информацию

Пример: ошибка = неиспользуемый ФД, шаблон = “DF {0} is never used”, в дополнительной информации будет его имя. Пример из luna_trace:

image:https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/blob/wiki-resources/error-notation-pictures/1-luna_trace-example.png[picture]

Тут шаблон мог бы выглядеть как “{0} (declared in {1}) is initialised multiple times:”, и потом 
уровень серьезности (ошибка, предупреждение, слабое предупреждение и т. п.).

## Формат описания типовых объектов

### *Элемент стека вызовов (_call_stack_entry_)*

 * `file`: `строка` - имя файла,
 * `line`: `целое число` - номер строки,
 * `name`: `строка` - имя элемента (для циклов - for, while; для вызовов процедур - имя процедуры).

_Пример:_
```
{
  "file": "main.fa",
  "line": 42,
  "name": "set_int" (или, например, "for")
}
```

### *Стек вызовов (_call_stack_)*

`Список _call_stack_entry_`.

### *ФД* (_df_)

 * `name`: `строка` - имя при объявлении,
 * `declared`:  `список _call_stack_` - места объявления,
 * _[опционально]_ `initialized`: `список _call_stack_` - места инициализации,
 * _[опционально]_ `used`: `список _call_stack_` - места использования.

_Пример:_
```
{
  "name": "x",
  "declared": [[
     {
       "file": "main.fa",
       "line": 10,
       "name": "main"
     }
  ]],
  "initialized": [
    [
      {..., "line": 11}
    ],
    [
      {..., "line": 13}
    ],
  ]
}
```

### *Ссылка на ФД (_df_ref_)*

 * `df`: `df` - информация об объявлении базовго имени,
 * `local`: `expression` - локальное выражение,
 * `true`: `expression` - глобальное/истинное выражение,
 * `where`: `call_stack` - место использования.

### *Индексированный ФД/Диапазон индексов (_index_range_)*

 * `df_ref`: `df_ref` - информация об индексируемом ФД [1],
 * `loop`: `for` - цикл, задающий диапазон индексов,
 * `step`: `expression` - шаг индекса;
 * `offset`:  `expression` - смещение индекса.

Рассматриваются выражения вида `step * loop.var + offset`.

### *ФК (_sub_)*

 * `name`: `строка` - имя,
 * `type`: `"struct"` | `"extern"` - тип,
 * `file`: `строка` - имя файла,
 * `line`: `целое число` - строка объявления (строка импортирования).

_Пример:_
```
{
  "name": "set_int",
  "type": "extern",
  "file": "main.fa",
  "line": 3
}
```

### Цикл *for* (_for_)

 * `var`: `строка` - имя переменной-счетчика,
 * `first`: `expression` - нижняя граница,
 * `last`: `expression` - верхняя граница,
 * `where`: `call_stack` - место цикла в коде.

_Пример:_
```
{
  "var": "i",
  "first": "1",
  "last": "N + 1",
  "where": [
    ...
  ]
}
```

### Арифметическое выражение (_expression_)

Является атомарным полем, а не структурой. Представляет собой строку, которая содержит арифметическое выражение.

_Пример:_
```
{
  "expression": "1 + x * y";
}
```

## Форматы описания ошибок

Для каждой ошибки указано, какие средства её уже умеют искать (DDG, AST, Prolog, Trace).

**LUNA01. Несоответствие типов аргументов LuNA типам C++ при вызове атомарного ФК**

AST

----
{
    "error_code": "LUNA01",
    "details": {
      "call_stack_entry": <call_stack_entry>,
      "cf": <cf>
    }
}
----

**LUNA02. Несуществующая LuNA-подпрограмма**

DDG, AST

----
{
    "error_code": "LUNA02",
    "details" : {
      "call_stack_entry": <call_stack_entry>
    }
}
----

**LUNA03. Повторная инициализация ФД** 

DDG, Trace

 * df: можно оставить одно значение определения, указать все инициализации, использования можно не указывать.
----
{
    "error_code": "LUNA03",
    "details": {
        "df": <df>
    }
}
----

**LUNA04. Несоответствие типов аргументов LuNA при вызове атомарного ФК** 

AST

----
{
    "error_code": "LUNA04",
    "details" : {
           "call_stack_entry": <call_stack_entry>,
           "cf": <cf>
    }
}
----

**LUNA05. Попытка использования неинициализированного ФД** 

 * df: оставить одно значение определения, пустое поле с инициализациями, но использования указать все.

DDG, Trace

----
{
    "error_code": "LUNA05",
    "details": {
        "df": <df>
    }
}
----

**LUNA06. Несовпадение количества аргументов при объявлении ФК и его вызове**

AST

----
{
    "error_code": "LUNA06",
    "details": {
        "call_stack_entry": <call_stack_entry>,
        "cf": <cf>
    }
}
----

**LUNA07. Два или более объявлений ФД в подпрограмме** 

AST

----
{
    "error_code": "LUNA07",
    "details": {
      "df": <df>
    }
}
----

**LUNA09. Использование ФД после его удаления** туду
----
{
    "error_code": "LUNA09",
    "details": {
        
    }
}
----

**LUNA10. Неиспользуемый ФД** 

DDG, AST

* df: оставить одно значение определения, все инициализации, пустое поле использований.

Под ФД тут подразумевается как базовое имя ФД, так и аргумент структурированного ФК.
----
{
    "error_code": "LUNA10",
    "details": {
        "df": <df>
    }
}
----

**LUNA11. Импорт нескольких разных функций под одним алиасом**

AST

----
{
    "error_code": "LUNA11",
    "details": {
        "cfs": [ <cf> ]
    }
}
----

**LUNA12. Нет main** 

AST

----
{
    "error_code": "LUNA12",
    "details": {}
}
----

**LUNA13. DF с одинаковыми названиями в одной области видимости** 

DDG, AST

----
{
    "error_code": "LUNA13",
    "details": {
        "dfs": [
            <список df>
        ]
    }
}
----

**LUNA14. Попытка использования необъявленного ФД** 

AST, DDG

----
{
    "error_code": "LUNA14",
    "details": {
        "df": <df>
    }
}
----

**LUNA15. Циклическая зависимость по данным** туду
----
{
    "error_code": "LUNA15",
    "details": {
        
    }
}
----

**LUNA16. Повторное определение ФК**



----
{
    "error_code": "LUNA16",
    "details" : {
      "cfs" : [ cf {...} ]
    }
}
----

**LUNA17. Импорт несуществующей C/C++ функции** 




----
{
    "error_code": "LUNA17",
    "details": {
      "cf": <cf>
    }
}
----

**Несоответствие границ циклов инициализации и использования**

Предположим, что цикл инициализации единственный. Тогда дополнительная информация должна содержать:

 * "initialized": <index_range> - цикл инициализации,
 * "used": <index_range> - циклы использования.

Варианты ошибки: 

- LUNA18 - предупрежедние, нижняя граница;
- LUNA19 - ошибка, нижняя граница;
- LUNA20 - предупреждение, верхняя граница;
- LUNA21 - ошибка, верхняя граница.

**LUNA18. Несоответствие границ циклов инициализации и использования - Нижняя граница цикла использования может быть как больше, так и меньше нижней границы цикла инициализации** 

Prolog

----
{
    "error_code": "LUNA18",
    "details": {
        "initialized": <index_range>,
        "used": <index_range>
    }
}
----

**LUNA19. Несоответствие границ циклов инициализации и использования - Нижняя граница цикла использования строго меньше нижней границы цикла инициализации** 

Prolog

----
{
    "error_code": "LUNA19",
    "details": {
        "initialized": <index_range>,
        "used": <index_range>
    }
}
----

**LUNA20. Несоответствие границ циклов инициалиации и использования - Верхняя граница цикла использования может быть как меньше, так и больше верхней границы цикла инициализации** 

Prolog

----
{
    "error_code": "LUNA20",
    "details": {
        "initialized": <index_range>,
        "used": <index_range>
    }
}
----

**LUNA21. Несоответствие границ циклов инициалиации и использования - Верхняя граница цикла использования строго больше верхней границы цикла инициализации** 

Prolog

----
{
    "error_code": "LUNA21",
    "details": {
        "initialized": <index_range>,
        "used": <index_range>
    }
}
----

**LUNA22. Шаг цикла использования не кратен шагу цикла инициализации**

Prolog

----
{
    "error_code": "LUNA22",
    "details": {
        "initialized": <index_range>,
        "used": <index_range>
    }
}
----

**LUNA23. Формула в if тождественно истинна/ложна**

DDG, Prolog

----
{
    "error_code": "LUNA23",
    "details": {
        "type": bool,
        "condition": string, //expression
        "where": <_call_stack_entry_>
    }
}
----

**LUNA24. Формула в if истинна/ложна во всех путях выполнения**



----
{
    "error_code": "LUNA24",
    "details": {
        "type": bool,
        "condition": string, //expression
        "where": <_call_stack_entry_>
    }
}
----

**LUNA25. Операторы, возвращающие булево значение, используются в целочисленном контексте**

Prolog

----
{
    "error_code": "LUNA25",
    "details": {
        "bad_expr": string, //expression
        "arg_index": int,
        "where": <_call_stack_entry_>
    }
}
----

**LUNA26. Попытка инициализации объекта, не являющегося ФД** 

DDG

----
{
    "error_code": "LUNA26",
    "details": {
        "expression": string,
        "cf": _<cf>_, 
        "callstack": <_call_stack_> 
    }
}
----

**LUNA27. Попытка запросить неинициализированный ФД при помощи request** туду
----
{
    "error_code": "LUNA27",
    "details": {
        
    }
}
----

**LUNA28. Попытка использования ФД после превышения допустимого числа запросов** туду
----
{
    "error_code": "LUNA28",
    "details": {
        
    }
}
----

**LUNA29. Использование ФД после его удаления при помощи соответствующего оператора** туду
----
{
    "error_code": "LUNA29",
    "details": {
        
    }
}
----

**LUNA30. Использование оператора информационной зависимости для структурированного ФК** туду
----
{
    "error_code": "LUNA30",
    "details": {
        
    }
}
----

**LUNA31. Не передать значение для nfparam** туду
----
{
    "error_code": "LUNA31",
    "details": {
        
    }
}
----

**LUNA32. Попытка запросить ФД из узла, где его нет** туду
----
{
    "error_code": "LUNA32",
    "details": {
        
    }
}
----

**LUNA33. Неправильный параметр для nfparam** туду
----
{
    "error_code": "LUNA33",
    "details": {
        
    }
}
----

**LUNA34. Безусловная рекурсия** туду
----
{
    "error_code": "LUNA34",
    "details": {
        
    }
}
----

**LUNA35. Пересечение диапазонов инициализируемых индексов**

Prolog

----
{
    "error_code": "LUNA35",
    "details": {
        "ranges": [
            список <_index_range_>, пока что всегда из двух
        ]
    }
}
----

**LUNA36. Попытка индексации объекта, не являющегося ФД**

DDG

----
{
    "error_code": "LUNA36",
    "details": {
        "expression": string,
        "callstack": _<call_stack>_
    }
}
----

**LUNA37. Нет (гарантированного) цикла инициализации**

Prolog

* use_conditions, initialized, init_conditions: опционально, если циклы инициализации под условиями, которые могут не выполниться.
----
{
    "error_code": "LUNA37",
    "details": {
        "used": <_index_range_>,
        "use_conditions": [
            список _expression_,
        ],
        "initialized": <_index_range_>,
        "init_conditions": [
            список _expression_,
        ]
    }
}
----
