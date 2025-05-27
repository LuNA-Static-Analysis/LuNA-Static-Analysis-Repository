## [df_ref](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%A4%D0%BE%D1%80%D0%BC%D0%B0%D1%82-%D0%B2%D1%8B%D0%B2%D0%BE%D0%B4%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#%D1%81%D1%81%D1%8B%D0%BB%D0%BA%D0%B0-%D0%BD%D0%B0-%D1%84%D0%B4-df_ref)
TODO change all links!

Предлагается к уже [существующим полям](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%A4%D0%BE%D1%80%D0%BC%D0%B0%D1%82-%D0%B2%D1%8B%D0%B2%D0%BE%D0%B4%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#%D1%81%D1%81%D1%8B%D0%BB%D0%BA%D0%B0-%D0%BD%D0%B0-%D1%84%D0%B4-df_ref) добавить поле `"conditions"`, которое будет хранить список условий, при которых данная инициализация/использование выполняются, в виде строк, например:
```json
{
    ...
    "conditions": ["x > 0", "(y[0] < 3) && (N > 10)"]
}
```

## [SEM2.1](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#sem21---%D0%BF%D0%BE%D0%B2%D1%82%D0%BE%D1%80%D0%BD%D0%B0%D1%8F-%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D1%8F-%D0%BE%D0%B4%D0%B8%D0%BD%D0%BE%D1%87%D0%BD%D0%BE%D0%B3%D0%BE-%D1%84%D0%B4)

Предлагается следующий формат:
```json
{
    "error_code": "SEM2.1",
    "details": {
         "initialized": <df_ref>,
         "other_initializations": [<df_ref> | <index_range>, ...]
    }
}
```

Поле `"initialized"` - инициализация, рассматриваемая сейчас, а `"other_initializations"` - список остальных инициализаций, конфликтующих с данной, каждый элемент которого - это либо `df_ref`, либо `index_range`, в зависимости от того, инициализирован ФД в цикле или нет.

## [SEM3.1](https://github.com/LuNA-Static-Analysis/LuNA-Static-Analysis-Repository/wiki/%D0%91%D0%B0%D0%B7%D0%B0-%D0%BE%D1%88%D0%B8%D0%B1%D0%BE%D0%BA#sem31---%D0%BD%D0%B5%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%BD%D1%8B%D0%B9-%D1%84%D0%B4-%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D1%83%D0%B5%D1%82%D1%81%D1%8F-%D0%B2%D0%BD%D0%B5-%D1%86%D0%B8%D0%BA%D0%BB%D0%B0)

Предлагается следующий формат:
```json
{
    "error_code": "SEM3.1",
    "details": {
         "used": <df_ref>,
         "initialized": [<df_ref> | <index_range>, ...]
    }
}
```


Поле `"used"` - использование неинициализированного ФД, а `"initialized"` - список инициализаций, каждый элемент которого - это либо `df_ref`, либо `index_range`, в зависимости от того, инициализирован ФД в цикле или нет.
