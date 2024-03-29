# templater
Шаблонизатор для подстановки переменных в текстовый шаблон. Предназначено для TeX, но можно использовать и для других текстовых форматов, где пробельные символы малозначащи.

Шаблонизатор получает из командной строки три аргумента: файл шаблона, файл переменных и имя файла для вывода результата.

# Файл переменных

Переменные определяются строками `имя переменной = значение переменной` или
```
имя переменной ==
многострочное
значение переменной
==
```
В имени переменной могут быть любые символы кроме `=` и последовательности `]]`. 
Пробельные символы в начале и конце имён переменных и значений, а также в конце строк многострочных значений игнорируются.

Также можно включить другой файл переменных командой
```
включить имя-файла-переменных
```
Этот файл переменных должен находиться в том же каталоге, где лежит файл шаблона.

И можно указать использовать другой файл шаблона
```
@имя-файла-шаблона
```
Этот файл шаблона должен находиться в том же каталоге, где лежит файл шаблона, переданный в параметрах.

Предполагается, что при вызове передаётся файл шаблона по-умолчанию, но в файле переменных его можно переопределить.

# Файл шаблона
Подстановка переменных осуществляется конструкцией `[[имя переменной]]`. Результат подстановки также раскрывается. То есть, если в файле переменных
```
имя пользователя = Иван
приветствие = Привет, [[имя пользователя]]
```
а в файле шаблона
```
[[приветствие]]!

Начнём работу.
```
то в результате будет
```
Привет, Иван!

Начнём работу.
```

Также можно использовать конструкцию `%[[строка файла переменных]]`, в которую можно вставлять команды для изменения переменных или подгрузки дополнительных файлов переменных.
Разумеется, `%[[@...]]` работать не будет, так как файл шаблона уже обрабатывается.
