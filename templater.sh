#!/bin/sh
# пример скрипта дял запуска шаблонизатора
# предполагается, что файл с переменными имеет расширение tpl, а шаблон и результат являются файлами tex.
second=`echo $1 | sed 's/tpl$/tex/'`
templater "~/templates/template.tex" "$1" "${2:-$second}"
