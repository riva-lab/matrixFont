matrixFont
==========

MD/REPO: [English](readme.en.md) | [**Русский**](readme.md)

HTML: [English](readme.en.html) | [**Русский**](readme.html)

![](help/light/screenshots/matrixFont.png)

## О приложении

**matrixFont** позволяет создавать, редактировать растровые шрифты и генерировать код *C* для подключения созданного шрифта к проекту программного обеспечения.

Основная область применения — разработка проектов программного обеспечения с использованием графического интерфейса пользователя во встраиваемых системах.

### Возможности

- растровый шрифт:
  
  - размеры символа от 1 × 1 до 100 × 100 (технически до 512 × 512);
  - диапазон символов от 0 до 255;
  - выбор кодовой страницы (кодировки);
  - компактный собственный формат файла **RHF**;
  - поддержка загрузки/сохранения шрифта в формате RBF;

- создание проекта:
  
  - на основе системного шрифта;
  - из кода (поддержка форматов *matrixFont*, *AdaFruit GFX*, *LCD Vision V1.34*);
  - из изображения карты символов с метаинформацией;

- редактор символов:
  
  - удобное редактирование мышью;
  - масштабирование рабочего поля колесом мыши;
  - копирование, вставка и перемещение символов;
  - импортирование изображения в символ;
  - история операций редактирования для каждого символа;
  - операции: инверсия, отображение, поворот, сдвиг, прижатие, центрирование символов;
  - пакетное редактирование всех символов шрифта;

- навигация и предпросмотр:
  
  - навигатор по символам с отображением символов;
  - поиск символов по коду или по названию;
  - просмотр образца текста;
  - карта символов;
  - экспортирование карты символов в изображение;

- инструменты:
  
  - изменение диапазона символов шрифта;
  - изменение размера символов шрифта;
  - оптимизация размера символов;
  - генератор кода *C* с тонкой настройкой;

- интерфейс:
  
  - интуитивный и дружественный дизайн,
  - полная поддержка дисплеев разной плотности,
  - поддержка локализаций,
  - поддержка тем: светлая и темная,
  - гибкость — настройка внешнего вида, цветов и пр.,

- а также:
  
  - подробная справка.

## Компиляция

Особенности компиляции (со временем могут стать неактуальными):

1. FPC имеет модуль RegExpr, который поставляется с компилятором. Однако обычно он редко обновляется. Обновите его вручную: скопируйте с заменой файлы из `.\libraries\TRegExpr\src\` в `<LAZARUS_DIR>\fpc\<VERSION>\source\packages\regexpr\src\`. Этот модуль используется также в системных модулях, поэтому необходимо обновить скомпилированные объектные файлы. Для этого сделайте следующее:
   - Откройте в IDE проект `testregexpr.lpi` из каталога `<LAZARUS_DIR>\fpc\<VERSION>\source\packages\regexpr\tests\` и скомпилируйте его для всех требуемых целевых платформ, например, для `x86_64-win64` и `i386-win32`. Если сразу не компилируется, закомментируйте строки, на которые указал компилятор, и повторите компиляцию.
   - Перейдите в каталог `<LAZARUS_DIR>\fpc\<VERSION>\source\packages\regexpr\tests\lib\`.
   - Скопируйте с заменой файлы из подкаталогов `<TARGET>` в соответствующие каталоги `<LAZARUS_DIR>\fpc\<VERSION>\units\<TARGET>\regexpr`.

## Локализация

Хотите видеть интерфейс **matrixFont** на своем родном языке? Присоединяйтесь к переводу **matrixFont**. Начните переводить, выбрав один из следующих вариантов:

1. Переводите файлы Gettext прямо из репозитория, следуя [инструкции](help/matrixFont-help.md#помощь-в-локализации-интерфейса) в справке.

Перевод будет добавлен в ближайший релиз, если он покрывает не менее 2/3 (~67%).

## Установка

**matrixFont** может быть установлен как обычное приложение. Также доступна портативная версия, не требующая установки и работающая из любого каталога. Установочные и портативные файлы доступны в разделе [Releases](https://gitlab.com/riva-lab/matrixFont/-/releases): так можно получить самую свежую версию.

## Как пользоваться

Руководство пользователя на русском — [help/matrixFont-help.md](help/matrixFont-help.md).

Ознакомительная статья по теме шрифтов и приложении — [Шрифты для графического дисплея? Это же очень просто](article/mf-article.md).

## Ответственность

**matrixFont** предоставляется для свободного использования, без каких-либо гарантий и технической поддержки. Вы используете приложение по своему усмотрению и несете свою собственную ответственность за результаты его работы.

## Вопросы и предложения

Если Вы обнаружили ошибку в работе приложения или хотите предложить что-то для улучшения приложения, пожалуйста, перейдите в раздел [Задачи](https://gitlab.com/riva-lab/matrixFont/-/issues) проекта **matrixFont**. Сначала изучите, не открыта ли ранее похожая или такая же задача. Не создавайте дублирующие задачи, обновляйте или переоткрывайте существующие — это ускоряет их рассмотрение. Если Ваш вопрос не поднимался ранее, создавайте новую задачу.

If your native language is not Russian or Ukrainian, please write your messages in English. You can also add a translation into Russian or Ukrainian below the original if you want.

Ваши вопросы и предложения помогают совершенствовать **matrixFont**.

## Авторство

Copyright 2015-2025 Riva, [FreeBSD License, modified](license.md). История версий — [versions.md](versions.md).

Разработано в [Free Pascal RAD IDE Lazarus](http://www.lazarus-ide.org) v3.0, компилятор [Free Pascal Compiler](https://freepascal.org) v3.2.2.

Установщик для Windows создан в [Inno Setup](https://jrsoftware.org/isinfo.php). [Copyright](https://jrsoftware.org/files/is/license.txt) (C) 1997-2023, Jordan Russell, Martijn Laan.

Значок установщика: [icon-icons.com](https://icon-icons.com/icon/software/76005), [CC BY 4.0](https://creativecommons.org/licenses/by/4.0).

## Зависимости

- [TRegExpr](https://github.com/andgineer/TRegExpr) — regular expressions engine in pure Object Pascal. Copyright (c) 1999-2004 Andrey V. Sorokin.
- [metadarkstyle](https://github.com/zamtmn/metadarkstyle) — package that adds dark theme to your program under windows 10. Copyright (c) 2023 zamtmn.
- [BGRABitmap](https://bgrabitmap.github.io/) — a package designed to modify and create images with transparency.
- [BGRA Controls](https://bgrabitmap.github.io/bgracontrols/) — a set of graphical UI elements. Author: Lainz.
- [ImageSVGListDsgn](https://gitlab.com/riva-lab/ImageSVGListDsgn) — a list of SVG images instead of regular bitmaps. Copyright (c) 2023 Riva.
- [OnlineUpdater](https://gitlab.com/riva-lab/OnlineUpdater) — package for updating application from online repository. Copyright (c) 2023 Riva.
- [AppFeaturesPkg](https://gitlab.com/riva-lab/AppFeaturesPkg) — package for customizing GUI applications and implementing standard application functionality. Copyright (c) 2024 Riva.
