﻿# matrixFont

![](help/screenshots/s1.png)

## О приложении

**matrixFont** позволяет создавать, редактировать растровые шрифты и генерировать код **Си** для подключения созданного шрифта к проекту программного обеспечения. Основная область применения — разработка проектов программного обеспечения с использованием графического интерфейса пользователя во встраиваемых системах.

Возможности **matrixFont**:

- диапазон символов шрифта от 0 до 255;
- размеры символа от 1х1 до 100х100 (технически до 512x512);
- выбор кодовой страницы (кодировки);
- компактный собственный формат файла шрифта **RHF**;
- просмотр образца текста создаваемым шрифтом;
- карта символов шрифта;
- создание проекта на основе системного шрифта (импорт с последующей растеризацией);
- создание проекта из кода на C в форматах matrixFont, AdaFruit, LCDVisionV1.34 или пользовательском;
- гибко настраиваемая генерация кода **Си**;
- удобное редактирование символов мышью;
- история операций редактирования для каждого символа шрифта;
- копирование и вставка символов шрифта;
- поиск символов по коду или по названию;
- масштабирование рабочего поля редактора при помощи колеса мыши;
- пиксельная сетка, шахматный фон;
- эффекты редактирования: инверсия, отображение, сдвиг, прижатие символов;
- пакетное редактирование всех символов шрифта;
- навигатор по символам шрифта с отображением начертания созданных символов;
- оптимизация размера холста символов;
- изменение размера холста символов после создания проекта шрифта;
- изменение диапазона символов после создания проекта шрифта;
- широкие возможности настройки (внешний вид, цвета и прочее).

## Установка

**matrixFont** не нуждается в установке (portable), работает из любого каталога. Скачать можно [отсюда](https://gitlab.com/riva-lab/matrixFont/-/releases).

## Как пользоваться

**matrixFont** имеет встроенную справку в *HTML*, которая поставляется вместе с приложением. Также это руководство пользователя на русском доступно в репозитории [help/matrixFont-help.md](help/matrixFont-help.md).

Ознакомительная статья по теме шрифтов и приложении — [Шрифты для графического дисплея? Это же очень просто](article/mf-article.md).

## Ответственность

**matrixFont** предоставляется для свободного использования, без каких-либо гарантий и технической поддержки. Вы используете приложение по своему усмотрению и несете свою собственную ответственность за результаты его работы.

## Авторство

Copyright 2015-2024 Riva, [FreeBSD License, modified](license.md)

[История версий](versions.md)

Разработано в [Free Pascal RAD IDE Lazarus](http://www.lazarus-ide.org) v2.2.6, компилятор [Free Pascal Compiler](https://freepascal.org) v3.2.2.
