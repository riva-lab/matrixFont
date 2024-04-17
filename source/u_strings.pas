unit u_strings;

{$mode objfpc}{$H+}

interface

resourcestring

  WARN_NOTSAVED = 'Изменения в текущем файле не сохранены.' + LineEnding
    + 'Сохранить?';

  WARN_OPTIMIZE = 'Все символы шрифта пусты. Оптимизация недоступна';

  WARN_LOAD = 'Не удалось загрузить проект шрифта!' + LineEnding
    + 'Возможно файл поврежден или возникла другая критическая ошибка.';

  WARN_CREATE = 'Не удалось создать проект шрифта!' + LineEnding
    + 'Возникла критическая ошибка. Перезапустите приложение.' + LineEnding
    + 'Завершить работу приложения?';

  WARN_SETTINGS = 'Не удалось применить все настройки!' + LineEnding
    + 'Возникла критическая ошибка. Перезапустите приложение.' + LineEnding
    + 'Завершить работу приложения?';

  WARN_NOREDO = 'Внимание! История правок символов будет очищена. '
    + 'Данную операцию невозможно отменить после применения.';

  WARN_RESET = 'Приложение будет закрыто, а текущие настройки вернутся к значениям по умолчанию.' + LineEnding
    + 'Вы действительно хотите сбросить настройки?';

  TXT_CONFIRM = 'Подтверждение';
  TXT_WARNING = 'Предупреждение';
  TXT_ERROR   = 'Ошибка';
  TXT_RESET   = 'Сброс';


  { Theme }

  TXT_THEME_ALLOWDARK = 'Системная';
  TXT_THEME_LIGHT     = 'Светлая';
  TXT_THEME_DARK      = 'Темная';


const
  TXT_THEME: array[0..2] of String = (TXT_THEME_ALLOWDARK, TXT_THEME_LIGHT, TXT_THEME_DARK);


implementation

end.

