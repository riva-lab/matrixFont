unit u_strings;

{$mode objfpc}{$H+}

interface

resourcestring

  WARN_NOTSAVED = 'Изменения в текущем файле не сохранены.' + LineEnding
    + 'Сохранить?';

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

  WARN_IMPORT = 'В изображении карты символов обнаружены метаданные,' + LineEnding
    + 'которые позволяют импортировать весь шрифт.' + LineEnding
    + 'Выполнить импортирование?';

  WARN_UPDATE = 'Сейчас скачивается обновление.' + LineEnding
    + 'Прервать и закрыть приложение?';

  TXT_CONFIRM = 'Подтверждение';
  TXT_WARNING = 'Предупреждение';
  TXT_ERROR   = 'Ошибка';
  TXT_RESET   = 'Сброс';


  { File size }

  TXT_BYTE_SIZE       = 'Размер: %s';
  TXT_BYTE_SHORT      = 'Б';
  TXT_BYTE_KB         = 'КБ';
  TXT_BYTE_MB         = 'МБ';
  TXT_BYTE_GB         = 'ГБ';


  { Update }

  TXT_UPD_CHECKING    = 'Поиск обновлений...';
  TXT_UPD_NEWVER      = 'Доступна новая версия: v%d.%d.%d.%d (%s)';
  TXT_UPD_UPTODATE    = 'Приложение актуально. Обновление не требуется.';
  TXT_UPD_DOWNLOADING = 'Скачивание обновления, %s...';
  TXT_UPD_UNZIPPING   = 'Распаковка...';
  TXT_UPD_READY       = 'Все готово для обновления. Перезапустите приложение для завершения.';
  TXT_UPD_ERROR       = 'Произошла ошибка при обновлении. Повторите попытку еще раз.';
                
  TXT_UPD_AUTO        = 'Автоматически';
  TXT_UPD_CHECK       = 'Только проверять';
  TXT_UPD_MANUAL      = 'Вручную';

  TXT_UPD_ATSTARTUP   = 'При запуске';
  TXT_UPD_DAYLY       = 'Раз в день';
  TXT_UPD_WEEKLY      = 'Раз в неделю';
  TXT_UPD_MONTHLY     = 'Раз в месяц';
  TXT_UPD_QUARTER     = 'Раз в квартал';


  { Theme }

  TXT_THEME_ALLOWDARK = 'Системная';
  TXT_THEME_LIGHT     = 'Светлая';
  TXT_THEME_DARK      = 'Темная';


type
  TAppUpdateWay  = (uwAuto, uwCheck, uwManual);
  TAppUpdateFreq = (ufAtStartup, ufDaily, ufWeekly, ufMonthly, ufQuarterly);


const
  TXT_THEME: array[0..2] of String = (TXT_THEME_ALLOWDARK, TXT_THEME_LIGHT, TXT_THEME_DARK);

  TXT_UPDATE_WAY: array[TAppUpdateWay] of String = (
    TXT_UPD_AUTO, TXT_UPD_CHECK, TXT_UPD_MANUAL);

  TXT_UPDATE_FREQ: array[TAppUpdateFreq] of String = (
    TXT_UPD_ATSTARTUP, TXT_UPD_DAYLY, TXT_UPD_WEEKLY, TXT_UPD_MONTHLY, TXT_UPD_QUARTER);

  CAppUpdateDays: array [TAppUpdateFreq] of Integer = (0, 1, 7, 30, 90);

implementation

end.

