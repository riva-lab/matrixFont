unit font;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, LConvEncoding, StrUtils, Graphics, regexpr,
  symbol, u_encodings, u_helpers;

resourcestring
  RHF_COMMENT_NAME   = 'Название шрифта';
  RHF_COMMENT_AUTHOR = 'Автор шрифта';
  RHF_COMMENT_TIME   = 'Дата и время генерации';
  RHF_COMMENT_APP    = 'Сгенерировано';
  RHF_COMMENT_CP     = 'Кодовая страница';
  RHF_COMMENT_CHAR   = 'Символ';
  RHF_CHAR_DIGIT     = 'Цифры';
  RHF_CHAR_LATCAP    = 'Латиница, прописные';
  RHF_CHAR_LAT       = 'Латиница, строчные';
  RHF_CHAR_CYRCAP    = 'Кириллица, прописные';
  RHF_CHAR_CYR       = 'Кириллица, строчные';

const
  FILE_EXTENSION     = 'RHF';
  IMPORTC_EXTENSION  = 'C,CPP,H,HPP,TXT';

type
  TQWordArray      = array of QWord;
  TFontSet         = array of TSymbol;
  TCanOptimize     = symbol.TCanOptimize;
  TDirection       = symbol.TDirection;
  TBorder          = symbol.TBorder;
  TMirror          = symbol.TMirror;
  TSymbolField     = symbol.TSymbolField;
  TClipboardAction = symbol.TClipboardAction;
  TPasteMode       = symbol.TPasteMode;
  TImportMode      = (imOwn, imAdafruit, imLCDVision, imCustom);

  TFileRHFHeader = record
    AppVersion: String[16];
    FontName:   String[80];       // название шрифта
    FontAuthor: String[80];       // автор шрифта
    //SoftName:   String[80];       // название приложения-генератора
    DateCreate: TDateTime;        // дата и время создания
    DateChange: TDateTime;        // дата и время изменения
    CharFirst:  Int32;            // код начального символа в наборе
    CharLength: Int32;            // кол-во символов в наборе
    CharWidth:  Int32;            // ширина символа в пикселях
    CharHeight: Int32;            // высота символа в пикселях
  end;

  TFileRHFHeader1 = record
    FontName:   String[80];             // название шрифта
    FontAuthor: String[80];             // автор шрифта
    AppCreate:  String[40];             // приложение создания
    AppChange:  String[40];             // приложение изменения
    Encoding:   String[20];             // кодовая страница
    DateCreate: TDateTime;              // дата и время создания
    DateChange: TDateTime;              // дата и время изменения
    CharFirst:  Int32;                  // код начального символа в наборе
    CharLength: Int32;                  // кол-во символов в наборе
    CharWidth:  Int32;                  // ширина символа в пикселях
    CharHeight: Int32;                  // высота символа в пикселях
    Reserved:   array [0..127] of Byte; // резерв
  end;

  { TFont }

  TFont = class
  private
    FAuthor:        String;        // автор шрифта
    FName:          String;        // имя шрифта
    FAppAdditional: String;        // название компании-разработчика приложения-генератора
    FAppCreate:     String;        // название приложения создания файла
    FAppChange:     String;        // название приложения изменения файла
    FAppCurrent:    String;        // название приложения, обрабатывающего файл
    FSymbol:        TFontSet;      // набор символов
    FDefPrefix:     String;        // префикс для #define-ов
    FEncoding:      String;        // кодировка (кодовая страница)

    // ================================ Поля ===================================

    FFontStartItem: Integer;       // код начального символа в наборе
    FFontLength:    Integer;       // кол-во символов в наборе
    FDateCreate:    TDateTime;     // дата и время создания
    FDateChange:    TDateTime;     // дата и время изменения

    // поля внешнего вида и параметров символа

    FWidth:               Integer; // поле - ширина символа в пикселях
    FHeight:              Integer; // поле - высота символа в пикселях
    FGridStep:            Integer; // поле - шаг сетки в пикселях
    FGridThickness:       Integer; // поле - толщина линий сетки
    FGridColor:           TColor;  // поле - цвет сетки
    FBackgroundColor:     TColor;  // поле - цвет фона
    FActiveColor:         TColor;  // поле - цвет нарисованного элемента
    FShowGrid:            Boolean; // поле - флаг видимости сетки
    FGridChessBackground: Boolean; // поле - флаг отображения сетки в виде шахматного фона

    // поля модификации символа

    FShiftRollover: Boolean;       // поле - флаг циклического режима сдвига
    FPasteMode:     TPasteMode;    // поле - режим вставки

    // поля настройки знакогенератора

    FScanColsFirst:    Boolean;     // поле - флаг очередности сканирования: столбцы-строки
    FScanColsToRight:  Boolean;     // поле - флаг направления сканирования столбцов
    FScanRowsToDown:   Boolean;     // поле - флаг направления сканирования строк
    FNumbersView:      TNumberView; // поле - настройка представления выходных чисел
    FMSBFirst:         Boolean;     // поле - порядок вывода бит (используется только в импорте из кода)
    FNumbersInversion: Boolean;     // поле - битовая инверсия представления выходных чисел
    FEmptyBits:        TEmptyBit;   // поле - настройка заполнения пустых разрядов
    FFontType:         TFontType;   // поле - тип шрифта
    FNumbersBits:      Integer;     // поле - разрядность выходных чисел
    FCommentStyle:     Integer;     // поле - тип комментария Си (0: С99, 1: С89)
    //==========================================================================

    // ========================= Внутренние методы =============================
    // Методы чтения и записи свойств
    procedure SetActiveColor(AValue: TColor);
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetFontLength(AValue: Integer);
    procedure SetFontStartItem(AValue: Integer);
    procedure SetGridChessBackground(AValue: Boolean);
    procedure SetGridColor(AValue: TColor);
    procedure SetGridThickness(AValue: Integer);
    procedure SetShiftRollover(AValue: Boolean);
    procedure SetShowGrid(AValue: Boolean);
    procedure SetWidth(AWidth: Integer);
    procedure SetHeight(AHeight: Integer);
    procedure SetGridStep(AGridStep: Integer);

  public
    // =================================== Методы ==============================

    // очистить все символы шрифта
    procedure Clear;

    // инвертировать изображение всех символов шрифта
    procedure Invert;

    // отобразить все символы шрифта
    procedure Mirror(MirrorDirection: TMirror);

    // сдвиг всех символов шрифта
    procedure Shift(Direction: TDirection);

    // прижать все символы шрифта к краю
    procedure Snap(Border: TBorder);

    // центрирование всех символов шрифта
    procedure Center(AVertical: Boolean);

    // поворот всех символов шрифта
    procedure Rotate(AClockWise: Boolean);

    // обменять местами символы в таблице
    function SwapChars(AIndex1, AIndex2: Integer): Boolean;

    // генерировать код шрифта
    function GenerateCode(StartChar: Integer = 0; EndChar: Integer = 0): String;

    // увеличение масштаба изображения всех символов шрифта
    procedure ZoomIn;

    // уменьшение масштаба изображения всех символов шрифта
    procedure ZoomOut;

    // сохранение шрифта в файл
    procedure SaveToFile(FileName: String);

    // чтение шрифта из файла
    function ReadFromFile(FileName: String): Boolean;

    // очистить историю изменений
    procedure ClearChanges;

    // отменить одну правку с конца истории
    procedure UndoChange;

    // повторить отмененную ранее правку
    procedure RedoChange;

    // пакетная вставка
    procedure Paste(AMode: TPasteMode = pmNorm);

    // импорт системного шрифта для растеризации
    procedure Import(Font: Graphics.TFont; Width, Height: Integer);

    // импорт кода C
    function Import(ACode: String; AOffset, ASkip: Integer; AType: TImportMode
      ): Boolean;

    // изменение размеров холста символов шрифта
    // при обрезке: если значение < 0, то применяем оптимизацию
    procedure ChangeSize(Up, Down, Left, Right: Integer; Crop: Boolean);

    // определение возможности усечь символ
    function CanOptimize(Direction: TCanOptimize): Integer;

    // установка диапазона символов шрифта
    procedure SetRange(StartCode: Integer = 32; EndCode: Integer = 255);

    // получение имени символа по его коду
    function GetCharName(ACode: Integer; AFull: Boolean = False): String;

    //======================== Конструкторы и деструкторы ======================
    constructor Create;
    destructor Destroy; override;
    //==========================================================================

    // =========================== Свойства ====================================

    // свойства внешнего вида и параметров символа

    // ширина символа в пикселях
    property Width: Integer read FWidth write SetWidth;

    // высота символа в пикселях
    property Height: Integer read FHeight write SetHeight;

    // шаг сетки в пикселях
    property GridStep: Integer read FGridStep write SetGridStep;

    // толщина линий сетки
    property GridThickness: Integer read FGridThickness write SetGridThickness;

    // цвет сетки
    property GridColor: TColor read FGridColor write SetGridColor;

    // флаг отображения сетки в виде шахматного фона
    property GridChessBackground: Boolean read FGridChessBackground write SetGridChessBackground;

    // цвет фона
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;

    // цвет нарисованного элемента
    property ActiveColor: TColor read FActiveColor write SetActiveColor;

    // флаг видимости сетки
    property ShowGrid: Boolean read FShowGrid write SetShowGrid;

    //--------------------------------------------------------------------------
    // свойства модификации символа

    // флаг циклического режима сдвига
    property ShiftRollover: Boolean read FShiftRollover write SetShiftRollover;

    // режим вставки из буфера обмена
    property PasteMode: TPasteMode read FPasteMode write FPasteMode;

    //--------------------------------------------------------------------------
    // свойства настройки знакогенератора

    // флаг очередности сканирования: столбцы-строки
    property ScanColsFirst: Boolean read FScanColsFirst write FScanColsFirst;

    // флаг направления сканирования столбцов
    property ScanColsToRight: Boolean read FScanColsToRight write FScanColsToRight;

    // флаг направления сканирования строк
    property ScanRowsToDown: Boolean read FScanRowsToDown write FScanRowsToDown;

    // настройка представления выходных чисел
    property NumbersView: TNumberView read FNumbersView write FNumbersView;

    // порядок вывода бит (используется только в импорте из кода)
    property MSBFirst: Boolean read FMSBFirst write FMSBFirst;

    // битовая инверсия представления выходных чисел
    property NumbersInversion: Boolean read FNumbersInversion write FNumbersInversion;

    // настройка заполнения пустых разрядов
    property EmptyBits: TEmptyBit read FEmptyBits write FEmptyBits;

    // тип шрифта
    property FontType: TFontType read FFontType write FFontType;

    // разрядность выходных чисел
    property NumbersBits: Integer read FNumbersBits write FNumbersBits;

    // тип комментария Си (0: С99, 1: С89)
    property CommentStyle: Integer read FCommentStyle write FCommentStyle;

    // код начального символа в наборе
    property FontStartItem: Integer read FFontStartItem write SetFontStartItem;

    // кол-во символов в наборе
    property FontLength: Integer read FFontLength write SetFontLength;

    // массив символов шрифта
    property Item: TFontSet read FSymbol write FSymbol;

    // свойство шрифта: имя
    property Name: String read FName write FName;

    // свойство шрифта: автор
    property Author: String read FAuthor write FAuthor;

    // свойство шрифта: дополнительная информация (для генератора)
    property AppAdditional: String read FAppAdditional write FAppAdditional;

    // свойство шрифта: название приложение создания
    property AppCreate: String read FAppCreate write FAppCreate;

    // свойство шрифта: название приложения изменения файла
    property AppChange: String read FAppChange write FAppChange;

    // название приложения, обрабатывающего файл
    property AppCurrent: String read FAppCurrent write FAppCurrent;

    // свойство шрифта: префикс для #define-ов
    property DefPrefix: String read FDefPrefix write FDefPrefix;

    // свойство шрифта: дата и время создания
    property DateCreate: TDateTime read FDateCreate;

    // свойство шрифта: дата и время изменения
    property DateChange: TDateTime read FDateChange;

    // свойство шрифта: кодировка (кодовая страница)
    property Encoding: String read FEncoding write FEncoding;
  end;

// функция латинизации кириллицы (транслитерация)
function Transliterate(cyr: String): String;

implementation

// функция латинизации кириллицы (транслитерация)
function Transliterate(cyr: String): String;
  const
    lat_h: array [0..31] of String = (
      'A', 'B', 'V', 'G', 'D', 'E', 'ZH', 'Z', 'I', 'J',
      'K', 'L', 'M', 'N', 'O', 'P', 'R', 'S', 'T', 'U',
      'F', 'H', 'TS', 'CH', 'SH', 'SCH', '', 'Y', '', 'E',
      'JU', 'JA');
    lat_l: array [0..31] of String = (
      'a', 'b', 'v', 'g', 'd', 'e', 'zh', 'z', 'i', 'j',
      'k', 'l', 'm', 'n', 'o', 'p', 'r', 's', 't', 'u',
      'f', 'h', 'ts', 'ch', 'sh', 'sch', '', 'y', '', 'e',
      'ju', 'ja');
  var
    i, len: Integer;
  begin
    cyr    := UTF8ToWinCP(cyr);
    len    := Length(cyr);
    Result := '';
    for i := 1 to len do
      case Ord(cyr[i]) of
        0..31, 33..47, 58..63, 91..94, 96, 123..191:
          Result := Result + '_';
        32:
          Result := Result + ' ';
        48..57, 65..90, 95, 97..122:
          Result := Result + cyr[i];
        192..223:
          Result := Result + lat_h[Ord(cyr[i]) - 192];
        224..255:
          Result := Result + lat_l[Ord(cyr[i]) - 224];
        end;
  end;

{ -----
  TFont
  ----- }

// очистить все символы шрифта
procedure TFont.Clear;
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      begin
      FSymbol[i - 1].Clear;
      FSymbol[i - 1].SaveChange;
      end;
  end;

// инвертировать изображение всех символов шрифта
procedure TFont.Invert;
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      begin
      FSymbol[i - 1].Invert;
      FSymbol[i - 1].SaveChange;
      end;
  end;

// отобразить все символы шрифта
procedure TFont.Mirror(MirrorDirection: TMirror);
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      begin
      FSymbol[i - 1].Mirror(MirrorDirection);
      FSymbol[i - 1].SaveChange;
      end;
  end;

// сдвиг всех символов шрифта
procedure TFont.Shift(Direction: TDirection);
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      begin
      FSymbol[i - 1].Shift(Direction);
      FSymbol[i - 1].SaveChange;
      end;
  end;

// прижать все символы шрифта к краю
procedure TFont.Snap(Border: TBorder);
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      begin
      FSymbol[i - 1].Snap(Border);
      FSymbol[i - 1].SaveChange;
      end;
  end;

// центрирование всех символов шрифта
procedure TFont.Center(AVertical: Boolean);
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      begin
      FSymbol[i - 1].Center(AVertical);
      FSymbol[i - 1].SaveChange;
      end;
  end;

// поворот всех символов шрифта
procedure TFont.Rotate(AClockWise: Boolean);
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      begin
      FSymbol[i - 1].Rotate(AClockWise);
      FSymbol[i - 1].SaveChange;
      end;
  end;

// обменять местами символы в таблице
function TFont.SwapChars(AIndex1, AIndex2: Integer): Boolean;

  procedure DrawNewChar(AIndex: Integer; AData: TSymbolField);
    var
      action: array[Boolean] of TPixelAction = (paClear, paSet);
      w, h:   Integer;
    begin
      for w := 0 to FWidth - 1 do
        for h := 0 to FHeight - 1 do
          FSymbol[AIndex].PixelAction(w, h, action[AData[w, h]]);

      SetLength(AData, 0, 0);
    end;

  var
    sf1, sf2: TSymbolField;
    w, h, i:  Integer;

  begin
    Result := False;
    if not (AIndex1 in [0..FFontLength - 1]) then Exit;
    if not (AIndex2 in [0..FFontLength - 1]) then Exit;

    SetLength(sf1, FWidth, FHeight);
    SetLength(sf2, FWidth, FHeight);

    for w := 0 to FWidth - 1 do
      for h := 0 to FHeight - 1 do
        begin
        sf1[w, h] := FSymbol[AIndex1].Symbol[w, h];
        sf2[w, h] := FSymbol[AIndex2].Symbol[w, h];
        end;

    DrawNewChar(AIndex2, sf1);
    DrawNewChar(AIndex1, sf2);

    for i := 1 to FFontLength do
      FSymbol[i - 1].SaveChange;

    Result := True;
  end;

// генерировать код шрифта
function TFont.GenerateCode(StartChar: Integer; EndChar: Integer): String;
  const
    tab_spaces = 32;
  var
    id, dfs, dfl, dfw, dfh, dft, dfa, ew: String;
    com_s, com_e, com_m: String;

    ch_s, ch_e: Integer;
    i, len, len_c: Integer;

    s:       String = '';
    muls:    String = '';
    int_str: array[0..3] of String = (
      'unsigned char ',
      'unsigned short',
      'unsigned long ',
      'unsigned long ');

  begin
    if FCommentStyle = 0 then
      begin
      com_s := '// ';  // начало комментария С99
      com_m := '// ';  // продолжение комментария С99
      com_e := ' ';    // конец комментария С99
      end
    else
      begin
      com_s := '/* '; // начало комментария С89
      com_m := '   '; // продолжение комментария С89
      com_e := ' */'; // конец комментария С89
      end;

    if (StartChar > 0) and (StartChar > FFontStartItem) then
      ch_s := StartChar - FFontStartItem
    else
      ch_s := 0;

    if (EndChar > 0) and (EndChar >= StartChar) and (EndChar < FFontStartItem + FFontLength) then
      ch_e := EndChar - FFontStartItem
    else
      ch_e := FFontLength - 1;

    if FDefPrefix[1] in ['0'..'9'] then
      FDefPrefix := '_' + FDefPrefix;
    id           := Transliterate(FDefPrefix);
    len          := (length(id) - 10) and not 3 + 4;
    if len < 0 then
      len := 0;
    len := len + tab_spaces;

    dfl := (id) + '_LENGTH';
    dfs := (id) + '_START_CHAR';
    dfw := (id) + '_CHAR_WIDTH';
    dfh := (id) + '_CHAR_HEIGHT';
    dft := (id) + '_FONT_TYPE';
    dfa := (id) + '_ARRAY_LENGTH';

    if FScanColsFirst then
      begin
      ew     := dfw;
      if (FHeight - 1) div FNumbersBits > 0 then
        muls := ' * ' + IntToStr((FHeight - 1) div FNumbersBits + 1);
      end
    else
      begin
      ew     := dfh;
      if (FWidth - 1) div FNumbersBits > 0 then
        muls := ' * ' + IntToStr((FWidth - 1) div FNumbersBits + 1);
      end;

    // информационный блок
    len_c := 0;
    if UTF8Length(RHF_COMMENT_NAME) > len_c then len_c := UTF8Length(RHF_COMMENT_NAME);
    if UTF8Length(RHF_COMMENT_AUTHOR) > len_c then len_c := UTF8Length(RHF_COMMENT_AUTHOR);
    if UTF8Length(RHF_COMMENT_TIME) > len_c then len_c := UTF8Length(RHF_COMMENT_TIME);
    if UTF8Length(RHF_COMMENT_APP) > len_c then len_c := UTF8Length(RHF_COMMENT_APP);
    if UTF8Length(RHF_COMMENT_CP) > len_c then len_c := UTF8Length(RHF_COMMENT_CP);
    len_c := ((len_c + 3) div 4 + 1) * 4 - 3;

    s += com_s + UTF8PadRight(RHF_COMMENT_NAME, len_c) + FName + LineEnding;
    s += com_m + UTF8PadRight(RHF_COMMENT_AUTHOR, len_c) + FAuthor + LineEnding;
    s += com_m + UTF8PadRight(RHF_COMMENT_TIME, len_c) + DateTimeToStr(Now) + LineEnding;
    s += com_m + UTF8PadRight(RHF_COMMENT_APP, len_c) + FAppCurrent + LineEnding;
    s += com_m + UTF8PadRight(RHF_COMMENT_CP, len_c) + GetEncodingCaption(FEncoding) + LineEnding;
    s += com_m + FAppAdditional;
    s += com_e + LineEnding + LineEnding;

    // защита от множественных include
    s += '#ifndef ' + UpperCase(id) + '_H' + LineEnding;
    s += '#define ' + UpperCase(id) + '_H' + LineEnding + LineEnding;

    // объявление используемых констант
    s += '#ifndef FONT_TYPE_MONOSPACED' + LineEnding;
    s += PadRight('#define FONT_TYPE_MONOSPACED', len) + '0' + LineEnding;
    s += '#endif' + LineEnding + LineEnding;
    s += '#ifndef FONT_TYPE_PROPORTIONAL' + LineEnding;
    s += PadRight('#define FONT_TYPE_PROPORTIONAL', len) + '1' + LineEnding;
    s += '#endif' + LineEnding + LineEnding;

    // объявление константных параметров шрифта
    s += PadRight('#define ' + dfl, len) + IntToStr(ch_e - ch_s + 1) + LineEnding;
    s += PadRight('#define ' + dfs, len) + IntToStr(FFontStartItem + ch_s) + LineEnding;
    s += PadRight('#define ' + dfw, len) + IntToStr(FWidth) + LineEnding;
    s += PadRight('#define ' + dfh, len) + IntToStr(FHeight) + LineEnding;
    if FFontType = ftMONOSPACE then
      s += PadRight('#define ' + dft, len) + '(FONT_TYPE_MONOSPACED)' + LineEnding
    else
      s += PadRight('#define ' + dft, len) + '(FONT_TYPE_PROPORTIONAL)' + LineEnding;

    s += PadRight('#define ' + dfa, len);
    s += '(' + dfl + ' * ';

    if FFontType = ftPROPORTIONAL then
      s += '(1 + ';
    s   += ew + muls;

    if FFontType = ftPROPORTIONAL then
      s += ')';
    s   += ')' + LineEnding;

    // объявление массива данных
    s += LineEnding;
    s += 'const ' + int_str[FNumbersBits div 8 - 1] + ' ' + LowerCase(id);

    s += '[' + dfa + '] =' + LineEnding;
    s += '{' + LineEnding;

    // генерация массива данных
    for i := ch_s to ch_e do
      begin

      // разделение набора на логические блоки
      case FFontStartItem + i of
        48: s  += LineEnding + AddChar(' ', '', 8) + com_s +
            'Digits / ' + RHF_CHAR_DIGIT + com_e + LineEnding;
        58: s  += LineEnding;
        65: s  += LineEnding + AddChar(' ', '', 8) + com_s +
            'Roman Capitals / ' + RHF_CHAR_LATCAP + com_e + LineEnding;
        91: s  += LineEnding;
        97: s  += LineEnding + AddChar(' ', '', 8) + com_s +
            'Roman Smalls / ' + RHF_CHAR_LAT + com_e + LineEnding;
        123: s += LineEnding;
        192: if FEncoding = EncodingCP1251 then
            s += LineEnding + AddChar(' ', '', 8) + com_s +
              'Cyrillic Capitals / ' + RHF_CHAR_CYRCAP + com_e + LineEnding;
        224: if FEncoding = EncodingCP1251 then
            s += LineEnding + AddChar(' ', '', 8) + com_s +
              'Cyrillic Smalls / ' + RHF_CHAR_CYR + com_e + LineEnding;
        end;

      // бинарный код символа
      s += AddChar(' ', '', 4) + FSymbol[i].GenerateCode(
        FScanColsFirst,
        FScanColsToRight,
        FScanRowsToDown,
        FNumbersInversion,
        FNumbersView,
        FEmptyBits,
        FFontType,
        FNumbersBits);

      if i = ch_e then
        s += ' '
      else
        s += ',';

      // комментарий к символу
      s += ' ' + com_s + RHF_COMMENT_CHAR + ' ';
      s += PadRight(IntToStr(FFontStartItem + i), 4);
      s += '<' + GetCharName(FFontStartItem + i, True);
      s += '>' + com_e + LineEnding;
      end;

    // конец массива данных
    s += '};' + LineEnding + LineEnding;

    // защита от множественных include - завершение
    s += '#endif ' + com_s + UpperCase(id) + '_H' + com_e + LineEnding;

    Result := s;
  end;

// увеличение масштаба изображения всех символов шрифта
procedure TFont.ZoomIn;
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      FSymbol[i - 1].ZoomIn;
  end;

// уменьшение масштаба изображения всех символов шрифта
procedure TFont.ZoomOut;
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      FSymbol[i - 1].ZoomOut;
  end;

// сохранение шрифта в файл
procedure TFont.SaveToFile(FileName: String);
  var
    file_header: file of TFileRHFHeader1;
    file_binary: file of Byte;
    header:      TFileRHFHeader1;
    i, w, h:     Integer;
    tmp_byte:    Byte = 0;
    mask:        Longint = 1;
  begin
    // установка полей заголовка файла
    FDateChange := Now;
    FAppChange  := FAppCurrent;
    with header do
      begin
      FontName   := FName;
      FontAuthor := FAuthor;
      DateCreate := FDateCreate;
      DateChange := FDateChange;
      CharFirst  := FFontStartItem;
      CharLength := FFontLength;
      CharHeight := FHeight;
      CharWidth  := FWidth;
      AppCreate  := FAppCreate;
      AppChange  := FAppChange;
      Encoding   := FEncoding;
      end;

    // запись заголовка файла
    AssignFile(file_header, FileName);
    Rewrite(file_header);
    Write(file_header, header);
    CloseFile(file_header);

    // --- дописывание данных о символах ---

    AssignFile(file_binary, FileName);
    Reset(file_binary);
    Seek(file_binary, SizeOf(TFileRHFHeader1)); // переход в конец файла - начало секции данных

    // запись данных символов (старый формат) {file_: file of Boolean;}
    //for i := 1 to FFontLength do
    //  for h := 0 to FHeight - 1 do
    //    for w := 0 to FWidth - 1 do
    //      Write(file_, FSymbol[i - 1].Symbol[w, h]);

    // запись данных символов (новый формат: побитовая запись)
    for i := 1 to FFontLength do
      for h := 0 to FHeight - 1 do
        for w := 0 to FWidth - 1 do
          begin
          if FSymbol[i - 1].Symbol[w, h] then
            tmp_byte := tmp_byte or mask;

          mask := (mask shl 1) and $FF;
          if mask = 0 then
            begin
            Write(file_binary, tmp_byte);
            tmp_byte := 0;
            mask     := 1;
            end;
          end;
    Write(file_binary, tmp_byte);

    CloseFile(file_binary);
  end;

// чтение шрифта из файла
function TFont.ReadFromFile(FileName: String): Boolean;
  var
    file_header: file of TFileRHFHeader1;
    file_binary: file of Byte;
    header:      TFileRHFHeader1;
    symbol_tmp:  TSymbolField;
    i, w, h:     Integer;
    tmp_byte:    Byte = 0;
    mask:        Longint = 0;
  begin
    if not FileExists(FileName) then Exit(False);
    Result := False;

    // чтение заголовка файла
    AssignFile(file_header, FileName);
    Reset(file_header);
    Read(file_header, header);
    CloseFile(file_header);

    // установка свойств шрифта
    with header do
      begin
      Result := (CharFirst in [0..255]) and ((CharLength - 1) in [0..255])
        and (CharHeight in [1..150]) and (CharWidth in [1..150]);

      if not Result then Exit(Result);

      FName         := FontName;
      FAuthor       := FontAuthor;
      FAppCreate    := AppCreate;
      FAppChange    := AppChange;
      FDateCreate   := DateCreate;
      FDateChange   := DateChange;
      FontStartItem := CharFirst;
      FontLength    := CharLength;
      Height        := CharHeight;
      Width         := CharWidth;
      FEncoding     := GetEncodingAdapted(Encoding);
      end;

    // --- чтение данных о символах ---

    AssignFile(file_binary, FileName);
    Reset(file_binary);

    // переход к началу секции данных
    Seek(file_binary, SizeOf(TFileRHFHeader1));

    // инициализация временного массива для символа
    SetLength(symbol_tmp, FWidth, FHeight);

    // чтение данных символов (старый формат) {file_: file of Boolean;}
    //for i := 1 to FFontLength do
    //  begin
    //  for h := 0 to FHeight - 1 do
    //    for w := 0 to FWidth - 1 do
    //      Read(file_, symbol_tmp[w, h]);
    //  FSymbol[i - 1].LoadSymbol(@symbol_tmp);
    //  end;

    // чтение данных символов (новый формат: побитовая запись)
      try
      for i := 1 to FFontLength do
        begin
        for h := 0 to FHeight - 1 do
          for w := 0 to FWidth - 1 do
            begin
            if mask = 0 then
              begin
              Read(file_binary, tmp_byte);
              mask := 1;
              end;

            symbol_tmp[w, h] := (tmp_byte and mask) <> 0;
            mask             := (mask shl 1) and $FF;
            end;

        FSymbol[i - 1].LoadSymbol(@symbol_tmp);
        end;
      except
      Result := False; // файл поврежден
      end;

    CloseFile(file_binary);
  end;

// очистить историю изменений
procedure TFont.ClearChanges;
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      FSymbol[i - 1].ClearChanges;
  end;

// отменить одну правку с конца истории
procedure TFont.UndoChange;
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      FSymbol[i - 1].UndoChange;
  end;

// повторить отмененную ранее правку
procedure TFont.RedoChange;
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      FSymbol[i - 1].RedoChange;
  end;

// пакетная вставка
procedure TFont.Paste(AMode: TPasteMode);
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      begin
      FSymbol[i - 1].ClipboardAction(cbPaste, AMode);
      FSymbol[i - 1].SaveChange;
      end;
  end;

// импорт системного шрифта для растеризации
procedure TFont.Import(Font: Graphics.TFont; Width, Height: Integer);
  var
    i: Integer;
  begin
    SetWidth(Width);
    SetHeight(Height);

    for i := 1 to FFontLength do
      begin
      FSymbol[i - 1].Import(Font, FFontStartItem + i - 1, FEncoding);
      FSymbol[i - 1].SaveChange;
      end;
  end;

// импорт кода C
function TFont.Import(ACode: String; AOffset, ASkip: Integer; AType: TImportMode): Boolean;

  type
    TGFXglyph = record           // Data stored PER GLYPH
      bitmapOffset:     Integer; //  Pointer into GFXfont->bitmap
      bwidth, bheight:  Integer; //  Bitmap dimensions in pixels
      xAdvance:         Integer; //  Distance to advance cursor (x axis)
      xOffset, yOffset: Integer; //  Dist from cursor pos to UL corner
      end;

    TGFXArray = array of TGFXglyph;

  function NormalizeCode(AStr: String): String;
    var
      i: Integer;
      r: array of String = (         // Transformations:
        '\/\*(?:.|\s)*?\*\/', ' ',   // - remove block comments
        '\/\/.*?$', ' ',             // - remove line comments
        ',', ', ',                   // - commas
        '\{', ' { ',                 // - opening brace {
        '\}', ' } '                  // - closing brace }
        );
    begin
      Result := AStr;

      for i := 0 to High(r) div 2 do
        Result := ReplaceRegExpr(
          r[i * 2], Result, r[i * 2 + 1], [rroModifierI, rroModifierM]);

      Result := Result.Replace(#13, #10).Replace(#10, ' ');

      for i := 0 to 8 do
        Result := Result.Replace('  ', ' ');
    end;

  function GetBits(AStr: String): Integer;
    begin
      Result := 0;
      case LowerCase(AStr) of

        'char', 'byte', 'uint8_t':
          Result := 8;

        'short', 'uint16_t':
          Result := 16;

        'uint24_t':
          Result := 24;

        'int', 'long', 'uint32_t':
          Result := 32;

        'long long', 'uint64_t':
          Result := 64;
        end;
    end;

  function GetValue(APrefix, AStr: String): QWord;
    var
      prefix: Char;
    begin
      case LowerCase(APrefix) of
        '0x':
          prefix := '$';
        '0b':
          prefix := '%';
        '0':
          prefix := '&';
        else
          prefix := ' ';
        end;

      Result := StrToQWord(prefix + AStr);
    end;

  function GetValueInt(APrefix, AStr: String): Integer;
    begin
      Result := GetValue(APrefix, AStr);
      if APrefix = '-' then Result := -Result;
    end;

  function StrToArray(AStr: String; var AArray: TQWordArray; out OBits: Integer; out OName: String): Boolean;
    label
      _end;
    var
      re:        TRegExpr;
      byteCount: Integer = 0;
    begin
      Result := False;
      re     := TRegExpr.Create;

        try
        // STAGE 1. Get array as string from code
        re.ModifierI   := True;
        re.InputString := UnicodeString(AStr);
        re.Expression  :=
          '(?: (char|byte|int|short|long|long long|uint8_t|uint16_t|uint24_t' +
          '|uint32_t|uint64_t) (?:[^;:,]*? )?)?([a-z_][a-z\d_]*?)\[( ?' +
          '[_a-z][a-z\d_]*? ?)?\](?:[^=]*?)=\s*?\{([,a-z\d\s#_]*?)\}';
        if not re.Exec then goto _end;

        AStr  := String(re.Match[4]) + ',';    // array as string
        OBits := GetBits(String(re.Match[1]));
        OName := String(re.Match[2]);

        // STAGE 2. Read data from array string to array
        re.InputString := UnicodeString(AStr);
        re.Expression  := '[,\s](0x|0b|0)?([\da-f]+?)(?:,| )';
        SetLength(AArray, Length(AStr));

        if re.Exec then
          repeat
            AArray[byteCount] := GetValue(String(re.Match[1]), String(re.Match[2]));
            Inc(byteCount);
          until not re.ExecNext;

        Result := True;

        _end: ;
        except
        end;

      SetLength(AArray, byteCount);
      re.Free;
    end;

  function StrToArrayAdafruit(AStr: String; var AArray: TQWordArray; var AGFXArray: TGFXArray; out OName: String): Boolean;
    label
      _end;
    var
      re:           TRegExpr;
      byteCount:    Integer = 0;
      xAdvMax:      Integer = 0;
      intVal:       Integer;
      _bmpID, _gID: String;
    begin
      Result := False;
      re     := TRegExpr.Create;

      // STAGE 1. Get GFXfont structure from code
      //   typedef struct { // Data stored for FONT AS A WHOLE:
      //     uint8_t  *bitmap;      // Glyph bitmaps, concatenated
      //     GFXglyph *glyph;       // Glyph array
      //     uint8_t   first, last; // ASCII extents
      //     uint8_t   yAdvance;    // Newline distance (y axis)
      //   } GFXfont;
      re.ModifierI   := True;
      re.InputString := UnicodeString(AStr);
      re.Expression  :=
        '\s+?GFXfont(?:\s+[\w\d_]*)?\s+?([a-z_][a-z\d_]*?)\s+?(?:[^{]*?)\s*?=\s*\{\s*' +
        '(?:\([^)]*\))?([a-z_][a-z\d_]*?)\s*,\s*' +
        '(?:\([^)]*\))?([a-z_][a-z\d_]*?)\s*,\s*' +
        '[,\s](0x|0b|0)?([\da-f]+?)\s*,\s*' +
        '[,\s](0x|0b|0)?([\da-f]+?)\s*,\s*' +
        '[,\s](0x|0b|0)?([\da-f]+?)\s*\}';
      if not re.Exec then goto _end;

      OName         := String(re.Match[1]);
      _bmpID        := String(re.Match[2]);
      _gID          := String(re.Match[3]);
      FontStartItem := GetValue(String(re.Match[4]), String(re.Match[5]));
      FontLength    := GetValue(String(re.Match[6]), String(re.Match[7])) - FontStartItem + 1;
      Height        := Round(GetValue(String(re.Match[8]), String(re.Match[9])) * 1.8);

      // STAGE 2. Get array of GFXglyph structures as string
      re.InputString := UnicodeString(AStr);
      re.Expression  :=
        '\s+?GFXglyph(?:\s+[\w\d_]*)?\s+?' + _gID +
        '\s*?\[\]\s+?(?:[^{]*?)\s*?=\s*\{(.*?)\}\s*;';
      if not re.Exec then goto _end;

      SetLength(AArray, Length(AStr));
      SetLength(AGFXArray, 256);

      // STAGE 3. Read glyphs data
      re.InputString := re.Match[1]; // glyphs structure
      re.Expression  := '[,\s](0x|0b|0|-)?([\da-f]+?)(?:,| )';
      if not re.Exec then
        goto _end
      else
        repeat
          intVal := GetValueInt(String(re.Match[1]), String(re.Match[2]));
          case byteCount mod 6 of
            0: AGFXArray[byteCount div 6].bitmapOffset := intVal;
            1: AGFXArray[byteCount div 6].bwidth := intVal;
            2: AGFXArray[byteCount div 6].bheight := intVal;
            3:
              begin
              AGFXArray[byteCount div 6].xAdvance := intVal;
              if xAdvMax < intVal then xAdvMax := intVal; // accumulate max value
              end;
            4: AGFXArray[byteCount div 6].xOffset := intVal;
            5: AGFXArray[byteCount div 6].yOffset := intVal;
            end;

          Inc(byteCount);
        until not re.ExecNext;

      Width := Round(xAdvMax * 1.8) + 8;

      // STAGE 4. Get bitmap array as string
      re.InputString := UnicodeString(AStr);
      re.Expression  :=
        '\s+?uint8_t(?:\s+[\w\d_]*)?\s+?' + _bmpID +
        '\s*?\[\]\s+?(?:[^{]*?)\s*?=\s*\{(.*?)\}\s*;';
      if not re.Exec then goto _end;

      // STAGE 5. Read bitmap data
      byteCount      := 0;
      re.InputString := re.Match[1]; // bitmap array
      re.Expression  := '[,\s](0x|0b|0)?([\da-f]+?)(?:,| )';
      if not re.Exec then
        goto _end
      else
        repeat
          AArray[byteCount] := GetValue(String(re.Match[1]), String(re.Match[2]));
          Inc(byteCount);
        until not re.ExecNext;

      SetLength(AArray, byteCount);
      Result := True;

      _end: ;
      re.Free;
    end;

  procedure SwapInts(var Int1, Int2: Integer);
    var
      tmp: QWord;
    begin
      tmp  := Int1;
      Int1 := Int2;
      Int2 := tmp;
    end;

  procedure GetMatrixFontParams(ACode: String);

    function GetMatrixFontParamInt(AKey: String): Integer;
      begin
        Result := 0;
        with TRegExpr.Create do
            try
            ModifierI   := True;
            InputString := ACode;
            Expression  := AKey + '\s+?(0x|0b|0)?([\da-f]+)';
            if Exec then Result := GetValue(Match[1], Match[2]);
            finally
            Free;
            end;
      end;

    function GetMatrixFontParamStr(AKey: String): String;
      begin
        Result := '';
        with TRegExpr.Create do
            try
            ModifierI   := True;
            InputString := ACode;
            Expression  := AKey + '\s+?\((\w[^\s]*)\)';
            if Exec then Result := Match[1];
            finally
            Free;
            end;
      end;

    begin
      Width         := GetMatrixFontParamInt('char_width');
      Height        := GetMatrixFontParamInt('char_height');
      FontStartItem := GetMatrixFontParamInt('start_char');
      FontLength    := GetMatrixFontParamInt('length');
      NumbersBits   := 0;
      AOffset       := 0;
      ASkip         := 0;
      if GetMatrixFontParamStr('font_type') = 'FONT_TYPE_PROPORTIONAL' then
        ASkip := 1;
      FontType := TFontType(ASkip);
    end;

  procedure GetLCDVisionFontParams(AArray: TQWordArray);
    var
      i: Integer;
      w: QWord = 0;
    begin
        try
        if Length(AArray) < 4 then Exit;
        Width         := Integer(AArray[0] and $3FF);
        Height        := Integer(AArray[1] and $3FF);
        FontStartItem := Integer(AArray[2] and $3FF);
        FontLength    := Integer(AArray[3] and $3FF);
        NumbersBits   := 8;
        AOffset       := 4;
        ASkip         := 0;
        FontType      := ftMONOSPACE;

        if Width = 0 then
          begin
          if Length(AArray) < AOffset + FontLength then Exit;
          for i := 0 to FontLength - 1 do
            if AArray[AOffset + i] > w then w := AArray[AOffset + i];

          Width    := Integer(w and $3FF);
          AOffset  += FontLength;
          FontType := ftPROPORTIONAL;
          end;

        if ExecRegExpr('#ifndef\s+?_GLCD_DATA_BYTEY_', ACode) then
          ScanColsFirst := False;
        except
        end;
    end;

  procedure AdjustParameters(AArray: TQWordArray);
    begin
      case AType of

        imOwn:
          GetMatrixFontParams(ACode);

        imAdafruit:
          begin
          FontType    := ftPROPORTIONAL;
          NumbersBits := 8;
          AOffset     := 0;
          ASkip       := 0;
          end;

        imLCDVision:
          GetLCDVisionFontParams(AArray);
        end;
    end;

  procedure ChangeBitOrder(var AArray: TQWordArray);

    function ChangeBitOrder(AValue: QWord): QWord;
      var
        i: Integer;
      begin
        Result   := 0;
        for i    := 1 to FNumbersBits do
          begin
          Result := Result shl 1;
          if (AValue and 1) <> 0 then
            Result := Result or 1;
          AValue := AValue shr 1;
          end;
      end;

    var
      i: Integer;
    begin
      for i := 0 to High(AArray) do
        AArray[i] := ChangeBitOrder(AArray[i]);
    end;

  procedure ReadAdafruitChar(AChar: Integer; AGFXArr: TGFXArray; ABitmap: TQWordArray);
    var
      x:     Integer = 0;
      y:     Integer = 0;
      b, i:  Integer;
      _data: QWord;
    begin
      if AGFXArr[AChar].bwidth = 0 then Exit;
      if AGFXArr[AChar].bheight = 0 then Exit;
      i := AGFXArr[AChar].bitmapOffset;

      while True do
        begin
        _data := ABitmap[i];
        Inc(i);

        for b := 0 to 7 do
          begin
          FSymbol[AChar].PixelAction(
            x + AGFXArr[AChar].xOffset + 8,
            y + AGFXArr[AChar].yOffset + Round(Height / 1.8) + 1,
            (_data and $80 > 0).Select(paSet, paClear));

          _data := _data shl 1;
          Inc(x);
          if x >= AGFXArr[AChar].bwidth then
            begin
            x := 0;
            Inc(y);
            if y >= AGFXArr[AChar].bheight then Exit;
            end;
          end;
        end;
    end;

  procedure ReadChar();
    begin
      { #todo : перенести сюда код }
    end;

  var
    values:             TQWordArray;
    gfxArr:             TGFXArray;
    currentValue:       QWord;
    ready:              Boolean;
    i, j, b, sx, sy:    Integer;
    ch, chSize, chLine: Integer;

  begin
    Clear;
    ClearChanges;
    Result := False;
    ACode  := NormalizeCode(ACode);

    if AType = imAdafruit then
      ready := StrToArrayAdafruit(ACode, values, gfxArr, FName)
    else
      ready := StrToArray(ACode, values, b, FName);

    if not ready then Exit;
    AdjustParameters(values);

    // check fields
    if AOffset > High(values) then Exit;
    if FNumbersBits = 0 then FNumbersBits := b;
    if FNumbersBits = 0 then Exit;
    if FWidth = 0 then Exit;
    if FHeight = 0 then Exit;
    if FFontLength = 0 then Exit;

    sx := FWidth;
    sy := FHeight;
    if FScanColsFirst then SwapInts(sx, sy);
    if not (AType in [imAdafruit, imLCDVision]) then
      if FMSBFirst then ChangeBitOrder(values);

    chLine := (sx - 1) div FNumbersBits + 1;
    chSize := chLine * sy - 1;
    i      := AOffset;
    Result := True;

    for ch := 0 to FFontLength - 1 do
      if AType = imAdafruit then
        ReadAdafruitChar(ch, gfxArr, values)
      else
        begin
        i += ASkip;

        if (AType = imLCDVision) and (FontType = ftPROPORTIONAL) then
          begin
          chLine := (values[4 + ch] - 1) div FNumbersBits + 1;
          chSize := chLine * Height - 1;
          end;

        for j := 0 to chSize do
          begin
          if i > High(values) then Exit;
          currentValue := values[i];

          for b := 0 to FNumbersBits - 1 do
            begin
            sx := (j mod chLine) * FNumbersBits + b;
            sy := j div chLine;
            if FScanColsFirst then SwapInts(sx, sy);

            FSymbol[ch].PixelAction(sx, sy,
              (currentValue and 1 > 0).Select(paSet, paClear));

            currentValue := currentValue shr 1;
            end;

          Inc(i);
          end;
        end;
  end;

// изменение размеров холста символов шрифта
procedure TFont.ChangeSize(Up, Down, Left, Right: Integer; Crop: Boolean);
  var
    i: Integer;
  begin
    if Crop then // при обрезке: если значение < 0, то применяем оптимизацию
      begin
      if Up < 0 then Up       := CanOptimize(TCanOptimize.coUp);
      if Down < 0 then Down   := CanOptimize(TCanOptimize.coDown);
      if Left < 0 then Left   := CanOptimize(TCanOptimize.coLeft);
      if Right < 0 then Right := CanOptimize(TCanOptimize.coRight);
      end;

    for i := 1 to FFontLength do
      FSymbol[i - 1].ChangeSize(Up, Down, Left, Right, Crop);

    FHeight := FSymbol[0].Height;
    FWidth  := FSymbol[0].Width;
    ClearChanges;
  end;

// определение возможности усечь символ
function TFont.CanOptimize(Direction: TCanOptimize): Integer;
  var
    i, min, tmp: Integer;
  begin
    min := MaxInt;

    for i := 1 to FFontLength do
      begin
      tmp   := FSymbol[i - 1].CanOptimize(Direction);
      if tmp < min then
        min := tmp;
      end;

    Result := min;
  end;

// установка диапазона символов шрифта
procedure TFont.SetRange(StartCode: Integer; EndCode: Integer);
  var
    i, w, h: Integer;
    old_start, old_length: Integer;
    tmp: TFontSet;
  begin

    SetLength(tmp, FFontLength);
    for i := 0 to FFontLength - 1 do
      begin
      tmp[i]        := TSymbol.Create;
      tmp[i].Width  := FWidth;
      tmp[i].Height := FHeight;
      for h := 0 to FHeight - 1 do
        for w := 0 to FWidth - 1 do
          tmp[i].Symbol[w, h] := FSymbol[i].Symbol[w, h];
      end;

    old_start  := FFontStartItem;
    old_length := FFontLength;

    FFontStartItem := StartCode;
    FFontLength    := EndCode - StartCode + 1;

    SetLength(FSymbol, FFontLength);
    for i := 0 to FFontLength - 1 do
      if FSymbol[i] = nil then
        begin
        FSymbol[i] := TSymbol.Create;

        with FSymbol[i] do
          begin
          Height          := FHeight;
          Width           := FWidth;
          GridStep        := FGridStep;
          GridColor       := FGridColor;
          GridThickness   := FGridThickness;
          BackgroundColor := FBackgroundColor;
          ActiveColor     := FActiveColor;
          ShowGrid        := FShowGrid;
          ShiftRollover   := FShiftRollover;
          end;
        end;

    // сдвиг символов для соответствия коду
    for i := 0 to old_length - 1 do
      if (i + old_start - FFontStartItem < FFontLength)
        and (i + old_start - FFontStartItem >= 0)
        and (FSymbol[i + old_start - FFontStartItem] <> nil) then
        for h := 0 to FHeight - 1 do
          for w := 0 to FWidth - 1 do
            FSymbol[i + old_start - FFontStartItem].Symbol[w, h] :=
              tmp[i].Symbol[w, h];

    for i := 0 to old_length - 1 do
      FreeAndNil(tmp[i]);

    // очистка добавленных символов
    if old_start > FFontStartItem then
      for i := 0 to old_start - FFontStartItem - 1 do
        FSymbol[i].Clear;

    // очистка истории правок символов
    ClearChanges;
  end;

// получение имени символа по его коду
function TFont.GetCharName(ACode: Integer; AFull: Boolean): String;

  const
    // https://ru.wikipedia.org/wiki/Управляющие_символы
    char_name: array [0..32] of String = (
      'NUL', 'SOH', 'STX', 'ETX', 'EOT', 'ENQ', 'ACK', 'BEL',
      'BS', 'HT', 'LF', 'VT', 'FF', 'CR', 'SO', 'SI',
      'DLE', 'DC1', 'DC2', 'DC3', 'DC4', 'NAK', 'SYN', 'ETB',
      'CAN', 'EM', 'SUB', 'ESC', 'FS', 'GS', 'RS', 'US', 'DEL');

    char_name_full: array [0..32] of String = (
      'NULL',
      'START OF HEADING',
      'START OF TEXT',
      'END OF TEXT',
      'END OF TRANSMISSION',
      'ENQUIRY',
      'ACKNOWLEDGE',
      'BELL',
      'BACKSPACE',
      'CHARACTER TABULATION (horizontal tabulation)',
      'LINE FEED',
      'LINE TABULATION (vertical tabulation)',
      'FORM FEED',
      'CARRIAGE RETURN',
      'SHIFT OUT (locking-shift one)',
      'SHIFT IN (locking-shift zero)',
      'DATA LINK ESCAPE',
      'DEVICE CONTROL ONE',
      'DEVICE CONTROL TWO',
      'DEVICE CONTROL THREE',
      'DEVICE CONTROL FOUR',
      'NEGATIVE ACKNOWLEDGE',
      'SYNCHRONOUS IDLE',
      'END TRANSMISSION BLOCK',
      'CANCEL',
      'END OF MEDIUM',
      'SUBSTITUTE',
      'ESCAPE',
      'INFORMATION SEPARATOR FOUR (file separator)',
      'INFORMATION SEPARATOR THREE (group separator)',
      'INFORMATION SEPARATOR TWO (record separator)',
      'INFORMATION SEPARATOR ONE (unit separator)',
      'DELETE');
  begin
    case ACode of
      0..31:
        if AFull then
          Result := char_name_full[ACode] else
          Result := char_name[ACode];

      127:
        if AFull then
          Result := char_name_full[32] else
          Result := char_name[32];

      else
        Result := EncodingToUTF8(Char(ACode), FEncoding);
      end;
  end;

procedure TFont.SetHeight(AHeight: Integer);
  var
    i: Integer;
  begin
    FHeight := AHeight;

    for i := 1 to FFontLength do
      FSymbol[i - 1].Height := FHeight;
  end;

procedure TFont.SetWidth(AWidth: Integer);
  var
    i: Integer;
  begin
    FWidth := AWidth;

    for i := 1 to FFontLength do
      FSymbol[i - 1].Width := FWidth;
  end;

procedure TFont.SetFontStartItem(AValue: Integer);
  begin
    if FFontStartItem = AValue then
      Exit;
    FFontStartItem := AValue;
  end;

procedure TFont.SetGridChessBackground(AValue: Boolean);
  var
    i: Integer;
  begin
    FGridChessBackground := AValue;

    for i := 1 to FFontLength do
      FSymbol[i - 1].GridChessBackground := FGridChessBackground;
  end;

procedure TFont.SetGridColor(AValue: TColor);
  var
    i: Integer;
  begin
    FGridColor := AValue;

    for i := 1 to FFontLength do
      FSymbol[i - 1].GridColor := FGridColor;
  end;

procedure TFont.SetGridThickness(AValue: Integer);
  var
    i: Integer;
  begin
    FGridThickness := AValue;

    for i := 1 to FFontLength do
      FSymbol[i - 1].GridThickness := FGridThickness;
  end;

procedure TFont.SetGridStep(AGridStep: Integer);
  var
    i: Integer;
  begin
    FGridStep := AGridStep;

    for i := 1 to FFontLength do
      FSymbol[i - 1].GridStep := FGridStep;
  end;

procedure TFont.SetShiftRollover(AValue: Boolean);
  var
    i: Integer;
  begin
    FShiftRollover := AValue;

    for i := 1 to FFontLength do
      FSymbol[i - 1].ShiftRollover := FShiftRollover;
  end;

procedure TFont.SetShowGrid(AValue: Boolean);
  var
    i: Integer;
  begin
    FShowGrid := AValue;

    for i := 1 to FFontLength do
      FSymbol[i - 1].ShowGrid := FShowGrid;
  end;

procedure TFont.SetFontLength(AValue: Integer);
  var
    i: Integer;
  begin
    SetLength(FSymbol, AValue);
    for i := 1 to AValue do
      if FSymbol[i - 1] = nil then
        begin
        FSymbol[i - 1] := TSymbol.Create;
        with FSymbol[i - 1] do
          begin
          Height          := FHeight;
          Width           := FWidth;
          GridStep        := FGridStep;
          GridColor       := FGridColor;
          GridThickness   := FGridThickness;
          BackgroundColor := FBackgroundColor;
          ActiveColor     := FActiveColor;
          ShowGrid        := FShowGrid;
          ShiftRollover   := FShiftRollover;
          //Zoom(FSymbol[0].ScaleFactor);
          end;
        end;

    FFontLength := AValue;
  end;

procedure TFont.SetActiveColor(AValue: TColor);
  var
    i: Integer;
  begin
    if FActiveColor = AValue then
      Exit;
    FActiveColor := AValue;

    for i := 1 to FFontLength do
      FSymbol[i - 1].ActiveColor := FActiveColor;
  end;

procedure TFont.SetBackgroundColor(AValue: TColor);
  var
    i: Integer;
  begin
    if FBackgroundColor = AValue then
      Exit;
    FBackgroundColor := AValue;

    for i := 1 to FFontLength do
      FSymbol[i - 1].BackgroundColor := FBackgroundColor;
  end;

constructor TFont.Create;
  var
    i: Integer;
  begin
    FFontStartItem := 32; // начальный символ в наборе
    FFontLength    := 1;  // кол-во символов в наборе
    FPasteMode     := pmNorm;

    SetLength(FSymbol, FFontLength);
    for i := 1 to FFontLength do
      begin
      FreeAndNil(FSymbol[i - 1]);
      FSymbol[i - 1] := TSymbol.Create;
      end;

    FHeight          := FSymbol[0].Height;
    FWidth           := FSymbol[0].Width;
    FGridStep        := FSymbol[0].GridStep;
    FGridColor       := FSymbol[0].GridColor;
    FGridThickness   := FSymbol[0].GridThickness;
    FBackgroundColor := FSymbol[0].BackgroundColor;
    FActiveColor     := FSymbol[0].ActiveColor;
    FShowGrid        := FSymbol[0].ShowGrid;
    FShiftRollover   := FSymbol[0].ShiftRollover;
    FDateCreate      := Now;
    FDateChange      := Now;
    FEncoding        := EncodingAnsi;

    //Clear;
    //FScanColsFirst    := True;
    //FScanColsToRight  := True;
    //FScanRowsToDown   := True;
    //FNumbersView      := nvHEX;
    //FNumbersInversion := False;
    //FEmptyBits        := emBIT_0;
    //FFontType         := ftMONOSPACE;
  end;

destructor TFont.Destroy;
  var
    i: Integer;
  begin
    for i := 0 to FFontLength - 1 do
      FreeAndNil(FSymbol[i]);
    inherited; // Эквивалентно: inherited Destroy;
  end;

end.
