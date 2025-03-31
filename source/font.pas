unit font;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, LConvEncoding, StrUtils, Graphics, regexpr,
  BGRABitmap, Math,
  symbol, u_encodings, u_helpers, u_utilities;

resourcestring
  RHF_COMMENT_NAME   = 'Название шрифта';
  RHF_COMMENT_AUTHOR = 'Автор шрифта';
  RHF_COMMENT_TIME   = 'Дата и время генерации';
  RHF_COMMENT_APP    = 'Сгенерировано';
  RHF_COMMENT_CP     = 'Кодовая страница';
  RHF_COMMENT_CHAR   = 'Символ';

const
  FILE_EXTENSION     = 'RHF';
  IMPORTC_EXTENSION  = 'C,CPP,H,HPP,TXT';

type
  TQWordArray      = array of QWord;
  TMxCharArray     = array of TMatrixChar;
  TDirection       = symbol.TDirection;
  TMirror          = symbol.TMirror;
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

  { TMatrixFont }

  TMatrixFont = class
  private
    FAuthor:        String;        // автор шрифта
    FName:          String;        // имя шрифта
    FAppAdditional: String;        // название компании-разработчика приложения-генератора
    FAppCreate:     String;        // название приложения создания файла
    FAppChange:     String;        // название приложения изменения файла
    FAppCurrent:    String;        // название приложения, обрабатывающего файл
    FCharArray:     TMxCharArray;  // набор символов
    FDefPrefix:     String;        // префикс для #define-ов
    FEncoding:      String;        // кодировка (кодовая страница)

    FFontStartItem: Integer;       // код начального символа в наборе
    FFontLength:    Integer;       // кол-во символов в наборе
    FDateCreate:    TDateTime;     // дата и время создания
    FDateChange:    TDateTime;     // дата и время изменения

    FWidth:     Integer;    // ширина символа в пикселях
    FHeight:    Integer;    // высота символа в пикселях
    FUndoLimit: Integer;    // undo limit (history depth)
    FPasteMode: TPasteMode; // режим вставки

    // поля настройки знакогенератора

    FGroupIsVertical:  Boolean;     // поле - направление считывания группы битов: верт/гориз
    FScanColsFirst:    Boolean;     // поле - флаг очередности сканирования: столбцы-строки
    FScanColsToRight:  Boolean;     // поле - флаг направления сканирования столбцов
    FScanRowsToDown:   Boolean;     // поле - флаг направления сканирования строк
    FBitOrderLSBFirst: Boolean;     // поле - порядок записи бит в байте
    FNumbersView:      TNumberView; // поле - настройка представления выходных чисел
    FMSBFirst:         Boolean;     // поле - порядок вывода бит (используется только в импорте из кода)
    FNumbersInversion: Boolean;     // поле - битовая инверсия представления выходных чисел
    FEmptyBits:        TEmptyBit;   // поле - настройка заполнения пустых разрядов
    FFontType:         TFontType;   // поле - тип шрифта
    FBitsPerGroup:     Integer;     // поле - разрядность группы битов
    FValuesPerLine:    Integer;     // поле - количество значений в строке

    procedure SetFontLength(AValue: Integer);
    procedure SetFontStartItem(AValue: Integer);

  public

    // очистить все символы шрифта
    procedure Clear;

    // инвертировать изображение всех символов шрифта
    procedure Invert;

    // отобразить все символы шрифта
    procedure Mirror(MirrorDirection: TMirror);

    // сдвиг всех символов шрифта
    procedure Shift(ADirection: TDirection; AShiftRollover: Boolean);

    // прижать все символы шрифта к краю
    procedure Snap(ADirection: TDirection);

    // центрирование всех символов шрифта
    procedure Center(AVertical: Boolean);

    // поворот всех символов шрифта
    procedure Rotate(AClockWise: Boolean);

    // обменять местами символы в таблице
    function SwapChars(AIndex1, AIndex2: Integer): Boolean;

    // генерировать код шрифта
    function GenerateCode(StartChar: Integer = 0; EndChar: Integer = 0): String;

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

    // set char width and height
    procedure SetSize(AWidth, AHeight: Integer);

    // set char undo limit (history depth), will take effect after ClearChanges
    procedure SetUndoLimit(ALimit: Integer);

    // изменение размеров холста символов шрифта
    // при обрезке: если значение < 0, то применяем оптимизацию
    procedure ChangeSize(Up, Down, Left, Right: Integer; Crop: Boolean);

    // определение возможности усечь символ
    function CanOptimize(ADirection: TDirection): Integer;

    // установка диапазона символов шрифта
    procedure SetRange(StartCode: Integer = 32; EndCode: Integer = 255);

    // получение имени символа по его коду
    function GetCharName(ACode: Integer; AFull: Boolean = False): String;


    constructor Create;
    destructor Destroy; override;


    property Width: Integer read FWidth;
    property Height: Integer read FHeight;

    // режим вставки из буфера обмена
    property PasteMode: TPasteMode read FPasteMode write FPasteMode;

    // флаг направления считывания группы битов: вертикально/горизонтально
    property GroupIsVertical: Boolean read FGroupIsVertical write FGroupIsVertical;

    // флаг очередности сканирования: столбцы-строки
    property ScanColsFirst: Boolean read FScanColsFirst write FScanColsFirst;

    // флаг направления сканирования столбцов
    property ScanColsToRight: Boolean read FScanColsToRight write FScanColsToRight;

    // флаг направления сканирования строк
    property ScanRowsToDown: Boolean read FScanRowsToDown write FScanRowsToDown;

    // порядок записи бит в байте
    property BitOrderLSBFirst: Boolean read FBitOrderLSBFirst write FBitOrderLSBFirst;

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

    // разрядность группы битов
    property BitsPerGroup: Integer read FBitsPerGroup write FBitsPerGroup;

    property ValuesPerLine: Integer read FValuesPerLine write FValuesPerLine;

    // код начального символа в наборе
    property FontStartItem: Integer read FFontStartItem write SetFontStartItem;

    // кол-во символов в наборе
    property FontLength: Integer read FFontLength write SetFontLength;

    // массив символов шрифта
    property Item: TMxCharArray read FCharArray write FCharArray;

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
  TMatrixFont
  ----- }

// очистить все символы шрифта
procedure TMatrixFont.Clear;
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      begin
      FCharArray[i - 1].Clear;
      FCharArray[i - 1].SaveChange;
      end;
  end;

// инвертировать изображение всех символов шрифта
procedure TMatrixFont.Invert;
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      begin
      FCharArray[i - 1].Invert;
      FCharArray[i - 1].SaveChange;
      end;
  end;

// отобразить все символы шрифта
procedure TMatrixFont.Mirror(MirrorDirection: TMirror);
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      begin
      FCharArray[i - 1].Mirror(MirrorDirection);
      FCharArray[i - 1].SaveChange;
      end;
  end;

// сдвиг всех символов шрифта
procedure TMatrixFont.Shift(ADirection: TDirection; AShiftRollover: Boolean);
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      begin
      FCharArray[i - 1].Shift(ADirection, AShiftRollover);
      FCharArray[i - 1].SaveChange;
      end;
  end;

// прижать все символы шрифта к краю
procedure TMatrixFont.Snap(ADirection: TDirection);
  var
    mxChar: TMatrixChar;
  begin
    for mxChar in FCharArray do
      begin
      mxChar.Snap(ADirection);
      mxChar.SaveChange;
      end;
  end;

// центрирование всех символов шрифта
procedure TMatrixFont.Center(AVertical: Boolean);
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      begin
      FCharArray[i - 1].Center(AVertical);
      FCharArray[i - 1].SaveChange;
      end;
  end;

// поворот всех символов шрифта
procedure TMatrixFont.Rotate(AClockWise: Boolean);
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      begin
      FCharArray[i - 1].Rotate(AClockWise);
      FCharArray[i - 1].SaveChange;
      end;
  end;

// обменять местами символы в таблице
function TMatrixFont.SwapChars(AIndex1, AIndex2: Integer): Boolean;
  var
    tmpMatrixChar: TMatrixChar;

  begin
    Result := False;
    if not InRange(AIndex1, 0, FFontLength - 1) then Exit;
    if not InRange(AIndex2, 0, FFontLength - 1) then Exit;

    tmpMatrixChar       := FCharArray[AIndex2];
    FCharArray[AIndex2] := FCharArray[AIndex1];
    FCharArray[AIndex1] := tmpMatrixChar;

    Result := True;
  end;

// генерировать код шрифта
function TMatrixFont.GenerateCode(StartChar: Integer; EndChar: Integer): String;
  var
    i, w, space: Integer;
    s, id:       String;
    dataType:    array[0..3] of String = (
      'unsigned char',
      'unsigned short',
      'uint24_t',
      'unsigned long');

  function AddBlockCaption(ACharCode: Integer): String;
    begin
      if ACharCode in [16, 32, 58, 91, 123] then Exit(LineEnding);
      if (ACharCode > 127) and (ACharCode mod 16 = 0) then Exit(LineEnding);

      Result := '';

      case ACharCode of
        48: Result := 'Digits';
        65: Result := 'Latin Uppercase';
        97: Result := 'Latin Lowercase';
        end;

      if Result <> '' then
        Result := LineEnding + '    /* ' + Result + ' */' + LineEnding;
    end;

  begin
    if (StartChar < 0) or (StartChar < FFontStartItem) then
      StartChar := FFontStartItem;

    if (EndChar < 0) or (EndChar < StartChar) or (EndChar >= FFontStartItem + FFontLength) then
      EndChar := FFontStartItem + FFontLength - 1;

    if FDefPrefix[1] in ['0'..'9'] then FDefPrefix := '_' + FDefPrefix;
    id := Transliterate(FDefPrefix);

    Result := '';

    // generate array of chars
    for i := StartChar to EndChar do
      begin
      w      := FCharArray[i - FFontStartItem].GetCharWidth;
      Result += UnicodeFormat('%s    /*%s %-3d %-3s, width = %d */  %s%s%s', [
        AddBlockCaption(i),
        RHF_COMMENT_CHAR,
        i,
        ((i < 32) or (i = 127)).Select(GetCharName(i), '<' + GetCharName(i) + '>'),
        w,
        (FFontType = ftPROPORTIONAL).Select(w.ToString + ',', '') + LineEnding,
        FCharArray[i - FFontStartItem].GenerateCode(
        FGroupIsVertical,
        FScanColsFirst,
        FScanColsToRight,
        FScanRowsToDown,
        FBitOrderLSBFirst,
        FNumbersInversion,
        FNumbersView,
        FEmptyBits,
        FFontType,
        FBitsPerGroup,
        FValuesPerLine),
        (i = EndChar).Select('', ',')]) + LineEnding + LineEnding;
      end;

    i := FGroupIsVertical.Select(
      FWidth * ((FHeight - 1) div FBitsPerGroup + 1),
      FHeight * ((FWidth - 1) div FBitsPerGroup + 1));
    i += (FFontType = ftMONOSPACE).Select(0, 1);
    i *= EndChar - StartChar + 1;

    space := 0;
    for s in [RHF_COMMENT_NAME, RHF_COMMENT_AUTHOR, RHF_COMMENT_TIME, RHF_COMMENT_APP, RHF_COMMENT_CP] do
      if UTF8Length(s) > space then space := UTF8Length(s);
    space := ((space + 3) div 4 + 1) * 4 - 3;

    Result := GetResourceAsString('CODETEMPDEF')
      .Replace('%FontData%', Result)
      .Replace('%LabelFontName%', UTF8PadRight(RHF_COMMENT_NAME, space))
      .Replace('%LabelFontAuthor%', UTF8PadRight(RHF_COMMENT_AUTHOR, space))
      .Replace('%LabelDateTime%', UTF8PadRight(RHF_COMMENT_TIME, space))
      .Replace('%LabelApplication%', UTF8PadRight(RHF_COMMENT_APP, space))
      .Replace('%LabelFontEncoding%', UTF8PadRight(RHF_COMMENT_CP, space))
      .Replace('%FontName%', FName)
      .Replace('%FontAuthor%', FAuthor)
      .Replace('%DateTime%', DateTimeToStr(Now))
      .Replace('%Application%', FAppCurrent)
      .Replace('%FontEncoding%', GetEncodingCaption(FEncoding))
      .Replace('%AppAdditional%', FAppAdditional)
      .Replace('%FontIDUpperCase%', id)
      .Replace('%FontIDLowerCase%', LowerCase(id))
      .Replace('%FontLength%', (EndChar - StartChar + 1).ToString)
      .Replace('%FontStartChar%', StartChar.ToString)
      .Replace('%FontWidth%', FWidth.ToString)
      .Replace('%FontHeight%', FHeight.ToString)
      .Replace('%FontValueBits%', FBitsPerGroup.ToString)
      .Replace('%FontBitOrder%', FBitOrderLSBFirst.Select('LSB', 'MSB'))
      .Replace('%FontIsGroupVert%', FGroupIsVertical.Select('1', '0'))
      .Replace('%FontEmptyBits%', (FEmptyBits = emBIT_1).Select('1', '0'))
      .Replace('%FontRowByRow%', FScanColsFirst.Select('CbyC', 'RbyR'))
      .Replace('%FontColumnsMode%', FScanColsToRight.Select('L2R', 'R2L'))
      .Replace('%FontRowsMode%', FScanRowsToDown.Select('T2B', 'B2T'))
      .Replace('%FontType%', (FFontType = ftMONOSPACE).Select('MONOSPACED', 'PROPORTIONAL'))
      .Replace('%FontType4%', (FFontType = ftMONOSPACE).Select('MONO', 'PROP'))
      .Replace('%FontArraySize%', i.ToString)
      .Replace('%FontArrayBytes%', (i * ((FBitsPerGroup + 7) div 8)).ToString)
      .Replace('%DataType%', dataType[(FBitsPerGroup - 1) div 8])
      .Replace('%DataModifier1%', '');
  end;

// сохранение шрифта в файл
procedure TMatrixFont.SaveToFile(FileName: String);
  var
    file_header: file of TFileRHFHeader1;
    file_binary: file of Byte;
    header:      TFileRHFHeader1;
    mxChar:      TMatrixChar;
    w, h:        Integer;
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

      for w := 0 to High(Reserved) do Reserved[w] := 42;
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
    //      Write(file_, FCharArray[i - 1].Canvas[w, h]);

    // запись данных символов (новый формат: побитовая запись)
    for mxChar in FCharArray do
      for h := 0 to FHeight - 1 do
        for w := 0 to FWidth - 1 do
          begin
          if mxChar.Canvas.Pixels[w, h] = CHAR_COLOR_FG then
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
function TMatrixFont.ReadFromFile(FileName: String): Boolean;
  var
    file_header: file of TFileRHFHeader1;
    file_binary: file of Byte;
    header:      TFileRHFHeader1;
    mxChar:      TMatrixChar;
    w, h:        Integer;
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
      Result := InRange(CharFirst, 0, 255) and InRange(CharLength, 0, 255)
        and InRange(CharHeight, 0, 1024) and InRange(CharWidth, 0, 1024);

      if not Result then Exit(Result);

      FName         := FontName;
      FAuthor       := FontAuthor;
      FAppCreate    := AppCreate;
      FAppChange    := AppChange;
      FDateCreate   := DateCreate;
      FDateChange   := DateChange;
      FontStartItem := CharFirst;
      FontLength    := CharLength;
      FEncoding     := GetEncodingAdapted(Encoding);
      SetSize(CharWidth, CharHeight);
      end;

    // --- чтение данных о символах ---

    AssignFile(file_binary, FileName);
    Reset(file_binary);

    // переход к началу секции данных
    Seek(file_binary, SizeOf(TFileRHFHeader1));

    // чтение данных символов (старый формат) {file_: file of Boolean;}
    //for i := 1 to FFontLength do
    //  begin
    //  for h := 0 to FHeight - 1 do
    //    for w := 0 to FWidth - 1 do
    //      Read(file_, char_tmp[w, h]);
    //  FCharArray[i - 1].LoadChar(@char_tmp);
    //  end;

    // чтение данных символов (новый формат: побитовая запись)
      try
      for mxChar in FCharArray do
        begin
        mxChar.ClearChanges;

        for h := 0 to FHeight - 1 do
          for w := 0 to FWidth - 1 do
            begin
            if mask = 0 then
              begin
              Read(file_binary, tmp_byte);
              mask := 1;
              end;

            mxChar.Canvas.Pixels[w, h] :=
              ((tmp_byte and mask) <> 0).Select(CHAR_COLOR_FG, CHAR_COLOR_BG);

            mask := (mask shl 1) and $FF;
            end;

        end;
      except
      Result := False; // файл поврежден
      end;

    CloseFile(file_binary);
  end;

// очистить историю изменений
procedure TMatrixFont.ClearChanges;
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      begin
      FCharArray[i - 1].SetUndoLimit(FUndoLimit);
      FCharArray[i - 1].ClearChanges;
      end;
  end;

// отменить одну правку с конца истории
procedure TMatrixFont.UndoChange;
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      FCharArray[i - 1].UndoChange;
  end;

// повторить отмененную ранее правку
procedure TMatrixFont.RedoChange;
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      FCharArray[i - 1].RedoChange;
  end;

// пакетная вставка
procedure TMatrixFont.Paste(AMode: TPasteMode);
  var
    i: Integer;
  begin
    for i := 1 to FFontLength do
      begin
      FCharArray[i - 1].ClipboardAction(cbPaste, AMode);
      FCharArray[i - 1].SaveChange;
      end;
  end;

// импорт системного шрифта для растеризации
procedure TMatrixFont.Import(Font: Graphics.TFont; Width, Height: Integer);
  var
    i: Integer;
  begin
    SetSize(Width, Height);

    for i := 1 to FFontLength do
      begin
      FCharArray[i - 1].Import(Font, FFontStartItem + i - 1, FEncoding);
      FCharArray[i - 1].SaveChange;
      end;
  end;

// импорт кода C
function TMatrixFont.Import(ACode: String; AOffset, ASkip: Integer; AType: TImportMode): Boolean;

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
      SetSize(0, Round(GetValue(String(re.Match[8]), String(re.Match[9])) * 1.8));

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

      SetSize(Round(xAdvMax * 1.8) + 8, 0);
      Clear;

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

    function GetMatrixFontParamInt(AKey: String; ADef: Integer = 0): Integer;
      begin
        Result := ADef;
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
          Expression  := AKey + '\s+?\(?\s?(\w[\w\d_]*)\)?';
          if Exec then Result := Match[1];
          finally
          Free;
          end;
      end;

    begin
      SetSize(
        GetMatrixFontParamInt('char_width'),
        GetMatrixFontParamInt('char_height'));
      FontStartItem    := GetMatrixFontParamInt('start_char');
      FontLength       := GetMatrixFontParamInt('length');
      FGroupIsVertical := GetMatrixFontParamInt('is_group_vert', FScanColsFirst.Select(1, 0)) <> 0;
      BitsPerGroup     := 0;
      AOffset          := 0;
      ASkip            := 0;
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
        SetSize(
          Integer(AArray[0] and $3FF),
          Integer(AArray[1] and $3FF));
        FontStartItem := Integer(AArray[2] and $3FF);
        FontLength    := Integer(AArray[3] and $3FF);
        BitsPerGroup  := 8;
        AOffset       := 4;
        ASkip         := 0;
        FontType      := ftMONOSPACE;

        if AArray[0] = 0 then // check width
          begin
          if Length(AArray) < AOffset + FontLength then Exit;
          for i := 0 to FontLength - 1 do
            if AArray[AOffset + i] > w then w := AArray[AOffset + i];

          SetSize(Integer(w and $3FF), 0);
          AOffset  += FontLength;
          FontType := ftPROPORTIONAL;
          end;

        // autoset values below are not used for more flexibility
        //ScanColsFirst   := False;
        //GroupIsVertical := not ExecRegExpr('#ifndef\s+?_GLCD_DATA_BYTEY_', ACode);
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
          FontType     := ftPROPORTIONAL;
          BitsPerGroup := 8;
          AOffset      := 0;
          ASkip        := 0;
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
        for i    := 1 to FBitsPerGroup do
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

  procedure ReadAdafruitChar(AChar: Integer; AGFXArr: TGFXArray; ABitmap: TQWordArray); inline;
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
          FCharArray[AChar].PixelAction(
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

  procedure SetCharSizeAndGroups(out ASize, AGroups: Integer; AWidth, AHeight: Integer);
    begin
      AGroups := FGroupIsVertical.Select(AHeight - 1, AWidth - 1) div FBitsPerGroup + 1;
      ASize   := AGroups * FGroupIsVertical.Select(AWidth, AHeight) - 1;
      if FGroupIsVertical and not FScanColsFirst then AGroups := AWidth else
      if not FGroupIsVertical and FScanColsFirst then AGroups := FHeight;
    end;

  procedure ReadChar(var AIndex: Integer; AArray: TQWordArray; AChar, ASize, AGroups: Integer); inline;
    var
      j, b, x, y: Integer;
      _data:      QWord;
    begin
      FCharArray[AChar].Clear;
      AIndex += ASkip;

      if (AType = imLCDVision) and (FontType = ftPROPORTIONAL) then
        SetCharSizeAndGroups(ASize, AGroups, AArray[4 + AChar], FHeight);

      for j := 0 to ASize do
        begin
        if AIndex > High(AArray) then Exit;
        _data := AArray[AIndex];

        x := FScanColsFirst.Select(j div AGroups, j mod AGroups)
          * FGroupIsVertical.Select(1, FBitsPerGroup);

        y := FScanColsFirst.Select(j mod AGroups, j div AGroups)
          * FGroupIsVertical.Select(FBitsPerGroup, 1);

        for b := 0 to FBitsPerGroup - 1 do
          begin
          FCharArray[AChar].PixelAction(x, y,
            (_data and 1 > 0).Select(paSet, paClear));

          if FGroupIsVertical then
            y += 1 else
            x += 1;

          _data := _data shr 1;
          end;

        AIndex += 1;
        end;
    end;

  var
    values:       TQWordArray;
    gfxArr:       TGFXArray;
    ready:        Boolean;
    i, b, ch:     Integer;
    cSize, cLine: Integer;

  begin
    Result := False;
    ACode  := NormalizeCode(ACode);

    if AType = imAdafruit then
      ready := StrToArrayAdafruit(ACode, values, gfxArr, FName)
    else
      ready := StrToArray(ACode, values, b, FName);

    if not ready then
      begin
      Clear;
      Exit;
      end;

    AdjustParameters(values);

    if AType = imAdafruit then
      begin
      for ch := 0 to FFontLength - 1 do ReadAdafruitChar(ch, gfxArr, values);
      Exit(True);
      end;

    // check fields
    if AOffset > High(values) then Exit;
    if FBitsPerGroup = 0 then FBitsPerGroup := b;
    if FBitsPerGroup = 0 then Exit;
    if FWidth = 0 then Exit;
    if FHeight = 0 then Exit;
    if FFontLength = 0 then Exit;

    if FMSBFirst then ChangeBitOrder(values);

    SetCharSizeAndGroups(cSize, cLine, FWidth, FHeight);
    i := AOffset;

    for ch := 0 to FFontLength - 1 do
      ReadChar(i, values, ch, cSize, cLine);

    Result := True;
  end;

// set char width and height
procedure TMatrixFont.SetSize(AWidth, AHeight: Integer);
  var
    mxChar: TMatrixChar;
  begin
    if AWidth > 0 then  FWidth  := AWidth;
    if AHeight > 0 then FHeight := AHeight;
    for mxChar in FCharArray do mxChar.SetSize(FWidth, FHeight);
  end;

// set char undo limit (history depth), will take effect after ClearChanges
procedure TMatrixFont.SetUndoLimit(ALimit: Integer);
  begin
    FUndoLimit := ALimit;
  end;

// изменение размеров холста символов шрифта
procedure TMatrixFont.ChangeSize(Up, Down, Left, Right: Integer; Crop: Boolean);
  var
    mxChar: TMatrixChar;
  begin
    if Crop then // при обрезке: если значение < 0, то применяем оптимизацию
      begin
      if Up < 0 then    Up    := CanOptimize(dirUp);
      if Down < 0 then  Down  := CanOptimize(dirDown);
      if Left < 0 then  Left  := CanOptimize(dirLeft);
      if Right < 0 then Right := CanOptimize(dirRight);
      end;

    for mxChar in FCharArray do
      mxChar.ChangeSize(Up, Down, Left, Right, Crop);

    FHeight := FCharArray[0].Height;
    FWidth  := FCharArray[0].Width;
    ClearChanges;
  end;

// определение возможности усечь символ
function TMatrixFont.CanOptimize(ADirection: TDirection): Integer;
  var
    mxChar: TMatrixChar;
  begin
    Result := MaxInt;

    for mxChar in FCharArray do
      Result := min(Result, mxChar.CanOptimize(ADirection));
  end;

// установка диапазона символов шрифта
procedure TMatrixFont.SetRange(StartCode: Integer; EndCode: Integer);
  var
    i, w, h: Integer;
    old_start, old_length: Integer;
    tmp: TMxCharArray;
  begin

    SetLength(tmp, FFontLength);
    for i := 0 to FFontLength - 1 do
      begin
      tmp[i] := TMatrixChar.Create;
      tmp[i].SetSize(FWidth, FHeight);

      for h := 0 to FHeight - 1 do
        for w := 0 to FWidth - 1 do
          tmp[i].Canvas.Pixels[w, h] := FCharArray[i].Canvas.Pixels[w, h];
      end;

    old_start  := FFontStartItem;
    old_length := FFontLength;

    FFontStartItem := StartCode;
    FFontLength    := EndCode - StartCode + 1;

    SetLength(FCharArray, FFontLength);
    for i := 0 to FFontLength - 1 do
      if FCharArray[i] = nil then
        begin
        FCharArray[i] := TMatrixChar.Create;

        with FCharArray[i] do
          SetSize(FWidth, FHeight);
        end;

    // сдвиг символов для соответствия коду
    for i := 0 to old_length - 1 do
      if (i + old_start - FFontStartItem < FFontLength)
        and (i + old_start - FFontStartItem >= 0)
        and (FCharArray[i + old_start - FFontStartItem] <> nil) then
        for h := 0 to FHeight - 1 do
          for w := 0 to FWidth - 1 do
            FCharArray[i + old_start - FFontStartItem].Canvas.Pixels[w, h] :=
              tmp[i].Canvas.Pixels[w, h];

    for i := 0 to old_length - 1 do
      FreeAndNil(tmp[i]);

    // очистка добавленных символов
    if old_start > FFontStartItem then
      for i := 0 to old_start - FFontStartItem - 1 do
        FCharArray[i].Clear;

    // очистка истории правок символов
    ClearChanges;
  end;

// получение имени символа по его коду
function TMatrixFont.GetCharName(ACode: Integer; AFull: Boolean): String;

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

procedure TMatrixFont.SetFontStartItem(AValue: Integer);
  begin
    if FFontStartItem = AValue then
      Exit;
    FFontStartItem := AValue;
  end;

procedure TMatrixFont.SetFontLength(AValue: Integer);
  var
    i: Integer;
  begin
    SetLength(FCharArray, AValue);
    for i := 1 to AValue do
      if FCharArray[i - 1] = nil then
        begin
        FCharArray[i - 1] := TMatrixChar.Create;
        with FCharArray[i - 1] do
          SetSize(FWidth, FHeight);
        end;

    FFontLength := AValue;
  end;


constructor TMatrixFont.Create;
  var
    i: Integer;
  begin
    FFontStartItem := 32; // начальный символ в наборе
    FFontLength    := 1;  // кол-во символов в наборе
    FPasteMode     := pmNorm;

    SetLength(FCharArray, FFontLength);
    for i := 1 to FFontLength do
      begin
      FreeAndNil(FCharArray[i - 1]);
      FCharArray[i - 1] := TMatrixChar.Create;
      end;

    FHeight     := FCharArray[0].Height;
    FWidth      := FCharArray[0].Width;
    FDateCreate := Now;
    FDateChange := Now;
    FEncoding   := EncodingAnsi;
  end;

destructor TMatrixFont.Destroy;
  var
    i: Integer;
  begin
    for i := 0 to FFontLength - 1 do
      FreeAndNil(FCharArray[i]);
    inherited; // Эквивалентно: inherited Destroy;
  end;

end.
