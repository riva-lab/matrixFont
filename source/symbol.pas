unit symbol;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, SysUtils, Clipbrd, LazUTF8, LConvEncoding, LCLType,
  u_encodings;

const
  EXCHANGE_BUFFER_TYPE_ID = 'matrixFontApp'; // ID буфера обмена

type
  TNumberView      = (nvHEX, nvBIN, nvDEC);
  TFontType        = (ftMONOSPACE, ftPROPORTIONAL);
  TEmptyBit        = (emBIT_0, emBIT_1);
  TSymbolField     = array of array of Boolean;
  TPSymbolField    = ^TSymbolField;
  //TPBitmap         = ^TBitmap;
  TCanOptimize     = (coUp, coDown, coLeft, coRight);
  TDirection       = (dirUp, dirDown, dirLeft, dirRight);
  TBorder          = (brUp, brDown, brLeft, brRight);
  TMirror          = (mrHorizontal, mrVertical);
  TClipboardAction = (cbCut, cbCopy, cbPaste);
  TPasteMode       = (pmNorm, pmOr, pmXor, pmAnd);
  TPixelAction     = (paSet, paClear, paInvert, paNone);

  { TSymbol }

  TSymbol = class
  PRIVATE
    // ================================ Поля ===================================
    FSymbol: TSymbolField; // холст символа

    // поля внешнего вида и параметров символа
    FWidth:               Integer; // поле - ширина символа в пикселях
    FHeight:              Integer; // поле - высота символа в пикселях
    FHeightInPixels:      Integer; // поле - высота изображения символа в пикселях
    FWidthInPixels:       Integer; // поле - ширина изображения символа в пикселях
    FGridStep:            Integer; // поле - шаг сетки в пикселях
    FGridThickness:       Integer; // поле - толщина линий сетки
    FGridColor:           Integer; // поле - цвет сетки
    FBackgroundColor:     Integer; // поле - цвет фона
    FActiveColor:         Integer; // поле - цвет нарисованного элемента
    FShowGrid:            Boolean; // поле - флаг видимости сетки
    FGridChessBackground: Boolean; // поле - флаг отображения сетки в виде шахматного фона
    FCurrentPixel:        TPoint;  // поле - координаты текущего нарисованного пикселя

    // поля модификации символа
    FShiftRollover: Boolean; // поле - флаг циклического режима сдвига

    // поля истории изменений
    FHistory:         array of TSymbolField; // поле - массив для истории изменений
    FHistoryPosition: Integer; // поле - текущее изменение символа
    FHistoryEmpty:    Boolean; // поле - флаг пустоты истории изменений
    FHistoryNoRedo:   Boolean; // поле - флаг невозможности отмены

    // поля для работы с буфером обмена
    FCopyBufferEmpty: Boolean; // поле - флаг заполненности буфера обмена

    // ========================= Внутренние методы =============================
    // Методы чтения и записи свойств
    function GetCopyBufferEmpty: Boolean;
    procedure SetGridThickness(AValue: Integer);
    procedure SetWidth(AWidth: Integer);
    procedure SetHeight(AHeight: Integer);
    procedure SetGridStep(AGridStep: Integer);

    //==========================================================================

  PUBLIC
    // =================================== Методы ==============================

    // работа с пикселем (установка/очистка/инверсия)
    procedure PixelAction(AX, AY: Integer; AAction: TPixelAction);

    // очистить символ
    procedure Clear;

    // инвертировать изображение символа
    procedure Invert;

    // отобразить символ
    procedure Mirror(MirrorDirection: TMirror);

    // сдвиг символа
    procedure Shift(Direction: TDirection);

    // прижать символ к краю
    procedure Snap(Border: TBorder);

    // вывести изображение символа в битмап
    procedure Draw(bmp: TBitmap);

    // вывести изображение предпросмотра в битмап
    procedure DrawPreview(bmp: TBitmap; Transparency: Boolean = True; ColorBG: TColor =
      $FFFFFF; ColorActive: TColor = 0);

    // генерировать код символа
    function GenerateCode(fnScanColsFirst, fnScanColsToRight, fnScanRowsToDown,
      fnNumbersInversion: Boolean; fnNumbersView: TNumberView; fnEmptyBits: TEmptyBit;
      fnFontType: TFontType; fnNumbersBits: Integer): String;

    // очистить историю изменений
    procedure ClearChanges;

    // сохранить текущую правку символа в историю
    procedure SaveChange;

    // отменить одну правку с конца истории
    procedure UndoChange;

    // повторить отмененную ранее правку
    procedure RedoChange;

    // увеличение масштаба изображения символа (+10%)
    procedure ZoomIn;

    // уменьшение масштаба изображения символа (-10%)
    procedure ZoomOut;

    // масштаб изображения символа: вписанный в заданную область
    procedure ZoomFitToArea(Width, Height: Integer);

    // импорт символа из системного шрифта для растеризации
    procedure Import(Font: TFont; Index: Integer; AEncoding: String);

    // получение ширины символа
    function GetCharWidth: Integer;

    // загрузка символа целиком
    procedure LoadSymbol(ASymbol: TPSymbolField);

    // изменение размеров холста символа
    procedure ChangeSize(Up, Down, Left, Right: Integer; Crop: Boolean);

    // определение возможности усечь символ: результат - кол-во пустых строк/стоблцов
    function CanOptimize(Direction: TCanOptimize): Integer;

    // операции с буфером обмена
    procedure ClipboardAction(Action: TClipboardAction; Mode: TPasteMode = pmNorm);

    //==========================================================================

    //======================== Конструкторы и деструкторы ======================
    constructor Create;
    destructor Destroy; OVERRIDE;
    //==========================================================================

    // =========================== Свойства ====================================

    // свойства внешнего вида и параметров символа

    // ширина символа в пикселях
    property Width: Integer read FWidth write SetWidth;

    // высота символа в пикселях
    property Height: Integer read FHeight write SetHeight;

    // ширина изображения символа в пикселях
    property WidthInPixels: Integer read FWidthInPixels;

    // высота изображения символа в пикселях
    property HeightInPixels: Integer read FHeightInPixels;

    // шаг сетки в пикселях
    property GridStep: Integer read FGridStep write SetGridStep;

    // толщина линий сетки
    property GridThickness: Integer read FGridThickness write SetGridThickness;

    // цвет сетки
    property GridColor: Integer read FGridColor write FGridColor;

    // флаг отображения сетки в виде шахматного фона
    property GridChessBackground: Boolean read FGridChessBackground write FGridChessBackground;

    // цвет фона
    property BackgroundColor: Integer read FBackgroundColor write FBackgroundColor;

    // цвет нарисованного элемента
    property ActiveColor: Integer read FActiveColor write FActiveColor;

    // флаг видимости сетки
    property ShowGrid: Boolean read FShowGrid write FShowGrid;

    // координаты текущего нарисованного пикселя
    property CurrentPixel: TPoint read FCurrentPixel;

    //--------------------------------------------------------------------------
    // свойства истории изменений

    // флаг пустоты истории изменений
    property HistoryEmpty: Boolean read FHistoryEmpty;

    // флаг невозможности отмены
    property HistoryNoRedo: Boolean read FHistoryNoRedo;

    //--------------------------------------------------------------------------
    // флаг заполненности буфера обмена
    property CopyBufferEmpty: Boolean read GetCopyBufferEmpty;

    //--------------------------------------------------------------------------
    // свойства модификации символа

    // флаг циклического режима сдвига
    property ShiftRollover: Boolean read FShiftRollover write FShiftRollover;

    //--------------------------------------------------------------------------
    property Symbol: TSymbolField read FSymbol;
  end;

implementation

{ TSymbol }

procedure TSymbol.SetWidth(AWidth: Integer);
  begin
    FWidth := AWidth;
    SetLength(FSymbol, FWidth, FHeight);
    SetGridStep(FGridStep);
  end;

function TSymbol.GetCopyBufferEmpty: Boolean;
  begin
      try
      FCopyBufferEmpty := Clipboard.FindFormatID(EXCHANGE_BUFFER_TYPE_ID) = 0;
      except
      FCopyBufferEmpty := True;
      end;
    Result := FCopyBufferEmpty;
  end;

procedure TSymbol.SetGridThickness(AValue: Integer);
  begin
    FGridThickness := AValue;
    SetGridStep(FGridStep);
  end;

procedure TSymbol.SetHeight(AHeight: Integer);
  begin
    FHeight := AHeight;
    SetLength(FSymbol, FWidth, FHeight);
    SetGridStep(FGridStep);
  end;

procedure TSymbol.SetGridStep(AGridStep: Integer);
  begin
    FGridStep       := AGridStep;
    FWidthInPixels  := FWidth * FGridStep + FGridThickness;
    FHeightInPixels := FHeight * FGridStep + FGridThickness;
  end;

// работа с пикселем (установка/очистка/инверсия)
procedure TSymbol.PixelAction(AX, AY: Integer; AAction: TPixelAction);
  var
    state: Boolean;
  begin
    state := AAction = paSet;
    if (AX >= 0) and (AY >= 0) and (AX < FWidth) and (AY < FHeight) then
      FSymbol[AX, AY] := state or (AAction = paInvert) and not FSymbol[AX, AY];
  end;

// очистить символ
procedure TSymbol.Clear;
  var
    w, h: Integer;
  begin
    for h := 0 to FHeight - 1 do
      for w := 0 to FWidth - 1 do
        FSymbol[w, h] := False;
  end;

// инвертировать изображение символа
procedure TSymbol.Invert;
  var
    w, h: Integer;
  begin
    for h := 0 to FHeight - 1 do
      for w := 0 to FWidth - 1 do
        FSymbol[w, h] := not FSymbol[w, h];
  end;

// отобразить символ
procedure TSymbol.Mirror(MirrorDirection: TMirror);
  var
    w, h: Integer;
    a:    Boolean;
  begin
    case MirrorDirection of

      // отобразить символ горизонтально
      mrHorizontal:
        for h := 0 to FHeight - 1 do
          for w := 0 to (FWidth - 1) div 2 do
            begin
            a             := FSymbol[w, h];
            FSymbol[w, h] := FSymbol[FWidth - 1 - w, h];
            FSymbol[FWidth - 1 - w, h] := a;
            end;

      // отобразить символ вертикально
      mrVertical:
        for h := 0 to (FHeight - 1) div 2 do
          for w := 0 to FWidth - 1 do
            begin
            a             := FSymbol[w, h];
            FSymbol[w, h] := FSymbol[w, FHeight - 1 - h];
            FSymbol[w, FHeight - 1 - h] := a;
            end;

      end;
  end;

// сдвиг символа
procedure TSymbol.Shift(Direction: TDirection);
  var
    w, h: Integer;
    a:    array of Boolean;
  begin
    case Direction of

      // сдвиг символа вверх
      dirUp:
        begin
        SetLength(a, FWidth);

        for w := 0 to FWidth - 1 do
          begin
          a[w] := FSymbol[w, 0];

          for h := 0 to FHeight - 2 do
            FSymbol[w, h] := FSymbol[w, h + 1];

          if FShiftRollover then
            FSymbol[w, FHeight - 1] := a[w]
          else
            FSymbol[w, FHeight - 1] := False;

          end;
        end;

      // сдвиг символа вниз
      dirDown:
        begin
        SetLength(a, FWidth);

        for w := 0 to FWidth - 1 do
          begin
          a[w] := FSymbol[w, FHeight - 1];

          for h := FHeight - 1 downto 1 do
            FSymbol[w, h] := FSymbol[w, h - 1];

          if FShiftRollover then
            FSymbol[w, 0] := a[w]
          else
            FSymbol[w, 0] := False;
          end;
        end;

      // сдвиг символа влево
      dirLeft:
        begin
        SetLength(a, FHeight);

        for h := 0 to FHeight - 1 do
          begin
          a[h] := FSymbol[0, h];

          for w := 0 to FWidth - 2 do
            FSymbol[w, h] := FSymbol[w + 1, h];

          if FShiftRollover then
            FSymbol[FWidth - 1, h] := a[h]
          else
            FSymbol[FWidth - 1, h] := False;

          end;
        end;

      // сдвиг символа вправо
      dirRight:
        begin
        SetLength(a, FHeight);

        for h := 0 to FHeight - 1 do
          begin
          a[h] := FSymbol[FWidth - 1, h];

          for w := FWidth - 1 downto 1 do
            FSymbol[w, h] := FSymbol[w - 1, h];

          if FShiftRollover then
            FSymbol[0, h] := a[h]
          else
            FSymbol[0, h] := False;
          end;

        end;
      end;
    a := nil;
  end;

// прижать символ к краю
procedure TSymbol.Snap(Border: TBorder);
  var
    w, h:  Integer;
    empty: Boolean;
  begin
    case Border of

      // прижать символ к верхнему краю
      brUp:
        for h := 0 to FHeight - 1 do
          begin
          empty := True;

          for w := 0 to FWidth - 1 do
            if FSymbol[w, 0] = True then
              begin
              empty := False;
              break;
              end;

          if empty then
            Shift(dirUp)
          else
            break;
          end;

      // прижать символ к нижнему краю
      brDown:
        for h := 0 to FHeight - 1 do
          begin
          empty := True;

          for w := 0 to FWidth - 1 do
            if FSymbol[w, FHeight - 1] = True then
              begin
              empty := False;
              break;
              end;

          if empty then
            Shift(dirDown)
          else
            break;
          end;

      // прижать символ к левому краю
      brLeft:
        for w := 0 to FWidth - 1 do
          begin
          empty := True;

          for h := 0 to FHeight - 1 do
            if FSymbol[0, h] = True then
              begin
              empty := False;
              break;
              end;

          if empty then
            Shift(dirLeft)
          else
            break;
          end;

      // прижать символ к правому краю
      brRight:
        for w := 0 to FWidth - 1 do
          begin
          empty := True;

          for h := 0 to FHeight - 1 do
            if FSymbol[FWidth - 1, h] = True then
              begin
              empty := False;
              break;
              end;

          if empty then
            Shift(dirRight)
          else
            break;
          end;

      end;
  end;

// вывести изображение символа в битмап
procedure TSymbol.Draw(bmp: TBitmap);
  var
    start_x, start_y, end_x, end_y: Integer;
    w, h: Integer;
  begin
    with bmp.Canvas do
      begin
      Brush.Color := FBackgroundColor;
      Clear;
      Clear;
      end;

    for h := 0 to FHeight - 1 do
      for w := 0 to FWidth - 1 do
        with bmp.Canvas do
          begin
          start_x := FGridThickness div 2 + 1 + w * FGridStep;
          start_y := FGridThickness div 2 + 1 + h * FGridStep;
          end_x   := start_x + FGridStep;
          end_y   := start_y + FGridStep;

          if FSymbol[w, h] then
            Brush.Color := FActiveColor
          else
            if FShowGrid and FGridChessBackground and ((w + h) mod 2 = 0) then
              Brush.Color := FGridColor
            else
              Brush.Color := FBackgroundColor;

          FillRect(start_x - 1, start_y - 1, end_x, end_y);

          if not FGridChessBackground and (FGridThickness < 1) then
            FGridThickness := 1;

          if FShowGrid and (FGridThickness > 0) then
            begin
            Pen.JoinStyle := pjsMiter; // квадратные углы концов линий фигур
            Pen.Width     := FGridThickness;
            Pen.Color     := FGridColor;

            Rectangle(start_x - 1, start_y - 1, end_x, end_y);
            end;
          end;
  end;

// вывести изображение предпросмотра в битмап
procedure TSymbol.DrawPreview(bmp: TBitmap; Transparency: Boolean = True;
  ColorBG: TColor = $FFFFFF; ColorActive: TColor = 0);
  var
    w, h: Integer;
  begin
    for h := 0 to FHeight - 1 do
      for w := 0 to FWidth - 1 do
        with bmp.Canvas do
          begin
          if FSymbol[w, h] then
            Brush.Color := ColorActive
          else
            Brush.Color := ColorBG;

          Pixels[w, h] := Brush.Color;
          end;

    if Transparency then
      begin
      bmp.TransparentColor := ColorBG;
      bmp.Transparent      := True;
      end;
  end;

// генерировать код символа
function TSymbol.GenerateCode(
  fnScanColsFirst,             // поле - флаг очередности сканирования: столбцы-строки
  fnScanColsToRight,           // поле - флаг направления сканирования столбцов
  fnScanRowsToDown,            // поле - флаг направления сканирования строк
  fnNumbersInversion: Boolean; // поле - битовая инверсия представления выходных чисел
  fnNumbersView: TNumberView;  // поле - настройка представления выходных чисел
  fnEmptyBits: TEmptyBit;      // поле - настройка заполнения пустых разрядов
  fnFontType: TFontType;       // поле - тип шрифта
  fnNumbersBits: Integer       // поле - разрядность выходных чисел
  ): String;

  function create_number(stb: String; fnNView: TNumberView): String;
    var
      i, max: Integer;
      number: QWord;
    begin
      max    := trunc(fnNumbersBits / 10 * 3);
      number := StrToQWord('%' + stb);
      case fnNView of
        nvBIN: Result := '0b' + stb;
        nvHEX: Result := '0x' + IntToHex(number, fnNumbersBits div 4);
        nvDEC:
          begin
          Result := IntToStr(number);

          if number > 0 then
            number := max - trunc(ln(number) / ln(10))
          else
            number := max;

          if fnNView = nvDEC then
            for i := 1 to number do
              Result := ' ' + Result;
          end;
        end;
    end;

  var
    ch:           Char;
    x, y:         Integer;
    x_end, y_end: Integer;
    w_st, h_st:   Integer;
    str_binary:   String;
    str:          String = '';
    bit1, bit0:   Char;
    element:      Boolean;

  begin
    if fnScanColsFirst then
      begin
      x_end := FWidth;
      y_end := FHeight;
      end
    else
      begin
      x_end := FHeight;
      y_end := FWidth;
      end;

    if fnScanColsToRight then
      w_st := 0 // сканирование столбцов слева направо
    else
      w_st := FWidth - 1; // сканирование столбцов справа налево

    if fnScanRowsToDown then
      h_st := 0 // сканирование строк снизу вверх
    else
      h_st := FHeight - 1; // сканирование строк сверху вниз

    bit0 := '0';
    bit1 := '1';
    if fnNumbersInversion then
      begin
      bit0 := '1';
      bit1 := '0';
      end;

    for x := 0 to x_end - 1 do
      begin
      str_binary := '';

      if (Length(str) > 0) then
        str := str + ', ';

      for y := 0 to y_end - 1 do
        begin

        if fnScanColsFirst then
          element := FSymbol[abs(w_st - x), abs(h_st - y)]
        else
          element := FSymbol[abs(w_st - y), abs(h_st - x)];

        if element then
          str_binary := bit1 + str_binary
        else
          str_binary := bit0 + str_binary;

        if (y_end > fnNumbersBits) and ((y + 1) mod fnNumbersBits = 0) and
          (y + 1 <> y_end) then
          begin
          str        := str + create_number(str_binary, fnNumbersView) + ', ';
          str_binary := '';
          end;
        end;

      Inc(y);
      while (y mod fnNumbersBits <> 0) do // дополнение пустых разрядов
        begin
        if fnEmptyBits = emBIT_0 then
          ch := '0'
        else
          ch := '1';
        //str_binary := str_binary + ch; // побитовое выравнивание по левой стороне
        str_binary := ch + str_binary;  // побитовое выравнивание по правой стороне

        Inc(y);
        end;

      str := str + create_number(str_binary, fnNumbersView);
      end;

    if fnFontType = ftPROPORTIONAL then
      Result := create_number(binStr(GetCharWidth, fnNumbersBits), nvDEC) + ', /*N*/ ' + str
    else
      Result := str;
  end;

// очистить историю изменений
procedure TSymbol.ClearChanges;
  begin
    FHistoryPosition := 1;
    SaveChange;
    FHistoryEmpty  := True;
    FHistoryNoRedo := True;
  end;

// сохранить текущую правку символа в историю
procedure TSymbol.SaveChange;
  begin
    FHistoryPosition := FHistoryPosition + 1;
    SetLength(FHistory, FHistoryPosition);
    FHistory[FHistoryPosition - 1] := FSymbol;
    FHistoryEmpty  := False;
    FHistoryNoRedo := True;
    SetLength(FSymbol, FWidth, FHeight);
  end;

// отменить одну правку с конца истории
procedure TSymbol.UndoChange;
  begin
    if FHistoryEmpty then Exit;

    FHistoryPosition := FHistoryPosition - 1;
    FSymbol          := FHistory[FHistoryPosition - 1];

    if FHistoryPosition = 2 then
      FHistoryEmpty := True;

    FHistoryNoRedo := False;
    SetLength(FSymbol, FWidth, FHeight);
  end;

// повторить отмененную ранее правку
procedure TSymbol.RedoChange;
  begin
    if FHistoryNoRedo then Exit;

    if FHistoryPosition < high(FHistory) + 1 then
      begin
      FHistoryEmpty    := False;
      FSymbol          := FHistory[FHistoryPosition];
      FHistoryPosition := FHistoryPosition + 1;
      FHistoryNoRedo   := FHistoryPosition = (high(FHistory) + 1);
      end;
    SetLength(FSymbol, FWidth, FHeight);
  end;

// увеличение масштаба изображения символа (+10%)
procedure TSymbol.ZoomIn;
  var
    tmp: Integer;
  begin
    tmp := round(FGridStep * 1.1);
    if tmp = FGridStep then
      SetGridStep(FGridStep + 1)
    else
      if FGridStep < 150 then
        SetGridStep(tmp);
  end;

// уменьшение масштаба изображения символа (-10%)
procedure TSymbol.ZoomOut;
  var
    tmp: Integer;
  begin
    tmp := round(FGridStep / 1.1);
    if tmp = FGridStep then
      Dec(tmp);
    if tmp < FGridThickness + 1 then
      SetGridStep(FGridThickness + 1)
    else
      SetGridStep(tmp);
  end;

// масштаб изображения символа: вписанный в заданную область
procedure TSymbol.ZoomFitToArea(Width, Height: Integer);
  begin
    Width  := Width - FGridThickness;
    Height := Height - FGridThickness;

    FWidthInPixels  := Width div FWidth;    // FWidthInPixels as temp var
    FHeightInPixels := Height div FHeight;  // FHeightInPixels as temp var
    if FWidthInPixels < FHeightInPixels then
      SetGridStep(FWidthInPixels)
    else
      SetGridStep(FHeightInPixels);
  end;

// импорт символа из системного шрифта для растеризации
procedure TSymbol.Import(Font: TFont; Index: Integer; AEncoding: String);
  var
    tmp:  TBitmap;
    w, h: Integer;
  begin
    tmp             := TBitmap.Create;
    tmp.Canvas.Font := Font;
    tmp.Width       := FWidth;
    tmp.Height      := FHeight;

    with tmp.Canvas do
      begin
      Brush.Color := 1;
      Clear;
      Clear;
      Pen.Color    := 0;
      Font.Quality := fqNonAntialiased;
      TextOut(1, 0, EncodingToUTF8(Char(Index), AEncoding));

      // растеризация символа во внутренний формат
      for h := 0 to FHeight - 1 do
        for w := 0 to FWidth - 1 do
          FSymbol[w, h] := Pixels[w, h] = 0;
      end;

    FreeAndNil(tmp);
  end;

// получение ширины символа
function TSymbol.GetCharWidth: Integer;
  var
    x, y, w, tmp: Integer;
    empty:        Boolean;
  begin
    w   := 0;
    tmp := 0;

    for x := 0 to FWidth - 1 do
      begin

      empty := True;
      for y := 0 to FHeight - 1 do
        if FSymbol[x, y] then
          begin
          empty := False;
          break;
          end;

      Inc(tmp);
      if not empty then
        w := tmp;
      end;

    if w = 0 then
      w := FWidth div 2 + 1;

    Result := w;
  end;

// загрузка символа целиком (вызывается обычно после создания символа)
procedure TSymbol.LoadSymbol(ASymbol: TPSymbolField);
  begin
    FSymbol := ASymbol^;
    SetLength(FSymbol, FWidth, FHeight);

    SetLength(FHistory, FHistoryPosition);
    FHistory[FHistoryPosition - 1] := FSymbol;
    SetLength(FSymbol, FWidth, FHeight);
  end;

// изменение размеров холста символа
procedure TSymbol.ChangeSize(Up, Down, Left, Right: Integer; Crop: Boolean);
  var
    h, w: Integer;
    tmp:  TSymbolField;
  begin
    // временная копия символа
    SetLength(tmp, FWidth, FHeight);
    tmp := FSymbol;

    Up    := abs(Up);
    Down  := abs(Down);
    Left  := abs(Left);
    Right := abs(Right);

    if Crop then

      // обрезка символа
      begin
      for h := 0 to FHeight - 1 - Down - Up do
        for w := 0 to FWidth - 1 - Right - Left do
          Symbol[w, h] := tmp[w + Left, h + Up];

      SetHeight(FHeight - Up - Down);
      SetWidth(FWidth - Left - Right);
      end
    else

      // расширение символа
      begin
      SetHeight(FHeight + Up + Down);
      SetWidth(FWidth + Left + Right);

      // очистка фона добавленной области
      for h := 0 to FHeight - 1 do
        for w := 0 to FWidth - 1 do
          Symbol[w, h] := False;

      for h := 0 to FHeight - 1 - Down - Up do
        for w := 0 to FWidth - 1 - Right - Left do
          Symbol[w + Left, h + Up] := tmp[w, h];
      end;
  end;

// определение возможности усечь символ: результат - кол-во пустых строк/стоблцов
function TSymbol.CanOptimize(Direction: TCanOptimize): Integer;
  var
    w, h, Count: Integer;
    exit_:       Boolean;
  begin
    Count := 0;

    case Direction of

      // кол-во пустых строк сверху
      coUp:
        begin
        for h := 0 to FHeight - 1 do
          begin
          exit_ := False;

          for w := 0 to FWidth - 1 do
            if FSymbol[w, h] = True then
              begin
              exit_ := True;
              break;
              end;

          if exit_ then
            break
          else
            Inc(Count);
          end;
        end;

      // кол-во пустых строк снизу
      coDown:
        begin
        for h := FHeight - 1 downto 0 do
          begin
          exit_ := False;

          for w := 0 to FWidth - 1 do
            if FSymbol[w, h] = True then
              begin
              exit_ := True;
              break;
              end;

          if exit_ then
            break
          else
            Inc(Count);
          end;
        end;

      // кол-во пустых столбцов слева
      coLeft:
        begin
        for w := 0 to FWidth - 1 do
          begin
          exit_ := False;

          for h := 0 to FHeight - 1 do
            if FSymbol[w, h] = True then
              begin
              exit_ := True;
              break;
              end;

          if exit_ then
            break
          else
            Inc(Count);
          end;
        end;

      // кол-во пустых столбцов справа
      coRight:
        begin
        for w := FWidth - 1 downto 0 do
          begin
          exit_ := False;

          for h := 0 to FHeight - 1 do
            if FSymbol[w, h] = True then
              begin
              exit_ := True;
              break;
              end;

          if exit_ then
            break
          else
            Inc(Count);
          end;
        end;
      end;

    Result := Count;
  end;

// операции с буфером обмена
procedure TSymbol.ClipboardAction(Action: TClipboardAction; Mode: TPasteMode);
  var
    h, w, cw, ch: Integer;
    pixel:        Boolean;
    Stream:       TMemoryStream;
    cb_fmt:       TClipboardFormat;
  begin
    // копирование символа
    if (Action = cbCopy) or (Action = cbCut) then
      with Stream do
          try
          if GetCopyBufferEmpty then
            cb_fmt := RegisterClipboardFormat(EXCHANGE_BUFFER_TYPE_ID)
          else
            cb_fmt := Clipboard.FindFormatID(EXCHANGE_BUFFER_TYPE_ID);

          Stream   := TMemoryStream.Create;
          Position := 0;
          WriteByte(FWidth);
          WriteByte(FHeight);

          for h := 0 to Height - 1 do
            for w := 0 to Width - 1 do
              if FSymbol[w, h] then
                WriteByte(1)
              else
                WriteByte(0);

          Clipboard.AddFormat(cb_fmt, Stream);
          FCopyBufferEmpty := False;
          finally
          FreeAndNil(Stream);
          end;

    // вырезание символа: копировать + очистить
    if Action = cbCut then
      Clear;

    // вставка символа
    if Action = cbPaste then
      with Stream do
        begin
          try
          cb_fmt := Clipboard.FindFormatID(EXCHANGE_BUFFER_TYPE_ID);
          except
          cb_fmt := 0;
          end;

        if cb_fmt <> 0 then
            try
            Stream := TMemoryStream.Create;

            if Clipboard.GetFormat(cb_fmt, Stream) then
              begin
              Position := 0;
              cw       := ReadByte;
              ch       := ReadByte;

              for h := 0 to ch - 1 do
                for w := 0 to cw - 1 do
                  begin
                  pixel := (ReadByte = 1);

                  if (w < FWidth) and (h < FHeight) then
                    case Mode of
                      pmNorm:
                        FSymbol[w, h] := pixel;
                      pmOr:
                        FSymbol[w, h] := FSymbol[w, h] or pixel;
                      pmXor:
                        FSymbol[w, h] := FSymbol[w, h] xor pixel;
                      pmAnd:
                        FSymbol[w, h] := FSymbol[w, h] and pixel;
                      end;
                  end;
              end;
            finally
            FreeAndNil(Stream);
            end;
        end;
  end;

constructor TSymbol.Create;
  begin
    FHeight              := 1;
    FWidth               := 1;
    FHeightInPixels      := 3;
    FWidthInPixels       := 3;
    FGridStep            := 40;
    FGridThickness       := 1;
    FGridChessBackground := False;
    FGridColor           := $BBBBBB;
    FBackgroundColor     := $FFFFFF;
    FActiveColor         := $000000;
    FShowGrid            := True;
    FShiftRollover       := True;
    FHistoryPosition     := 1;
    SetLength(FSymbol, FWidth, FHeight);
    Clear;
    SaveChange;
    FHistoryEmpty  := True;
    FHistoryNoRedo := True;

    GetCopyBufferEmpty;
  end;

destructor TSymbol.Destroy;
  begin
    FSymbol := nil;
    inherited; // Эквивалентно: inherited Destroy;
  end;

end.
