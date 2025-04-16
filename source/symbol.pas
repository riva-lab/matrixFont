unit symbol;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, SysUtils, Clipbrd, LazUTF8, LazFileUtils,
  LCLType, GraphUtil, Math, StrUtils, BGRABitmap, BGRACanvas, BGRABitmapTypes,
  u_encodings, u_utilities, u_helpers, u_imghist, FPImage;

const
  EXCHANGE_BUFFER_TYPE_ID = 'image/bmp'; // ID буфера обмена  matrixFontApp
  CHAR_IMPORT_FORMATS     = 'png,jpg,jpeg,bmp,pnm,pgm,ppm,pbm,tif,tiff,ico';
  CHAR_COLOR_FG           = clBlack;
  CHAR_COLOR_BG           = clWhite;

type
  TNumberView      = (nvHEX, nvBIN, nvDEC);
  TFontType        = (ftMONOSPACE, ftPROPORTIONAL);
  TEmptyBit        = (emBIT_0, emBIT_1);
  TDirection       = (dirUp, dirDown, dirLeft, dirRight);
  TMirror          = (mrHorizontal, mrVertical);
  TClipboardAction = (cbCut, cbCopy, cbPaste);
  TPasteMode       = (pmNorm, pmOr, pmXor, pmAnd);
  TPixelAction     = (paSet, paClear, paInvert, paNone);
  THistoryAction   = (haClear, haSave, haUndo, haRedo);


  { TMatrixCharProperties }

  TMatrixCharProperties = class
    Active: Boolean;
    Index:  Integer;

    constructor Create;
  end;


  { TMatrixChar }

  TMatrixChar = class
  private
    FBitmap:  TBGRABitmap;
    FCanvas:  TBGRACanvas;
    FHistory: TBGRABitmapHistory;
    FProps:   TMatrixCharProperties;

    FWidth:        Integer; // ширина символа в пикселях
    FHeight:       Integer; // высота символа в пикселях
    FUndoLimit:    Integer; // undo limit (history depth)
    FCurrentPixel: TPoint;  // координаты текущего нарисованного пикселя

    function GetCanPaste: Boolean;
    function GetCanUndo: Boolean;
    function GetCanRedo: Boolean;

  public

    // работа с пикселем (установка/очистка/инверсия)
    procedure PixelAction(AX, AY: Integer; AAction: TPixelAction);

    // очистить символ
    procedure Clear;

    // инвертировать изображение символа
    procedure Invert;

    // отобразить символ
    procedure Mirror(MirrorDirection: TMirror);

    // сдвиг символа
    procedure Shift(ADirection: TDirection; AShiftRollover: Boolean; APixels: Integer = 1);

    // прижать символ к краю
    procedure Snap(ADirection: TDirection);

    // центрирование символа
    procedure Center(AVertical: Boolean);

    // поворот символа
    procedure Rotate(AClockWise: Boolean);

    // вывести изображение предпросмотра в битмап
    procedure Draw(ABitmap: TBitmap; ATransparent: Boolean; AColorBG, AColorActive: TColor);

    // генерировать код символа
    function GenerateCode(fnGroupIsVertical, fnScanColsFirst, fnScanColsToRight,
      fnScanRowsToDown, fnBitOrderLSBFirst, fnNumbersInversion: Boolean;
      fnNumbersView: TNumberView; fnEmptyBits: TEmptyBit;
      fnFontType: TFontType; fnBitsPerBlock: Integer; fnValuesPerLine: Integer): String;

    // manage changes history
    procedure History(AHistoryAction: THistoryAction);

    // импорт символа из системного шрифта для растеризации
    procedure Import(AFont: TFont; AIndex: Integer; AEncoding: String);

    // импорт изображения символа из файла
    procedure Import(AFilename: String; APasteMode: TPasteMode; ATreshold: Byte = 128);

    procedure Import(ABitmap: TBitmap; APasteMode: TPasteMode; ATreshold: Byte = 128);

    // получение ширины символа
    function GetCharWidth: Integer;

    // загрузка символа целиком
    procedure LoadChar(ASymbol: TBGRABitmap);

    // set char width and height
    procedure SetSize(AWidth, AHeight: Integer);

    // set char undo limit (history depth), will take effect after ClearChanges
    procedure SetUndoLimit(ALimit: Integer);

    // изменение размеров холста символа
    procedure ChangeSize(Up, Down, Left, Right: Integer; Crop: Boolean);

    // определение возможности усечь символ: результат - кол-во пустых строк/стоблцов
    function CanOptimize(ADirection: TDirection): Integer;

    // операции с буфером обмена
    procedure ClipboardAction(Action: TClipboardAction; Mode: TPasteMode = pmNorm);


    constructor Create;
    destructor Destroy; override;


    property Props: TMatrixCharProperties read FProps write FProps;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;

    property CurrentPixel: TPoint read FCurrentPixel;

    property CanUndo: Boolean read GetCanUndo;
    property CanRedo: Boolean read GetCanRedo;

    property CanPaste: Boolean read GetCanPaste;

    property Canvas: TBGRACanvas read FCanvas;
  end;


  { Additional functions }

procedure DrawChessBackground(ABitmap: TBitmap; AWidth, AHeight: Integer;
  AColor1, AColor2: TColor);


implementation

procedure DrawChessBackground(ABitmap: TBitmap; AWidth, AHeight: Integer;
  AColor1, AColor2: TColor);
  var
    h, w: Integer;
  begin
    if Assigned(ABitmap) then
      with ABitmap do
        begin
        BeginUpdate(True);
        SetSize(AWidth, AHeight);
        Canvas.Brush.Color := AColor1;
        Canvas.Clear;
        Canvas.Clear;

        for h := 0 to AHeight - 1 do
          for w := 0 to AWidth - 1 do
            if ((w + h) mod 2 = 0) then Canvas.Pixels[w, h] := AColor2;

        EndUpdate;
        end;
  end;


{ TMatrixCharProperties }

constructor TMatrixCharProperties.Create;
  begin
    inherited;
    Active := False;
    Index  := -1;
  end;


{ TMatrixChar }

function TMatrixChar.GetCanPaste: Boolean;
  begin
    Result := Clipboard.HasPictureFormat;
  end;

function TMatrixChar.GetCanUndo: Boolean;
  begin
    Result := FHistory.CanUndo;
  end;

function TMatrixChar.GetCanRedo: Boolean;
  begin
    Result := FHistory.CanRedo;
  end;

// работа с пикселем (установка/очистка/инверсия)
procedure TMatrixChar.PixelAction(AX, AY: Integer; AAction: TPixelAction);
  begin
    if InRange(AX, 0, FWidth - 1) and InRange(AY, 0, FHeight - 1) then
      case AAction of
        paSet: FCanvas.Pixels[AX, AY]    := CHAR_COLOR_FG;
        paClear: FCanvas.Pixels[AX, AY]  := CHAR_COLOR_BG;
        paInvert: FCanvas.Pixels[AX, AY] := (FCanvas.Pixels[AX, AY] = CHAR_COLOR_FG)
            .Select(CHAR_COLOR_BG, CHAR_COLOR_FG);
        end;
  end;

// очистить символ
procedure TMatrixChar.Clear;
  begin
    FBitmap.Fill(ColorToBGRA(CHAR_COLOR_BG));
  end;

// инвертировать изображение символа
procedure TMatrixChar.Invert;
  begin
    FBitmap.Negative;
  end;

// отобразить символ
procedure TMatrixChar.Mirror(MirrorDirection: TMirror);
  begin
    case MirrorDirection of
      mrHorizontal: FBitmap.HorizontalFlip;
      mrVertical: FBitmap.VerticalFlip;
      end;
  end;

// сдвиг символа
procedure TMatrixChar.Shift(ADirection: TDirection; AShiftRollover: Boolean;
  APixels: Integer);
  var
    x, y: Integer;
    b:    TBGRABitmap = nil;
  begin
    if APixels < 1 then Exit;

      try
      x := (ADirection in [dirLeft, dirRight]).Select(APixels, 0);
      y := (ADirection in [dirUp, dirDown]).Select(APixels, 0);
      x *= (ADirection in [dirRight, dirUp]).Select(-1, 1);
      y *= (ADirection in [dirLeft, dirDown]).Select(-1, 1);

      b := TBGRABitmap.Create;
      b.SetSize(FWidth, FHeight);
      b.Canvas.Draw(0, 0, FBitmap.Bitmap);

      Clear;
      FCanvas.Brush.Color := CHAR_COLOR_BG;
      FCanvas.CopyRect(0, 0, b, Rect(x, y, FWidth + x, FHeight + y));

      if not AShiftRollover then
        FCanvas.FillRect(
          (ADirection = dirLeft).Select(FWidth - APixels, 0),
          (ADirection = dirUp).Select(FHeight - APixels, 0),
          (ADirection = dirRight).Select(APixels, FWidth),
          (ADirection = dirDown).Select(APixels, FHeight));
      finally
      b.Free;
      end;
  end;

// прижать символ к краю
procedure TMatrixChar.Snap(ADirection: TDirection);
  begin
    Shift(ADirection, False, CanOptimize(ADirection));
  end;

// центрирование символа
procedure TMatrixChar.Center(AVertical: Boolean);
  var
    pixels: Integer;
  begin
    if AVertical then
      begin
      pixels := (CanOptimize(dirUp) - CanOptimize(dirDown)) div 2;
      Shift((pixels > 0).Select(dirUp, dirDown), False, abs(pixels));
      end
    else
      begin
      pixels := (CanOptimize(dirLeft) - CanOptimize(dirRight)) div 2;
      Shift((pixels > 0).Select(dirLeft, dirRight), False, abs(pixels));
      end;
  end;

// поворот символа
procedure TMatrixChar.Rotate(AClockWise: Boolean);
  var
    b: TBGRABitmap = nil;
  begin
      try
      if AClockWise then
        b := FBitmap.RotateCW else
        b := FBitmap.RotateCCW;
      FCanvas.Draw(0, 0, b);
      finally
      b.Free;
      end;
  end;

// вывести изображение предпросмотра в битмап
procedure TMatrixChar.Draw(ABitmap: TBitmap; ATransparent: Boolean; AColorBG,
  AColorActive: TColor);
  var
    b: TBGRABitmap = nil;
  begin
    if Assigned(ABitmap) then
      try
      ABitmap.Width  := FWidth;
      ABitmap.Height := FHeight;

      ABitmap.TransparentColor := clNone;

      b := TBGRABitmap.Create(FWidth, FHeight, ColorToBGRA(AColorActive));
      b.FillMask(0, 0, FBitmap, ColorToBGRA(AColorBG));
      ABitmap.Canvas.Draw(0, 0, b.Bitmap);

      if ATransparent then
        begin
        ABitmap.TransparentMode  := tmFixed;
        ABitmap.TransparentColor := AColorBG;
        ABitmap.Transparent      := True;
        end;

      finally
      b.Free;
      end;
  end;

// генерировать код символа
function TMatrixChar.GenerateCode(
  fnGroupIsVertical,           // направление считывания блока
  fnScanColsFirst,             // поле - флаг очередности сканирования: столбцы-строки
  fnScanColsToRight,           // поле - флаг направления сканирования столбцов
  fnScanRowsToDown,            // поле - флаг направления сканирования строк
  fnBitOrderLSBFirst,          // порядок записи бит в байте
  fnNumbersInversion: Boolean; // поле - битовая инверсия представления выходных чисел
  fnNumbersView: TNumberView;  // поле - настройка представления выходных чисел
  fnEmptyBits: TEmptyBit;      // поле - настройка заполнения пустых разрядов
  fnFontType: TFontType;       // поле - тип шрифта
  fnBitsPerBlock: Integer;     // поле - разрядность блока в битах
  fnValuesPerLine: Integer     // количество значений в строке
  ): String;

  var
    numberChars: Integer;

  function createNumber(stb: String; fnNView: TNumberView; IsLSBF: Boolean = False): String;
    var
      number: QWord;
      ch:     Char;
    begin
      Result := IsLSBF.Select('', stb);
      if IsLSBF then
        for ch in stb do Result := ch + Result;

      number := StrToQWord('%' + Result);

      case fnNView of
        nvBIN: Result := '0b' + Result;
        nvHEX: Result := '0x' + IntToHex(number, fnBitsPerBlock div 4);
        nvDEC: Result := PadLeft(IntToStr(number), numberChars);
        end;
    end;

  function readBlock(AX, AY: Integer): String;
    var
      i, cx, cy: Integer;
    begin
      Result := AddChar(fnNumbersInversion.Select('1', '0'), '', fnBitsPerBlock);

      if not (fnGroupIsVertical or fnScanColsToRight) then AX -= fnBitsPerBlock - 1;
      if fnGroupIsVertical and not fnScanRowsToDown then   AY -= fnBitsPerBlock - 1;

      for i := 0 to fnBitsPerBlock - 1 do
        begin
        cx := AX + fnGroupIsVertical.Select(0, i);
        cy := AY + fnGroupIsVertical.Select(i, 0);

        if (cx < 0) or (cx >= FWidth) or (cy < 0) or (cy >= FHeight) then
          Result[i + 1] := (fnEmptyBits = emBIT_0).Select('0', '1')
        else
        if FCanvas.Pixels[cx, cy] = CHAR_COLOR_FG then
          Result[i + 1] := fnNumbersInversion.Select('0', '1');
        end;
    end;

  function readColByCol: String;
    var
      x, y, cx, cy, i: Integer;
    begin
      Result := '';
      x      := 0;
      i      := 0;

      while x < FWidth do
        begin
        y := 0;

        while y < FHeight do
          begin
          cx     := fnScanColsToRight.Select(x, FWidth - 1 - x);
          cy     := fnScanRowsToDown.Select(y, FHeight - 1 - y);
          Result += createNumber(readBlock(cx, cy), fnNumbersView, fnBitOrderLSBFirst) + ', ';
          y      += fnGroupIsVertical.Select(fnBitsPerBlock, 1);
          i      += 1;
          if i mod fnValuesPerLine = 0 then Result += LineEnding + '    ';
          end;

        x += fnGroupIsVertical.Select(1, fnBitsPerBlock);
        end;
    end;

  function readRowByRow: String;
    var
      x, y, cx, cy, i: Integer;
    begin
      Result := '';
      y      := 0;
      i      := 0;

      while y < FHeight do
        begin
        x := 0;

        while x < FWidth do
          begin
          cx     := fnScanColsToRight.Select(x, FWidth - 1 - x);
          cy     := fnScanRowsToDown.Select(y, FHeight - 1 - y);
          Result += createNumber(readBlock(cx, cy), fnNumbersView, fnBitOrderLSBFirst) + ', ';
          x      += fnGroupIsVertical.Select(1, fnBitsPerBlock);
          i      += 1;
          if i mod fnValuesPerLine = 0 then Result += LineEnding + '    ';
          end;

        y += fnGroupIsVertical.Select(fnBitsPerBlock, 1);
        end;
    end;

  begin
    numberChars := trunc(Log10((UInt64(1) shl fnBitsPerBlock) - 1)) + 1;

    if fnScanColsFirst then
      Result := readColByCol else
      Result := readRowByRow;

    Result := '    ' + Result.TrimRight([' ', ',', #10, #13]);
  end;

// manage changes history
procedure TMatrixChar.History(AHistoryAction: THistoryAction);
  begin
    case AHistoryAction of

      haClear:
        begin
        FHistory.Depth := FUndoLimit;
        FHistory.Clear;
        FHistory.Save;
        end;

      haSave: FHistory.Save;
      haUndo: if FHistory.CanUndo then FHistory.Undo;
      haRedo: if FHistory.CanRedo then FHistory.Redo;
      end;
  end;

// импорт символа из системного шрифта для растеризации
procedure TMatrixChar.Import(AFont: TFont; AIndex: Integer; AEncoding: String);
  begin
    if not Assigned(AFont) then Exit;

    with FBitmap.Canvas do
      begin
      Font.Assign(AFont);
      Font.Quality := fqNonAntialiased;
      Pen.Color    := CHAR_COLOR_FG;
      Brush.Color  := CHAR_COLOR_BG;
      Clear;
      Clear;
      TextOut(1, 0, EncodingToUTF8(Char(AIndex), AEncoding));
      end;

    Import(FBitmap.Bitmap, pmNorm);
  end;

// импорт изображения символа из файла
procedure TMatrixChar.Import(AFilename: String; APasteMode: TPasteMode; ATreshold: Byte);
  begin
    if FileExistsUTF8(AFilename) and FileExtCheck(AFilename, CHAR_IMPORT_FORMATS) then
      with TPicture.Create do
        try
        LoadFromFile(AFilename);
        Import(Bitmap, APasteMode, ATreshold);
        finally
        Free;
        end;
  end;

procedure TMatrixChar.Import(ABitmap: TBitmap; APasteMode: TPasteMode; ATreshold: Byte);
  var
    w, h, mw, mh:     Integer;
    px:               TColor;
    srcBGRA:          TBGRABitmap = nil;
    srcLine, dstLine: PBGRAPixel;
  begin
    if not Assigned(ABitmap) then Exit;

    mw := (ABitmap.Width < FWidth).Select(ABitmap.Width - 1, FWidth - 1);
    mh := (ABitmap.Height < FHeight).Select(ABitmap.Height - 1, FHeight - 1);

      try
      srcBGRA := TBGRABitmap.Create(ABitmap);
      srcBGRA.FilterGrayscale;

      for h := 0 to mh do
        begin
        srcLine := srcBGRA.ScanLine[h];
        dstLine := FBitmap.ScanLine[h];

        for w := 0 to mw do
          begin
          if srcLine^.red < ATreshold then
            px := CHAR_COLOR_FG else
            px := CHAR_COLOR_BG;

          case APasteMode of
            pmOr: px  := px and dstLine^.ToColor;
            pmXor: px := px xor not dstLine^.ToColor;
            pmAnd: px := px or dstLine^.ToColor;
            end;

          dstLine^.FromColor(px and $FFFFFF);
          dstLine += 1;
          srcLine += 1;
          end;
        end;

      finally
      srcBGRA.Free;
      end;
  end;

// получение ширины символа
function TMatrixChar.GetCharWidth: Integer;
  begin
    Result := FWidth - CanOptimize(dirRight);
    if Result = 0 then Result := FWidth div 2 + 1;
  end;

// загрузка символа целиком (вызывается обычно после создания символа)
procedure TMatrixChar.LoadChar(ASymbol: TBGRABitmap);
  begin
    FCanvas.Draw(0, 0, ASymbol.Bitmap);
  end;

// set char width and height
procedure TMatrixChar.SetSize(AWidth, AHeight: Integer);
  begin
    if AWidth > 0 then  FWidth  := AWidth;
    if AHeight > 0 then FHeight := AHeight;
    FBitmap.SetSize(FWidth, FHeight);
  end;

// set char undo limit (history depth), will take effect after ClearChanges
procedure TMatrixChar.SetUndoLimit(ALimit: Integer);
  begin
    FUndoLimit := ALimit;
  end;

// изменение размеров холста символа
procedure TMatrixChar.ChangeSize(Up, Down, Left, Right: Integer; Crop: Boolean);
  var
    h, w: Integer;
    b:    TBitmap = nil;
  begin
      try
      b := TBitmap.Create;
      b.SetSize(FWidth, FHeight);
      b.Canvas.Draw(0, 0, FBitmap.Bitmap);

      h := FHeight + (Up + Down) * Crop.Select(-1, 1);
      w := FWidth + (Left + Right) * Crop.Select(-1, 1);
      SetSize((w < 1).Select(1, w), (h < 1).Select(1, h));
      Clear;

      Left *= Crop.Select(-1, 1);
      Up   *= Crop.Select(-1, 1);
      FCanvas.Draw(Left, Up, b);
      finally
      b.Free;
      end;
  end;

// определение возможности усечь символ: результат - кол-во пустых строк/стоблцов
function TMatrixChar.CanOptimize(ADirection: TDirection): Integer;

  function GetEmptyLines(AVertical, ATopLeft: Boolean): Integer;
    var
      rCount, oCoord, iCoord, outerEnd, innerEnd, oStep: Integer;
    begin
      outerEnd := AVertical.Select(FWidth, FHeight) - 1;
      innerEnd := AVertical.Select(FHeight, FWidth) - 1;

      if outerEnd = 0 then Exit(0);

      oStep  := ATopLeft.Select(1, -1);
      oCoord := ATopLeft.Select(0, outerEnd);
      iCoord := 0;
      rCount := 0;

      while InRange(oCoord, 0, outerEnd) do
        begin
        for iCoord := 0 to innerEnd do
          if FCanvas.Pixels[
            AVertical.Select(oCoord, iCoord),
            AVertical.Select(iCoord, oCoord)] = CHAR_COLOR_FG then Exit(rCount);

        rCount += 1;
        oCoord += oStep;
        end;

      Result := rCount;
    end;

  begin
    Result := GetEmptyLines(
      ADirection in [dirLeft, dirRight],
      ADirection in [dirLeft, dirUp]);
  end;

// операции с буфером обмена
procedure TMatrixChar.ClipboardAction(Action: TClipboardAction; Mode: TPasteMode);
  var
    cb_fmt: TClipboardFormat;
    p:      TPicture = nil;
  begin
      try
      p := TPicture.Create;

      // копирование символа
      if (Action = cbCopy) or (Action = cbCut) then
        begin
        if CanPaste then
          cb_fmt := Clipboard.FindPictureFormatID
        else
          cb_fmt := RegisterClipboardFormat(EXCHANGE_BUFFER_TYPE_ID);

        p.Bitmap.SetSize(FWidth, FHeight);
        p.Bitmap.Canvas.Clear;
        FBitmap.Draw(p.Bitmap.Canvas, 0, 0);
        p.Bitmap.SaveToClipboardFormat(cb_fmt);
        end;

      // вырезание символа: копировать + очистить
      if Action = cbCut then
        Clear;

      // вставка символа
      if (Action = cbPaste) and Clipboard.HasPictureFormat then
        begin
        p.LoadFromClipboardFormat(Clipboard.FindPictureFormatID);
        Import(p.Bitmap, Mode);
        end;

      finally
      p.Free;
      end;
  end;

constructor TMatrixChar.Create;
  begin
    FHeight := 1;
    FWidth  := 1;

    FBitmap    := TBGRABitmap.Create(FWidth, FHeight);
    FCanvas    := FBitmap.CanvasBGRA;
    FProps     := TMatrixCharProperties.Create;
    FHistory   := TBGRABitmapHistory.Create(FBitmap);
    FUndoLimit := 0;

    Clear;
    History(haSave);
  end;

destructor TMatrixChar.Destroy;
  begin
    FreeAndNil(FHistory);
    FreeAndNil(FProps);
    FreeAndNil(FBitmap);
    inherited;
  end;

end.
