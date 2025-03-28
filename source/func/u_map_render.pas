
{ unit u_map_render.pas
  ---------------------
  Provides functions for rendering char map to PNG image.
  (c) Riva, 2024
  This file is the part of matrixFont project.
}
unit u_map_render;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, FPImgCmn, base64,
  font, symbol, appAbout, u_helpers;


function AddTextChunkToPNG(const AFilename, AKey, AValue: String): Boolean;
function GetTextChunkFromPNG(const AFilename, AKey: String): String;

procedure ImplantWatermark(ABitmap: TBitmap; AData: String);
function ExtractWatermark(ABitmap: TBitmap): String;

procedure RenderMapToPNG(AFileName: String; AFont: TMatrixFont; ACols, AScale, ASpace: Integer;
  AColorBG, AColor0, AColor1: TColor; ALabelFont: String);

function IsImageContainFontSet(AFileName: String; out AMetaData: String): Boolean;
function ImportFontFromPNG(AFileName, AMetaData: String; AFontOut: TMatrixFont): Boolean;


implementation

function SwapDWord(This: DWord): DWord;
  begin
    {$IFDEF ENDIAN_LITTLE}
    Result := Swap(This);
    {$ELSE}
    Result := This;
    {$ENDIF}
  end;


function AddTextChunkToPNG(const AFilename, AKey, AValue: String): Boolean;
  var
    _fs:    TFileStream;
    _chunk: TMemoryStream;
    _len:   LongWord;
    _data:  String;
    IEND:   array[0..11] of Byte;
  begin
    Result := False;
    if not (Length(AKey) in [1..79]) then Exit;

    _fs := TFileStream.Create(AFilename, fmOpenReadWrite);

      try
      _fs.Seek(-12, soFromEnd);     // go to 'IEND' chunk position
      _fs.Read(IEND, SizeOf(IEND)); // backup 'IEND' chunk
      _fs.Seek(-12, soFromEnd);     // go to 'IEND' chunk position

      _chunk  := TMemoryStream.Create;
        try
        _data := AKey + #0 + AValue;
        _len  := Length(_data);
        _data := 'tEXt' + _data;

        _chunk.Write(SwapDWord(_len), 4);
        _chunk.Write(_data[1], Length(_data));
        _chunk.Write(SwapDWord(CalculateCRC(_data[1], Length(_data))), 4);

        _chunk.Position := 0;
        _fs.CopyFrom(_chunk, 0);
        _fs.Write(IEND, SizeOf(IEND));

        Result := True;
        finally
        _chunk.Free;
        end;

      finally
      _fs.Free;
      end;
  end;

function GetTextChunkFromPNG(const AFilename, AKey: String): String;
  var
    _fs:              TFileStream;
    _buf:             TBytes;
    _name:            array[0..3] of Char;
    _tmp, _len, _crc: LongWord;
  begin
    Result := '';

    _fs := TFileStream.Create(AFilename, fmOpenRead);
      try
      // skip header
      _fs.Seek(8, soFromBeginning);

      // search for 'tEXt' chunk
      while _fs.Read(_len, 4) > 0 do
        begin
        _len := SwapDWord(_len);
        _fs.Read(_name, 4);

        if _name <> 'tEXt' then
          _fs.Seek(_len + 4, soFromCurrent) // skip
        else
          begin
          SetLength(_buf, _len);
          _fs.Read(_buf[0], _len);
          _fs.Read(_tmp, 4);

          _crc := CalculateCRC($FFFFFFFF, _name[0], 4);
          _crc := CalculateCRC(_crc, _buf[0], _len);
          _crc := SwapDWord(_crc xor $FFFFFFFF);
          if _crc <> _tmp then Exit;

          Result := String(StringOf(_buf));
          if Result.StartsWith(AKey + #0) then
            Break else
            Result := '';
          end;
        end;
      finally
      _fs.Free;
      end;

    Result := Result.Replace(AKey + #0, '');
  end;


procedure ImplantWatermark(ABitmap: TBitmap; AData: String);
  var
    i, j, x, y: Integer;
    b:          Byte;
    _buf:       TBytes;
    _crc:       Cardinal;
  begin
    _buf := BytesOf(Chr(AData.Length div $100) + Chr(AData.Length mod $100) + AData + '_crc');
    _crc := CalculateCRC(_buf[0], Length(_buf) - 4);

    for i := 1 to 4 do
      begin
      _buf[Length(_buf) - i] := _crc mod $100;
      _crc := _crc div $100;
      end;

    for i := 0 to High(_buf) do
      begin
      y := (i * 8) div ABitmap.Width;
      x := (i * 8) mod ABitmap.Width;
      b := _buf[i];

      for j := 0 to 7 do
        with ABitmap.Canvas do
          begin
          Pixels[x, y] := 2 * (Pixels[x, y] div 2);
          Pixels[x, y] := Pixels[x, y] + (b mod 2);

          b := b div 2;
          x += 1;
          end;
      end;
  end;

function ExtractWatermark(ABitmap: TBitmap): String;
  var
    i, x, y: Integer;
    _len:    Integer;
    _buf:    TBytes;
    _crc:    Cardinal = 0;

  function ReadByte: Byte;
    var
      i: Integer;
      r: Integer = 0;
    begin
      if x + 8 > ABitmap.Width then
        begin
        x := 0;
        y += 1;
        end;

      for i := 0 to 7 do
        r := r div 2 + $80 * (ABitmap.Canvas.Pixels[x + i, y] mod 2);

      x      += 8;
      Result := Byte(r);
    end;

  begin
    x      := 0;
    y      := 0;
    Result := '';

    SetLength(_buf, 2);
    _buf[0] := ReadByte;
    _buf[1] := ReadByte;
    _len    := _buf[0] * $100 + _buf[1];

    with ABitmap.Canvas do
      if _len > Width * Height div 8 then Exit;

    SetLength(_buf, _len + 2);
    for i := 1 to _len do _buf[1 + i] := ReadByte;
    for i := 1 to 4 do _crc := _crc * $100 + ReadByte;

    if _crc <> CalculateCRC(_buf[0], Length(_buf)) then
      Result := '' else
      Result := String(StringOf(_buf)).Remove(0, 2);
  end;


procedure RenderMapToPNG(AFileName: String; AFont: TMatrixFont; ACols, AScale, ASpace: Integer;
  AColorBG, AColor0, AColor1: TColor; ALabelFont: String);

  const
    MX: Integer = 3; // top left char horz offset (counting in char places)
    MY: Integer = 5; // top left char vert offset (counting in char places)

  var
    _export, _bmp: TBitmap;
    _scaled:       TPicture;
    _w, _h, _r:    Integer;
    _meta:         String;

  procedure ExportInit(AMapX, AMapY: Integer);
    var
      _txtExt: TSize;
    begin
      _w := ASpace * 2 + AFont.Width;
      _h := ASpace * 2 + AFont.Height;
      _r := 1 + (AFont.FontLength - 1) div ACols;

      _bmp    := TBitmap.Create;
      _export := TBitmap.Create;
      _scaled := TPicture.Create;

      _bmp.SetSize(AFont.Width, AFont.Height);
      _export.SetSize((ACols + AMapX) * _w, (_r + AMapY) * _h);
      _scaled.Bitmap.SetSize(AScale * _export.Width, AScale * _export.Height);

      with _export.Canvas do
        begin
        Brush.Color := AColorBG;
        Clear;
        Clear;
        end;

      with _scaled.Bitmap.Canvas do
        begin
        Brush.Color := AColorBG;
        Font.Color  := AColor1;
        Font.Name   := ALabelFont;
        _txtExt     := TextExtent('0');
        Font.Height := trunc(_w * AScale / 4 / _txtExt.Width * _txtExt.Height);
        end;
    end;

  procedure DrawChar(AIndex, AX, AY: Integer);
    begin
      if not (AIndex in [0..AFont.FontLength - 1]) then Exit;
      AFont.Item[AIndex].Draw(_bmp, False, AColor0, AColor1);
      _export.Canvas.Draw(_w * AX + ASpace, _h * AY + ASpace, _bmp);
    end;

  procedure DrawMap(AX, AY: Integer);
    var
      x, y: Integer;
    begin
      // render char map in 1:1 AScale
      for x := 0 to ACols - 1 do
        for y := 0 to _r - 1 do
          DrawChar(y * ACols + x, x + AX, y + AY);

      // output bitmap scaling
      with _scaled.Bitmap.Canvas do
        StretchDraw(Rect(0, 0, Width, Height), _export);
    end;

  procedure DrawInfo(AX, AY: Integer; S: String; CenterX: Boolean = False);
    var
      dx, dy: Integer;
    begin
      with _scaled.Bitmap.Canvas do
        begin
        if AX < 0 then
          dx := (Width - TextWidth(s)) div 2 else
          dx := _w * AScale * AX + CenterX.Select((_w * AScale - TextWidth(s)) div 2, 0);
        dy := _h * AScale * AY + (_h * AScale - TextHeight(s)) div 2;
        TextOut(dx, dy, S);
        end;
    end;

  procedure DrawOffset(AIndex, AX, AY: Integer; AHorz: Boolean);
    var
      s: String;
    begin
      if not AIndex in [0..AFont.FontLength] then Exit;
      if AHorz then
        s := Format('+%x', [AIndex]) else
        s := Format('%.2x', [AFont.FontStartItem + AIndex * ACols]);
      DrawInfo(AX, AY, s, True);
    end;

  procedure DrawOffsets(AX, AY: Integer);
    var
      i: Integer;
    begin
      // horizontal offsets
      for i := 0 to ACols - 1 do DrawOffset(i, i + AX, AY - 2, True);

      // vertical offsets
      for i := 0 to _r - 1 do DrawOffset(i, AX - 2, i + AY, False);
    end;

  function EncodeMetadata(AOffsetX, AOffsetY: Integer): String;
    begin
      with TStringList.Create do
        begin
        Add('matrixFontMeta'); // magic
        Add(AFont.Name);
        Add(AFont.Author);
        Add(AFont.Encoding);
        Add(AFont.AppChange);
        Add(AFont.FontStartItem.ToString);
        Add(AFont.FontLength.ToString);
        Add(AFont.Width.ToString);
        Add(AFont.Height.ToString);
        Add(AScale.ToString);
        Add(ASpace.ToString);
        Add(ACols.ToString);
        Add(AOffsetX.ToString);
        Add(AOffsetY.ToString);
        Result := CommaText;
        Free;
        end;
    end;

  begin
    if not Assigned(AFont) then Exit;

      try
      ExportInit(MX + 1, MY + 1);
      _meta := EncodeMetadata(MX, MY);

      DrawMap(MX, MY);
      DrawOffsets(MX, MY);
      DrawInfo(-1, 0, Format('Font "%s" by ©%s', [AFont.Name, AFont.Author]));
      DrawInfo(-1, 1, Format('Encoding: %s', [AFont.Encoding]));
      DrawInfo(-1, MY + _r, 'Created in ' + GetAppNameVersion);

      // place digital watermark on image with font metadata
      ImplantWatermark(_scaled.Bitmap, _meta);

      _scaled.SaveToFile(AFileName, 'png');

      finally
      FreeAndNil(_bmp);
      FreeAndNil(_export);
      FreeAndNil(_scaled);

      AddTextChunkToPNG(AFileName, 'matrixFontMeta', EncodeStringBase64(_meta));
      end;
  end;

function IsImageContainFontSet(AFileName: String; out AMetaData: String): Boolean;
  begin
    Result    := False;
    AMetaData := '';

    with TPicture.Create do
      try
      LoadFromFile(AFileName);

      if LowerCase(ExtractFileExt(AFileName)) = '.png' then
        AMetaData := DecodeStringBase64(GetTextChunkFromPNG(AFileName, 'matrixFontMeta'));

      if AMetaData.IsEmpty then AMetaData := ExtractWatermark(Bitmap);
      finally
      Free;
      end;

    Result := not AMetaData.IsEmpty;
  end;

function ImportFontFromPNG(AFileName, AMetaData: String; AFontOut: TMatrixFont): Boolean;
  var
    pic:      TPicture;
    pixRef:   TColor;
    isRefSet: Boolean = False;
    ch:       Integer = 0;
    scale, space, cols, offsetX, offsetY: Integer;

  function DecodeMetadata(S: String): Boolean;
    var
      i: Integer = 0;
      w: Integer;
      l: TStringList;

    function NextString: String;
      begin
        Result := l.Strings[i];
        Inc(i);
      end;

    function NextInteger: Integer;
      begin
        Result := NextString.ToInteger;
      end;

    begin
      Result      := False;
      l           := TStringList.Create;
      l.CommaText := S;

      if NextString.Equals('matrixFontMeta') then
        begin
        with AFontOut do
          begin
          Result        := True;
          Name          := NextString;
          Author        := NextString;
          Encoding      := NextString;
          AppCreate     := NextString;
          AppCurrent    := GetAppNameVersion;
          FontStartItem := NextInteger;
          FontLength    := NextInteger;
          w             := NextInteger;
          SetSize(w, NextInteger);
          end;

        scale   := NextInteger;
        space   := NextInteger;
        cols    := NextInteger;
        offsetX := NextInteger;
        offsetY := NextInteger;
        end;

      l.Free;
    end;

  procedure GetRefPixel(AX, AY: Integer);
    begin
      if not isRefSet then
        begin
        pixRef   := pic.Bitmap.Canvas.Pixels[AX, AY];
        isRefSet := True;
        end;
    end;

  function ExtractNextChar: Boolean;
    var
      i, j, x, y: Integer;
    begin
      Result := False;
        try
        if not (ch in [0..AFontOut.FontLength - 1]) then Exit;

        x := scale * (space + (offsetX + ch mod cols) * (2 * space + AFontOut.Width));
        y := scale * (space + (offsetY + ch div cols) * (2 * space + AFontOut.Height));
        GetRefPixel(x, y);

        for i := 0 to AFontOut.Width - 1 do
          for j := 0 to AFontOut.Height - 1 do
            with pic.Bitmap.Canvas do
              AFontOut.Item[ch].PixelAction(i, j,
                (abs(Pixels[x + i * scale, y + j * scale] - pixRef) > 1).Select(paSet, paClear));

        Result := True;
        Inc(ch);
        except
        end;
    end;

  function ExtractFont: Boolean;
    begin
      Result := False;
      while ExtractNextChar do ;
      Result := True;
    end;

  begin
      try
      pic    := TPicture.Create;
      pic.LoadFromFile(AFileName);
      Result := DecodeMetadata(AMetaData) and ExtractFont;
      except
      Result := False;
      end;

    pic.Free;
  end;


end.
