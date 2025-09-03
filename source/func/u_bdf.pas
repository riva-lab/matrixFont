
{ Unit u_bdf.pas
  ------------------------------------------------------------------------------
  BDF v2.1 (Glyph Bitmap Distribution Format) to matrixFont.RHF
  bidirectional converter class.
  Notes:
    1. BDF file may contain more than 256 chars, so only a subset will be
       imported, according to current project's codepage.
    2. Canvas size may be extended, but not shrunk. It extends when
       current project's canvas is too small for imported BDF chars.
  Instance of `TBDFFontConverter` with name `bdfConverter` is already created.
  This file is part of `matrixFont` project.
  ------------------------------------------------------------------------------
  (c) Riva, 2025.09.02
  https://riva-lab.gitlab.io        https://gitlab.com/riva-lab

  BDF Specification by Adobe Systems Incorporated:
  https://www.adobe.com/content/dam/Adobe/en/devnet/font/pdfs/5005.BDF_Spec.pdf
  ==============================================================================

  BDF is typically used in Unix X Window environments.
}
unit u_bdf;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, LazUTF8, u_helpers, u_encodings, regexpr,
  font, symbol;

const
  BDF_EXTENSION = 'bdf';

type

  { TBDFCharParameters }

  TBDFCharParameters = record
    id:       String;
    Width:    Integer; // width of black pixels (common use)
    Height:   Integer; // height of black pixels (common use)
    SWx:      Integer; // scalable width of glyph
    DWx:      Integer; // device pixels width of glyph
    BBw:      Integer; // width of black pixels
    ofx:      Integer; // X offset of lower left corner of bitmap from origin 0
    ofy:      Integer; // Y offset of lower left corner of bitmap from origin 0
    spLeft:   Integer; // empty space on the left in original RHF canvas
    spRight:  Integer; // empty space on the right in original RHF canvas
    spTop:    Integer; // empty space on the top in original RHF canvas
    spBottom: Integer; // empty space on the bottom in original RHF canvas
  end;


  { TBDFFontConverter }

  TBDFFontConverter = class

  private
    FData:      String;
    FFont:      TMatrixFont;
    FFile:      TextFile;
    FPointSize: Integer;
    FontDPI:    Integer;

  const
    CHR_MAX_NAME = 14;

  public
    Spacing:   Integer; // spacing between characters (if proportional)
    Baseline:  Integer; // baseline offset from bottom of char canvas
    Monospace: Boolean; // if TRUE then width will be the same for all chars
    SpaceWdth: Integer; // space char #32 width in % of font width
    DefChar:   Integer; // default char
    Name:      String;  // user-redefined name

    constructor Create;
    destructor Destroy; override;

    procedure AssignRHF(AFont: TMatrixFont);
    procedure LoadFromFile(AFilename: String);
    procedure SaveToFile(AFilename: String);
  end;


var
  bdfConverter: TBDFFontConverter;


implementation

const
  BDF_HEADER_TEMPLATE =
    'STARTFONT 2.1' + LineEnding +
    'COMMENT %bdfComment%' + LineEnding +
    'FONT %bdfFontnameXLFD%' + LineEnding +
    'SIZE %bdfPointSize% %bdfXres% %bdfYres%' + LineEnding +
    'FONTBOUNDINGBOX %bdfFBBx% %bdfFBBy% %bdfXoff% %bdfYoff%' + LineEnding +
    'STARTPROPERTIES 8' + LineEnding +
    'FOUNDRY "%bdfAuthor%"' + LineEnding +
    'FONT_NAME "%bdfFontname%"' + LineEnding +
    'COPYRIGHT "%bdfCopyright%"' + LineEnding +
    'RESOLUTION_X %bdfXres%' + LineEnding +
    'RESOLUTION_Y %bdfYres%' + LineEnding +
    'FONT_ASCENT %bdfFontAscent%' + LineEnding +
    'FONT_DESCENT %bdfFontDescent%' + LineEnding +
    'DEFAULT_CHAR %bdfDefCharCode%' + LineEnding +
    'ENDPROPERTIES' + LineEnding +
    'CHARS %bdfFontLength%' + LineEnding +
    '%bdfChars%ENDFONT' + LineEnding;

  BDF_CHAR_TEMPLATE   =
    'STARTCHAR %bdfCharName%' + LineEnding +
    'ENCODING %bdfCharCode%' + LineEnding +
    'SWIDTH %bdfCharSWX0% %bdfCharSWY0%' + LineEnding +
    'DWIDTH %bdfCharDWX0% %bdfCharDWY0%' + LineEnding +
    'BBX %bdfCharBBw% %bdfCharBBh% %bdfCharBBxoff0x% %bdfCharBByoff0y%' + LineEnding +
    'BITMAP' + LineEnding +
    '%bdfCharBitmap%ENDCHAR' + LineEnding;


  { TBDFFontConverter }

constructor TBDFFontConverter.Create;
  begin
    Spacing   := 1;
    Baseline  := 2;
    Monospace := False;
    SpaceWdth := 40;
    DefChar   := 32;
    FontDPI   := 96;
  end;

destructor TBDFFontConverter.Destroy;
  begin
    inherited Destroy;
  end;

procedure TBDFFontConverter.AssignRHF(AFont: TMatrixFont);
  begin
    FFont      := AFont;
    Name       := FFont.Props.Name;
    FPointSize := FFont.Height;
  end;

procedure TBDFFontConverter.LoadFromFile(AFilename: String);
  var
    _re:       TRegExpr;
    p:         TBDFCharParameters;
    firstRead: Boolean;

  function GetToken(AInput, ARegEx: String; AMatchGroupId: Integer): String;
    var
      re: TRegExpr;
    begin
      re := TRegExpr.Create;

        try
        re.ModifierI   := True;
        re.ModifierM   := True;
        re.InputString := UnicodeString(AInput);
        re.Expression  := UnicodeString(ARegEx);
        if re.Exec then
          Result := String(re.Match[AMatchGroupId]).Trim;
        except
        Result := '';
        end;

      re.Free;
    end;

  function GetStr(AInput, AToken: String; ADefault: String = ''): String;
    begin
      Result := GetToken(AInput, '^' + AToken + '\s+(.+?)$', 1)
        .Trim('"').Replace('""', '"');
      if Result.IsEmpty then Result := ADefault;
    end;

  function GetInt(AInput, AToken: String; AIndex: Integer): Integer;
    begin
      Result := StrToIntDef(GetToken(AInput, '^' + AToken +
        '\s+([+-]?\d+)' +
        '(?:[\s\.]+([+-]?\d+))?' +
        '(?:[\s\.]+([+-]?\d+))?' +
        '(?:[\s\.]+([+-]?\d+))?.*?$', AIndex),
        0);
    end;

  function IsVersionUnsupported: Boolean;
    begin
      Result := not (
        (GetInt(FData, 'STARTFONT', 1) = 2) and
        (GetInt(FData, 'STARTFONT', 2) >= 1));
    end;

  procedure ReadHeader;
    begin
      FFont.Props.Name   := GetStr(FData, 'FONT_NAME', GetStr(FData, 'FONT'));
      FFont.Props.Author := GetStr(FData, 'FOUNDRY', GetStr(FData, 'COPYRIGHT'));

      Baseline := GetInt(FData, 'FONT_DESCENT', 1);
      p.Width  := Max(GetInt(FData, 'FONTBOUNDINGBOX', 1), FFont.Width);
      p.Height := Max(GetInt(FData, 'FONT_ASCENT', 1) + Baseline, FFont.Height);

      FFont.SetRange(0, 255);
      FFont.SetSize(p.Width, p.Height);
      FFont.Clear;
    end;

  procedure ReadCharsInit;
    begin
      _re := TRegExpr.Create;

        try
        firstRead       := True;
        _re.ModifierI   := True;
        _re.ModifierM   := True;
        _re.InputString := UnicodeString(FData);
        _re.Expression  := UnicodeString('^STARTCHAR\s+(.+?)ENDCHAR$');
        except
        end;
    end;

  function GetNextChar: String;
    begin
        try
        if (firstRead and _re.Exec) or (not firstRead and _re.ExecNext) then
          Result := String(_re.Match[1])
        else
          Result := '';

        firstRead := False;
        except
        Result    := '';
        end;
    end;

  procedure ReadCharsFinish;
    begin
      _re.Free;
    end;

  function GetCorrectedIndex(ABuffer: String): Integer;
    var
      tmp: String;
    begin
      Result := GetInt(ABuffer, 'ENCODING', 1);
      if Result < 128 then Exit;
      tmp := UTF8ToEncoding(UnicodeToUTF8(Result), FFont.Props.Encoding);
      if not tmp.IsEmpty then Result := Ord(tmp[1]);
    end;

  procedure ReadBitmap(ABuffer: String; AIndex: Integer);
    var
      buffer:      String;
      i, j, n:     Integer;
      dx, dy, num: Integer;
    begin
      for i := 1 to p.Height do
        begin
        dx     := p.ofx;
        dy     := p.ofy + i - 1;
        buffer := GetToken(ABuffer, Format('^BITMAP\s+(?:.+?\n){%d}(.+?)\n', [i - 1]), 1);

        for j := 1 to buffer.Length do
          begin
          num := StrToInt('$' + buffer[j]);

          for n := 0 to 3 do
            begin
            if num and 8 > 0 then FFont.Item[AIndex].PixelAction(dx, dy, paSet);
            dx  += 1;
            num *= 2;
            end;
          end;
        end;
    end;

  function ReadCharNext: Boolean;
    var
      buffer: String;
      index:  Integer;
    begin
      buffer := GetNextChar;
      if buffer.IsEmpty then Exit(False);

      index := GetCorrectedIndex(buffer);
      if not InRange(index, 0, 255) then Exit(True);

      p.Height := GetInt(buffer, 'BBX', 2);
      p.ofx    := GetInt(buffer, 'BBX', 3);
      p.ofy    := FFont.Height - Baseline - p.Height - GetInt(buffer, 'BBX', 4);
      FFont.Item[index].Clear;
      ReadBitmap(buffer, index);

      Result := True;
    end;

  begin
    if not Assigned(FFont) then Exit;

    FData := GetFileAsString(AFilename);
    if IsVersionUnsupported then Exit;

    ReadHeader;
    ReadCharsInit;
    while ReadCharNext do ;
    ReadCharsFinish;
  end;

procedure TBDFFontConverter.SaveToFile(AFilename: String);
  var
    buffer: String;

  procedure Replace(AFind: String; AValue: String);
    begin
      buffer := buffer.Replace(AFind, AValue);
    end;

  procedure Replace(AFind: String; AValue: Integer);
    begin
      Replace(AFind, AValue.ToString);
    end;

  function GetScalableWidth(ASize: Integer): Integer;
    begin
      Result := round(72000 * ASize / FPointSize / FontDPI);
    end;

  function GetBitmap(AIndex: Integer; ASpace: TRect): String;
    var
      ch:   TMatrixChar;
      h, w: LongInt;
      i:    Integer = 0;
      b:    Byte = 0;
      m:    Byte = $80;

    procedure WriteNextByte;
      begin
        if i > 0 then
          begin
          Result += IntToHex(b, 2);
          i      := 0;
          b      := 0;
          m      := $80;
          end;
      end;

    begin
      Result := '';
      ch     := FFont.Item[AIndex];

      for h := ASpace.Top to ch.Height - ASpace.Bottom - 1 do
        begin
        for w := ASpace.Left to ch.Width - ASpace.Right - 1 do
          begin
          if ch.Canvas.Pixels[w, h] = CHAR_COLOR_FG then b += m;
          m := m div 2;
          i += 1;
          if i mod 8 = 0 then WriteNextByte;
          end;

        WriteNextByte;
        Result += LineEnding;
        end;
    end;

  procedure PrepareCharData;
    var
      i, x: Integer;
      ch:   TMatrixChar;
      p:    TBDFCharParameters;
    begin
      FData := '';

      for i := 0 to FFont.FontLength - 1 do
        begin
        x  := FFont.FontStartItem + i;
        ch := FFont.Item[x];

        p.spLeft   := ch.CanOptimize(dirLeft);
        p.spRight  := ch.CanOptimize(dirRight);
        p.spTop    := ch.CanOptimize(dirUp);
        p.spBottom := ch.CanOptimize(dirDown);

        p.Width := (p.spLeft = ch.Width).Select(
          (ch.Width + Spacing) * SpaceWdth div 100,
          ch.Width - p.spLeft - p.spRight);

        p.Height := (p.spTop = ch.Height).Select(
          0,
          ch.Height - p.spTop - p.spBottom);

        p.SWx := GetScalableWidth(p.Width + Spacing);
        p.DWx := Monospace.Select(ch.Width, (p.spLeft = ch.Width).Select(0, p.spLeft) + p.Width + Spacing);
        p.BBw := (p.spLeft = ch.Width).Select(0, p.Width);
        p.ofx := (p.spLeft = ch.Width).Select(0, p.spLeft);
        p.ofy := (p.spTop = ch.Height).Select(0, p.spBottom - Baseline);
        p.id  := (x > 127).Select('U+' + IntToHex(x, 4), FFont.GetCharName(x));

        buffer := BDF_CHAR_TEMPLATE;
        Replace('%bdfCharName%', p.id);
        Replace('%bdfCharCode%', x);
        Replace('%bdfCharSWX0%', p.SWx);
        Replace('%bdfCharSWY0%', 0);
        Replace('%bdfCharDWX0%', p.DWx);
        Replace('%bdfCharDWY0%', 0);
        Replace('%bdfCharBBw%', p.BBw);
        Replace('%bdfCharBBh%', p.Height);
        Replace('%bdfCharBBxoff0x%', p.ofx);
        Replace('%bdfCharBByoff0y%', p.ofy);
        Replace('%bdfCharBitmap%', GetBitmap(x, Rect(p.spLeft, p.spTop, p.spRight, p.spBottom)));
        FData += buffer;
        end;
    end;

  procedure WriteBDF;
    var
      t: TSystemTime;
    begin
      AssignFile(FFile, AFilename);
      Rewrite(FFile);

      buffer := BDF_HEADER_TEMPLATE;
      DateTimeToSystemTime(FFont.Props.DateChange, t);

      Replace('%bdfComment%', 'Exported by ' + FFont.Props.AppCurrent);
      Replace('%bdfFontnameXLFD%', Name.Replace(' ', '_'));
      Replace('%bdfPointSize%', FPointSize);
      Replace('%bdfXres%', FontDPI);
      Replace('%bdfYres%', FontDPI);
      Replace('%bdfFBBx%', (FFont.Width + Spacing));
      Replace('%bdfFBBy%', FFont.Height);
      Replace('%bdfXoff%', 0);
      Replace('%bdfYoff%', -Baseline);
      Replace('%bdfAuthor%', FFont.Props.Author.Replace('"', '""'));
      Replace('%bdfFontname%', FFont.Props.Name.Replace('"', '""'));
      Replace('%bdfCopyright%', 'Copyright (c) ' + FFont.Props.Author.Replace('"', '""') + ', ' + t.Year.ToString);
      Replace('%bdfFontAscent%', (FFont.Height - Baseline));
      Replace('%bdfFontDescent%', Baseline);
      Replace('%bdfDefCharCode%', DefChar);
      Replace('%bdfFontLength%', FFont.FontLength);
      Replace('%bdfChars%', FData);

      Write(FFile, buffer);
      CloseFile(FFile);
    end;

  begin
    if not Assigned(FFont) then Exit;

    PrepareCharData;
    WriteBDF;
  end;


initialization
  bdfConverter := TBDFFontConverter.Create;

end.
