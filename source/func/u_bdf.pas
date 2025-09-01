
{ Unit u_bdf.pas
  ------------------------------------------------------------------------------
  BDF v2.1 (Glyph Bitmap Distribution Format) to matrixFont.RHF
  bidirectional converter class.
  Instance of `TBDFFontConverter` with name `bdfConverter` is already created.
  This file is part of `matrixFont` project.
  ------------------------------------------------------------------------------
  (c) Riva, 2025
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
  Classes, SysUtils, u_helpers, font, symbol;

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

  procedure ReadHeaderAndWidth;
    begin
    end;

  procedure ReadCharData;
    begin
    end;

  begin
    if not Assigned(FFont) then Exit;

    ReadHeaderAndWidth;
    ReadCharData;
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
