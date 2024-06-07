
{ Unit u_rbf.pas
  ------------------------------------------------------------------------------
  RBF (Raster Bitmap Font) to matrixFont.RHF bidirectional converter class.
  Instance of `TRBFFontConverter` with name `rbfConverter` is already created.
  This file is part of `matrixFont` project.
  ------------------------------------------------------------------------------
  (c) Riva, 2024.05.29 - 2024.05.30
  https://riva-lab.gitlab.io        https://gitlab.com/riva-lab
  ==============================================================================

  Influenced by https://sourceforge.net/projects/rbfeditor/
  RBF fonts are used in some e-book readers and also in CHDK firmware.
}
unit u_rbf;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, u_helpers, font, symbol;

const
  RBF_EXTENSION = 'rbf';

type

  { RBF font structure
    ------------------
    This two commented fields implemented directly in the code,
    since FPC includes string length byte, that needs to be omitted.
    magic:     String[8];  // magic sequence
    fname:     String[64]; // name of font (max 64 characters)
  }
  TRBFHeader = record
    CharSize:  Int32;      // bytes occupied by one char, multiplier of 8
    Points:    Int32;      // font size in points
    Height:    Int32;      // font element height in pixels
    MaxWidth:  Int32;      // width of widest char in pixels
    CharFirst: Int32;      // ASCII code of first char of font
    CharLast:  Int32;      // ASCII code of last char of font
    Unknown4:  Int32;      // somewhat tricky
    WMapAddr:  Int32;      // offset of char width map
    CMapAddr:  Int32;      // offset of char map
    Baseline:  Int32;      // font baseline offset from bottom of char canvas
    Interline: Int32;      // interline spacing
  end;


  { TRBFFontConverter }

  TRBFFontConverter = class

  private
    FFont: TMatrixFont;

  const
    FNT_HDR_MAGIC = #224#14#240#13#3#0#0#0''; // E0 0E F0 0D 03 00 00 00
    FNT_HDR_SIZE  = 116;
    FNT_MAX_NAME  = 64;

  public
    Spacing:   Integer; // spacing between characters
    Interline: Integer; // spacing between lines
    Baseline:  Integer; // baseline offset from bottom of char canvas
    Monospace: Boolean; // if TRUE then width will be the same for all chars
    SpaceWdth: Integer; // space char #32 width in % of font width
    Name:      String[FNT_MAX_NAME];

    constructor Create;
    destructor Destroy; override;

    procedure AssignRHF(AFont: TMatrixFont);
    procedure LoadFromFile(AFilename: String);
    procedure SaveToFile(AFilename: String);
  end;

var
  rbfConverter: TRBFFontConverter;

implementation

{ TRBFFontConverter }

constructor TRBFFontConverter.Create;
  begin
    Spacing   := 1;
    Interline := 2;
    Baseline  := 2;
    Monospace := False;
    SpaceWdth := 40;
  end;

destructor TRBFFontConverter.Destroy;
  begin
    inherited Destroy;
  end;

procedure TRBFFontConverter.AssignRHF(AFont: TMatrixFont);
  begin
    FFont := AFont;
    Name  := FFont.Name;
  end;

procedure TRBFFontConverter.LoadFromFile(AFilename: String);
  var
    memstr: TMemoryStream;
    header: TRBFHeader;

  procedure ReadHeaderAndWidth;
    var
      str: String;
    begin
      SetLength(str, SizeOf(FNT_HDR_MAGIC));
      memstr.ReadBuffer(str[1], SizeOf(FNT_HDR_MAGIC));
      if str <> FNT_HDR_MAGIC then Exit;

      SetLength(str, FNT_MAX_NAME);
      memstr.ReadBuffer(str[1], FNT_MAX_NAME);
      FFont.Name   := str.Remove(str.IndexOf(Chr(0)));
      FFont.Author := 'RBF2RHF Import';

      memstr.ReadBuffer(header, SizeOf(header));

      with header do
        begin
        FFont.Height        := Height;
        FFont.Width         := MaxWidth;
        FFont.FontStartItem := CharFirst;
        FFont.FontLength    := CharLast - FFont.FontStartItem + 1;
        Self.Baseline       := Baseline;
        Self.Interline      := Interline;
        end;

      SetLength(str, FFont.FontLength);
      memstr.ReadBuffer(str[1], FFont.FontLength);
    end;

  procedure ReadCharData;
    var
      i, w, h, wmax: Integer;
      b:             Byte;
    begin
      wmax := 8 * header.CharSize div FFont.Height - 1;

      for i := 0 to FFont.FontLength - 1 do
        for h := 0 to FFont.Height - 1 do
          for w := 0 to wmax do
            begin
              try
              if w mod 8 = 0 then b := memstr.ReadByte;
              except
              b := 0;
              end;

            if (w < FFont.Width) and (b and 1 > 0) then
              FFont.Item[i].PixelAction(w, h, paSet);
            b := b div 2;
            end;
    end;

  begin
    if not Assigned(FFont) then Exit;

    memstr := TMemoryStream.Create;
    memstr.LoadFromFile(AFilename);

    ReadHeaderAndWidth;
    ReadCharData;

    memstr.Free;
  end;

procedure TRBFFontConverter.SaveToFile(AFilename: String);
  var
    memstr: TMemoryStream;

  procedure WriteHeader;
    var
      header:  TRBFHeader;
      fntname: String[FNT_MAX_NAME];
    begin
      with header do
        begin
        MaxWidth  := FFont.Width + Spacing;
        CharSize  := (MaxWidth + 7) div 8 * FFont.Height;
        Points    := FFont.Height;
        Height    := FFont.Height;
        CharFirst := FFont.FontStartItem;
        CharLast  := FFont.FontStartItem + FFont.FontLength - 1;
        Unknown4  := 32;
        WMapAddr  := FNT_HDR_SIZE;
        CMapAddr  := WMapAddr + FFont.FontLength;
        Baseline  := Self.Baseline;
        Interline := Self.Interline;
        end;

      FillChar(fntname[1], FNT_MAX_NAME, 0);
      fntname := Name;

      memstr.Write(FNT_HDR_MAGIC, SizeOf(FNT_HDR_MAGIC));
      memstr.Write(fntname[1], FNT_MAX_NAME);
      memstr.Write(header, SizeOf(header));
    end;

  procedure WriteCharWidth;
    var
      i, w: Integer;
    begin
      for i := 0 to FFont.FontLength - 1 do
        begin
        if Monospace then
          w := FFont.Width + Spacing else
          w := FFont.Item[i].GetCharWidth + Spacing;

        if not Monospace and (FFont.FontStartItem + i = 32) then
          w := round(FFont.Width * SpaceWdth / 100);

        memstr.WriteByte(w);
        end;
    end;

  procedure WriteCharData;
    var
      i, w, h, wmax: Integer;
      b:             Byte = 0;
    begin
      wmax := ((FFont.Width + Spacing + 7) div 8) * 8 - 1;

      for i := 0 to FFont.FontLength - 1 do
        for h := 0 to FFont.Height - 1 do
          for w := 0 to wmax do
            begin
            b := b div 2;
            if (w < FFont.Width) and FFont.Item[i].CharCanvas[w, h] then b += $80;

            if (w mod 8 = 7) or (w = wmax) then
              begin
              memstr.WriteByte(b);
              b := 0;
              end;
            end;
    end;

  begin
    if not Assigned(FFont) then Exit;

    memstr := TMemoryStream.Create;

    WriteHeader;
    WriteCharWidth;
    WriteCharData;

    memstr.SaveToFile(AFilename);
    memstr.Free;
  end;


initialization
  rbfConverter := TRBFFontConverter.Create;

end.
