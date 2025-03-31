
{ Unit u_imghist.pas
  ------------------------------------------------------------------------------
  TBGRABitmapHistory class for saving history of BGRABitmap changes.

  The class works with bitonal bitmaps which have only black and white pixels.
  History has its own depth, so if you commit a new save when the history
  is already full then the very old data will be lost.
  DO NOT resize (width and height) the bitmap. This will result in unexpected
  behavior. Always call the `Clear` method after resizing the bitmap.

  This file is a part of the `matrixFont` project.
  ------------------------------------------------------------------------------
  (c) Riva, 2025.03.31
  https://riva-lab.gitlab.io        https://gitlab.com/riva-lab/matrixFont
  ==============================================================================
}
unit u_imghist;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes;

type

  { TBGRABitmapHistory }

  TBGRABitmapHistory = class
  private
    FBGRABmp:   TBGRABitmap;
    FStream:    TMemoryStream;
    FBlockSize: Integer;
    FCurrent:   Integer;
    FDepth:     Integer;
    FOffset:    Integer;
    FSaved:     Integer;

    procedure SetStreamPosition;
    procedure WriteToStream;
    procedure ReadFromStream;

    function IsInvalidParameters: Boolean;

  public
    procedure SetWorkBitmap(ABitmap: TBGRABitmap);

    procedure Clear;
    procedure Save;
    procedure Undo;
    procedure Redo;

    function CanUndo: Boolean;
    function CanRedo: Boolean;

    constructor Create;
    constructor Create(AWorkBitmap: TBGRABitmap);
    destructor Destroy; override;

    property Depth: Integer read FDepth write FDepth;

  const
    DefaultDepth = 100;
  end;

implementation

{ TBGRABitmapHistory }

procedure TBGRABitmapHistory.SetStreamPosition;
  begin
    FStream.Position := ((FOffset + FCurrent) mod FDepth) * FBlockSize;
  end;

procedure TBGRABitmapHistory.WriteToStream;
  var
    px:    PBGRAPixel;
    y, x:  Integer;
    i:     DWord = 0;
    sData: Byte = 0;
  begin
    if IsInvalidParameters then Exit;
    SetStreamPosition;

    for y := 0 to FBGRABmp.Height - 1 do
      begin
      px := FBGRABmp.ScanLine[y];

      for x := 0 to FBGRABmp.Width - 1 do
        begin
        sData := (sData shr 1) + (px^.red) and $80;

        i := (i - 1) and 7;

        if i = 0 then
          begin
          FStream.WriteByte(sData);
          sData := 0;
          end;

        Inc(px);
        end;
      end;
    FStream.WriteByte(sData shr i);
  end;

procedure TBGRABitmapHistory.ReadFromStream;
  var
    px:    PBGRAPixel;
    y, x:  Integer;
    i:     DWord = 0;
    sData: Byte = 0;
  begin
    if IsInvalidParameters then Exit;
    SetStreamPosition;

    for y := 0 to FBGRABmp.Height - 1 do
      begin
      px := FBGRABmp.ScanLine[y];

      for x := 0 to FBGRABmp.Width - 1 do
        begin
        if i = 0 then sData := not FStream.ReadByte;

        px^.red   := Byte((sData and 1) - 1);
        px^.green := px^.red;
        px^.blue  := px^.red;
        px^.alpha := 255;
        sData     := sData shr 1;

        i := (i - 1) and 7;
        Inc(px);
        end;
      end;
    FBGRABmp.InvalidateBitmap;
  end;

function TBGRABitmapHistory.IsInvalidParameters: Boolean;
  begin
    Result := not Assigned(FBGRABmp);
    Result := Result or (FBlockSize <= 1);
  end;

procedure TBGRABitmapHistory.SetWorkBitmap(ABitmap: TBGRABitmap);
  begin
    FBGRABmp := ABitmap;
    Clear;
  end;

procedure TBGRABitmapHistory.Clear;
  begin
    FCurrent := -1;
    FSaved   := 0;
    FOffset  := 0;
    FStream.Clear;

    if Assigned(FBGRABmp) then
      FBlockSize := FBGRABmp.Width * FBGRABmp.Height div 8 + 1;
  end;

procedure TBGRABitmapHistory.Save;
  begin
    if FCurrent < FDepth - 1 then
      FCurrent += 1
    else
      FOffset  := (FOffset + 1) mod FDepth;

    FSaved := FCurrent;

    WriteToStream;
  end;

procedure TBGRABitmapHistory.Undo;
  begin
    if not CanUndo then Exit;
    FCurrent -= 1;
    ReadFromStream;
  end;

procedure TBGRABitmapHistory.Redo;
  begin
    if not CanRedo then Exit;
    FCurrent += 1;
    ReadFromStream;
  end;

function TBGRABitmapHistory.CanUndo: Boolean;
  begin
    Result := FCurrent > 0;
  end;

function TBGRABitmapHistory.CanRedo: Boolean;
  begin
    Result := FCurrent < FSaved;
  end;

constructor TBGRABitmapHistory.Create;
  begin
    inherited Create;
    FBGRABmp := nil;
    FDepth   := DefaultDepth;
    FStream  := TMemoryStream.Create;
    Clear;
  end;

constructor TBGRABitmapHistory.Create(AWorkBitmap: TBGRABitmap);
  begin
    Create;
    SetWorkBitmap(AWorkBitmap);
  end;

destructor TBGRABitmapHistory.Destroy;
  begin
    FreeAndNil(FStream);
    inherited Destroy;
  end;

end.
