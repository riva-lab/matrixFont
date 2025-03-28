
{ Unit u_imghist.pas
  ------------------------------------------------------------------------------
  Unit provides TBGRABitmapHistory class for saving bitmap changes history.
  This file is part of `matrixFont` project.
  ------------------------------------------------------------------------------
  (c) Riva, 2025.03.20
  https://riva-lab.gitlab.io        https://gitlab.com/riva-lab
  ==============================================================================
}
unit u_imghist;

{$mode ObjFPC}{$H+}

interface

uses
  Graphics, SysUtils, LCLType, BGRABitmap, BGRABitmapTypes, ImgList;

type

  { TBGRABitmapHistory }

  TBGRABitmapHistory = class(TCustomImageList)
  private
    FBitmap:  TBitmap;
    FBGRABmp: TBGRABitmap;
    FCurrent: Integer;

    procedure UpdateBitmap;

  public
    procedure SetWorkBitmap(ABitmap: TBitmap);
    procedure SetWorkBitmap(ABitmap: TBGRABitmap);

    procedure Clear;
    procedure Save;
    procedure Undo;
    procedure Redo;

    function CanUndo: Boolean;
    function CanRedo: Boolean;

    constructor Create;
    constructor Create(AWorkBitmap: TBitmap);
    constructor Create(AWorkBitmap: TBGRABitmap);
    destructor Destroy; override;

    property Current: Integer read FCurrent;
  end;

implementation

{ TBGRABitmapHistory }

procedure TBGRABitmapHistory.UpdateBitmap;
  begin
    if Assigned(FBGRABmp) and not Assigned(FBitmap) then
      FBitmap := TBitmap.Create;

    GetBitmap(FCurrent, FBitmap);

    if Assigned(FBGRABmp) then
      begin
      FBGRABmp.PutImage(0, 0, FBitmap, dmSet);
      FreeAndNil(FBitmap);
      end;
  end;

procedure TBGRABitmapHistory.SetWorkBitmap(ABitmap: TBitmap);
  begin
    FBitmap := ABitmap;
    Clear;
  end;

procedure TBGRABitmapHistory.SetWorkBitmap(ABitmap: TBGRABitmap);
  begin
    FBGRABmp := ABitmap;
    Clear;
  end;

procedure TBGRABitmapHistory.Clear;
  begin
    if Count = 0 then Exit;
    inherited Clear;
    FCurrent := 0;
  end;

procedure TBGRABitmapHistory.Save;
  begin
      try
      while FCurrent < Count - 1 do Delete(Count - 1);

      if Assigned(FBGRABmp) then
        begin
        Width  := FBGRABmp.Width;
        Height := FBGRABmp.Height;
        Add(FBGRABmp.Bitmap, nil);
        end
      else
        begin
        Width  := FBitmap.Width;
        Height := FBitmap.Height;
        Add(FBitmap, nil);
        end;

      FCurrent := Count - 1;
      finally
      end;
  end;

procedure TBGRABitmapHistory.Undo;
  begin
    if not CanUndo then Exit;
    FCurrent -= 1;
    UpdateBitmap;
  end;

procedure TBGRABitmapHistory.Redo;
  begin
    if not CanRedo then Exit;
    FCurrent += 1;
    UpdateBitmap;
  end;

function TBGRABitmapHistory.CanUndo: Boolean;
  begin
    Result := FCurrent > 0;
  end;

function TBGRABitmapHistory.CanRedo: Boolean;
  begin
    Result := FCurrent < Count - 1;
  end;

constructor TBGRABitmapHistory.Create;
  begin
    inherited Create(nil);
    Clear;
  end;

constructor TBGRABitmapHistory.Create(AWorkBitmap: TBitmap);
  begin
    Create;
    SetWorkBitmap(AWorkBitmap);
  end;

constructor TBGRABitmapHistory.Create(AWorkBitmap: TBGRABitmap);
  begin
    Create;
    SetWorkBitmap(AWorkBitmap);
  end;

destructor TBGRABitmapHistory.Destroy;
  begin
    inherited Destroy;
  end;

end.
