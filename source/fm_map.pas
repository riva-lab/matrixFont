unit fm_map;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Types, SysUtils, Forms, Controls, Graphics, Grids, ExtCtrls, StdCtrls,
  Spin, IniPropStorage, LazUTF8,
  fm_settings,
  font, u_encodings, u_helpers;

resourcestring
  FM_MAP_CAPTION = 'Карта символов';

type

  { TfmMap }

  TfmMap = class(TForm)
    cbMapWidth:    TComboBox;
    IniStorageMap: TIniPropStorage;
    lbMapCols:     TLabel;
    lbMapOffset:   TLabel;
    pMapControls:  TPanel;
    seSpaceX:      TSpinEdit;
    seSpaceY:      TSpinEdit;
    sgMap:         TStringGrid;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sgMapDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure sgMapSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure sgMapChangeBounds(Sender: TObject);
    procedure sgMapSelection(Sender: TObject; aCol, aRow: Integer);
    procedure sgMapDblClick(Sender: TObject);

  private
    WantSingleClick: Boolean;

    procedure DoMouseClick(IsDouble: Boolean);

  public
    FontX:         TFont;
    OnMouseEvent:  TMouseEvent;
    SelectedIndex: Integer;

    procedure UpdateMap;
  end;

var
  fmMap: TfmMap;

implementation

{$R *.lfm}

{ TfmMap }

procedure TfmMap.FormCreate(Sender: TObject);
  begin
    FontX           := nil;
    OnMouseEvent    := nil;
    WantSingleClick := False;

    IniStorageMap.IniFileName := ExtractFileDir(ParamStrUTF8(0)) + SETTINGS_FILE;
  end;

procedure TfmMap.FormShow(Sender: TObject);
  var
    w, h: Integer;
  begin
    // init block, executed only once
    if Tag = 0 then
      begin
      if not Showing then Exit;

      cbMapWidth.Constraints.MaxWidth := Canvas.TextWidth('0') * 8;

      w        := Width;
      h        := Height;
      Tag      := 1;
      Scaled   := True;
      AutoSize := True;
      AutoSize := False;
      Position := poMainFormCenter;

      Constraints.MinWidth  := Width;
      Constraints.MinHeight := Height;
      Constraints.MaxWidth  := Screen.Width;
      Constraints.MaxHeight := Screen.Height;

      // for correct restoring form W/H
      Width            := (pMapControls.Tag = 1).Select(w, Width);
      Height           := (pMapControls.Tag = 1).Select(h, Height);
      pMapControls.Tag := 1;
      end;

    UpdateMap;
  end;

procedure TfmMap.sgMapDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
  var
    bm_tmp:      TBitmap;
    dw, dh, i:   Integer;
    _isSelected: Boolean;
    _txtStyle:   TTextStyle;
    _txtExt:     TSize;
  begin
    with sgMap do
      begin
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := fmSettings.ColorNaviBG;
      Canvas.Pen.Width   := 0;
      Canvas.Pen.Style   := psSolid;
      Canvas.Pen.Color   := Canvas.Brush.Color;

      _txtStyle           := Canvas.TextStyle;
      _txtStyle.Alignment := taCenter;
      Canvas.TextStyle    := _txtStyle;
      _txtExt             := Canvas.TextExtent('0');
      Canvas.Font.Height  := trunc(ColWidths[1] / 4 / _txtExt.Width * _txtExt.Height);
      Canvas.Font.Color   := clBlack;

      Canvas.Rectangle(aRect);

      if FontX = nil then Exit;

      if aRow + aCol > 0 then
        if (aRow = 0) or (aCol = 0) then
          begin
          Canvas.TextRect(aRect, aRect.Left, aRect.Top, (aRow = 0).Select(
            Format('+%x', [aCol - 1]),
            Format('%.2x', [FontX.FontStartItem + (aRow - 1) * (ColCount - 1)]))
            );
          end
        else
          begin
          _isSelected := (aRow = Row) and (aCol = Col);
          i           := (aRow - 1) * (ColCount - 1) + (aCol - 1);

          if i in [0 .. FontX.FontLength - 1] then
            begin
              try
              bm_tmp        := TBitmap.Create;
              bm_tmp.Width  := FontX.Width;
              bm_tmp.Height := FontX.Height;
              dw            := round(ColWidths[aCol] * (1 - FontX.Width / (FontX.Width + seSpaceX.Value)));
              dh            := round(RowHeights[aCol] * (1 - FontX.Height / (FontX.Height + seSpaceY.Value)));
              aRect.Left    := aRect.Left + dw div 2;
              aRect.Right   := aRect.Right - dw div 2;
              aRect.Top     := aRect.Top + dh div 2;
              aRect.Bottom  := aRect.Bottom - dh div 2;

              FontX.Item[i].DrawPreview(bm_tmp, False,
                _isSelected.Select(fmSettings.ColorPreviewBG, fmSettings.ColorNaviBG),
                _isSelected.Select(fmSettings.ColorPreviewA, fmSettings.ColorNaviA));

              Canvas.StretchDraw(aRect, bm_tmp);

              finally
              FreeAndNil(bm_tmp);
              end;

            if _isSelected then
              begin
              SelectedIndex := i;
              i             += FontX.FontStartItem;
              fmMap.Caption := FM_MAP_CAPTION + Format(' - %d, %.2x, <%s>',
                [i, i, EncodingToUTF8(Chr(i), FontX.Encoding)]);

              if WantSingleClick then DoMouseClick(False);
              end;
            end;
          end;
      end;
  end;

procedure TfmMap.sgMapSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
  begin
  end;

procedure TfmMap.sgMapChangeBounds(Sender: TObject);
  var
    _colWidth: Double;
  begin
    if FontX = nil then Exit;

    with sgMap do
      begin
      ColCount         := (2 shl cbMapWidth.ItemIndex) + 1;
      RowCount         := (FontX.FontLength - 1) div (ColCount - 1) + 2;
      _colWidth        := Width / ColCount;
      ColWidths[0]     := round(_colWidth);
      DefaultRowHeight := round(_colWidth / (FontX.Width + seSpaceX.Value) * (FontX.Height + seSpaceY.Value));
      ColWidths[0]     := ColWidths[0] + ColWidths[ColCount - 1] - sgMap.ColWidths[1];
      end;
  end;

procedure TfmMap.sgMapSelection(Sender: TObject; aCol, aRow: Integer);
  begin
    // only signal to call event, because of getting correct selected char index
    WantSingleClick := True;
  end;

procedure TfmMap.sgMapDblClick(Sender: TObject);
  begin
    DoMouseClick(True);
  end;

procedure TfmMap.DoMouseClick(IsDouble: Boolean);
  var
    _state: TShiftState = [];
  begin
    WantSingleClick := False;
    if not Assigned(OnMouseEvent) then Exit;
    if IsDouble then _state := [ssDouble];

    with sgMap do
      OnMouseEvent(sgMap, mbLeft, _state, Col - FixedCols, Row - FixedRows);
  end;

procedure TfmMap.UpdateMap;
  begin
    sgMapChangeBounds(nil);
  end;

end.
