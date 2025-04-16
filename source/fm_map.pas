unit fm_map;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Types, SysUtils, Forms, Controls, Graphics, Grids, ExtCtrls, StdCtrls,
  Spin, ComCtrls, Dialogs, LazUTF8, AppSettings,
  font, u_encodings, u_map_render, u_helpers, config_record;

resourcestring
  FM_MAP_CAPTION = 'Карта символов';

type

  { TfmMap }

  TfmMap = class(TForm)
    cbMapWidth:   TComboBox;
    lbMapCols:    TLabel;
    lbMapOffset:  TLabel;
    pMapControls: TPanel;
    SaveDlg:      TSaveDialog;
    seSpaceX:     TSpinEdit;
    seSpaceY:     TSpinEdit;
    sgMap:        TStringGrid;
    tbControls:   TToolBar;
    ToolButton1:  TToolButton;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure sgMapDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure sgMapSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure sgMapChangeBounds(Sender: TObject);
    procedure sgMapSelection(Sender: TObject; aCol, aRow: Integer);
    procedure sgMapDblClick(Sender: TObject);
    procedure actionExport(Sender: TObject);

  private
    WantSingleClick: Boolean;

    procedure DoMouseClick(IsDouble: Boolean);

  public
    FontX:         TMatrixFont;
    OnMouseEvent:  TMouseEvent;
    SelectedIndex: Integer;

    procedure UpdateMap;
    procedure InitConfig;
    procedure SaveConfig;
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

    InitConfig;
  end;

procedure TfmMap.FormShow(Sender: TObject);
  var
    w, h: Integer;
  begin
    // init block, executed only once
    if Tag = 0 then
      begin
      Height := cfg.map.h;
      Width  := cfg.map.w;

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
      Width        := cfg.map.init.Select(Width, w);
      Height       := cfg.map.init.Select(Height, h);
      cfg.map.init := False;
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
      Canvas.Brush.Color := cfg.color.map.bg;
      Canvas.Pen.Width   := 0;
      Canvas.Pen.Style   := psSolid;
      Canvas.Pen.Color   := Canvas.Brush.Color;

      _txtStyle           := Canvas.TextStyle;
      _txtStyle.Alignment := taCenter;
      Canvas.TextStyle    := _txtStyle;
      _txtExt             := Canvas.TextExtent('0');
      Canvas.Font.Height  := trunc(ColWidths[1] / 4 / _txtExt.Width * _txtExt.Height);
      Canvas.Font.Color   := cfg.color.map.active;

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

              FontX.Item[FontX.FontStartItem + i].Draw(bm_tmp, False,
                _isSelected.Select(cfg.color.map.selbg, cfg.color.map.bg),
                _isSelected.Select(cfg.color.map.selact, cfg.color.map.active));

              Canvas.StretchDraw(aRect, bm_tmp);
              finally
              FreeAndNil(bm_tmp);
              end;

            if _isSelected then
              begin
              SelectedIndex := i;
              i             += FontX.FontStartItem;
              fmMap.Caption := FM_MAP_CAPTION + Format(' - %d, %.2x, <%s>',
                [i, i, EncodingToUTF8(Chr(i), FontX.Props.Encoding)]);

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
    sgMap.Color := cfg.color.map.bg;

    { fix bug: when form is visible and you disable main form option 'on top'
      cbMapWidth unexpectedly empties }
    with cbMapWidth do
      begin
      if Items.Count = 0 then
        begin
        Items.CommaText := '2,4,8,16,32,64';
        ItemIndex       := Tag;
        end;

      Tag := ItemIndex;
      end;

    with sgMap do
      try
      ColCount         := (2 shl cbMapWidth.ItemIndex) + 1;
      RowCount         := (FontX.FontLength - 1) div (ColCount - 1) + 2;
      _colWidth        := Width / ColCount;
      ColWidths[0]     := round(_colWidth);
      DefaultRowHeight := round(_colWidth / (FontX.Width + seSpaceX.Value) * (FontX.Height + seSpaceY.Value));
      ColWidths[0]     := ColWidths[0] + ColWidths[ColCount - 1] - sgMap.ColWidths[1];
      except
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

procedure TfmMap.actionExport(Sender: TObject);

  function GetFilename(AIndex: Integer): String;
    begin
      Result := Format('%s_charmap_%d.png', [FontX.Props.Name, AIndex]);
    end;

  procedure DialogPrepare;
    var
      n: Integer = 1;
    begin
      while FileExists(SaveDlg.InitialDir + GetFilename(n)) do Inc(n);
      SaveDlg.InitialDir := ExtractFilePath(GetFilename(n));
      SaveDlg.FileName   := ExtractFileName(GetFilename(n));
    end;

  begin
    if not Assigned(FontX) then Exit;
    DialogPrepare;

    with SaveDlg do
      if Execute then
        try
        if LowerCase(ExtractFileExt(FileName)) <> '.png' then
          FileName := FileName + '.png';

        RenderMapToPNG(
          FileName, FontX, sgMap.ColCount - 1,
          cfg.map.export.scale, cfg.map.export.space,
          cfg.color.map.export, cfg.color.map.bg, cfg.color.map.active,
          Screen.Fonts[cfg.nav.code.font]);
        except
        end;
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

procedure TfmMap.InitConfig;
  begin
    Settings.Add(cbMapWidth, @cfg.map.cols);
    Settings.Add(seSpaceX, @cfg.map.spacex);
    Settings.Add(seSpaceY, @cfg.map.spacey);
    Settings.Add('_cfg.map.h', stInt, @cfg.map.h, Height.ToString);
    Settings.Add('_cfg.map.w', stInt, @cfg.map.w, Width.ToString);
    Settings.Add('_cfg.map.init', stBool, @cfg.map.init, '1');
  end;

procedure TfmMap.SaveConfig;
  begin
    cfg.map.h := Height;
    cfg.map.w := Width;
  end;

end.
