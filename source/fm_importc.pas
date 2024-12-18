unit fm_importc;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, ActnList, StdCtrls, Spin, SynEdit, LazUTF8, SynHighlighterCpp,
  Buttons, AppLocalizer, AppTuner,
  fm_gen,
  font, u_utilities, u_encodings, u_helpers, config_record, appAbout;

resourcestring
  FM_IMPC_CAPTION = 'Импорт шрифта из кода C';

  FM_IMPC_TYPE_4  = 'Настройка вручную';

  FM_IMPC_ORDER_1 = 'Сначала столбцы';
  FM_IMPC_ORDER_2 = 'Сначала строки';

  FM_IMPC_BITOR_1 = 'LSB первый';
  FM_IMPC_BITOR_2 = 'MSB первый';

  FM_IMPC_BITS_1  = 'Определить автоматически';
  FM_IMPC_BITS_2  = '8 бит';
  FM_IMPC_BITS_3  = '16 бит';
  FM_IMPC_BITS_4  = '24 бита';
  FM_IMPC_BITS_5  = '32 бита';
  FM_IMPC_BITS_6  = '64 бита';

const
  FM_IMPC_TYPE_1  = 'matrixFont';
  FM_IMPC_TYPE_2  = 'AdaFruit GFX';
  FM_IMPC_TYPE_3  = 'LCD Vision V1.34';

type

  { TfmImportC }

  TfmImportC = class(TForm)

    acClearCode:      TAction;
    acImportDo:       TAction;
    acTabCode:        TAction;
    acTabParams:      TAction;
    acUpdatePreview:  TAction;
    alActionListImpC: TActionList;

    bvDivider1: TBevel;
    bvDivider3: TBevel;
    bvDivider2: TBevel;

    cbImpBitOrder: TComboBox;
    cbImpExample:  TCheckBox;
    cbImpNBits:    TComboBox;
    cbImpOptimize: TCheckBox;
    cbImpOrder:    TComboBox;
    cbImpSnapLeft: TCheckBox;
    cbImpType:     TComboBox;

    edImpChar:     TEdit;
    edImpCharHex:  TEdit;
    edImpExample:  TEdit;
    edImpFileName: TEdit;

    imImpChar:    TImage;
    imImpExample: TImage;

    lbImpBitOrder: TLabel;
    lbImpChar:     TLabel;
    lbImpCharCode: TLabel;
    lbImpCharHex:  TLabel;
    lbImpDropFile: TLabel;
    lbImpFile:     TLabel;
    lbImpNBits:    TLabel;
    lbImpOffset:   TLabel;
    lbImpOrder:    TLabel;
    lbImpRange:    TLabel;
    lbImpSizes:    TLabel;
    lbImpType:     TLabel;
    lbInfo:        TLabel;

    pCharPreview: TPanel;
    pCharSelect:  TPanel;
    pCode:        TPanel;
    pFilename:    TPanel;
    pFileBar:     TPanel;
    pImpControls: TPanel;
    pImpExample:  TPanel;
    pImpOffset:   TPanel;
    pImpRange:    TPanel;
    pImpSizes:    TPanel;
    pLeft:        TPanel;
    pMain:        TPanel;
    pParams:      TPanel;
    pPreview:     TPanel;
    pRight:       TPanel;
    pSelector:    TPanel;
    pSeparator1:  TPanel;
    pSeparator2:  TPanel;
    pSpacer:      TPanel;
    pSpacerCtrl:  TPanel;

    pcPages: TPageControl;

    sbClearCode: TSpeedButton;

    seImpCharCode:  TSpinEdit;
    seImpHeight:    TSpinEdit;
    seImpLastItem:  TSpinEdit;
    seImpOffset:    TSpinEdit;
    seImpSkip:      TSpinEdit;
    seImpStartItem: TSpinEdit;
    seImpWidth:     TSpinEdit;

    snImpEdit: TSynEdit;

    tbCode:      TToolBar;
    tbSelector:  TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;

    tsCode:   TTabSheet;
    tsParams: TTabSheet;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);

    procedure actionExecute(Sender: TObject);
    procedure actionParamExecute(Sender: TObject);
    procedure edImpCharUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);

    procedure CheckRanges;
    procedure UpdatePreview(ASingle: Boolean = False);

  private
    procedure InitConfig;

  public
    procedure OnLanguageChange;
    procedure UpdateFont(AFont: TMatrixFont = nil);

  end;

var
  fmImportC: TfmImportC;
  FontImp:   TMatrixFont;

implementation

{$R *.lfm}

{ TfmImportC }

procedure TfmImportC.FormCreate(Sender: TObject);
  begin
    InitConfig;

    if appTunerEx.IsDarkTheme then
      begin
      pSeparator1.Show;
      pSeparator2.Show;
      pImpControls.ParentColor := True;
      lbInfo.ParentColor       := True;
      end;

    acTabCode.Execute;
  end;

procedure TfmImportC.FormShow(Sender: TObject);
  begin
    BeginFormUpdate;

    Caption        := FM_IMPC_CAPTION;
    lbInfo.Caption := GetAppNameAuthor;

    snImpEdit.Highlighter := fmGen.snCppSyntax;

    EndFormUpdate;

    // init block, executed only once
    if Tag = 0 then
      begin
      if not Showing then Exit;

      tbSelector.ButtonHeight       := pSelector.Height;
      tbCode.ButtonHeight           := pCode.Height;
      pcPages.ShowTabs              := False;
      pImpExample.Color             := cfg.color.import.bg;
      lbInfo.Constraints.MinHeight  := pImpControls.Height;
      pSpacer.Constraints.MinHeight := cbImpSnapLeft.Height - 1;
      snImpEdit.SetFocus;

      Tag      := 1;
      AutoSize := True;
      AutoSize := False;
      Position := poMainFormCenter;

      Constraints.MinWidth  := Width;
      Constraints.MinHeight := Height;

      actionParamExecute(seImpStartItem);
      actionParamExecute(seImpLastItem);
      actionParamExecute(cbImpType);

      if acTabCode.Checked then actionExecute(acTabCode);
      if acTabParams.Checked then actionExecute(acTabParams);
      actionExecute(seImpCharCode);
      end;
  end;

procedure TfmImportC.FormDropFiles(Sender: TObject; const FileNames: array of String);
  begin
    BeginFormUpdate;

    edImpFileName.Text := FileNames[0];
    pFilename.Visible  := True;
    Caption            := FM_IMPC_CAPTION + ' - ' + ExtractFileName(edImpFileName.Text);

    snImpEdit.Lines.LoadFromFile(edImpFileName.Text);
    UpdateFont;
    EndFormUpdate;
  end;

procedure TfmImportC.actionExecute(Sender: TObject);
  begin
    if Sender = nil then Exit;
    BeginFormUpdate;

    case TComponent(Sender).Name of

      'acTabCode':
        begin
        acTabCode.Checked := True;
        tsCode.Show;
        end;

      'acTabParams':
        begin
        acTabParams.Checked := True;
        tsParams.Show;
        UpdateFont;
        end;

      'acImportDo':
        ModalResult := mrOk;

      'acClearCode':
        snImpEdit.ClearAll;

      'edImpChar':
        begin
        if edImpChar.Text <> '' then
          seImpCharCode.Value := Ord(UTF8ToEncoding(edImpChar.Text, FontImp.Encoding)[1]);
        edImpCharHex.Text     := IntToHex(seImpCharCode.Value, 2);
        UpdatePreview(True);
        end;

      'seImpCharCode':
        begin
        edImpChar.Text := EncodingToUTF8(Char(seImpCharCode.Value), FontImp.Encoding);
        UpdatePreview(True);
        end;

      'edImpExample':
        UpdatePreview;

      'cbImpExample':
        begin
        imImpExample.Picture.Bitmap.Width := 0;
        imImpExample.Visible              := cbImpExample.Checked;
        edImpExample.Enabled              := cbImpExample.Checked;
        EndFormUpdate;
        BeginFormUpdate;
        if cbImpExample.Checked then UpdatePreview;
        end;

      end;

    EndFormUpdate;
  end;

procedure TfmImportC.actionParamExecute(Sender: TObject);
  var
    _enabled:    Boolean;
    _importMode: TImportMode;
  begin
    if Sender = nil then Exit;
    BeginFormUpdate;

    case TComponent(Sender).Name of

      'cbImpType':
        begin
        _importMode            := TImportMode(cbImpType.ItemIndex);
        _enabled               := _importMode = imCustom;
        cbImpOrder.Enabled     := _importMode <> imAdafruit;
        cbImpBitOrder.Enabled  := not (_importMode in [imAdafruit, imLCDVision]);
        cbImpNBits.Enabled     := _enabled;
        lbImpNBits.Enabled     := _enabled;
        seImpStartItem.Enabled := _enabled;
        seImpLastItem.Enabled  := _enabled;
        seImpWidth.Enabled     := _enabled;
        seImpHeight.Enabled    := _enabled;
        seImpOffset.Enabled    := _enabled;
        lbImpOffset.Enabled    := _enabled;
        seImpSkip.Enabled      := _enabled;
        end;

      'cbImpOrder':
        begin
        end;

      'cbImpBits':
        begin
        end;

      'seImpStartItem':
        begin
        end;

      'seImpLastItem':
        begin
        end;

      'seImpWidth':
        begin
        end;

      'seImpHeight':
        begin
        end;

      'seImpOffset':
        begin
        end;

      'seImpSkip':
        begin
        end;

      'cbImpOptimize':
        begin
        end;

      end;

    CheckRanges;
    UpdateFont;
    EndFormUpdate;
  end;

procedure TfmImportC.edImpCharUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  begin
    edImpChar.Text := '';
  end;

procedure TfmImportC.CheckRanges;
  begin
    if seImpStartItem.Value >= seImpLastItem.MaxValue - 1 then
      seImpStartItem.Value := seImpLastItem.MaxValue - 2;

    seImpCharCode.MinValue := seImpStartItem.Value;
    seImpCharCode.MaxValue := seImpLastItem.Value;
    seImpLastItem.MinValue := seImpStartItem.Value + 1;
  end;

procedure TfmImportC.UpdatePreview(ASingle: Boolean);
  var
    bmp: TBitmap;
    i:   Integer;

  procedure DrawBlank(ADest: TBitmap; AIsSingle: Boolean);
    begin
      with ADest do
        begin
        Canvas.Brush.Color := AIsSingle.Select(cfg.color.prev.bg, cfg.color.import.bg);
        Canvas.Clear;
        Canvas.Clear;
        end;
    end;

  procedure DrawChar(AChar, AX, AW: Integer; ADest: TBitmap; AIsSingle: Boolean);
    begin
      with ADest do
        begin
        Width  := AW;
        Height := bmp.Height;

        if not AIsSingle and (AX <= 1) then
          begin
          Canvas.Brush.Color := cfg.color.import.bg;
          Canvas.Clear;
          Canvas.Clear;
          end;

        if AChar in [0..FontImp.FontLength - 1] then
          begin
          FontImp.Item[AChar].DrawPreview(
            bmp, False,
            AIsSingle.Select(cfg.color.prev.bg, cfg.color.import.bg),
            AIsSingle.Select(cfg.color.prev.active, cfg.color.import.active));

          Canvas.Draw(AX, 0, bmp);
          end;
        end;
    end;

  begin
    if not Visible then Exit;

    BeginFormUpdate;

    if FontImp.FontLength * FontImp.Width * FontImp.Height = 0 then
      begin

      // clear previews if font not detected
      DrawBlank(imImpChar.Picture.Bitmap, True);
      DrawBlank(imImpExample.Picture.Bitmap, False);
      end
    else
      with FontImp do
        try
        bmp        := TBitmap.Create;
        bmp.Width  := Width;
        bmp.Height := Height;

        // draw single char preview
        DrawChar(
          seImpCharCode.Value - FontStartItem, 0, bmp.Width,
          imImpChar.Picture.Bitmap, True);

        // draw text example preview
        if cbImpExample.Checked and not ASingle then
          for i := 1 to Length(edImpExample.Text) do
            DrawChar(
              Ord(UTF8ToEncoding(edImpExample.Text[i], Encoding)[1]) - FontStartItem,
              1 + (i - 1) * (Width + 1),
              1 + Length(edImpExample.Text) * (Width + 1),
              imImpExample.Picture.Bitmap, False);

        finally
        bmp.Free;
        end;

    EndFormUpdate;
  end;

procedure TfmImportC.InitConfig;
  begin
    Settings.Add(cbImpOptimize, @cfg.importc.optimize);
    Settings.Add(cbImpSnapLeft, @cfg.importc.snapleft);
    Settings.Add(cbImpBitOrder, @cfg.importc.metrics.bitorder);
    Settings.Add(cbImpType, @cfg.importc.metrics.codetype);
    Settings.Add(seImpHeight, @cfg.importc.metrics.h);
    Settings.Add(seImpLastItem, @cfg.importc.metrics.last);
    Settings.Add(cbImpNBits, @cfg.importc.metrics.nbits);
    Settings.Add(seImpOffset, @cfg.importc.metrics.offset);
    Settings.Add(cbImpOrder, @cfg.importc.metrics.order);
    Settings.Add(seImpSkip, @cfg.importc.metrics.skip);
    Settings.Add(seImpStartItem, @cfg.importc.metrics.start);
    Settings.Add(seImpWidth, @cfg.importc.metrics.w);
    Settings.Add(cbImpExample, @cfg.importc.example.enable);
    Settings.Add(seImpCharCode, @cfg.importc.example.char);
    Settings.Add(edImpExample, @cfg.importc.example.str);
    Settings.Add(acTabCode, @cfg.importc.tab.code);
    Settings.Add(acTabParams, @cfg.importc.tab.params);
  end;

procedure TfmImportC.OnLanguageChange;
  begin
    with appLocalizerEx do
      begin
      Localize(cbImpType, [FM_IMPC_TYPE_1, FM_IMPC_TYPE_2, FM_IMPC_TYPE_3, FM_IMPC_TYPE_4]);
      Localize(cbImpBitOrder, [FM_IMPC_BITOR_1, FM_IMPC_BITOR_2]);
      Localize(cbImpOrder, [FM_IMPC_ORDER_1, FM_IMPC_ORDER_2]);
      Localize(cbImpNBits, [FM_IMPC_BITS_1, FM_IMPC_BITS_2, FM_IMPC_BITS_3, FM_IMPC_BITS_4, FM_IMPC_BITS_5, FM_IMPC_BITS_6]);
      end;
  end;

procedure TfmImportC.UpdateFont(AFont: TMatrixFont);
  begin
    if AFont = nil then AFont := FontImp;
    BeginFormUpdate;

    with AFont do
      begin

      // set font parameters
      ScanColsFirst := cbImpOrder.ItemIndex = 0;
      NumbersBits   := cbImpNBits.ItemIndex * 8;
      MSBFirst      := cbImpBitOrder.ItemIndex > 0;
      Width         := seImpWidth.Value;
      Height        := seImpHeight.Value;
      FontStartItem := seImpStartItem.Value;
      FontLength    := seImpLastItem.Value - FontStartItem + 1;

      // try to decode C-code
        try
        if Import(snImpEdit.Text, seImpOffset.Value, seImpSkip.Value, TImportMode(cbImpType.ItemIndex)) then
          begin

          // apply post decoding operations
          if cbImpSnapLeft.Checked then Snap(TBorder.brLeft);
          if cbImpOptimize.Checked then ChangeSize(-1, -1, -1, -1, True);

          // set some parameters with autodetected values
          if TImportMode(cbImpType.ItemIndex) <> imCustom then
            begin
            seImpLastItem.MinValue := 1;
            SetValueWithoutAction(seImpWidth, Width);
            SetValueWithoutAction(seImpHeight, Height);
            SetValueWithoutAction(seImpStartItem, FontStartItem);
            SetValueWithoutAction(seImpLastItem, FontLength + FontStartItem - 1);
            end;
          end;
        except
        EndFormUpdate;
        Exit;
        end;
      end;

    CheckRanges;
    UpdatePreview;
    EndFormUpdate;
  end;


initialization
  FontImp := TMatrixFont.Create;

end.
