unit fm_importc;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, ActnList, StdCtrls, Spin, IniPropStorage, SynEdit, LazUTF8, SynHighlighterCpp,
  fm_settings, fm_about,
  font, u_utilities, u_encodings, u_helpers;

resourcestring
  FM_IMPC_CAPTION = 'Импорт шрифта из кода C';

  FM_IMPC_TYPE_4  = 'Настройка вручную';

  FM_IMPC_ORDER_1 = 'Сначала столбцы';
  FM_IMPC_ORDER_2 = 'Сначала строки';

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

    acImportDo:       TAction;
    acTabCode:        TAction;
    acTabParams:      TAction;
    acUpdatePreview:  TAction;
    alActionListImpC: TActionList;

    bvDivider: TBevel;

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

    IniStorageImportC: TIniPropStorage;

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
    pSpacer:      TPanel;
    pSpacerCtrl:  TPanel;

    pcPages: TPageControl;

    seImpCharCode:  TSpinEdit;
    seImpHeight:    TSpinEdit;
    seImpLastItem:  TSpinEdit;
    seImpOffset:    TSpinEdit;
    seImpSkip:      TSpinEdit;
    seImpStartItem: TSpinEdit;
    seImpWidth:     TSpinEdit;

    snCppSyntax: TSynCppSyn;
    snImpEdit:   TSynEdit;

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

  public
    procedure UpdateFont(AFont: TFont = nil);

  end;

var
  fmImportC: TfmImportC;
  FontImp:   TFont;

implementation

{$R *.lfm}

{ TfmImportC }

procedure TfmImportC.FormCreate(Sender: TObject);
  begin
    IniStorageImportC.IniFileName := ExtractFileDir(ParamStrUTF8(0)) + SETTINGS_FILE;

    acTabCode.Execute;
  end;

procedure TfmImportC.FormShow(Sender: TObject);
  begin
    BeginFormUpdate;

    Caption        := FM_IMPC_CAPTION;
    lbInfo.Caption := fmAbout.AppIntName + ', ' + GetAuthorName(fmAbout.AppCopyright);

    UpdateComboBox(cbImpType, [FM_IMPC_TYPE_1, FM_IMPC_TYPE_2, FM_IMPC_TYPE_3, FM_IMPC_TYPE_4]);
    UpdateComboBox(cbImpOrder, [FM_IMPC_ORDER_1, FM_IMPC_ORDER_2]);
    UpdateComboBox(cbImpNBits, [FM_IMPC_BITS_1, FM_IMPC_BITS_2, FM_IMPC_BITS_3, FM_IMPC_BITS_4, FM_IMPC_BITS_5, FM_IMPC_BITS_6]);

    EndFormUpdate;

    // init block, executed only once
    if Tag = 0 then
      begin
      if not Showing then Exit;

      tbSelector.ButtonHeight       := pSelector.Height;
      tbCode.ButtonHeight           := pCode.Height;
      pcPages.ShowTabs              := False;
      pImpExample.Color             := fmSettings.ColorImportBG;
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

      actionExecute(acTabCode);
      actionExecute(acTabParams);
      actionExecute(seImpCharCode);

      // for test
      //if edImpFileName.Text = '' then
      //    try
      //    FormDropFiles(Sender, ['ex\matrixFont\_verdana_10_special_font-mono.h']);
      //    //FormDropFiles(Sender, ['ex\LCDVision\_lcdvisionCourier13.h']);
      //    //FormDropFiles(Sender, ['ex\LCDVision\_lcdvisionArialNarrow16.h']);
      //    //cbImpType.ItemIndex := 2;
      //    //FormDropFiles(Sender, ['ex\Adafruit\1\Font5x7FixedMono.h']);
      //    //cbImpType.ItemIndex := 1;
      //    acTabParams.Execute;
      //    except
      //    end;
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
        acTabCode.Checked := acTabCode.Checked;
        if acTabCode.Checked then tsCode.Show;
        end;

      'acTabParams':
        begin
        acTabParams.Checked := acTabParams.Checked;
        if acTabParams.Checked then tsParams.Show;
        UpdateFont;
        end;

      'acImportDo':
        ModalResult := mrOk;

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
    _enabled: Boolean;
  begin
    if Sender = nil then Exit;
    BeginFormUpdate;

    case TComponent(Sender).Name of

      'cbImpType':
        begin
        _enabled               := TImportMode(cbImpType.ItemIndex) = imCustom;
        cbImpOrder.Enabled     := TImportMode(cbImpType.ItemIndex) <> imAdafruit;
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
        Canvas.Brush.Color := AIsSingle.Select(fmSettings.ColorPreviewBG, fmSettings.ColorImportBG);
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
          Canvas.Brush.Color := fmSettings.ColorImportBG;
          Canvas.Clear;
          Canvas.Clear;
          end;

        if AChar in [0..FontImp.FontLength - 1] then
          begin
          FontImp.Item[AChar].DrawPreview(
            bmp, False,
            AIsSingle.Select(fmSettings.ColorPreviewBG, fmSettings.ColorImportBG),
            AIsSingle.Select(fmSettings.ColorPreviewA, fmSettings.ColorImportA));

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

procedure TfmImportC.UpdateFont(AFont: TFont);
  begin
    if AFont = nil then AFont := FontImp;
    BeginFormUpdate;

    with AFont do
      begin

      // set font parameters
      ScanColsFirst := cbImpOrder.ItemIndex = 0;
      NumbersBits   := cbImpNBits.ItemIndex * 8;
      Width         := seImpWidth.Value;
      Height        := seImpHeight.Value;
      FontStartItem := seImpStartItem.Value;
      FontLength    := seImpLastItem.Value - FontStartItem + 1;

      // try to decode C-code
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
      end;

    CheckRanges;
    UpdatePreview;
    EndFormUpdate;
  end;


initialization
  FontImp := TFont.Create;

end.
