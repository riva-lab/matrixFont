unit fm_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynHighlighterCpp, SynEdit, Forms, Clipbrd, strutils, Graphics,
  ExtCtrls, StdCtrls, ActnList, ComCtrls, Spin, Dialogs, LazUTF8,
  AppLocalizer, AppTuner,
  symbol, font, appAbout, u_encodings, u_utilities, u_helpers, config_record;


resourcestring
  FM_GEN_CAPTION = 'Генератор кода';

  FM_GEN_BLKD_1  = 'Вертикально';
  FM_GEN_BLKD_2  = 'Горизонтально';

  FM_GEN_SCUD_1  = 'Столбцами групп';
  FM_GEN_SCUD_2  = 'Строками групп';

  FM_GEN_SCLR_1  = 'Слева направо';
  FM_GEN_SCLR_2  = 'Справа налево';

  FM_GEN_SRUD_1  = 'Сверху вниз';
  FM_GEN_SRUD_2  = 'Снизу вверх';

  FM_GEN_FT_1    = 'Моноширинный';
  FM_GEN_FT_2    = 'Пропорциональный';

  FM_GEN_NV_1    = 'HEX';
  FM_GEN_NV_2    = 'BIN';
  FM_GEN_NV_3    = 'DEC';
  FM_GEN_NV_4    = 'HEX (Инверсия)';
  FM_GEN_NV_5    = 'BIN (Инверсия)';
  FM_GEN_NV_6    = 'DEC (Инверсия)';

  FM_GEN_BO_1    = 'Сначала LSB';
  FM_GEN_BO_2    = 'Сначала MSB';

  FM_GEN_EB_1    = '0 - нули';
  FM_GEN_EB_2    = '1 - единицы ';

  FM_GEN_NB_1    = '8 (uint8_t, unsigned char)';
  FM_GEN_NB_2    = '16 (uint16_t, unsigned short int)';
  FM_GEN_NB_3    = '24 (uint24_t)';
  FM_GEN_NB_4    = '32 (uint32_t, unsigned long int)';

  FM_GEN_UPDATE  = 'ОБНОВИТЕ КОД';

type

  { TfmGen }

  TfmGen = class(TForm)
    lbRangeReset:      TLabel;
    pRange:            TPanel;
    pRangeTxt:         TPanel;
    pSettings:         TPanel;
    pControls:         TPanel;
    pSelector:         TPanel;
    pSeparator:        TPanel;
    pCode:             TPanel;
    SaveDlg:           TSaveDialog;
    snCppSyntax:       TSynCppSyn;
    snEdit:            TSynEdit;
    pcPages:           TPageControl;
    tbSelector:        TToolBar;
    tbCode:            TToolBar;
    ToolButton1:       TToolButton;
    ToolButton10:      TToolButton;
    ToolButton2:       TToolButton;
    ToolButton3:       TToolButton;
    ToolButton4:       TToolButton;
    ToolButton5:       TToolButton;
    ToolButton6:       TToolButton;
    ToolButton9:       TToolButton;
    tsGenSettings:     TTabSheet;
    tsCode:            TTabSheet;
    edDefPrefix:       TEdit;
    cbGroupDirection:  TComboBox;
    cbScanColsFirst:   TComboBox;
    cbScanColsToRight: TComboBox;
    cbScanRowsToDown:  TComboBox;
    cbFontType:        TComboBox;
    cbNumbersView:     TComboBox;
    cbBitOrderLSB:     TComboBox;
    cbEmptyBits:       TComboBox;
    cbNumbersBits:     TComboBox;
    lbGenValsPerLine:  TLabel;
    lbGroupDirection:  TLabel;
    lbScanColsFirst:   TLabel;
    lbScanColsToRight: TLabel;
    lbScanRowsToDown:  TLabel;
    lbFontType:        TLabel;
    lbNumbersView:     TLabel;
    lbBitOrder:        TLabel;
    lbEmptyBits:       TLabel;
    lbNumbersBits:     TLabel;
    lbInfo:            TLabel;
    lbDefPrefix:       TLabel;
    lbRange:           TLabel;
    lbDots:            TLabel;
    seGenValsPerLine:  TSpinEdit;
    seStart:           TSpinEdit;
    seEnd:             TSpinEdit;
    ActionList1:       TActionList;
    acTabSelSettings:  TAction;
    acTabSelCode:      TAction;
    acResetRange:      TAction;
    acRefreshOut:      TAction;
    acExport:          TAction;
    acCopyToClipboard: TAction;

    // инициализация формы при показе
    procedure FormShow(Sender: TObject);

    // handler executed when parameter was changed
    procedure OnChangeParameter(Sender: TObject);

    // действие: <КОПИРОВАТЬ В БУФЕР ОБМЕНА>
    procedure acCopyToClipboardExecute(Sender: TObject);

    // изменение префикса (переопределение названия шрифта)
    procedure edDefPrefixChange(Sender: TObject);

    // экспорт кода во внешний *.h файл
    procedure acExportExecute(Sender: TObject);

    // действие: <ОБНОВИТЬ ВЫВОД>
    procedure acRefreshOutExecute(Sender: TObject);

    // восстановление диапазона вывода
    procedure acResetRangeExecute(Sender: TObject);

    // изменение начала диапазона вывода
    procedure seStartChange(Sender: TObject);

    // выбор вкладки для отображения
    procedure TabSelExecute(Sender: TObject);

  public
    procedure OnLanguageChange;

  end;

var
  mxFont: TMatrixFont;
  fmGen:  TfmGen;

implementation

{$R *.lfm}

 { TfmGen }

 // инициализация формы при показе
procedure TfmGen.FormShow(Sender: TObject);
  begin
    if appTunerEx.IsDarkTheme then
      begin
      pSeparator.Show;
      pControls.ParentColor := True;
      end;

    pControls.Show;
    BeginFormUpdate;

    if mxFont <> nil then
      with mxFont do
        begin
        SaveDlg.FileName := AnsiReplaceText(LowerCase(Props.Name), ' ', '_') + '_font.h';
        edDefPrefix.Text := Transliterate(UpperCase('FONT_' + AnsiReplaceText(Props.Name, ' ', '_')));
        end;

    EndFormUpdate;

    // установка минимальных размеров формы и инициализация
    if Tag = 0 then
      begin
      acTabSelSettings.Execute;
      if not Showing then Exit;

      lbInfo.Caption          := GetAppNameAuthor;
      tbSelector.ButtonHeight := pSelector.Height;
      tbCode.ButtonHeight     := pCode.Height;
      pcPages.ShowTabs        := False;

      Tag      := 1;
      AutoSize := True;
      AutoSize := False;

      Constraints.MinWidth  := Width;
      Constraints.MinHeight := Height;
      end;

    acResetRangeExecute(nil);
  end;

// handler executed when parameter was changed
procedure TfmGen.OnChangeParameter(Sender: TObject);
  begin
    if cfg.gen.refresh then
      acRefreshOut.Execute
    else
      begin
      lbInfo.Color   := appTunerEx.IsDarkTheme.Select($0000DD, $CCCCFF);
      lbInfo.Caption := ' ' + FM_GEN_UPDATE + ' ';
      end;
  end;


// действие: <КОПИРОВАТЬ В БУФЕР ОБМЕНА>
procedure TfmGen.acCopyToClipboardExecute(Sender: TObject);
  begin
    Clipboard.AsText := snEdit.Text;
  end;

// изменение префикса (переопределение названия шрифта)
procedure TfmGen.edDefPrefixChange(Sender: TObject);
  var
    cursor_position, old_str_length, new_str_length: Integer;
  begin
    if mxFont = nil then Exit;

    with edDefPrefix do
      begin
      cursor_position := SelStart;
      old_str_length  := Length(UTF8ToEncoding(Text, mxFont.Props.Encoding));
      Text            := Transliterate(Text);
      new_str_length  := Length(UTF8ToEncoding(Text, mxFont.Props.Encoding));
      SelStart        := cursor_position + new_str_length - old_str_length;

      if Length(Text) = 0 then
        Text := UpperCase('FONT_' + AnsiReplaceText(mxFont.Props.Name, ' ', '_'));
      end;

    OnChangeParameter(Sender);
  end;

// экспорт кода во внешний *.h файл
procedure TfmGen.acExportExecute(Sender: TObject);
  begin
    if SaveDlg.Execute then
      snEdit.Lines.SaveToFile(SaveDlg.FileName);
  end;

// действие: <ОБНОВИТЬ ВЫВОД>
procedure TfmGen.acRefreshOutExecute(Sender: TObject);
  var
    TopLine_: Integer;
  begin
    if mxFont = nil then Exit;
    if not Visible then Exit;

    with mxFont do
      begin
      GroupIsVertical  := cbGroupDirection.ItemIndex = 0;
      ScanColsFirst    := cbScanColsFirst.ItemIndex = 0;
      ScanColsToRight  := cbScanColsToRight.ItemIndex = 0;
      ScanRowsToDown   := cbScanRowsToDown.ItemIndex = 0;
      NumbersInversion := cbNumbersView.ItemIndex > 2;
      NumbersView      := TNumberView(cbNumbersView.ItemIndex mod 3);
      BitOrderLSBFirst := cbBitOrderLSB.ItemIndex = 0;
      EmptyBits        := TEmptyBit(cbEmptyBits.ItemIndex);
      FontType         := TFontType(cbFontType.ItemIndex);
      BitsPerGroup     := cbNumbersBits.ItemIndex * 8 + 8;
      ValuesPerLine    := seGenValsPerLine.Value;
      DefPrefix        := edDefPrefix.Text;
      end;

    BeginFormUpdate;
    TopLine_       := snEdit.TopLine; // запоминаем положение текста
    snEdit.Text    := mxFont.GenerateCode(seStart.Value, seEnd.Value);
    snEdit.TopLine := TopLine_;       // восстанивливаем положение текста
    EndFormUpdate;

    lbInfo.ParentColor := True;
    lbInfo.Caption     := GetAppNameAuthor;
  end;

// восстановление диапазона вывода
procedure TfmGen.acResetRangeExecute(Sender: TObject);
  begin
    if mxFont = nil then Exit;

    with mxFont do
      begin
      seStart.MinValue := FontStartItem;
      seStart.MaxValue := FontLength + FontStartItem - 1;
      seEnd.MinValue   := seStart.MinValue;
      seEnd.MaxValue   := seStart.MaxValue;

      // вызов с nil обновит только диапазоны полей
      if Sender <> nil then
        begin
        seStart.Value := seStart.MinValue;
        seEnd.Value   := seStart.MaxValue;
        end;
      end;

    OnChangeParameter(Sender);
  end;

// изменение начала диапазона вывода
procedure TfmGen.seStartChange(Sender: TObject);
  begin
    seEnd.MinValue := seStart.Value;
    OnChangeParameter(Sender);
  end;

// выбор вкладки для отображения
procedure TfmGen.TabSelExecute(Sender: TObject);
  begin
    if TAction(Sender) = acTabSelSettings then
      begin
      acTabSelSettings.Checked := True;
      tsGenSettings.Show;
      end
    else
      begin
      acTabSelCode.Checked := True;
      tsCode.Show;
      end;

    Caption := FM_GEN_CAPTION + '.  ' + TAction(Sender).Caption;
  end;


procedure TfmGen.OnLanguageChange;
  begin
    with appLocalizerEx do
      begin
      Localize(cbGroupDirection, [FM_GEN_BLKD_1, FM_GEN_BLKD_2]);
      Localize(cbScanColsFirst, [FM_GEN_SCUD_1, FM_GEN_SCUD_2]);
      Localize(cbScanColsToRight, [FM_GEN_SCLR_1, FM_GEN_SCLR_2]);
      Localize(cbScanRowsToDown, [FM_GEN_SRUD_1, FM_GEN_SRUD_2]);
      Localize(cbFontType, [FM_GEN_FT_1, FM_GEN_FT_2]);
      Localize(cbEmptyBits, [FM_GEN_EB_1, FM_GEN_EB_2]);
      Localize(cbNumbersBits, [FM_GEN_NB_1, FM_GEN_NB_2, FM_GEN_NB_3, FM_GEN_NB_4]);
      Localize(cbNumbersView, [FM_GEN_NV_1, FM_GEN_NV_2, FM_GEN_NV_3, FM_GEN_NV_4, FM_GEN_NV_5, FM_GEN_NV_6]);
      Localize(cbBitOrderLSB, [FM_GEN_BO_1, FM_GEN_BO_2]);
      end;
  end;


end.
