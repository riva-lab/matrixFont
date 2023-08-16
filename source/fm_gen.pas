unit fm_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynHighlighterCpp, SynEdit, Forms, Clipbrd, strutils, Graphics,
  ExtCtrls, StdCtrls, ActnList, ComCtrls, Spin, Dialogs, LazUTF8,
  symbol, font, fm_about, u_encodings, u_utilities;


resourcestring
  FM_GEN_CAPTION = 'Генератор кода';


  FM_GEN_SCUD_1  = 'Сначала столбцы';
  FM_GEN_SCUD_2  = 'Сначала строки';

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

  FM_GEN_EB_1    = '0 - нули';
  FM_GEN_EB_2    = '1 - единицы ';

  FM_GEN_NB_1    = '8 (uint8_t, unsigned char)';
  FM_GEN_NB_2    = '16 (uint16_t, unsigned short int)';
  FM_GEN_NB_3    = '24 (uint24_t)';
  FM_GEN_NB_4    = '32 (uint32_t, unsigned long int)';

  FM_GEN_LNG_1   = 'C (C99)';
  FM_GEN_LNG_2   = 'C (C89)';

type

  { TfmGen }

  TfmGen = class(TForm)
    lbRangeReset:      TLabel;
    pRange:            TPanel;
    pRangeTxt:         TPanel;
    pSettings:         TPanel;
    pControls:         TPanel;
    pSelector:         TPanel;
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
    cbScanColsFirst:   TComboBox;
    cbScanColsToRight: TComboBox;
    cbScanRowsToDown:  TComboBox;
    cbFontType:        TComboBox;
    cbNumbersView:     TComboBox;
    cbEmptyBits:       TComboBox;
    cbNumbersBits:     TComboBox;
    cbLanguage:        TComboBox;
    lbScanColsFirst:   TLabel;
    lbScanColsToRight: TLabel;
    lbScanRowsToDown:  TLabel;
    lbFontType:        TLabel;
    lbNumbersView:     TLabel;
    lbEmptyBits:       TLabel;
    lbNumbersBits:     TLabel;
    lbLanguage:        TLabel;
    lbInfo:            TLabel;
    lbDefPrefix:       TLabel;
    lbRange:           TLabel;
    lbDots:            TLabel;
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

  private

    // обновление выпадающего списка значениями из массива
    procedure UpdateComboBox(AComponent: TComboBox; AList: array of String);
  end;

var
  FontSet: TFont;
  fmGen:   TfmGen;

implementation

{$R *.lfm}

 { TfmGen }

 // инициализация формы при показе
procedure TfmGen.FormShow(Sender: TObject);
  begin
    BeginFormUpdate;

    // обновляем строки выпадающих списков
    UpdateComboBox(cbScanColsFirst, [FM_GEN_SCUD_1, FM_GEN_SCUD_2]);
    UpdateComboBox(cbScanColsToRight, [FM_GEN_SCLR_1, FM_GEN_SCLR_2]);
    UpdateComboBox(cbScanRowsToDown, [FM_GEN_SRUD_1, FM_GEN_SRUD_2]);
    UpdateComboBox(cbFontType, [FM_GEN_FT_1, FM_GEN_FT_2]);
    UpdateComboBox(cbEmptyBits, [FM_GEN_EB_1, FM_GEN_EB_2]);
    UpdateComboBox(cbNumbersBits, [FM_GEN_NB_1, FM_GEN_NB_2, FM_GEN_NB_3, FM_GEN_NB_4]);
    UpdateComboBox(cbLanguage, [FM_GEN_LNG_1, FM_GEN_LNG_2]);
    UpdateComboBox(cbNumbersView, [FM_GEN_NV_1, FM_GEN_NV_2, FM_GEN_NV_3,
      FM_GEN_NV_4, FM_GEN_NV_5, FM_GEN_NV_6]);

    if FontSet <> nil then
      with FontSet do
        begin
        SaveDlg.FileName := AnsiReplaceText(LowerCase(Name), ' ', '_') + '_font.h';
        edDefPrefix.Text := Transliterate(UpperCase('FONT_' + AnsiReplaceText(Name, ' ', '_')));
        end;

    EndFormUpdate;

    // установка минимальных размеров формы и инициализация
    if Tag = 0 then
      begin
      acTabSelSettings.Execute;
      if not Showing then Exit;

      lbInfo.Caption          := fmAbout.AppIntName + ', ' + GetAuthorName(fmAbout.AppCopyright);
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
    acRefreshOut.Execute;
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
    if FontSet = nil then Exit;

    with edDefPrefix do
      begin
      cursor_position := SelStart;
      old_str_length  := Length(UTF8ToEncoding(Text, FontSet.Encoding));
      Text            := Transliterate(Text);
      new_str_length  := Length(UTF8ToEncoding(Text, FontSet.Encoding));
      SelStart        := cursor_position + new_str_length - old_str_length;

      if Length(Text) = 0 then
        Text := UpperCase('FONT_' + AnsiReplaceText(FontSet.Name, ' ', '_'));
      end;

    acRefreshOut.Execute;
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
    if FontSet = nil then Exit;

    with FontSet do
      begin
      ScanColsFirst    := cbScanColsFirst.ItemIndex = 0;
      ScanColsToRight  := cbScanColsToRight.ItemIndex = 0;
      ScanRowsToDown   := cbScanRowsToDown.ItemIndex = 0;
      NumbersInversion := cbNumbersView.ItemIndex > 2;
      NumbersView      := TNumberView(cbNumbersView.ItemIndex mod 3);
      EmptyBits        := TEmptyBit(cbEmptyBits.ItemIndex);
      FontType         := TFontType(cbFontType.ItemIndex);
      NumbersBits      := cbNumbersBits.ItemIndex * 8 + 8;
      CommentStyle     := cbLanguage.ItemIndex;
      DefPrefix        := edDefPrefix.Text;
      end;

    BeginFormUpdate;
    TopLine_       := snEdit.TopLine; // запоминаем положение текста
    snEdit.Text    := FontSet.GenerateCode(seStart.Value, seEnd.Value);
    snEdit.TopLine := TopLine_;       // восстанивливаем положение текста
    EndFormUpdate;
  end;

// восстановление диапазона вывода
procedure TfmGen.acResetRangeExecute(Sender: TObject);
  begin
    if FontSet = nil then Exit;

    with FontSet do
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

    acRefreshOut.Execute;
  end;

// изменение начала диапазона вывода
procedure TfmGen.seStartChange(Sender: TObject);
  begin
    seEnd.MinValue := seStart.Value;
    acRefreshOut.Execute;
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

// обновление выпадающего списка значениями из массива
procedure TfmGen.UpdateComboBox(AComponent: TComboBox; AList: array of String);
  var
    i, index: Integer;
  begin
    with AComponent do
      begin
      index := ItemIndex;
      if index < 0 then index := 0;
      Clear;
      for i := 0 to High(AList) do
        Items.Append(AList[i]);
      ItemIndex := index;
      end;
  end;

end.
