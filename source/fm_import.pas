unit fm_import;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Graphics, Dialogs, StdCtrls, ExtCtrls, Spin, Buttons,
  ComCtrls, LazUTF8, Classes, Types,
  u_encodings, config_record;

resourcestring
  //FM_IMPORT_EXAMPLE = 'Образец';
  FM_IMPORT_SCALE        = 'предпросмотр';

const
  IMPORT_EXAMPLE_DEFAULT = 'AaBbCcXxYyZz АаБбВвЭэЮюЯя' + LineEnding + 'Example Пример 12345';

type

  { TfmImport }

  TfmImport = class(TForm)
    cbEncoding:    TComboBox;
    cbSnapLeft:    TCheckBox;
    cbOptimize:    TCheckBox;
    cbStyleBold:   TCheckBox;
    cbStyleStrike: TCheckBox;
    cbStyleItalic: TCheckBox;
    cbStyleUnder:  TCheckBox;
    cbExampleEdit: TCheckBox;
    dlgFont:       TFontDialog;
    imPreview:     TImage;
    lbScale:       TLabel;
    lbItemStart:   TLabel;
    lbItemLast:    TLabel;
    lbFontSystem:  TLabel;
    lbFontSize:    TLabel;
    lbRange:       TLabel;
    lbSize:        TLabel;
    lbEmpty:       TLabel;
    mmExample:     TMemo;
    pcPages:       TPageControl;
    pFontSize:     TPanel;
    pControls:     TPanel;
    pOptions:      TPanel;
    cbFontList:    TComboBox;
    bbImport:      TBitBtn;
    lbItemWidth:   TLabel;
    lbItemHeight:  TLabel;
    pSize:         TPanel;
    pRange:        TPanel;
    pValues:       TPanel;
    pFontStyle:    TPanel;
    pFont:         TPanel;
    pMain:         TPanel;
    pFontName:     TPanel;
    scbImage:      TScrollBox;
    seLastItem:    TSpinEdit;
    seStartItem:   TSpinEdit;
    seW:           TSpinEdit;
    seH:           TSpinEdit;
    seSize:        TSpinEdit;
    sbMore:        TSpeedButton;
    shPreviewBG:   TShape;
    tsExample:     TTabSheet;
    tsExampleText: TTabSheet;

    // создание формы импорта системного шрифта
    procedure FormCreate(Sender: TObject);

    // вывод на экран формы импорта системного шрифта
    procedure FormShow(Sender: TObject);


    // показать окно выбора шрифта
    procedure btnMoreClick(Sender: TObject);

    // выбор шрифта из списка доступных
    procedure fontChange(Sender: TObject);

    // изменение текста предпросмотра
    procedure mmExampleChange(Sender: TObject);

    // масштабирование изображения образца
    procedure scbImageMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

  private
    FFontName: String;
    FScale:    Integer;

    // применение взаимосвязанных изменений
    procedure ApplyChange;

    procedure InitConfig;

  public
    { public declarations }
  end;

var
  fmImport: TfmImport;

implementation

{$R *.lfm}

 { TfmImport }

 // создание формы импорта системного шрифта
procedure TfmImport.FormCreate(Sender: TObject);
  begin
    InitConfig;
    cbFontList.Items.Assign(Screen.Fonts);

    FScale               := 3;
    FFontName            := 'Tahoma';
    dlgFont.Font.Size    := 8;
    dlgFont.Font.Quality := fqNonAntialiased;
    mmExample.Text       := IMPORT_EXAMPLE_DEFAULT;
  end;

// вывод на экран формы импорта системного шрифта
procedure TfmImport.FormShow(Sender: TObject);
  begin
    EncodingsListAssign(cbEncoding.Items);
    dlgFont.Font.Name       := FFontName;
    cbFontList.Text         := FFontName;
    cbEncoding.ItemIndex    := cfg.new.enc;
    pcPages.ShowTabs        := False;
    pcPages.ActivePageIndex := 0;
    cbExampleEdit.Checked   := False;
    AutoSize                := False;
    Constraints.MinHeight   := Height;
    Constraints.MinWidth    := Width;
    shPreviewBG.Brush.Color := cfg.color.import.bg;
    shPreviewBG.Pen.Color   := cfg.color.import.bg;
    ApplyChange;
  end;


// показать окно выбора шрифта
procedure TfmImport.btnMoreClick(Sender: TObject);
  begin
    if dlgFont.Execute then
      with dlgFont.Font do
        begin
        cbStyleBold.Checked   := (Style * [fsBold]) <> [];
        cbStyleStrike.Checked := (Style * [fsStrikeOut]) <> [];
        cbStyleItalic.Checked := (Style * [fsItalic]) <> [];
        cbStyleUnder.Checked  := (Style * [fsUnderline]) <> [];
        cbFontList.Text       := Name;
        seSize.Value          := Size;

        ApplyChange;
        end;
  end;

  // выбор шрифта из списка доступных
procedure TfmImport.fontChange(Sender: TObject);

  // применение стиля по флагу
  procedure SetStyleByFlag(AFlag: Boolean; AStyle: TFontStyles);
    begin
      with dlgFont.Font do
        if AFlag then
          Style := Style + AStyle
        else
          Style := Style - AStyle;
    end;

  begin
    pcPages.ActivePageIndex := cbExampleEdit.Checked.ToInteger;

      try
      with dlgFont.Font do
        begin
        Name      := cbFontList.Text;
        Size      := seSize.Value;
        FFontName := Name;
        end;

      SetStyleByFlag(cbStyleBold.Checked, [fsBold]);
      SetStyleByFlag(cbStyleStrike.Checked, [fsStrikeOut]);
      SetStyleByFlag(cbStyleItalic.Checked, [fsItalic]);
      SetStyleByFlag(cbStyleUnder.Checked, [fsUnderline]);

      ApplyChange;
      finally
      end;
  end;

    // изменение текста предпросмотра
procedure TfmImport.mmExampleChange(Sender: TObject);
  begin
    // пустое поле - сброс текста
    if mmExample.Text = '' then
      mmExample.Text := IMPORT_EXAMPLE_DEFAULT;

    ApplyChange;
  end;

// масштабирование изображения образца
procedure TfmImport.scbImageMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  begin
    FScale  += WheelDelta div abs(WheelDelta);
    Handled := True;
    ApplyChange;
  end;


// применение взаимосвязанных изменений
procedure TfmImport.ApplyChange;
  var
    bmp:         TBitmap;
    i, w, h, h1: Integer;
    cw, ch, ct:  Integer;
  begin
    if not Visible then Exit;

    bmp := TBitmap.Create;
    with bmp do
      begin
      // задаем текущий шрифт для операций
      Canvas.Font.Assign(dlgFont.Font);
      Canvas.Font.Quality := fqNonAntialiased; // сглаживание откл.

      // расчет размеров изображения
      w     := 0;
      for i := 0 to mmExample.Lines.Count - 1 do
        begin
        h   := Canvas.GetTextWidth(mmExample.Lines.Strings[i]);
        if h > w then w := h;
        end;
      h1 := Canvas.GetTextHeight(mmExample.Text) + 1;
      h  := h1 * mmExample.Lines.Count + 2;
      w  += 2;
      SetSize(w, h);

      // подготовка буфера
      Canvas.Brush.Color := cfg.color.import.bg;
      Canvas.Font.Color  := cfg.color.import.active;
      Canvas.Clear;
      Canvas.Clear;

      // рисование текста на изображении
      for i := 0 to mmExample.Lines.Count - 1 do
        Canvas.TextOut(1, i * h1, mmExample.Lines.Strings[i]);

      // расчет необходимых размеров холста для символа шрифта
      cw := 0;
      ch := 0;
      for i := seStartItem.Value to seLastItem.Value do
        begin
        ct                 := Canvas.TextWidth(EncodingToUTF8ByIndex(chr(i), cbEncoding.ItemIndex));
        if ct > cw then cw := ct;

        ct                 := Canvas.TextHeight(EncodingToUTF8ByIndex(chr(i), cbEncoding.ItemIndex));
        if ct > ch then ch := ct;
        end;
      seW.Value := cw + 2; // +2 для предотвращения срезания правой части,
      seH.Value := ch + 2; // т.к. некоторые символы имеют пустые столбцы слева
      end;

    // вывод изображения из буфера в компонент формы
    imPreview.Picture.Bitmap.Assign(bmp);

    // применение масштабирования
    if FScale < 1 then  FScale := 1;
    if FScale > 16 then FScale := 16;
    imPreview.Width := w * FScale;
    imPreview.Height := h * FScale;
    lbScale.Caption := FM_IMPORT_SCALE + LineEnding + FScale.ToString + ' : 1';
    //tsExample.Caption          := FM_IMPORT_EXAMPLE + ' [' + FScale.ToString + ':1]';

    bmp.Free;
  end;

procedure TfmImport.InitConfig;
  begin
    Settings.Add(cbOptimize, @cfg.import.optimize);
    Settings.Add(cbSnapLeft, @cfg.import.snapleft);
    Settings.Add(mmExample, @cfg.import.example);
  end;

end.
