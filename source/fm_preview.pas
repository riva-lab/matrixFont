unit fm_preview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, ExtCtrls, Spin,
  StdCtrls, ActnList, LazUTF8, ComCtrls, Dialogs, IniPropStorage, Types,
  fm_settings, font, u_encodings;

resourcestring
  FM_PREV_EXAMPLE = 'Образец текста';

type
  TPFontCustom = ^font.TFont; // указатель на шрифт

  { TfmPreview }

  TfmPreview = class(TForm)
    acRefresh:     TAction;
    imPreview:     TImage;
    IniStoragePV:  TIniPropStorage;
    mmPreview:     TMemo;
    pControls:     TPanel;
    pFontType:     TPanel;
    pButtons:      TPanel;
    scbImage:      TScrollBox;
    SaveDlg:       TSaveDialog;
    pcPages:       TPageControl;
    tabPreview:    TTabSheet;
    tabTxt:        TTabSheet;
    ActionList1:   TActionList;
    acEditText:    TAction;
    acExportImage: TAction;
    acResetText:   TAction;
    tbControls:    TToolBar;
    ToolButton1:   TToolButton;
    ToolButton2:   TToolButton;
    ToolButton3:   TToolButton;
    ToolButton4:   TToolButton;
    lbBackground:  TLabel;
    rbProp:        TRadioButton;
    rbMono:        TRadioButton;
    seSpace:       TSpinEdit;
    seDelta:       TSpinEdit;

    // экспорт изображения с текстом текущим шрифтом
    procedure acExportImageExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);

    // создание окна предпросмотра
    procedure FormCreate(Sender: TObject);

    // показ формы предпросмотра
    procedure FormShow(Sender: TObject);

    // перерисовка изображения предпросмотра при изменении настроек
    procedure rbPropChange(Sender: TObject);

    // установка выводимого по умолчанию образца текста
    procedure acResetTextExecute(Sender: TObject);

    // увеличение масштаба изображения
    procedure acZoomInExecute(Sender: TObject);

    // уменьшение масштаба изображения
    procedure acZoomOutExecute(Sender: TObject);

    // установка масштаба изображения колесиком мыши
    procedure scbImageMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

    // команда - РЕДАКТИРОВАТЬ ТЕКСТ
    procedure acEditTextExecute(Sender: TObject);

    // обновление изображения предпросмотра
    procedure UpdatePreview;

  PRIVATE
    { private declarations }
  PUBLIC
    { public declarations }
    PFontCustom: TPFontCustom;
  end;


const
  //https://ru.wikipedia.org/wiki/Панграмма
  PREVIEW_TEXT_DEFAULT =
    'PACK MY BOX WITH FIVE DOZEN LIQUOR JUGS' + LineEnding +
    'pack my box with five dozen liquor jugs' + LineEnding +
    'ЛЮБЯ, СЪЕШЬ ЩИПЦЫ, - ВЗДОХНЁТ МЭР, - КАЙФ ЖГУЧ' + LineEnding +
    'любя, съешь щипцы, - вздохнёт мэр, - кайф жгуч' + LineEnding +
    '1234567890_; .,:; +-*/=, $|&^, %#@, !?\, [] (), ''A'', "B"' + LineEnding +
    'A|B:1; @S&R.2=3, [Q"4"/5] ''6%\E'' (#7-8$) _X?+9^0*N!';
  PREVIEW_SCALE_MAX    = 16; // макс. масштаб изображения
var
  fmPreview: TfmPreview;
  scale:     Integer = 2; // масштаб изображения (кол-во пикселей на 1 пиксель символа)

implementation

{$R *.lfm}

{ TfmPreview }

// создание окна предпросмотра
procedure TfmPreview.FormCreate(Sender: TObject);
  begin
    IniStoragePV.IniFileName := ExtractFileDir(ParamStrUTF8(0)) + SETTINGS_FILE;
    pcPages.ActivePageIndex  := 0;
    pcPages.ShowTabs         := False;
  end;

// показ формы предпросмотра
procedure TfmPreview.FormShow(Sender: TObject);
  begin
    if mmPreview.Lines.Count = 0 then
      if mmPreview.Text = '' then
        mmPreview.Text := PREVIEW_TEXT_DEFAULT;

    UpdatePreview;
  end;


// установка выводимого по умолчанию образца текста
procedure TfmPreview.acResetTextExecute(Sender: TObject);
  begin
    mmPreview.Text := PREVIEW_TEXT_DEFAULT;
  end;

// увеличение масштаба изображения
procedure TfmPreview.acZoomInExecute(Sender: TObject);
  begin
    if scale < PREVIEW_SCALE_MAX then Inc(scale);
    UpdatePreview;
  end;

// уменьшение масштаба изображения
procedure TfmPreview.acZoomOutExecute(Sender: TObject);
  begin
    if scale > 1 then Dec(scale);
    UpdatePreview;
  end;

// экспорт изображения с текстом текущим шрифтом
procedure TfmPreview.acExportImageExecute(Sender: TObject);
  var
    pic: TPicture;
    i:   Integer = 1;
  begin
    while FileExists(SaveDlg.InitialDir + PFontCustom^.Name + '_preview_' +
        IntToStr(i) + '.png') do
      Inc(i);

    with SaveDlg do
      begin
      FileName := PFontCustom^.Name + '_preview_' + IntToStr(i) + '.png';

      if Execute then
          try
          pic := TPicture.Create;

          with pic.Bitmap do
            begin
            Width  := imPreview.Picture.Bitmap.Width * scale;
            Height := imPreview.Picture.Bitmap.Height * scale;

            // создание увеличенного в scale раз изображения
            Canvas.StretchDraw(Rect(0, 0, Width, Height), imPreview.Picture.Graphic);
            end;

          pic.SaveToFile(FileName, 'png');
          finally
          FreeAndNil(pic);
          end;
      end;
  end;

// обновление
procedure TfmPreview.acRefreshExecute(Sender: TObject);
  begin
    UpdatePreview;
  end;

// команда - РЕДАКТИРОВАТЬ ТЕКСТ
procedure TfmPreview.acEditTextExecute(Sender: TObject);
  begin
    if acEditText.Checked then
      tabTxt.Show
    else
      tabPreview.Show;
  end;

// перерисовка изображения предпросмотра при изменении настроек
procedure TfmPreview.rbPropChange(Sender: TObject);
  begin
    UpdatePreview;
  end;

// установка масштаба изображения колесиком мыши
procedure TfmPreview.scbImageMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
  begin
    if WheelDelta > 0 then
      acZoomInExecute(Sender) else
      acZoomOutExecute(Sender);
    Handled := True;
  end;


// обновление изображения предпросмотра
procedure TfmPreview.UpdatePreview;
  var
    bm_tmp: TBitmap;
    CharWidth, CharHeight, char_code, txt_length, x, xl, y, i: Integer;
  begin
    if PFontCustom = nil then Exit;

    bm_tmp        := TBitmap.Create;
    bm_tmp.Width  := PFontCustom^.Item[0].Width;
    bm_tmp.Height := PFontCustom^.Item[0].Height;

    CharWidth  := PFontCustom^.Width;
    CharHeight := PFontCustom^.Height + seDelta.Value;

    lbBackground.Color := fmSettings.ColorPreviewBG;

    // определяем ширину текста
    x := 0;
    for y := 0 to mmPreview.Lines.Count - 1 do
      begin
      xl         := x;
      x          := 0;
      txt_length := Length(UTF8ToEncoding(mmPreview.Lines.Strings[y], PFontCustom^.Encoding));

      if rbProp.Checked then
        for i := 1 to txt_length do
          with PFontCustom^ do
            begin
            char_code := Ord(UTF8ToEncoding(mmPreview.Lines.Strings[y],
              PFontCustom^.Encoding)[i]) - FontStartItem;
            if (char_code >= 0) and (char_code <= FontLength - 1) then
              x := x + Item[char_code].GetCharWidth + seSpace.Value;
            end
      else
        x := txt_length * (CharWidth + seSpace.Value) + 2;

      if xl > x then
        x := xl;
      end;

    with imPreview.Picture.Bitmap do
      begin
      // задаем размеры холста
      Width  := x + 2;
      Height := CharHeight * mmPreview.Lines.Count + 2;

      // очищаем холст
      //Canvas.Brush.Color := PFontCustom^.BackgroundColor;
      //Canvas.Pen.Color   := PFontCustom^.ActiveColor;
      Canvas.Brush.Color := fmSettings.ColorPreviewBG;
      Canvas.Pen.Color   := fmSettings.ColorPreviewA;
      Canvas.Clear;
      Canvas.Clear;

      // выводим текст
      for y := 0 to mmPreview.Lines.Count - 1 do
        begin
        x          := 0;
        txt_length := Length(UTF8ToEncoding(mmPreview.Lines.Strings[y], PFontCustom^.Encoding));
        for i := 1 to txt_length do
          with PFontCustom^ do
            begin
            char_code := Ord(UTF8ToEncoding(mmPreview.Lines.Strings[y],
              PFontCustom^.Encoding)[i]) - FontStartItem;

            if (char_code >= 0) and (char_code <= FontLength - 1) then
              begin
              Item[char_code].DrawPreview(bm_tmp, False,
                fmSettings.ColorPreviewBG, fmSettings.ColorPreviewA);

              Canvas.Draw(1 + x, 1 + y * CharHeight, bm_tmp);

              if rbProp.Checked then
                x := x + Item[char_code].GetCharWidth + seSpace.Value
              else
                x := x + CharWidth + seSpace.Value;
              end;
            end;
        end;


      // масштабирование
      imPreview.Width  := Width * scale;
      imPreview.Height := Height * scale;
      end;
    FreeAndNil(bm_tmp);
    Caption := FM_PREV_EXAMPLE + ' - ' + IntToStr(scale) + ':1';
  end;


end.
