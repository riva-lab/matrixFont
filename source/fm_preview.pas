unit fm_preview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, ExtCtrls, Spin,
  StdCtrls, ActnList, LazUTF8, ComCtrls, Dialogs, Types, AppSettings,
  font, u_encodings, config_record;

resourcestring
  FM_PREV_EXAMPLE = 'Образец текста';

type
  TPFontCustom = ^font.TMatrixFont; // указатель на шрифт

  { TfmPreview }

  TfmPreview = class(TForm)
    acRefresh:     TAction;
    imPreview:     TImage;
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

  private
    procedure InitConfig;

  public
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

implementation

{$R *.lfm}

{ TfmPreview }

// создание окна предпросмотра
procedure TfmPreview.FormCreate(Sender: TObject);
  begin
    InitConfig;
    pcPages.ActivePageIndex := 0;
    pcPages.ShowTabs        := False;
    mmPreview.Text          := PREVIEW_TEXT_DEFAULT;
  end;

// показ формы предпросмотра
procedure TfmPreview.FormShow(Sender: TObject);
  begin
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
    if cfg.prev.scale < PREVIEW_SCALE_MAX then Inc(cfg.prev.scale);
    UpdatePreview;
  end;

// уменьшение масштаба изображения
procedure TfmPreview.acZoomOutExecute(Sender: TObject);
  begin
    if cfg.prev.scale > 1 then Dec(cfg.prev.scale);
    UpdatePreview;
  end;

// экспорт изображения с текстом текущим шрифтом
procedure TfmPreview.acExportImageExecute(Sender: TObject);
  var
    pic: TPicture;
    i:   Integer = 1;
  begin
    while FileExists(SaveDlg.InitialDir + PFontCustom^.Props.Name + '_preview_' +
        IntToStr(i) + '.png') do
      Inc(i);

    with SaveDlg do
      begin
      FileName := PFontCustom^.Props.Name + '_preview_' + IntToStr(i) + '.png';

      if Execute then
        try
        pic := TPicture.Create;

        with pic.Bitmap do
          begin
          Width  := imPreview.Picture.Bitmap.Width * cfg.prev.scale;
          Height := imPreview.Picture.Bitmap.Height * cfg.prev.scale;

          // создание увеличенного в cfg.prev.scale раз изображения
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
    if not Assigned(PFontCustom) then Exit;
    if not Assigned(PFontCustom^) then Exit;

    bm_tmp := TBitmap.Create;

    CharWidth  := PFontCustom^.Width;
    CharHeight := PFontCustom^.Height + seDelta.Value;

    lbBackground.Color := cfg.color.prev.bg;

    // определяем ширину текста
    x            := 0;
    for y        := 0 to mmPreview.Lines.Count - 1 do
      begin
      xl         := x;
      x          := 0;
      txt_length := Length(UTF8ToEncoding(mmPreview.Lines.Strings[y], PFontCustom^.Props.Encoding));

      if rbProp.Checked then
        for i := 1 to txt_length do
          with PFontCustom^ do
            begin
            char_code := Ord(UTF8ToEncoding(mmPreview.Lines.Strings[y],
              PFontCustom^.Props.Encoding)[i]);
            if Item[char_code].Props.Active then
              x       := x + Item[char_code].GetCharWidth + seSpace.Value;
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
      Canvas.Brush.Color := cfg.color.prev.bg;
      Canvas.Pen.Color   := cfg.color.prev.active;
      Canvas.Clear;
      Canvas.Clear;

      // выводим текст
      for y := 0 to mmPreview.Lines.Count - 1 do
        begin
        x          := 0;
        txt_length := Length(UTF8ToEncoding(mmPreview.Lines.Strings[y], PFontCustom^.Props.Encoding));
        for i := 1 to txt_length do
          with PFontCustom^ do
            begin
            char_code := Ord(UTF8ToEncoding(mmPreview.Lines.Strings[y],
              PFontCustom^.Props.Encoding)[i]);

            if Item[char_code].Props.Active then
              begin
              Item[char_code].Draw(bm_tmp, False,
                cfg.color.prev.bg, cfg.color.prev.active);

              Canvas.Draw(1 + x, 1 + y * CharHeight, bm_tmp);

              if rbProp.Checked then
                x := x + Item[char_code].GetCharWidth + seSpace.Value
              else
                x := x + CharWidth + seSpace.Value;
              end;
            end;
        end;


      // масштабирование
      imPreview.Width  := Width * cfg.prev.scale;
      imPreview.Height := Height * cfg.prev.scale;
      end;
    FreeAndNil(bm_tmp);
    Caption := FM_PREV_EXAMPLE + ' - ' + IntToStr(cfg.prev.scale) + ':1';
  end;

procedure TfmPreview.InitConfig;
  begin
    Settings.Add(mmPreview, @cfg.prev.example);
    Settings.Add(seDelta, @cfg.prev.delta);
    Settings.Add(seSpace, @cfg.prev.space);
    Settings.Add(rbMono, @cfg.prev.mono);
    Settings.Add(rbProp, @cfg.prev.prop);
    Settings.Add('_cfg.prev.scale', stInt, @cfg.prev.scale, '2');
  end;


end.
