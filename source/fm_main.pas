unit fm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, ExtCtrls, Controls, StdCtrls, Grids, ComCtrls, ActnList,
  Windows, Graphics, Menus, StdActns, Dialogs, Spin, IniPropStorage, SysUtils,
  LazUTF8, Types, strutils, LCLIntf, LCLTranslator, PairSplitter, LazFileUtils,
  LCLType, ImageSVGList, AppTuner, AppLocalizer, AppSettings, config_record,

  // forms
  fm_gen, fm_new, fm_prop, fm_confirm, fm_import, fm_preview, fm_sizes,
  fm_optimize, fm_range, fm_about, fm_settings, fm_importc, fm_map,

  // functional units
  font, symbol, app_ver, cOpenFileList,

  // additional units
  u_utilities, u_strings, u_helpers, u_encodings;

resourcestring
  TXT_NAVIGATOR   = 'Навигатор';
  TXT_SYMBOL      = 'Символ';
  TXT_CHANGED     = 'изменен';

const
  HELP_DIR        = 'help';
  HELP_DIR_ONLINE = '-/blob/master/help';
  HELP_FILE       = 'matrixFont-help';

type

  { TfmMain }

  TfmMain = class(TForm)
    {$INCLUDE fm_main_controls.inc}

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormWindowStateChange(Sender: TObject);
    procedure FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight, MaxWidth, MaxHeight: TConstraintSize);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure tmrMain10msTimer(Sender: TObject);

    procedure sgNavigatorDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure sgNavigatorSelection(Sender: TObject; aCol, aRow: Integer);
    procedure sgNavigatorMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

    procedure imEditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imEditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

    procedure acNewFontExecute(Sender: TObject);
    procedure acFontOpenExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acSaveAsAccept(Sender: TObject);
    procedure acSaveAsBeforeExecute(Sender: TObject);

    procedure acFontImportExecute(Sender: TObject);
    procedure acFontImportCCodeExecute(Sender: TObject);
    procedure acFontPropertiesExecute(Sender: TObject);
    procedure acFontPreviewExecute(Sender: TObject);

    procedure acGenFormOnTopExecute(Sender: TObject);

    procedure actionZooming(Sender: TObject);
    procedure actionSymbolHistory(Sender: TObject);
    procedure actionSymbolFind(Sender: TObject);
    procedure actionPasteMode(Sender: TObject);
    procedure actionSymbolGeneral(Sender: TObject);
    procedure actionFontGeneral(Sender: TObject);
    procedure actionService(Sender: TObject);
    procedure edFindUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);

    procedure acFontCharsetExecute(Sender: TObject);
    procedure acFontOptimizeExecute(Sender: TObject);
    procedure acFontChangeSizesExecute(Sender: TObject);

    procedure acDeleteLastFilesListExecute(Sender: TObject);
    procedure LastFileAdd(FileName: String);
    procedure LastFileOpen(Sender: TObject);

    procedure acGenerateExecute(Sender: TObject);

    procedure SettingsApplyToCurrentSession(Sender: TObject = nil);

  private
    FLastFileMenuItem:  array [0..LAST_FILES_LIST_SIZE - 1] of TMenuItem;
    FLastFileMenuItem2: array [0..LAST_FILES_LIST_SIZE - 1] of TMenuItem;
    FOpenFileList:      TOpenFileList;
    FPasteMode:         TPasteMode;

    procedure FontLoadFromFile(AFileName: String);
    procedure FontCreateNew(w, h, si, l, e: Integer; n, a: String);
    procedure FontSave(AFileName: String);
    function GetConfirmation: Boolean;
    procedure OnMapSelectChar(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure FileStatusUpdate;
    procedure FontActionExecute;
    procedure FontCreateFinish;
    procedure ReDrawAfterAction;
    procedure ReDrawImage;
    procedure ReDrawContent;
    procedure AdjustComponentSizes;
    procedure AdjustThemeDependentValues;
    procedure LanguageChange;

    procedure InitConfig;
    procedure AfterLoadConfig;
    procedure BeforeSaveConfig;
  end;

var
  fmMain: TfmMain;

implementation

var
  file_changed: Boolean;
  timer_up:     Boolean;
  syncSettings: Boolean = False;
  cnt, frames:  Integer;

  {$R *.lfm}


  { TfmMain }


  { ***  Main form event handlers  *** }

// инициализация
procedure TfmMain.FormCreate(Sender: TObject);
  var
    i: Integer;
  begin
    ReadAppInfo;
    InitConfig;

    FOpenFileList := TOpenFileList.Create;

    // создаем пункты меню под список последних открытых файлов
    for i := 0 to LAST_FILES_LIST_SIZE - 1 do
      begin
      FLastFileMenuItem[i]         := TMenuItem.Create(MenuItem55);
      FLastFileMenuItem[i].OnClick := @LastFileOpen;
      MenuItem55.Insert(i, FLastFileMenuItem[i]);

      FLastFileMenuItem2[i]         := TMenuItem.Create(pmLastFiles);
      FLastFileMenuItem2[i].OnClick := @LastFileOpen;
      pmLastFiles.Items.Insert(i, FLastFileMenuItem2[i]);
      end;
  end;

// появление формы главного окна на экране
procedure TfmMain.FormShow(Sender: TObject);
  begin
    OnShow := nil;

    appLocalizerEx.EnumerateComponents;

    // init tuner
    appTunerEx.IniFile := Settings.IniFile;
    appTunerEx.AddForm(Self);
    appTunerEx.LoadProperties;

    Settings.SyncValues;
    Settings.Load;
    Settings.SyncComponents;
    AfterLoadConfig;

    // load property values to controls
    acStayOnTopToggle.Checked := appTunerEx.Form[Self].StayOnTop;

    LanguageChange;

    // загрузка файла, если он был перетащен на значок приложения
    // или открыт системой по аасоциации с расширением
    if (LazUTF8.ParamStrUTF8(1) <> '')
      and FileExtCheck(LazUTF8.ParamStrUTF8(1), FILE_EXTENSION) then
      FontLoadFromFile(LazUTF8.ParamStrUTF8(1)) else
      with cfg.new do
        FontCreateNew(w, h, start, last - start + 1, enc, title, author);

    SettingsApplyToCurrentSession;
    acZoomFit.Execute;
    tmrMain10msTimer(Sender);
    actionPasteMode(Sender);

    psSplit.Cursor      := crHSplit; // fix splitter cursor bug
    fmMap.OnMouseEvent  := @OnMapSelectChar;
    tmrMain10ms.Enabled := True;

    {$IfDef DEBUG}
    miFPS.Visible := True;
    {$Else}
    miFPS.Visible := False;
    {$EndIf}
  end;

// загрузка файла при перетаскивании его на форму или на значок приложения
procedure TfmMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
  begin
    // загрузка проекта шрифта
    if FileExtCheck(FileNames[0], FILE_EXTENSION) then
      FontLoadFromFile(FileNames[0]);

    // бездиалоговый импорт изображений
    if FileExtCheck(FileNames[0], CHAR_IMPORT_FORMATS) then
      begin
      dlgImport.FileName := FileNames[0];
      acSymbolImportImage.Execute;
      end;

    // import font from C-code
    if FileExtCheck(FileNames[0], IMPORTC_EXTENSION) then
      begin
      fmImportC.FormDropFiles(Sender, FileNames);
      acFontImportCCode.Execute;
      end;
  end;

// изменение состояния главного окна (свернуто, нормально, развернуто)
procedure TfmMain.FormWindowStateChange(Sender: TObject);
  var
    R, RP: TRect;
  begin
    if fmPreview.Visible or fmGen.Visible then
      begin
      GetWindowRect(Handle, R);
      case WindowState of

        // нормальное состояние - окно предпросмотра примагничено к низу главного
        wsNormal:
          begin
          if cfg.prev.magnet then
            begin
            fmPreview.Top   := Top + R.Bottom - R.Top;
            fmPreview.Left  := Left;
            fmPreview.Width := Width;
            end;

          // нормальное состояние - окно генератора справа от редактора
          if acGenFormMagnit.Checked then
            begin
            fmGen.Top    := Top;
            fmGen.Left   := Left + R.Right - R.Left;
            fmGen.Height := Height;
            end;
          end;

        // развернутое состояние - окно предпросмотра в правом нижнем углу
        wsMaximized:
          begin
          if cfg.prev.magnet then
            begin
            fmPreview.Width := fmPreview.Constraints.MinWidth;
            GetWindowRect(fmPreview.Handle, RP);
            fmPreview.Top   := R.Bottom + R.Top - RP.Bottom + RP.Top;
            fmPreview.Left  := R.Right + R.Left - RP.Right + RP.Left;
            end;

          // развернутое состояние - окно генератора в правом нижнем углу
          if acGenFormMagnit.Checked then
            begin
            GetWindowRect(fmGen.Handle, RP);
            fmGen.Top  := R.Bottom + R.Top - RP.Bottom + RP.Top;
            fmGen.Left := R.Right + R.Left - RP.Right + RP.Left;
            end;
          end;
        end;
      end;

    // выравнивание двух малых панелей по правой стороне
    with stStatusBar.Panels do
      Items[0].Width := Width - Items[1].Width - Items[2].Width - Items[3].Width;
  end;

// изменение положения или размера формы
procedure TfmMain.FormConstrainedResize(Sender: TObject; var MinWidth,
  MinHeight, MaxWidth, MaxHeight: TConstraintSize);
  begin
    if fmMain.Showing then
      FormWindowStateChange(Sender);
  end;

// действие при попытке закрыть приложение
procedure TfmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
    CanClose := GetConfirmation;

    if CanClose then
      begin
      Settings.SyncValues;
      BeforeSaveConfig;
      Settings.Save;
      appTunerEx.SaveProperties;
      end;
  end;

// таймер 10 мс
procedure TfmMain.tmrMain10msTimer(Sender: TObject);
  begin
    timer_up := True;

    // счетчик кадров в секунду
    Inc(cnt);
    if cnt = 33 then
      begin
      {$IfDef DEBUG}
      miFPS.Caption := IntToStr(round(frames * 100 / 33)) + ' FPS';
      frames        := 0;
      {$EndIf}
      cnt := 0;

      // synchronize changed settings for correct saving/restoring
      if syncSettings then
        begin
        Settings.SyncComponents;
        syncSettings := False;
        end;
      end;

    if cnt mod 4 = 0 then
      begin
      // отслеживание буфера обмена на наличие данных
      acSymbolPaste.Enabled := not FontSet.Item[0].CopyBufferEmpty;
      acFontPaste.Enabled   := not FontSet.Item[0].CopyBufferEmpty;
      end;

    // управление видимостью панелей кнопок
    tbFile.Visible     := acViewTBFile.Checked;
    tbTools.Visible    := acViewTBTools.Checked;
    pCharTools.Visible := acViewTBCharTools.Checked;
    pFontTools.Visible := acViewTBFontTools.Checked;
  end;


 { ***  Обработчики навигатора  *** }

 // рисование превьюшки в таблице
procedure TfmMain.sgNavigatorDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
  var
    bm_tmp: TBitmap;
  begin
      try
      bm_tmp        := TBitmap.Create;
      bm_tmp.Width  := FontSet.Item[0].Width;
      bm_tmp.Height := FontSet.Item[0].Height;

      with sgNavigator do
        begin
        Columns.Items[ColCount - 1].Width :=
          DefaultRowHeight * FontSet.Width div FontSet.Height;

        if (aRow >= TopRow) and (aRow - TopRow <= VisibleRowCount) and
          (aCol = ColCount - 1) and (aRow >= FixedRows) then
          begin
          // символ
          Cells[0, aRow] := FontSet.GetCharName(FontSet.FontStartItem + aRow - FixedRows);

          // код символа
          if cfg.nav.code.hex then
            Cells[1, aRow] := IntToHex(FontSet.FontStartItem + aRow - FixedRows, 2) else
            Cells[1, aRow] := IntToStr(FontSet.FontStartItem + aRow - FixedRows);

          // превью символа
          if cfg.nav.invert and (aRow = Row) then
            // выделенная строка в навигаторе
            FontSet.Item[aRow - FixedRows].DrawPreview(bm_tmp, cfg.nav.transparent,
              cfg.color.nav.active, cfg.color.nav.bg)
          else
            // невыделенная строка в навигаторе
            FontSet.Item[aRow - FixedRows].DrawPreview(bm_tmp, cfg.nav.transparent,
              cfg.color.nav.bg, cfg.color.nav.active);
          Canvas.StretchDraw(aRect, bm_tmp);
          end;

        // заголовок редактора
        lbEditor.Caption := TXT_SYMBOL + '  <' + sgNavigator.Cells[0, Row] + '>'
          + '  DEC = ' + IntToStr(FontSet.FontStartItem + Row - FixedRows)
          + ';  HEX = ' + IntToHex(FontSet.FontStartItem + Row - FixedRows, 2);
        end;
      finally
      FreeAndNil(bm_tmp);
      end;
  end;

// обновление изображения при смене символа (перемещении в навигаторе)
procedure TfmMain.sgNavigatorSelection(Sender: TObject; aCol, aRow: Integer);
  var
    item: TSymbol;
  begin
    item                 := FontSet.Item[sgNavigator.Row - sgNavigator.FixedRows];
    acSymbolUndo.Enabled := not item.HistoryEmpty;
    acSymbolRedo.Enabled := not item.HistoryNoRedo;
    ReDrawImage;
  end;

// изменение высоты строки навигатора мышью [+Ctrl]
procedure TfmMain.sgNavigatorMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  begin
    if ssCtrl in Shift then
      with cfg.nav do
        begin
        if WheelDelta <> 0 then rowheight := rowheight + WheelDelta div abs(WheelDelta);
        sgNavigator.DefaultRowHeight := rowheight;
        syncSettings := True;
        end;
  end;


 { ***  Обработчики холста символа  *** }

 // обработка нажатия мышью на рабочем холсте
procedure TfmMain.imEditorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  var
    i:         Integer;
    item:      TSymbol;
    pixAction: TPixelAction = TPixelAction.paNone;
  begin
    case Button of
      mbLeft: pixAction   := TPixelAction.paSet;
      mbRight: pixAction  := TPixelAction.paClear;
      mbMiddle: pixAction := TPixelAction.paInvert;
      end;

    if pixAction <> TPixelAction.paNone then
      begin
      X := (X - FontSet.GridThickness div 2) div FontSet.GridStep;
      Y := (Y - FontSet.GridThickness div 2) div FontSet.GridStep;

      item := FontSet.Item[sgNavigator.Row - sgNavigator.FixedRows];

      if ssCtrl in Shift then
        for i := 0 to FontSet.Width - 1 do
          item.PixelAction(i, y, pixAction);

      if ssShift in Shift then
        for i := 0 to FontSet.Height - 1 do
          item.PixelAction(x, i, pixAction);

      if not ((ssCtrl in Shift) xor (ssShift in Shift)) then
        item.PixelAction(x, y, pixAction);

      ReDrawImage;
      end;
  end;

// обработка перемещения мышью на рабочем холсте
procedure TfmMain.imEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  var
    Button: TMouseButton = mbExtra1;
  begin
    if timer_up then
      begin
      if ssLeft in Shift then Button  := mbLeft;
      if ssRight in Shift then Button := mbRight;
      imEditorMouseDown(Sender, Button, Shift, X, Y);

      stStatusBar.Panels.Items[2].Text := 'X,Y: ' + IntToStr((x - 1) div FontSet.GridStep + 1) +
        ', ' + IntToStr((y - 1) div FontSet.GridStep + 1);

      timer_up := False;
      Inc(frames); // счетчик FPS
      end;
  end;

// завершение рисования мышью, сохранение изменений
procedure TfmMain.imEditorMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  begin
    ReDrawAfterAction;
  end;

// масштаб колесом мыши
procedure TfmMain.imMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  begin
    if WheelDelta > 0 then
      acZoomIn.Execute else
      acZoomOut.Execute;
    Handled := True;
  end;


 { ***  Действия c файлами и новыми проектами  *** }

 // действие: открыть окно создания нового шрифта
procedure TfmMain.acNewFontExecute(Sender: TObject);
  begin
    if not GetConfirmation then Exit;

    with fmNew do
      if ShowModal = mrOk then
        FontCreateNew(
          seWidth.Value, seHeight.Value,
          seStartItem.Value, seLastItem.Value - seStartItem.Value + 1,
          cbEncoding.ItemIndex, edFontName.Text, edAuthor.Text);
  end;

// действие: открыть файл
procedure TfmMain.acFontOpenExecute(Sender: TObject);
  var
    tmp: String;
  begin
    if not GetConfirmation then Exit;

    with dlgOpen do
      begin
      tmp      := FileName;
      FileName := '';
      if Execute then
        begin
        file_changed := False;
        FontLoadFromFile(FileName);
        end
      else
        FileName := tmp;
      end;
  end;

// действие: сохранить файл
procedure TfmMain.acSaveExecute(Sender: TObject);
  begin
    with acSaveAs.Dialog do
      begin
      InitialDir := ExtractFileDir(FileName) + DirectorySeparator;
      FileName   := ExtractFileName(FileName);

      if dlgOpen.FileName = '' then
        begin
        FileName := AnsiReplaceText(FontSet.Name, ' ', '_');
        if Execute then FontSave(FileName);
        end
      else
        FontSave(dlgOpen.FileName);
      end;

    FileStatusUpdate();
  end;

// действие: сохранить файл как (после нажатия ОК в диалоге)
procedure TfmMain.acSaveAsAccept(Sender: TObject);
  begin
    FontSave(acSaveAs.Dialog.FileName);
    FileStatusUpdate();
  end;

// действие перед вызовом метода -сохранить файл как-
procedure TfmMain.acSaveAsBeforeExecute(Sender: TObject);
  begin
    with acSaveAs.Dialog do
      begin
      if FileName = '' then
        FileName := dlgOpen.FileName;

      if FileName = '' then
        FileName := AnsiReplaceText(FontSet.Name, ' ', '_');

      InitialDir := ExtractFileDir(FileName) + DirectorySeparator;
      FileName   := ExtractFileName(FileName);
      end;
  end;


// действие: импорт системного шрифта для растеризации
procedure TfmMain.acFontImportExecute(Sender: TObject);
  begin
    if not GetConfirmation then Exit;

    with fmImport do
      begin
      seStartItem.Value := cfg.new.start;
      seLastItem.Value  := cfg.new.last;

      if ShowModal = mrOk then
        begin
        dlgOpen.FileName := '';

        FontCreateNew(
          seW.Value, seH.Value, seStartItem.Value, seLastItem.Value - seStartItem.Value + 1,
          cbEncoding.ItemIndex, dlgFont.Font.Name + ' ' + dlgFont.Font.Size.ToString,
          cfg.new.author);
        FontSet.Import(dlgFont.Font, seW.Value, seH.Value);

        if cbSnapLeft.Checked then acFontSnapLeft.Execute else acFontCenterH.Execute;
        if cbOptimize.Checked then FontSet.ChangeSize(-1, -1, -1, -1, True);

        FontCreateFinish;
        end;
      end;
  end;

// действие: импорт шрифта из кода C
procedure TfmMain.acFontImportCCodeExecute(Sender: TObject);
  begin
    if not GetConfirmation then Exit;

    with fmImportC do
      begin
      FontImp.Encoding := FontSet.Encoding;

      if ShowModal = mrOk then
        begin
        dlgOpen.FileName := '';

        FontCreateNew(
          seImpWidth.Value, seImpHeight.Value,
          seImpStartItem.Value, seImpLastItem.Value - seImpStartItem.Value + 1,
          cfg.new.enc, cfg.new.title, cfg.new.author);
        UpdateFont(FontSet);
        FontCreateFinish;
        end;
      end;
  end;

// действие: просмотр и изменение свойств шрифта
procedure TfmMain.acFontPropertiesExecute(Sender: TObject);
  begin
    with fmProp, FontSet do
      begin
      prPath      := dlgOpen.FileName;
      prName      := Name;
      prAuthor    := Author;
      prFirst     := FontStartItem;
      prLast      := FontLength + FontStartItem - 1;
      prW         := Width;
      prH         := Height;
      prDTCreate  := DateTimeToStr(DateCreate);
      prDTChange  := DateTimeToStr(DateChange);
      prAppCreate := AppCreate;
      prAppChange := AppChange;
      prEnc       := GetIndexOfEncoding(Encoding);

      if ShowModal = mrOk then
        begin
        file_changed := True;
        Name         := edFontName.Text;
        Author       := edAuthor.Text;
        Encoding     := GetEncodingByIndex(cbEncoding.ItemIndex);

        FileStatusUpdate();
        ReDrawContent;
        end;
      end;
  end;

// действие: показать окно вывода образца текста
procedure TfmMain.acFontPreviewExecute(Sender: TObject);
  begin
    with acSaveAs.Dialog do
      begin
      if FileName = '' then FileName := dlgOpen.FileName;
      if FileName = '' then FileName := Application.Params[0];

      fmPreview.SaveDlg.InitialDir := ExtractFileDir(FileName) + DirectorySeparator;
      end;

    fmPreview.PFontCustom := @FontSet;
    fmPreview.Show;
    FormWindowStateChange(Sender);
  end;


 { ***  Действия c внешним видом и поведением  *** }


 // действие: окно генератора Поверх окна редактора
procedure TfmMain.acGenFormOnTopExecute(Sender: TObject);
  var
    tmp_top, tmp_left: Integer;
  begin
    with fmGen do
      begin
      tmp_top   := Top;
      tmp_left  := Left;
      OnShow    := nil;
      PopupMode := acGenFormOnTop.Checked.Select(pmExplicit, pmNone);
      OnShow    := @FormShow;
      Top       := tmp_top;
      Left      := tmp_left;

      // двойное применение устраняет отсутствие прорисовки полос прокрутки
      //snEdit.ScrollBars := ssAutoVertical;
      //snEdit.ScrollBars := ssAutoBoth;
      end;
  end;


{ ***  Действия с отдельными символами и шрифтом в целом  *** }

// действия с символом: масштабирование
procedure TfmMain.actionZooming(Sender: TObject);
  var
    item: TSymbol;
  begin
    item := FontSet.Item[sgNavigator.Row - sgNavigator.FixedRows];

    if Sender <> nil then
      case TAction(Sender).Name of

        'acZoomFit': // масштаб символа "вписанный в видимую область"
          item.ZoomFitToArea(scbEditor.Width, scbEditor.Height);

        'acZoomIn':  // увеличение масштаба
          item.ZoomIn;

        'acZoomOut': // уменьшение масштаба
          item.ZoomOut;
        end;

    imEditor.Width   := item.WidthInPixels;
    imEditor.Height  := item.HeightInPixels;
    FontSet.GridStep := item.GridStep;
    ReDrawImage;
  end;

// действия с символом: отмена/повтор
procedure TfmMain.actionSymbolHistory(Sender: TObject);
  var
    item: TSymbol;
  begin
    item := FontSet.Item[sgNavigator.Row - sgNavigator.FixedRows];

    case TAction(Sender).Name of

      'acSymbolUndo':
        item.UndoChange;

      'acSymbolRedo':
        item.RedoChange;

      'acFontUndo':
        FontSet.UndoChange;

      'acFontRedo':
        FontSet.RedoChange;
      end;

    acSymbolUndo.Enabled := not item.HistoryEmpty;
    acSymbolRedo.Enabled := not item.HistoryNoRedo;
    ReDrawImage;
    ReDrawContent;
  end;

// действия с символом: поиск
procedure TfmMain.actionSymbolFind(Sender: TObject);
  begin
    if TAction(Sender).Name = 'acSymbolFind' then
      begin
      // показать/скрыть панель поиска символа в таблице
      pFind.Visible := acSymbolFind.Checked;
      seFind.Value  := sgNavigator.Row + FontSet.FontStartItem - sgNavigator.FixedRows;
      edFind.Text   := EncodingToUTF8(Char(seFind.Value), FontSet.Encoding);
      end
    else
      begin

      // поиск и выделение найденного символа по коду
      if TSpinEdit(Sender).Name = 'seFind' then
        if (seFind.Value >= FontSet.FontStartItem) and
          (seFind.Value < FontSet.FontStartItem + FontSet.FontLength) then
          edFind.Text := EncodingToUTF8(Char(seFind.Value), FontSet.Encoding);

      // поиск и выделение найденного символа по названию
      if TEdit(Sender).Name = 'edFind' then
        if edFind.Text <> '' then
          seFind.Value := Ord(UTF8ToEncoding(edFind.Text, FontSet.Encoding)[1]);

      sgNavigator.Row := seFind.Value - FontSet.FontStartItem + sgNavigator.FixedRows;
      sgNavigatorSelection(Sender, 1, sgNavigator.Row);
      end;
  end;

// выбор режима вставки
procedure TfmMain.actionPasteMode(Sender: TObject);
  const
    modeStr: array[TPasteMode] of String[5] = ('NORM', 'OR', 'XOR', 'AND');
  begin
    if Sender = nil then
      FPasteMode := pmNorm; // режим по умолчанию

    if TAction(Sender).Checked then
      case TAction(Sender).Name of
        'acPasteModeNorm': FPasteMode := pmNorm;
        'acPasteModeAnd': FPasteMode  := pmAnd;
        'acPasteModeOr': FPasteMode   := pmOr;
        'acPasteModeXor': FPasteMode  := pmXor;
        end;

    case FPasteMode of
      pmNorm: acPasteModeNorm.Checked := True;
      pmAnd: acPasteModeAnd.Checked   := True;
      pmOr: acPasteModeOr.Checked     := True;
      pmXor: acPasteModeXor.Checked   := True;
      end;

    stStatusBar.Panels.Items[1].Text := modeStr[FPasteMode];
  end;

// действия с выбранным символом
procedure TfmMain.actionSymbolGeneral(Sender: TObject);
  var
    item: TSymbol;
  begin
    item := FontSet.Item[sgNavigator.Row - sgNavigator.FixedRows];

    case TAction(Sender).Name of

      'acSymbolCopy':       // действие: копирование символа в буфер обмена
        item.ClipboardAction(TClipboardAction.cbCopy);

      'acSymbolCut':        // действие: вырезание символа в буфер обмена
        item.ClipboardAction(TClipboardAction.cbCut);

      'acSymbolPaste':      // действие: вставка символа из буфера обмена
        item.ClipboardAction(TClipboardAction.cbPaste, FPasteMode);


      'acSymbolClear':      // действие: очистить символ
        item.Clear;

      'acSymbolInvert':     // действие: инверсия символа
        item.Invert;

      'acSymbolImportImage': // действие: импорт изображения символа из файла PNG
        with dlgImport do
          begin
          if (FileName <> '') or Execute then
            item.ImportImage(FileName, cfg.import.bwlevel);
          FileName := '';
          end;


      'acSymbolMirrorHorz': // действие: отображение символа горизонтально
        item.Mirror(TMirror.mrHorizontal);

      'acSymbolMirrorVert': // действие: отображение символа вертикально
        item.Mirror(TMirror.mrVertical);


      'acSymbolShiftDown':  // действие: сдвиг символа вниз
        item.Shift(TDirection.dirDown);

      'acSymbolShiftLeft':  // действие: сдвиг символа влево
        item.Shift(TDirection.dirLeft);

      'acSymbolShiftRight': // действие: сдвиг символа вправо
        item.Shift(TDirection.dirRight);

      'acSymbolShiftUp':    // действие: сдвиг символа вверх
        item.Shift(TDirection.dirUp);


      'acSymbolSnapDown':   // действие: прижатие символа вниз
        item.Snap(TBorder.brDown);

      'acSymbolSnapLeft':   // действие: прижатие символа влево
        item.Snap(TBorder.brLeft);

      'acSymbolSnapRight':  // действие: прижатие символа вправо
        item.Snap(TBorder.brRight);

      'acSymbolSnapUp':     // действие: прижатие символа вверх
        item.Snap(TBorder.brUp);


      'acSymbolCenterH':    // действие: центрирование символа горизонтально
        item.Center(False);

      'acSymbolCenterV':    // действие: центрирование символа вертикально
        item.Center(True);


      'acSymbolRotateCW':   // действие: поворот символа по ч.с.
        item.Rotate(True);

      'acSymbolRotateCCW':  // действие: поворот символа против ч.с.
        item.Rotate(False);
      end;

    ReDrawAfterAction;
  end;

// действия с шрифтом (пакетное редактирование символов)
procedure TfmMain.actionFontGeneral(Sender: TObject);
  var
    curr: Integer;
  begin
    curr := sgNavigator.Row - sgNavigator.FixedRows;

    case TAction(Sender).Name of

      'acFontClear':      // действие: очистка символов шрифта
        FontSet.Clear;

      'acFontInvert':     // действие: инверсия символов шрифта
        FontSet.Invert;

      'acFontMirrorHorz': // действие: отображение горизонтально символов шрифта
        FontSet.Mirror(TMirror.mrHorizontal);

      'acFontMirrorVert': // действие: отображение вертикально символов шрифта
        FontSet.Mirror(TMirror.mrVertical);


      'acFontShiftDown':  // действие: сдвиг вниз символов шрифта
        FontSet.Shift(TDirection.dirDown);

      'acFontShiftLeft':  // действие: сдвиг влево символов шрифта
        FontSet.Shift(TDirection.dirLeft);

      'acFontShiftRight': // действие: сдвиг вправо символов шрифта
        FontSet.Shift(TDirection.dirRight);

      'acFontShiftUp':    // действие: сдвиг вверх символов шрифта
        FontSet.Shift(TDirection.dirUp);


      'acFontSnapDown':   // действие: прижатие вниз символов шрифта
        FontSet.Snap(TBorder.brDown);

      'acFontSnapLeft':   // действие: прижатие влево символов шрифта
        FontSet.Snap(TBorder.brLeft);

      'acFontSnapRight':  // действие: прижатие вправо символов шрифта
        FontSet.Snap(TBorder.brRight);

      'acFontSnapUp':     // действие: прижатие вверх символов шрифта
        FontSet.Snap(TBorder.brUp);


      'acFontCenterH':    // действие: центрирование символов шрифта горизонтально
        FontSet.Center(False);

      'acFontCenterV':    // действие: центрирование символов шрифта вертикально
        FontSet.Center(True);


      'acFontRotateCW':   // действие: поворот символов шрифта по ч.с.
        FontSet.Rotate(True);

      'acFontRotateCCW':  // действие: поворот символов шрифта против ч.с.
        FontSet.Rotate(False);


      'acFontPaste':      // действие: пакетная вставка
        FontSet.Paste(FPasteMode);


      'acSymbolMoveUp':   // действие: переместить символ вверх
        if FontSet.SwapChars(curr, curr - 1) then
          sgNavigator.Row := sgNavigator.FixedRows + curr - 1;

      'acSymbolMoveDown': // действие: переместить символ вниз
        if FontSet.SwapChars(curr, curr + 1) then
          sgNavigator.Row := sgNavigator.FixedRows + curr + 1;
      end;

    FontActionExecute;
  end;

// действия сервисные
procedure TfmMain.actionService(Sender: TObject);
  begin
    case TAction(Sender).Name of

      'acMap':     // действие: показать окно "Карта символов"
        begin
        fmMap.FontX := FontSet;
        fmMap.Show;
        end;

      'acSetting': // действие: открыть окно "настройки приложения"
        if fmSettings.ShowModal = mrOk then
          begin
          SettingsApplyToCurrentSession;
          ReDrawImage;
          end;

      'acReset':   // действие: сброс настроек
        if fmConfirm.Show(TXT_RESET, WARN_RESET, [mbYes, mbNo], Self) = mrYes then
          begin
          Settings.Clear;
          appTunerEx.ClearProperties;
          Close;
          end;

      'acHelp':    // действие: вызов справки
        OpenURL('..' + DirectorySeparator + HELP_DIR + DirectorySeparator + HELP_FILE + '.html');

      'acHelpMD':  // действие: вызов справки markdown
        OpenURL('..' + DirectorySeparator + HELP_DIR + DirectorySeparator + HELP_FILE + '.md');

      'acHelpNet': // действие: вызов справки онлайн
        OpenURL(APP_SITE_ADDRESS + '/' + HELP_DIR_ONLINE + '/' + HELP_FILE + '.md');

      'acWebsite': // действие: домашняя страница
        OpenURL(APP_SITE_ADDRESS);

      'acOpenRepo':// действие: репозиторий проекта
        OpenURL(APP_SITE_ADDRESS);

      'acInfo':    // действие: информация о программе
        fmAbout.Show;

      'acAppExit': // действие: выход из приложения
        fmMain.Close;

      'acStayOnTopToggle': // действие: поверх всех окон
        appTunerEx.Form[Self].StayOnTop := acStayOnTopToggle.Checked;

      end;
  end;

procedure TfmMain.edFindUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  begin
    edFind.Text := '';
  end;


// действие: изменение диапазона символов шрифта
procedure TfmMain.acFontCharsetExecute(Sender: TObject);
  begin
    with fmRange, FontSet do
      begin
      seStart.Value := FontStartItem;
      seEnd.Value   := FontLength + FontStartItem - 1;

      if ShowModal = mrOk then
        begin
        SetRange(seStart.Value, seEnd.Value);

        sgNavigator.RowCount := FontSet.FontLength + sgNavigator.FixedRows;
        FontActionExecute;
        FileStatusUpdate();
        end;
      end;
  end;

// действие: оптимизация размеров холста символов шрифта
procedure TfmMain.acFontOptimizeExecute(Sender: TObject);
  begin
    with fmOptimize, FontSet do
      begin
      opt_oldHeight := Height;
      opt_oldWidth  := Width;
      opt_up        := CanOptimize(TCanOptimize.coUp);
      opt_down      := CanOptimize(TCanOptimize.coDown);
      opt_left      := CanOptimize(TCanOptimize.coLeft);
      opt_right     := CanOptimize(TCanOptimize.coRight);

      if not ((opt_oldHeight - opt_up - opt_down > 0) and
        (opt_oldWidth - opt_left - opt_right > 0)) then
        fmConfirm.Show(TXT_WARNING, WARN_OPTIMIZE, [mbCancel], Self)
      else
      if ShowModal = mrOk then
        begin
        ChangeSize(res_up, res_down, res_left, res_right, True);

        FontCreateFinish;
        end;
      end;
  end;

// действие: изменение размеров холста символов шрифта
procedure TfmMain.acFontChangeSizesExecute(Sender: TObject);
  begin
    with fmSizes do
      begin
      oldHeight := FontSet.Height;
      oldWidth  := FontSet.Width;

      if fmSizes.ShowModal = mrOk then
        begin
        FontSet.ChangeSize(
          seUp.Value, seDown.Value,
          seLeft.Value, seRight.Value,
          rgMode.ItemIndex = 1);

        FontCreateFinish;
        end;
      end;
  end;


 { ***  Работа со списком последних открытых файлов  *** }

 // действие: очистка списка последних открытых файлов
procedure TfmMain.acDeleteLastFilesListExecute(Sender: TObject);
  begin
    FOpenFileList.Clear;
    LastFileAdd(''); // обновление списка последних открытых файлов
  end;

// добавление файла в список последних открытых
procedure TfmMain.LastFileAdd(FileName: String);
  var
    i: Integer;
  begin
    FOpenFileList.FilePath[0] := FileName;

    acLastFilesList.Visible := False;
    for i := 0 to LAST_FILES_LIST_SIZE - 1 do
      begin
      with FLastFileMenuItem[i] do
        begin
        Hint    := FOpenFileList.FilePath[i];
        Caption := FOpenFileList.FileName[i];
        Visible := Caption <> '';

        acLastFilesList.Visible := acLastFilesList.Visible or Visible;
        end;

      // список в контекстном меню кнопки Открыть на панели быстрого доступа
      with FLastFileMenuItem2[i] do
        begin
        Hint    := FOpenFileList.FilePath[i];
        Caption := FOpenFileList.FileName[i];
        Visible := Caption <> '';
        end;

      if acLastFilesList.Visible then
        ToolBtnLastFiles.PopupMenu := pmLastFiles else
        ToolBtnLastFiles.PopupMenu := nil;
      end;
  end;

// открытие выбранного файла из списка последних открытых файлов
procedure TfmMain.LastFileOpen(Sender: TObject);
  begin
    FontLoadFromFile(TMenuItem(Sender).Hint);
  end;



// действие: открыть окно генератора
procedure TfmMain.acGenerateExecute(Sender: TObject);
  begin
    with acSaveAs.Dialog do
      begin
      if FileName = '' then FileName := dlgOpen.FileName;
      if FileName = '' then FileName := Application.Params[0];

      fmGen.SaveDlg.InitialDir := ExtractFileDir(FileName) + DirectorySeparator;
      end;

    acGenFormOnTopExecute(Sender);
    if Sender <> nil then fmGen.Show else fmGen.FormShow(nil);
    FormWindowStateChange(Sender);
  end;


// действие: применение настроек к текущей сессии
procedure TfmMain.SettingsApplyToCurrentSession(Sender: TObject);
  begin
    BeginFormUpdate;

    if FontSet <> nil then
      try
      with FontSet do
        begin
        ShowGrid              := acGridToggle.Checked;
        FontSet.ShiftRollover := acFontShiftRollover.Checked;

        BackgroundColor     := cfg.color.editor.bg;
        ActiveColor         := cfg.color.editor.active;
        GridColor           := cfg.color.editor.grid;
        GridThickness       := cfg.grid.size;
        GridChessBackground := cfg.grid.chess;

        // заголовок навигатора
        lbNavigator.Caption := TXT_NAVIGATOR + ': ' + IntToStr(FontStartItem) + ' - ' +
          IntToStr(FontStartItem + FontLength - 1) + ' ';

        imEditor.Width  := Item[0].WidthInPixels;
        imEditor.Height := Item[0].HeightInPixels;
        end;

      with sgNavigator.Columns do
        begin
        Items[0].Visible := cfg.nav.char.enable;
        Items[1].Visible := cfg.nav.code.enable;

        Items[0].Font.Name := Screen.Fonts[cfg.nav.char.font];
        Items[0].Font.Size := cfg.nav.char.fontsize;
        Items[1].Font.Name := Screen.Fonts[cfg.nav.code.font];
        Items[1].Font.Size := cfg.nav.code.fontsize;
        end;

      sgNavigator.DefaultRowHeight := cfg.nav.rowheight;

      if fmSettings <> nil then
        begin
        FormWindowStateChange(self);
        end;
      except
      if fmConfirm.Show(TXT_ERROR, WARN_SETTINGS, mbYesNo, self) = mrYes then
        Close;
      end;

    with sgNavigator do
      begin
      if cfg.nav.scroll then
        MouseWheelOption := mwCursor else
        MouseWheelOption := mwGrid;
      end;

    AdjustComponentSizes;
    AdjustThemeDependentValues;
    LanguageChange;
    acGenerateExecute(nil);
    actionZooming(nil);
    ReDrawContent;
    ReDrawImage;

    EndFormUpdate;
  end;

// загрузка файла шрифта
procedure TfmMain.FontLoadFromFile(AFileName: String);
  var
    tmp:        TFont;
    isReadable: Boolean;
  begin
    if not GetConfirmation then Exit;
    if not FileExists(AFileName) then Exit;

    if CompareFileExt(UpperCase(AFileName), UpperCase(FILE_EXTENSION), False) = 0 then
      begin
      tmp        := TFont.Create;
      isReadable := tmp.ReadFromFile(AFileName);
      FreeAndNil(tmp);

      // если файл поврежден - предупреждаем, выходим
      if not isReadable then
        begin
        fmConfirm.Show(TXT_ERROR, WARN_LOAD, [mbYes], self);
        Exit;
        end;

      // загружаем файл, если он не поврежден
      with FontSet do
        begin
        FreeAndNil(FontSet);
        FontSet := TFont.Create;
        ReadFromFile(AFileName);

        acSymbolRedo.Enabled     := False;
        acSymbolUndo.Enabled     := False;
        AppAdditional            := app_info.CompanyName;
        AppCurrent               := app_info.ProductName + ' v' + app_info.FileVersion;
        dlgOpen.FileName         := AFileName;
        acSaveAs.Dialog.FileName := AFileName;
        sgNavigator.RowCount     := FontLength + sgNavigator.FixedRows;

        LastFileAdd(AFileName);
        end;

      FontCreateFinish;
      file_changed := False;
      FileStatusUpdate;
      SettingsApplyToCurrentSession;
      end;
  end;

// создание нового шрифта
procedure TfmMain.FontCreateNew(w, h, si, l, e: Integer; n, a: String);
  begin
      try
      with FontSet do
        begin
        FreeAndNil(FontSet);
        FontSet              := TFont.Create;
        acSymbolRedo.Enabled := False;
        acSymbolUndo.Enabled := False;

        Name          := n;
        Author        := a;
        AppCreate     := app_info.ProductName + ' v' + app_info.FileVersion;
        AppCurrent    := AppCreate;
        AppAdditional := app_info.CompanyName;
        Encoding      := GetEncodingByIndex(e);

        Width                := w;
        Height               := h;
        FontStartItem        := si;
        FontLength           := l;
        sgNavigator.RowCount := FontLength + sgNavigator.FixedRows;
        end;

      FontCreateFinish;
      dlgOpen.FileName := '';
      file_changed     := False;
      FileStatusUpdate;
      SettingsApplyToCurrentSession;
      except
      if fmConfirm.Show(TXT_ERROR, WARN_CREATE, mbYesNo, self) = mrYes then
        Close;
      end;
  end;

// сохранение шрифта
procedure TfmMain.FontSave(AFileName: String);
  begin
    FontSet.SaveToFile(AFileName);
    LastFileAdd(AFileName);
    dlgOpen.FileName := AFileName;
    file_changed     := False;
  end;

// подтверждение при попытке закрыть несохраненный файл
function TfmMain.GetConfirmation: Boolean;
  begin
    if file_changed then
      case fmConfirm.Show(TXT_CONFIRM, WARN_NOTSAVED, mbYesNoCancel, Self) of

        mrYes:   // сохранить изменения
          acSave.Execute;

        mrNo:    // не сохранять изменения, закрыть
          Exit(True);
        end;

    Result := not file_changed;
  end;

// event on select char in map
procedure TfmMain.OnMapSelectChar(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
    if (Button = mbLeft) and (Shift = [ssDouble]) then
      sgNavigator.Row := fmMap.SelectedIndex + sgNavigator.FixedRows;
  end;


 { ***  Обновление состояния компонентов  *** }

 // обновление информации о состоянии файла
procedure TfmMain.FileStatusUpdate;
  var
    s: String;
  begin
    BeginFormUpdate;

    s := FontSet.Name;
    if dlgOpen.FileName <> '' then s += ' [' + ExtractFileName(dlgOpen.FileName) + ']';
    if file_changed then s += ' (' + TXT_CHANGED + ')';

    fmMain.Caption    := fmAbout.AppIntName + ' - ' + s;
    Application.Title := s;

    stStatusBar.Panels.Items[3].Text := app_info.FileVersion;

    with FontSet do
      miFontInfo.Caption := UpperCase(Encoding) + '   '
        + Width.ToString + ' x ' + Height.ToString;

    EndFormUpdate;
  end;

// действия после применения эффекта ко всем символам шрифта
procedure TfmMain.FontActionExecute;
  begin
    ReDrawImage;
    ReDrawContent;
    acSymbolUndo.Enabled := not FontSet.Item[sgNavigator.Row - sgNavigator.FixedRows].HistoryEmpty;
    acSymbolRedo.Enabled := False;
    file_changed         := True;
    FileStatusUpdate();
  end;

// завершение создания шрифта
procedure TfmMain.FontCreateFinish;
  begin
    FontSet.ClearChanges;

    imEditor.Width  := FontSet.Item[0].WidthInPixels;
    imEditor.Height := FontSet.Item[0].HeightInPixels;

    acZoomFit.Execute;
    FontActionExecute;
  end;

// действия после применения изменений к символу
procedure TfmMain.ReDrawAfterAction;
  begin
    FontSet.Item[sgNavigator.Row - sgNavigator.FixedRows].SaveChange;
    file_changed         := True;
    acSymbolUndo.Enabled := True;
    acSymbolRedo.Enabled := False;
    ReDrawImage;
    ReDrawContent;
    FileStatusUpdate();
  end;

// прорисовка изображения холста [и превью]
procedure TfmMain.ReDrawImage;
  var
    index: Integer;
  begin
    index            := sgNavigator.Row - sgNavigator.FixedRows;
    imEditor.Visible := False; // откл. видимость (и автообновление) компонента

    with imEditor.Picture do
      begin
      Bitmap.Width  := FontSet.Item[index].WidthInPixels;
      Bitmap.Height := FontSet.Item[index].HeightInPixels;
      FontSet.Item[index].Draw(Bitmap);
      end;

    imEditor.Visible := True; // отображаем готовое изображение
  end;

 // обновление контента (навигатор, предпросмотр)
procedure TfmMain.ReDrawContent;
  begin
    // обновление изображения символов в навигаторе
    sgNavigator.Repaint;

    // обновление изображения предпросмотра
    if (fmPreview <> nil) and fmPreview.Visible and cfg.prev.refresh then
      fmPreview.UpdatePreview;

    // обновление карты символов
    if (fmMap <> nil) and fmMap.Visible then
      fmMap.UpdateMap;
  end;

// adjust sizes of UI elements
procedure TfmMain.AdjustComponentSizes;

  procedure SetToolbarButtonSize(AToolbars: array of TToolBar; W: Integer; H: Integer = -1);
    var
      item: TToolBar;
      b:    Integer;
    begin
      if Length(AToolbars) = 0 then Exit;

      for item in AToolbars do
        begin
        item.ButtonHeight   := (H < 0).Select(W, H);
        item.ButtonWidth    := W;
        item.DisabledImages := ImListNew16D;

        // fix incorrect divider height in vertical toolbars
        for b := 0 to item.ButtonCount - 1 do
          with item.Buttons[b] do
            if Style = tbsDivider then
              begin
              Style := tbsButton;
              Style := tbsDivider;
              end;
        end;
    end;

  procedure SetStatusBarPanelWidth(AIndex: Integer; ASample: String);
    begin
      with stStatusBar.Panels do
        if AIndex in [0..Count - 1] then
          with Items[AIndex] do
            begin
            Width := Canvas.GetTextWidth(ASample);
            if AIndex = Count - 1 then Width := Width + stStatusBar.Height;
            end;
    end;

  procedure RenderSVGIcons(ASize: Integer; A, D: TImageList);
    begin
      imSVGList.Rendering      := False;
      imSVGList.RenderSize     := ASize;
      imSVGList.ImagesActive   := A;
      imSVGList.ImagesDisabled := D;
      imSVGList.Rendering      := True;
    end;
  var
    w, h, i: Integer;
  begin
    BeginFormUpdate;

    // on 96dpi's screen at 100% resolution muat be 16px
    RenderSVGIcons(Round(Scale96ToScreen(16) * 101 / 100), ImListNew16A, ImListNew16D);

    // иконки-значки в заголовке окон
    ImListNew16A.GetIcon(3, fmSettings.Icon);
    ImListNew16A.GetIcon(32, fmRange.Icon);
    ImListNew16A.GetIcon(36, fmOptimize.Icon);
    ImListNew16A.GetIcon(25, fmSizes.Icon);
    ImListNew16A.GetIcon(31, fmImport.Icon);
    ImListNew16A.GetIcon(28, fmProp.Icon);
    ImListNew16A.GetIcon(0, fmNew.Icon);
    ImListNew16A.GetIcon(29, fmGen.Icon);
    ImListNew16A.GetIcon(7, fmImportC.Icon);
    ImListNew16A.GetIcon(27, fmPreview.Icon);
    ImListNew16A.GetIcon(64, fmMap.Icon);
    ImListNew32A.GetBitmap(26, imFindIcon.Picture.Bitmap);

    Font.Height := 0; // set default size as reference
    Font.Height := Round(Canvas.GetTextHeight('0') * 101 / 100);

    // adjust font height for all forms
    for i := 0 to Screen.FormCount - 1 do
      begin
      Screen.Forms[i].Font.Height := Font.Height;

      // adjust font for some components with custom font
      with Screen.Forms[i] do
        for w := 0 to ComponentCount - 1 do
          if (Components[w].ClassName = TLabel.ClassName)
            or (Components[w].ClassName = TComboBox.ClassName) then
            TControl(Components[w]).Font.Height := Font.Height;
      end;

    Screen.HintFont.Height := Font.Height;
    Screen.MenuFont.Height := Font.Height;

    edFind.Constraints.MaxWidth := 2 * Font.Height;

    SetStatusBarPanelWidth(1, 'NORMAL');
    SetStatusBarPanelWidth(2, 'X,Y: 0000, 0000 ');
    SetStatusBarPanelWidth(3, '0.0.0.00000');

    // allow adjusting components with autosize option
    EndFormUpdate;

    BeginFormUpdate;

    h := Canvas.GetTextHeight('0');
    stStatusBar.Height := h + 2;

    // set size of toolbar's buttons which depends on size of icons
    h     := Max(16, round(imSVGList.RenderSize * 1.5));
    for w := 0 to ComponentCount - 1 do
      if (Components[w].ClassName = TToolBar.ClassName) then
        SetToolbarButtonSize([TToolBar(Components[w])], h);

    RenderSVGIcons(imSVGList.RenderSize * 2, ImListNew32A, nil);

    EndFormUpdate;

    // set form size limits
    Constraints.MinWidth           := tbFile.Width + tbTools.Width + 120;
    Constraints.MinHeight          := max(pCharTools.Height, pFontTools.Height) + tbFile.Height * 4 + stStatusBar.Height;
    fmPreview.Constraints.MinWidth := Constraints.MinWidth;
  end;

// adjust colors and some other values according to theme
procedure TfmMain.AdjustThemeDependentValues;

  procedure SetFont(AFont: Graphics.TFont; AIndex, ASize: Integer; AColor: TColor);
    begin
      AFont.Name  := Screen.Fonts[AIndex];
      AFont.Size  := ASize;
      AFont.Color := AColor;
    end;
  begin
    if appTunerEx.IsDarkTheme then
      begin                        // dark theme, if available

      imSVGList.List.Text := imSVGList.List.Text
        .Replace('#000', '#49d095')
        .Replace('stroke-width="1.7"', 'stroke-width="1.0"');

      sgNavigator.AlternateColor := $222222;
      sgNavigator.GridLineColor  := $555555;
      end
    else
      begin                        // light theme, default

      pFontToolsBox.Color := cl3DLight;
      pCharToolsBox.Color := cl3DLight;
      end;
  end;

// перевод интерфейса
procedure TfmMain.LanguageChange;
  begin
    appLocalizerEx.CurrentLanguage := cfg.app.lang;
    appTunerEx.TuneComboboxes      := True;

    fmAbout.UpdateInfo; // обновляем инфо на новом языке
  end;


 { ***  App config  *** }

// configure app settings
procedure TfmMain.InitConfig;
  var
    i: Integer;
  begin
    Settings.Add(acViewTBCharTools, @cfg.toolbar.chars);
    Settings.Add(acViewTBFontTools, @cfg.toolbar.fonts);
    Settings.Add(acViewTBFile, @cfg.toolbar.files);
    Settings.Add(acViewTBTools, @cfg.toolbar.tools);

    Settings.Add(psSplit, @cfg.app.splitter);
    Settings.Add(acGridToggle, @cfg.grid.enable);
    Settings.Add(acFontShiftRollover, @cfg.app.rollover);

    Settings.Add('_cfg.gen.fontsize', stInt, @cfg.gen.fontsize, '17');
    Settings.Add('_cfg.prev.enable', stBool, @cfg.prev.enable, '0');

    for i := 0 to LAST_FILES_LIST_SIZE - 1 do
      Settings.Add(Format('_cfg.app.lastfiles[%d]', [i]), stString, @cfg.app.lastfiles[i]);
  end;

// load some config variables
procedure TfmMain.AfterLoadConfig;
  var
    i: Integer;
  begin
    fmGen.snEdit.Font.Height := cfg.gen.fontsize;
    if cfg.prev.enable then acFontPreview.Execute;

    for i := LAST_FILES_LIST_SIZE - 1 downto 0 do
      FOpenFileList.FilePath[0] := cfg.app.lastfiles[i];
    LastFileAdd(''); // обновление списка последних открытых файлов
  end;

// save some config variables
procedure TfmMain.BeforeSaveConfig;
  var
    i: Integer;
  begin
    cfg.gen.fontsize := fmGen.snEdit.Font.Height;
    cfg.prev.enable  := fmPreview.Visible;

    for i := 0 to LAST_FILES_LIST_SIZE - 1 do
      cfg.app.lastfiles[i] := FOpenFileList.FilePath[i];

    fmMap.SaveConfig;
  end;


end.
