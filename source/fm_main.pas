unit fm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, ExtCtrls, Controls, StdCtrls, Grids, ComCtrls, ActnList,
  Windows, Graphics, Menus, StdActns, Dialogs, Spin, SysUtils,
  LazUTF8, Types, strutils, LCLIntf, LCLTranslator, PairSplitter, LazFileUtils,
  LCLType, ImageSVGList, AppTuner, AppLocalizer, AppSettings, config_record,

  // forms
  fm_gen, fm_new, fm_prop, fm_confirm, fm_import, fm_preview, fm_sizes,
  fm_optimize, fm_range, fm_about, fm_settings, fm_importc, fm_map, fm_rbf,

  // functional units
  font, symbol, cOpenFileList, appAbout, u_sticking, u_map_render, u_rbf,

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
    FGridStep:          Integer;

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
    procedure LoadProjectAtStartup;
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
    InitConfig;

    stStatusBar.Panels.Items[3].Text := GetAppVersion;

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

    // init sticking forms
    with StickingFormsEx do
      begin
      MainForm := Self;

      // forms added in default order
      Add(fmGen, FM_GEN_CAPTION);
      Add(fmPreview, FM_PREV_EXAMPLE);
      Add(fmMap, FM_MAP_CAPTION);

      Form[fmMap].Init(akLeft, True, True);
      Form[fmGen].Init(akRight, True, True);
      Form[fmPreview].Init(akBottom, True, True);
      end;

    with appTunerEx do
      begin
      IniFile := Settings.IniFile;

      AddAllForms;
      Form[Self].SaveProps := True; // save/restore props only for main form
      LoadProperties;

      // load font scale value into control
      fmSettings.seFontScale.Value := Form[Self].Scale;
      end;

    Settings.SyncValues;     // load default values to fields of 'cfg' record
    Settings.Load;           // load settings from ini file to 'cfg' record
    Settings.SyncComponents; // load controls from fields of 'cfg' record

    // load property values to controls
    acStayOnTopToggle.Checked := appTunerEx.Form[Self].StayOnTop;

    LoadProjectAtStartup;
    acZoomFit.Execute;
    tmrMain10msTimer(Sender);
    actionPasteMode(acPasteModeNorm);
    sgNavigatorSelection(Sender, 0, sgNavigator.FixedRows);
    AfterLoadConfig;

    psSplit.Cursor      := crHSplit; // fix splitter cursor bug
    fmMap.OnMouseEvent  := @OnMapSelectChar;
    tmrMain10ms.Enabled := True;

    // enable custom menu drawing
    appTunerEx.Form[Self].MenuShow := True;

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
    if FileExtCheck(FileNames[0], Format('%s,%s', [FILE_EXTENSION, RBF_EXTENSION])) then
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
  begin
    // handle sticking of forms to main form
    if cfg.sticking.enable then
      with StickingFormsEx do
        begin
        UseBorderCorrection := cfg.sticking.correct;

        case WindowState of
          wsNormal: Stick;
          wsMaximized: StickToRightBottomEdge;
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

    // some useful info
    stStatusBar.Panels[0].Text := Format('Window: %d x %d', [Width, Height]);
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
      acSymbolPaste.Enabled := mxFont.Item[0].CanPaste;
      acFontPaste.Enabled   := mxFont.Item[0].CanPaste;
      end;

    // управление видимостью панелей кнопок
    tbFile.Visible     := acViewTBFile.Checked;
    tbTools.Visible    := acViewTBTools.Checked;
    pCharTools.Visible := acViewTBCharTools.Checked;
    pFontTools.Visible := acViewTBFontTools.Checked;

    sgNavigator.Visible := acViewNavigator.Checked;
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
      bm_tmp.Width  := mxFont.Item[0].Width;
      bm_tmp.Height := mxFont.Item[0].Height;

      with sgNavigator do
        begin
        Columns.Items[ColCount - 1].Width :=
          DefaultRowHeight * mxFont.Width div mxFont.Height;

        if (aRow >= TopRow) and (aRow - TopRow <= VisibleRowCount) and
          (aCol = ColCount - 1) and (aRow >= FixedRows) then
          begin
          // символ
          Cells[0, aRow] := mxFont.GetCharName(mxFont.FontStartItem + aRow - FixedRows);

          // код символа
          if cfg.nav.code.hex then
            Cells[1, aRow] := IntToHex(mxFont.FontStartItem + aRow - FixedRows, 2) else
            Cells[1, aRow] := IntToStr(mxFont.FontStartItem + aRow - FixedRows);

          // превью символа
          if cfg.nav.invert and (aRow = Row) then
            // выделенная строка в навигаторе
            mxFont.Item[aRow - FixedRows].Draw(bm_tmp, cfg.nav.transparent,
              cfg.color.nav.active, cfg.color.nav.bg)
          else
            // невыделенная строка в навигаторе
            mxFont.Item[aRow - FixedRows].Draw(bm_tmp, cfg.nav.transparent,
              cfg.color.nav.bg, cfg.color.nav.active);
          Canvas.StretchDraw(aRect, bm_tmp);
          end;
        end;
      finally
      FreeAndNil(bm_tmp);
      end;
  end;

// обновление изображения при смене символа (перемещении в навигаторе)
procedure TfmMain.sgNavigatorSelection(Sender: TObject; aCol, aRow: Integer);
  var
    item: TMatrixChar;
  begin
    item                 := mxFont.Item[sgNavigator.Row - sgNavigator.FixedRows];
    acSymbolUndo.Enabled := item.CanUndo;
    acSymbolRedo.Enabled := item.CanRedo;
    ReDrawImage;

    aRow := mxFont.FontStartItem + sgNavigator.Row - sgNavigator.FixedRows;

    lbEditor.Caption := Format('%s  <%s>  DEC = %d;  HEX = %x', [
      TXT_SYMBOL, mxFont.GetCharName(aRow), aRow, aRow]);
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
  const
    prevPoint: DWord = MAXDWORD;
  var
    i:         Integer;
    item:      TMatrixChar;
    pixAction: TPixelAction = TPixelAction.paNone;
  begin
    case Button of
      mbLeft: pixAction   := TPixelAction.paSet;
      mbRight: pixAction  := TPixelAction.paClear;
      mbMiddle: pixAction := TPixelAction.paInvert;
      end;

    if pixAction <> TPixelAction.paNone then
      begin
      X := X div FGridStep;
      Y := Y div FGridStep;

      if prevPoint = (X shl 16) or (Y and $FFFF) then Exit
      else prevPoint := (X shl 16) or (Y and $FFFF);

      item := mxFont.Item[sgNavigator.Row - sgNavigator.FixedRows];

      if ssCtrl in Shift then
        for i := 0 to mxFont.Width - 1 do
          item.PixelAction(i, Y, pixAction);

      if ssShift in Shift then
        for i := 0 to mxFont.Height - 1 do
          item.PixelAction(X, i, pixAction);

      if not ((ssCtrl in Shift) xor (ssShift in Shift)) then
        item.PixelAction(X, Y, pixAction);

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

      if FGridStep > 0 then
        stStatusBar.Panels.Items[2].Text := Format('X,Y: %d, %d', [
          (X - 1) div FGridStep + 1,
          (Y - 1) div FGridStep + 1]);

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
        FileName := AnsiReplaceText(mxFont.Name, ' ', '_');
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
        FileName := AnsiReplaceText(mxFont.Name, ' ', '_');

      InitialDir := ExtractFileDir(FileName) + DirectorySeparator;
      FileName   := ExtractFileNameOnly(FileName);
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
        mxFont.Import(dlgFont.Font, seW.Value, seH.Value);

        if cbSnapLeft.Checked then acFontSnapLeft.Execute else acFontCenterH.Execute;
        if cbOptimize.Checked then mxFont.ChangeSize(-1, -1, -1, -1, True);

        FontCreateFinish;
        acSaveAs.Dialog.FileName := '';
        end;
      end;
  end;

// действие: импорт шрифта из кода C
procedure TfmMain.acFontImportCCodeExecute(Sender: TObject);
  begin
    if not GetConfirmation then Exit;

    with fmImportC do
      begin
      FontImp.Encoding := mxFont.Encoding;

      if ShowModal = mrOk then
        begin
        dlgOpen.FileName := '';

        FontCreateNew(
          seImpWidth.Value, seImpHeight.Value,
          seImpStartItem.Value, seImpLastItem.Value - seImpStartItem.Value + 1,
          cfg.new.enc, cfg.new.title, cfg.new.author);
        UpdateFont(mxFont);
        FontCreateFinish;
        end;
      end;
  end;

// действие: просмотр и изменение свойств шрифта
procedure TfmMain.acFontPropertiesExecute(Sender: TObject);
  begin
    with fmProp, mxFont do
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

    fmPreview.PFontCustom := @mxFont;
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
  begin
    if Sender <> nil then
      case TAction(Sender).Name of

        'acZoomFit': // масштаб символа "вписанный в видимую область"
          FGridStep := min(
            scbEditor.Width div mxFont.Width,
            scbEditor.Height div mxFont.Height);

        'acZoomIn':  // увеличение масштаба
          FGridStep := min(150, round((FGridStep + 1) * 1.1));

        'acZoomOut': // уменьшение масштаба
          FGridStep := max(1, round((FGridStep - 1) / 1.1));
        end;

    with shBackground.Constraints do
      begin
      MinWidth  := FGridStep * mxFont.Width;
      MaxWidth  := FGridStep * mxFont.Width;
      MinHeight := FGridStep * mxFont.Height;
      MaxHeight := FGridStep * mxFont.Height;
      end;

    ReDrawImage;
  end;

// действия с символом: отмена/повтор
procedure TfmMain.actionSymbolHistory(Sender: TObject);
  var
    item: TMatrixChar;
  begin
    item := mxFont.Item[sgNavigator.Row - sgNavigator.FixedRows];

    case TAction(Sender).Name of

      'acSymbolUndo':
        item.UndoChange;

      'acSymbolRedo':
        item.RedoChange;

      'acFontUndo':
        mxFont.UndoChange;

      'acFontRedo':
        mxFont.RedoChange;
      end;

    acSymbolUndo.Enabled := item.CanUndo;
    acSymbolRedo.Enabled := item.CanRedo;
    ReDrawImage;
    ReDrawContent;
    fmGen.OnChangeParameter(nil);
  end;

// действия с символом: поиск
procedure TfmMain.actionSymbolFind(Sender: TObject);
  begin
    if TAction(Sender).Name = 'acSymbolFind' then
      begin
      // показать/скрыть панель поиска символа в таблице
      pFind.Visible := acSymbolFind.Checked;
      seFind.Value  := sgNavigator.Row + mxFont.FontStartItem - sgNavigator.FixedRows;
      edFind.Text   := EncodingToUTF8(Char(seFind.Value), mxFont.Encoding);
      end
    else
      begin

      // поиск и выделение найденного символа по коду
      if TSpinEdit(Sender).Name = 'seFind' then
        if (seFind.Value >= mxFont.FontStartItem) and
          (seFind.Value < mxFont.FontStartItem + mxFont.FontLength) then
          edFind.Text := EncodingToUTF8(Char(seFind.Value), mxFont.Encoding);

      // поиск и выделение найденного символа по названию
      if TEdit(Sender).Name = 'edFind' then
        if edFind.Text <> '' then
          try
          seFind.Value := Ord(UTF8ToEncoding(edFind.Text, mxFont.Encoding)[1]);
          except
          seFind.Value := mxFont.FontStartItem;
          end;

      sgNavigator.Row := seFind.Value - mxFont.FontStartItem + sgNavigator.FixedRows;
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
    item: TMatrixChar;
    meta: String;
  begin
    item := mxFont.Item[sgNavigator.Row - sgNavigator.FixedRows];

    case TAction(Sender).Name of

      'acSymbolCopy':       // действие: копирование символа в буфер обмена
        begin
        item.ClipboardAction(TClipboardAction.cbCopy);
        Exit;
        end;

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
          if (FileName <> '') or Execute then
            repeat
              if not IsImageContainFontSet(FileName, meta) then
                begin
                item.Import(FileName, FPasteMode, cfg.import.bwlevel);
                FileName := '';
                Break;
                end;

              if (fmConfirm.Show(TXT_WARNING, WARN_IMPORT, mbYesNo, Self) = mrYes)
                and GetConfirmation
                and ImportFontFromPNG(FileName, meta, mxFont) then
                FontCreateFinish;

              FileName := '';
              Exit;
            until True
          else Exit;


      'acSymbolMirrorHorz': // действие: отображение символа горизонтально
        item.Mirror(TMirror.mrHorizontal);

      'acSymbolMirrorVert': // действие: отображение символа вертикально
        item.Mirror(TMirror.mrVertical);


      'acSymbolShiftDown':  // действие: сдвиг символа вниз
        item.Shift(dirDown, acFontShiftRollover.Checked);

      'acSymbolShiftLeft':  // действие: сдвиг символа влево
        item.Shift(dirLeft, acFontShiftRollover.Checked);

      'acSymbolShiftRight': // действие: сдвиг символа вправо
        item.Shift(dirRight, acFontShiftRollover.Checked);

      'acSymbolShiftUp':    // действие: сдвиг символа вверх
        item.Shift(dirUp, acFontShiftRollover.Checked);


      'acSymbolSnapDown':   // действие: прижатие символа вниз
        item.Snap(dirDown);

      'acSymbolSnapLeft':   // действие: прижатие символа влево
        item.Snap(dirLeft);

      'acSymbolSnapRight':  // действие: прижатие символа вправо
        item.Snap(dirRight);

      'acSymbolSnapUp':     // действие: прижатие символа вверх
        item.Snap(dirUp);


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
        mxFont.Clear;

      'acFontInvert':     // действие: инверсия символов шрифта
        mxFont.Invert;

      'acFontMirrorHorz': // действие: отображение горизонтально символов шрифта
        mxFont.Mirror(TMirror.mrHorizontal);

      'acFontMirrorVert': // действие: отображение вертикально символов шрифта
        mxFont.Mirror(TMirror.mrVertical);


      'acFontShiftDown':  // действие: сдвиг вниз символов шрифта
        mxFont.Shift(dirDown, acFontShiftRollover.Checked);

      'acFontShiftLeft':  // действие: сдвиг влево символов шрифта
        mxFont.Shift(dirLeft, acFontShiftRollover.Checked);

      'acFontShiftRight': // действие: сдвиг вправо символов шрифта
        mxFont.Shift(dirRight, acFontShiftRollover.Checked);

      'acFontShiftUp':    // действие: сдвиг вверх символов шрифта
        mxFont.Shift(dirUp, acFontShiftRollover.Checked);


      'acFontSnapDown':   // действие: прижатие вниз символов шрифта
        mxFont.Snap(dirDown);

      'acFontSnapLeft':   // действие: прижатие влево символов шрифта
        mxFont.Snap(dirLeft);

      'acFontSnapRight':  // действие: прижатие вправо символов шрифта
        mxFont.Snap(dirRight);

      'acFontSnapUp':     // действие: прижатие вверх символов шрифта
        mxFont.Snap(dirUp);


      'acFontCenterH':    // действие: центрирование символов шрифта горизонтально
        mxFont.Center(False);

      'acFontCenterV':    // действие: центрирование символов шрифта вертикально
        mxFont.Center(True);


      'acFontRotateCW':   // действие: поворот символов шрифта по ч.с.
        mxFont.Rotate(True);

      'acFontRotateCCW':  // действие: поворот символов шрифта против ч.с.
        mxFont.Rotate(False);


      'acFontPaste':      // действие: пакетная вставка
        mxFont.Paste(FPasteMode);


      'acSymbolMoveUp':   // действие: переместить символ вверх
        if mxFont.SwapChars(curr, curr - 1) then
          sgNavigator.Row := sgNavigator.FixedRows + curr - 1;

      'acSymbolMoveDown': // действие: переместить символ вниз
        if mxFont.SwapChars(curr, curr + 1) then
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
        fmMap.FontX := mxFont;
        fmMap.Show;
        FormWindowStateChange(Sender);
        end;

      'acMapExport': // export map to image file selected in dialog
        fmMap.actionExport(nil);

      'acSetting': // действие: открыть окно "настройки приложения"
        begin
        Settings.SyncValues; // load current values to fields of 'cfg' record
        if fmSettings.ShowModal = mrOk then
          begin
          SettingsApplyToCurrentSession;
          ReDrawImage;
          end;
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
        OpenURL(APP_URL_HELP);

      'acWebsite': // действие: домашняя страница
        OpenURL(APP_URL_HOME);

      'acOpenRepo':// действие: репозиторий проекта
        OpenURL(APP_URL_REPO);

      'acInfo':    // действие: информация о программе
        fmAbout.Show;

      'acAppExit': // действие: выход из приложения
        fmMain.Close;

      'acStayOnTopToggle': // действие: поверх всех окон
        appTunerEx.Form[Self].StayOnTop := acStayOnTopToggle.Checked;

      'acViewTBFontTools', 'acViewNavigator':
        begin
        tmrMain10msTimer(Sender);
        FileStatusUpdate;
        end;

      'acGridToggle':
        imBackground.Visible := acGridToggle.Checked;
      end;
  end;

procedure TfmMain.edFindUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  begin
    edFind.Text := '';
  end;


// действие: изменение диапазона символов шрифта
procedure TfmMain.acFontCharsetExecute(Sender: TObject);
  begin
    with fmRange, mxFont do
      begin
      seStart.Value := FontStartItem;
      seEnd.Value   := FontLength + FontStartItem - 1;

      if ShowModal = mrOk then
        begin
        SetRange(seStart.Value, seEnd.Value);

        sgNavigator.RowCount := mxFont.FontLength + sgNavigator.FixedRows;
        FontActionExecute;
        fmGen.acResetRange.Execute;
        end;
      end;
  end;

// действие: оптимизация размеров холста символов шрифта
procedure TfmMain.acFontOptimizeExecute(Sender: TObject);
  begin
    with fmOptimize, mxFont do
      begin
      opt_oldHeight := Height;
      opt_oldWidth  := Width;
      opt_up        := CanOptimize(dirUp);
      opt_down      := CanOptimize(dirDown);
      opt_left      := CanOptimize(dirLeft);
      opt_right     := CanOptimize(dirRight);

      if not ((opt_oldHeight - opt_up - opt_down > 0) and
        (opt_oldWidth - opt_left - opt_right > 0)) then
        fmConfirm.Show(TXT_WARNING, WARN_OPTIMIZE, [mbCancel], Self)
      else
      if ShowModal = mrOk then
        begin
        ChangeSize(res_up, res_down, res_left, res_right, True);

        SettingsApplyToCurrentSession;
        FontActionExecute;
        end;
      end;
  end;

// действие: изменение размеров холста символов шрифта
procedure TfmMain.acFontChangeSizesExecute(Sender: TObject);
  begin
    with fmSizes do
      begin
      oldHeight := mxFont.Height;
      oldWidth  := mxFont.Width;

      if fmSizes.ShowModal = mrOk then
        begin
        mxFont.ChangeSize(
          seUp.Value, seDown.Value,
          seLeft.Value, seRight.Value,
          rgMode.ItemIndex = 1);

        SettingsApplyToCurrentSession;
        FontActionExecute;
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
        begin
        ToolBtnLastFiles.DropdownMenu := pmLastFiles;
        ToolBtnLastFiles.Style        := tbsDropDown;

        appTunerEx.Form[Self].MenuShow := True;
        end
      else
        begin
        ToolBtnLastFiles.DropdownMenu := nil;
        ToolBtnLastFiles.Style        := tbsButton;
        end;
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
    AdjustThemeDependentValues;
    AdjustComponentSizes;

    if mxFont <> nil then
      try
      shBackground.Brush.Color := cfg.color.editor.bg;
      DrawChessBackground(imBackground.Picture.Bitmap,
        mxFont.Width, mxFont.Height,
        cfg.color.editor.bg, cfg.color.editor.grid);

      with sgNavigator.Columns do
        begin
        if cfg.nav.char.font < 0 then cfg.nav.char.font := 0;
        if cfg.nav.code.font < 0 then cfg.nav.code.font := 0;

        Items[0].Visible := cfg.nav.char.enable;
        Items[1].Visible := cfg.nav.code.enable;

        Items[0].Font.Name := Screen.Fonts[cfg.nav.char.font];
        Items[0].Font.Size := cfg.nav.char.fontsize;
        Items[1].Font.Name := Screen.Fonts[cfg.nav.code.font];
        Items[1].Font.Size := cfg.nav.code.fontsize;

        Items[0].Font.Color := cfg.color.nav.txt;
        Items[1].Font.Color := cfg.color.nav.txt;
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

    LanguageChange;
    actionService(acGridToggle);
    actionZooming(nil);
    ReDrawContent;
    ReDrawImage;

    EndFormUpdate;
  end;

// загрузка файла шрифта
procedure TfmMain.FontLoadFromFile(AFileName: String);
  var
    tmp:        TMatrixFont;
    isReadable: Boolean;
  begin
    if not GetConfirmation then Exit;
    if not FileExists(AFileName) then Exit;

    // load from original matrixFont font file
    if FileExtCheck(AFileName, FILE_EXTENSION) then
      begin
      tmp        := TMatrixFont.Create;
      isReadable := tmp.ReadFromFile(AFileName);
      FreeAndNil(tmp);

      // если файл поврежден - предупреждаем, выходим
      if not isReadable then
        begin
        fmConfirm.Show(TXT_ERROR, WARN_LOAD, [mbYes], self);
        Exit;
        end;

      // загружаем файл, если он не поврежден
      with mxFont do
        begin
        FreeAndNil(mxFont);
        mxFont := TMatrixFont.Create;
        ReadFromFile(AFileName);

        acSymbolRedo.Enabled     := False;
        acSymbolUndo.Enabled     := False;
        AppAdditional            := GetAppCompanyName;
        AppCurrent               := GetAppNameVersion;
        dlgOpen.FileName         := AFileName;
        acSaveAs.Dialog.FileName := AFileName;

        LastFileAdd(AFileName);
        end;

      FontCreateFinish;
      end;

    // import font from RBF file
    if FileExtCheck(AFileName, RBF_EXTENSION) then
      with rbfConverter do
        begin
        FontCreateNew(1, 1, 0, 1, 0, '', '');
        AssignRHF(mxFont);
        LoadFromFile(AFileName);
        LastFileAdd(AFileName);
        FontCreateFinish;
        end;
  end;

// создание нового шрифта
procedure TfmMain.FontCreateNew(w, h, si, l, e: Integer; n, a: String);
  begin
      try
      with mxFont do
        begin
        FreeAndNil(mxFont);
        mxFont               := TMatrixFont.Create;
        acSymbolRedo.Enabled := False;
        acSymbolUndo.Enabled := False;

        Name          := n;
        Author        := a;
        AppCreate     := GetAppNameVersion;
        AppCurrent    := AppCreate;
        AppAdditional := GetAppCompanyName;
        Encoding      := GetEncodingByIndex(e);

        SetSize(w, h);
        FontStartItem := si;
        FontLength    := l;
        Clear;
        end;

      FontCreateFinish;
      dlgOpen.FileName         := '';
      acSaveAs.Dialog.FileName := '';
      except
      if fmConfirm.Show(TXT_ERROR, WARN_CREATE, mbYesNo, self) = mrYes then
        Close;
      end;
  end;

// сохранение шрифта
procedure TfmMain.FontSave(AFileName: String);
  begin
    // save to original matrixFont font file
    if FileExtCheck(AFileName, FILE_EXTENSION) then
      begin
      mxFont.SaveToFile(AFileName);
      LastFileAdd(AFileName);
      dlgOpen.FileName := AFileName;
      file_changed     := False;
      end;

    // export font to RBF file
    if FileExtCheck(AFileName, RBF_EXTENSION) then
      with rbfConverter do
        begin
        AssignRHF(mxFont);
        if fmRbf.ShowModal = mrOk then
          SaveToFile(AFileName);
        end;
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
    if (Button = mbLeft) and (cfg.map.sclick xor (Shift = [ssDouble])) then
      sgNavigator.Row := fmMap.SelectedIndex + sgNavigator.FixedRows;
  end;


 { ***  Обновление состояния компонентов  *** }

 // обновление информации о состоянии файла
procedure TfmMain.FileStatusUpdate;
  var
    s: String;
  begin
    BeginFormUpdate;

    s := mxFont.Name;
    if dlgOpen.FileName <> '' then s += ' [' + ExtractFileName(dlgOpen.FileName) + ']';
    if file_changed then s += ' (' + TXT_CHANGED + ')';

    fmMain.Caption    := GetAppName + ' - ' + s;
    Application.Title := s;

    with mxFont do
      miFontInfo.Caption := UpperCase(Encoding) + '   '
        + Width.ToString + ' x ' + Height.ToString;

    // visibility of controls in navigator on the left side
    with mxFont, psSide1 do
      if acViewNavigator.Checked then
        begin
        lbNavigator.Caption  := Format(' %s: %d - %d ', [TXT_NAVIGATOR, FontStartItem, FontStartItem + FontLength - 1]);
        Constraints.MaxWidth := 0;
        Constraints.MinWidth := pFontToolsBox.Width + sgNavigator.Constraints.MinWidth + 16;
        end
      else
        begin
        lbNavigator.Caption  := ' ';
        Constraints.MaxWidth := acViewTBFontTools.Checked.Select(pFontToolsBox.Width, 1);
        Constraints.MinWidth := pFontToolsBox.Width;
        end;

    EndFormUpdate;
    fmGen.OnChangeParameter(nil);
  end;

// действия после применения эффекта ко всем символам шрифта
procedure TfmMain.FontActionExecute;
  begin
    ReDrawImage;
    ReDrawContent;
    acSymbolUndo.Enabled := mxFont.Item[sgNavigator.Row - sgNavigator.FixedRows].CanUndo;
    acSymbolRedo.Enabled := mxFont.Item[sgNavigator.Row - sgNavigator.FixedRows].CanRedo;
    file_changed         := True;
    FileStatusUpdate();
  end;

// завершение создания шрифта
procedure TfmMain.FontCreateFinish;
  begin
    mxFont.ClearChanges;

    sgNavigator.RowCount := mxFont.FontLength + sgNavigator.FixedRows;

    acZoomFit.Execute;
    FontActionExecute;

    file_changed := False;
    FileStatusUpdate;
    SettingsApplyToCurrentSession;
  end;

// действия после применения изменений к символу
procedure TfmMain.ReDrawAfterAction;
  begin
    mxFont.Item[sgNavigator.Row - sgNavigator.FixedRows].SaveChange;
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
    index := sgNavigator.Row - sgNavigator.FixedRows;

    mxFont.Item[index].Draw(imEditor.Picture.Bitmap, True,
      cfg.color.editor.bg, cfg.color.editor.active);
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

        // set sizes of panels-spacers
        for b := 0 to item.ControlCount - 1 do
          if item.Controls[b].ClassName = 'TPanel' then
            with TPanel(item.Controls[b]) do
              begin
              Constraints.MinHeight := item.ButtonHeight;
              Constraints.MinWidth  := item.ButtonWidth;
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

  const
    firstCall: Boolean = True;
  var
    w, h, i: Integer;
  begin
    BeginFormUpdate;

    // on 96dpi's screen at 100% resolution muat be 16px
    RenderSVGIcons(Round(Scale96ToScreen(16) * cfg.app.iconscale / 100), ImListNew16A, ImListNew16D);
    RenderSVGIcons(Round(Scale96ToScreen(16) * cfg.app.iconscale / 50), ImListNew32A, nil);

    ImListNew32A.GetBitmap(26, imFindIcon.Picture.Bitmap);

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

    SetStatusBarPanelWidth(1, 'NORMAL');
    SetStatusBarPanelWidth(2, 'X,Y: 0000, 0000 ');
    SetStatusBarPanelWidth(3, '0.0.0.00000');

    // allow adjusting components with autosize option
    EndFormUpdate;

    BeginFormUpdate;

    h := Canvas.GetTextHeight('0');

    stStatusBar.Height          := h + 2;
    Screen.HintFont.Height      := h;
    Screen.MenuFont.Height      := h;
    edFind.Constraints.MaxWidth := h * 2;

    // set size of toolbar's buttons which depends on size of icons
    h     := Max(16, round(ImListNew16A.Width * 1.5));
    for w := 0 to ComponentCount - 1 do
      if (Components[w].ClassName = TToolBar.ClassName) then
        SetToolbarButtonSize([TToolBar(Components[w])], h);

    // custom menu drawing setup
    appTunerEx.Form[Self].MenuAddHeight := Scale96ToScreen(2);
    appTunerEx.Form[Self].MenuTune      := True;

    // scale font for all forms
    appTunerEx.Scale := fmSettings.seFontScale.Value;

    EndFormUpdate;

    // set form size limits
    if firstCall then
      begin
      Constraints.MinWidth  := tbFile.Width + tbTools.Width + 120;
      Constraints.MinHeight := max(pCharTools.Height, pFontTools.Height) + tbFile.Height * 4 + stStatusBar.Height;
      end;

    fmPreview.Constraints.MinWidth := Constraints.MinWidth;

    firstCall := False;
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
      begin
      cfg.color := cfg.colord; // load dark colorset for char drawing  

      imSVGList.List.Text := imSVGList.List.Text
        .Replace('#000', '#49d095')
        .Replace('stroke-width="1.7"', 'stroke-width="1.0"');

      sgNavigator.AlternateColor := $222222;
      sgNavigator.GridLineColor  := $555555;
      end
    else
      begin
      cfg.color := cfg.colorl; // load light colorset for char drawing   

      pFontToolsBox.Color := cl3DLight;
      pCharToolsBox.Color := cl3DLight;
      end;
  end;

// перевод интерфейса
procedure TfmMain.LanguageChange;
  begin
    appLocalizerEx.CurrentLanguage := cfg.app.lang;
    appTunerEx.TuneComboboxes      := True;
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
    Settings.Add(acViewNavigator, @cfg.toolbar.nav);

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
    StickingFormsEx.Config   := cfg.sticking.config;
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
    cfg.sticking.config := StickingFormsEx.Config;

    cfg.gen.fontsize := fmGen.snEdit.Font.Height;
    cfg.prev.enable  := fmPreview.Visible;

    for i := 0 to LAST_FILES_LIST_SIZE - 1 do
      cfg.app.lastfiles[i] := FOpenFileList.FilePath[i];

    fmMap.SaveConfig;
  end;

// load existing or create new project at startup
procedure TfmMain.LoadProjectAtStartup;
  var
    _f: String = '';
  begin
    // get drag-n-drop file path
    _f := LazUTF8.ParamStrUTF8(1);

    // check drag-n-drop file
    if not FileExtCheck(_f, FILE_EXTENSION) then
      _f := '';

    // load last opened file if allowed 
    with cfg.app do
      if loadlast and _f.IsEmpty and not lastfiles[0].IsEmpty then
        _f := cfg.app.lastfiles[0];

    with cfg.new do
      if FileExistsUTF8(_f) then
        FontLoadFromFile(_f) else
        FontCreateNew(w, h, start, last - start + 1, enc, title, author);
  end;


end.
