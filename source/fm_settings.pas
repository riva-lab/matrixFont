unit fm_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, Dialogs, ComCtrls, Spin, StdCtrls,
  ExtCtrls, ActnList, Controls, Buttons, CheckLst,
  LazFileUtils, LazUTF8, Math, AppLocalizer, AppTuner, AppSettings,
  fm_gen, fm_importc, fm_preview, fm_map,
  u_encodings, u_sticking, u_utilities, u_strings, config_record;

const
  LANGUAGES_DIR  = 'lang';
  LANGUAGE_FILE  = 'matrixFont';
  LANGUAGES_FILE = 'languages.ini';

type

  { TfmSettings }

  TfmSettings = class(TForm)
    acCancel:           TAction;
    acOK:               TAction;
    alSettings:         TActionList;
    bbApply:            TBitBtn;
    bbCancel:           TBitBtn;
    bbDefaults:         TBitBtn;
    cbCharName:         TCheckBox;
    cbCharNameFont:     TComboBox;
    cbChessGrid:        TCheckBox;
    cbCodeHex:          TCheckBox;
    cbCodeName:         TCheckBox;
    cbCodeNameFont:     TComboBox;
    cbEncoding:         TComboBox;
    cbLanguage:         TComboBox;
    cbNaviInvert:       TCheckBox;
    cbNaviScroll:       TCheckBox;
    cbNaviTransparent:  TCheckBox;
    cbPreviewRefresh:   TCheckBox;
    cbStickingBorder:   TCheckBox;
    cbStickingEnable:   TCheckBox;
    cbStickingSide:     TComboBox;
    cbStickingSize:     TComboBox;
    cbTheme:            TComboBox;
    cbtnActive:         TColorButton;
    cbtnActiveD:        TColorButton;
    cbtnBackground:     TColorButton;
    cbtnBackgroundD:    TColorButton;
    cbtnGrid:           TColorButton;
    cbtnGridD:          TColorButton;
    cbtnImportA:        TColorButton;
    cbtnImportAD:       TColorButton;
    cbtnImportBG:       TColorButton;
    cbtnImportBGD:      TColorButton;
    cbtnNaviA:          TColorButton;
    cbtnNaviAD:         TColorButton;
    cbtnNaviBG:         TColorButton;
    cbtnNaviBGD:        TColorButton;
    cbtnNaviT:          TColorButton;
    cbtnNaviTD:         TColorButton;
    cbtnPreviewA:       TColorButton;
    cbtnPreviewAD:      TColorButton;
    cbtnPreviewBG:      TColorButton;
    cbtnPreviewBGD:     TColorButton;
    clbSticking:        TCheckListBox;
    edAuthor:           TEdit;
    edFontName:         TEdit;
    gbStickingScheme:   TGroupBox;
    lbAuthor:           TLabel;
    lbBWTreshold:       TLabel;
    lbColorA:           TLabel;
    lbColorBG:          TLabel;
    lbColorEditor:      TLabel;
    lbColorGrid:        TLabel;
    lbColorImport:      TLabel;
    lbColorImportA:     TLabel;
    lbColorImportBG:    TLabel;
    lbColorNavi:        TLabel;
    lbColorNaviA:       TLabel;
    lbColorNaviBG:      TLabel;
    lbColorNaviT:       TLabel;
    lbColorPreview:     TLabel;
    lbColorPreviewA:    TLabel;
    lbColorPreviewBG:   TLabel;
    lbColorsD:          TLabel;
    lbColorsL:          TLabel;
    lbColorsTheme:      TLabel;
    lbFontEncoding:     TLabel;
    lbFontName:         TLabel;
    lbFontScale:        TLabel;
    lbGridThickness:    TLabel;
    lbIconsScale:       TLabel;
    lbInterface:        TLabel;
    lbLanguage:         TLabel;
    lbNaviColumns:      TLabel;
    lbNaviHeight:       TLabel;
    lbNaviOptions:      TLabel;
    lbNewDefaults:      TLabel;
    lbNewHeight:        TLabel;
    lbNewItemLast:      TLabel;
    lbNewItemStart:     TLabel;
    lbNewWidth:         TLabel;
    lbStickingSide:     TLabel;
    lbStickingSize:     TLabel;
    lbTheme:            TLabel;
    nbPages:            TNotebook;
    pButtons:           TPanel;
    pControls:          TPanel;
    pcPageCtrl:         TPageControl;
    pNaviColumns:       TPanel;
    pNewDefaults1:      TPanel;
    pNewDefaults2:      TPanel;
    pSpacer1:           TPanel;
    pSpacer10:          TPanel;
    pSpacer11:          TPanel;
    pSpacer12:          TPanel;
    pSpacer13:          TPanel;
    pSpacer14:          TPanel;
    pSpacer2:           TPanel;
    pSpacer3:           TPanel;
    pSpacer4:           TPanel;
    pSpacer5:           TPanel;
    pSpacer6:           TPanel;
    pSpacer7:           TPanel;
    pSpacer8:           TPanel;
    pSpacer9:           TPanel;
    pStickingForms:     TPanel;
    pTitle1:            TPanel;
    pTitle2:            TPanel;
    pTitle3:            TPanel;
    pTitle4:            TPanel;
    pTitle6:            TPanel;
    pTitle7:            TPanel;
    pTitle8:            TPanel;
    pTitle9:            TPanel;
    pValues1:           TPanel;
    pValues2:           TPanel;
    pValues3:           TPanel;
    pValues4:           TPanel;
    seBWTreshold:       TSpinEdit;
    seCharNameFontSize: TSpinEdit;
    seCodeNameFontSize: TSpinEdit;
    seFontScale:        TSpinEdit;
    seGridThickness:    TSpinEdit;
    seIconsScale:       TSpinEdit;
    seNaviHeight:       TSpinEdit;
    seNewHeight:        TSpinEdit;
    seNewItemLast:      TSpinEdit;
    seNewItemStart:     TSpinEdit;
    seNewWidth:         TSpinEdit;
    tsColors:           TTabSheet;
    tsEditor:           TTabSheet;
    tsGeneral:          TTabSheet;
    tsNavigator:        TTabSheet;
    tsNewDefaults:      TTabSheet;
    tsPreview:          TTabSheet;
    tsSticking:         TTabSheet;
    tvTabs:             TTreeView;
    udStickingOrder:    TUpDown;


    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    procedure acOKExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);

    procedure clbStickingSelectionChange(Sender: TObject; User: Boolean);
    procedure cbLanguageChange(Sender: TObject);
    procedure cbStickingCtrlChange(Sender: TObject);
    procedure cbCharNameClick(Sender: TObject);
    procedure tvTabsSelectionChanged(Sender: TObject);
    procedure udStickingOrderChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);

  private
    procedure AdjustComponentsSizes;
    procedure FormAutosize;
    procedure InitConfig;
    procedure InitPages;

    procedure OnLangChange(Sender: TObject);
    procedure StickingListUpdate;
  end;

var
  fmSettings: TfmSettings;


implementation

{$R *.lfm}

{ TfmSettings }

procedure TfmSettings.FormCreate(Sender: TObject);
  var
    i: Integer;
    a: TComboBox;
    s: String;
    c: TControl;
  begin
    for a in [cbCharNameFont, cbCodeNameFont] do
      begin
      s       := a.Text;
      a.Items := Screen.Fonts;
      a.Text  := '';
      a.Text  := s;
      end;

    appLocalizerEx.OnLanguageChange := @OnLangChange;
    appLocalizerEx.Load(
      Format('%0:s%1:s%0:s%2:s', [DirectorySeparator, LANGUAGES_DIR, LANGUAGES_FILE]),
      Format('%0:s%1:s%0:s%2:s', [DirectorySeparator, LANGUAGES_DIR, LANGUAGE_FILE]));

    // theme selector
    cbTheme.Items.AddStrings(CAppTheme);
    cbTheme.ItemIndex := Integer(appTunerEx.Theme);
    cbTheme.Enabled   := appTunerEx.IsDarkThemeAvailable;

    cbLanguage.Items.SetStrings(appLocalizerEx.Languages);
    cbLanguage.ItemIndex := 0;

    Settings.IniFile := ExtractFilePath(ParamStrUTF8(0)) + SETTINGS_FILE;
    InitConfig;

    EncodingsListAssign(cbEncoding.Items);
    cbEncoding.ItemIndex := 0;

    // add chapter titles to treeview
    for i := 0 to pcPageCtrl.PageCount - 1 do
      tvTabs.Items.Add(TTreeNode.Create(nil), pcPageCtrl.Page[i].Caption);

    // disable dark theme color buttons if dark theme is not available
    if not appTunerEx.IsDarkThemeAvailable then
      begin
      for c in [pTitle2, pTitle4, pTitle3, pSpacer3, pSpacer9, pSpacer11, pSpacer13,
          cbtnActiveD, cbtnBackgroundD, cbtnGridD, cbtnImportAD, cbtnImportBGD,
          cbtnNaviAD, cbtnNaviBGD, cbtnNaviTD, cbtnPreviewAD, cbtnPreviewBGD] do
        c.Visible := False;

      for i := 0 to tsColors.ControlCount - 1 do
        if tsColors.Controls[i].ClassName = 'TLabel' then
          TLabel(tsColors.Controls[i]).Alignment := taLeftJustify;

      tsColors.ChildSizing.ControlsPerLine := 2;
      end;

    InitPages;
  end;

procedure TfmSettings.FormShow(Sender: TObject);
  begin
    // execute this block only once
    if Tag = 0 then
      begin
      Tag := 1;

      Settings.SyncComponents;
      cbCharNameClick(nil);
      cbLanguageChange(Sender);
      end;

    with clbSticking do
      begin
      ItemHeight            := Canvas.TextHeight('0') * 13 div 10;
      Constraints.MinHeight := Count * ItemHeight;
      Constraints.MaxHeight := Constraints.MinHeight;
      end;

    udStickingOrder.Width := cbStickingSide.Height * 13 div 10;
    cbStickingCtrlChange(Sender);

    Position := poDefault;
    FormAutosize;
    Position := poMainFormCenter;
  end;

procedure TfmSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  begin
    if ModalResult = mrCancel then
      acCancel.Execute;
  end;

procedure TfmSettings.FormAutosize;
  var
    tmp, i: Integer;
  begin
    AdjustComponentsSizes;

    tmp := nbPages.PageIndex;

    for i := 0 to nbPages.PageCount - 1 do
      begin
      nbPages.PageIndex := i;
      AutoSize          := True;

      Constraints.MinWidth  := Width;
      Constraints.MinHeight := Height;

      AutoSize := False;
      end;

    nbPages.PageIndex := tmp;
  end;



procedure TfmSettings.AdjustComponentsSizes;
  var
    i: Integer;
    w: Integer = 0;
  begin
    // get tree view min width
    for i := 0 to tvTabs.Items.Count - 1 do
      w := Max(w, Canvas.GetTextWidth(tvTabs.Items.Item[i].Text));

    // set tree view min sizes
    tvTabs.Constraints.MinWidth  := w + tvTabs.Indent * 2 + VertScrollBar.Size;
    tvTabs.Constraints.MinHeight := tvTabs.Items.Count * tvTabs.DefaultItemHeight;
  end;

procedure TfmSettings.InitConfig;
  begin
    Settings.Add(cbLanguage, @cfg.app.lang);
    Settings.Add(seIconsScale, @cfg.app.iconscale);

    Settings.Add(seBWTreshold, @cfg.import.bwlevel);

    Settings.Add(seGridThickness, @cfg.grid.size);
    Settings.Add(cbChessGrid, @cfg.grid.chess);

    Settings.Add(cbPreviewRefresh, @cfg.prev.refresh);

    Settings.Add(cbtnActive, @cfg.colorl.editor.active);
    Settings.Add(cbtnBackground, @cfg.colorl.editor.bg);
    Settings.Add(cbtnGrid, @cfg.colorl.editor.grid);
    Settings.Add(cbtnPreviewA, @cfg.colorl.prev.active);
    Settings.Add(cbtnPreviewBG, @cfg.colorl.prev.bg);
    Settings.Add(cbtnNaviA, @cfg.colorl.nav.active);
    Settings.Add(cbtnNaviBG, @cfg.colorl.nav.bg);
    Settings.Add(cbtnNaviT, @cfg.colorl.nav.txt);
    Settings.Add(cbtnImportA, @cfg.colorl.import.active);
    Settings.Add(cbtnImportBG, @cfg.colorl.import.bg);

    Settings.Add(cbtnActiveD, @cfg.colord.editor.active);
    Settings.Add(cbtnBackgroundD, @cfg.colord.editor.bg);
    Settings.Add(cbtnGridD, @cfg.colord.editor.grid);
    Settings.Add(cbtnPreviewAD, @cfg.colord.prev.active);
    Settings.Add(cbtnPreviewBGD, @cfg.colord.prev.bg);
    Settings.Add(cbtnNaviAD, @cfg.colord.nav.active);
    Settings.Add(cbtnNaviBGD, @cfg.colord.nav.bg);
    Settings.Add(cbtnNaviTD, @cfg.colord.nav.txt);
    Settings.Add(cbtnImportAD, @cfg.colord.import.active);
    Settings.Add(cbtnImportBGD, @cfg.colord.import.bg);

    Settings.Add(seNaviHeight, @cfg.nav.rowheight);
    Settings.Add(cbNaviTransparent, @cfg.nav.transparent);
    Settings.Add(cbNaviInvert, @cfg.nav.invert);
    Settings.Add(cbNaviScroll, @cfg.nav.scroll);

    Settings.Add(cbCharName, @cfg.nav.char.enable);
    Settings.Add(cbCharNameFont, @cfg.nav.char.font);
    Settings.Add(seCharNameFontSize, @cfg.nav.char.fontsize);

    Settings.Add(cbCodeName, @cfg.nav.code.enable);
    Settings.Add(cbCodeNameFont, @cfg.nav.code.font);
    Settings.Add(seCodeNameFontSize, @cfg.nav.code.fontsize);
    Settings.Add(cbCodeHex, @cfg.nav.code.hex);

    Settings.Add(edFontName, @cfg.new.title);
    Settings.Add(edAuthor, @cfg.new.author);
    Settings.Add(seNewWidth, @cfg.new.w);
    Settings.Add(seNewHeight, @cfg.new.h);
    Settings.Add(seNewItemStart, @cfg.new.start);
    Settings.Add(seNewItemLast, @cfg.new.last);
    Settings.Add(cbEncoding, @cfg.new.enc);

    Settings.Add(cbStickingBorder, @cfg.sticking.correct);
    Settings.Add(cbStickingEnable, @cfg.sticking.enable);
    Settings.Add('_cfg.sticking.config', stString, @cfg.sticking.config, '');
  end;

procedure TfmSettings.InitPages;
  var
    i, k: Integer;
  begin
      { In design time we use 'TPageControl:pcPageCtrl' for convenient GUI building.
        But in run-time there are some issues, such as unexpected flickering.
        So in run time we use 'TNotebook:nbPages' to show our GUI.
        Here we move all content from 'pcPageCtrl' to 'nbPages' pages. }

    pcPageCtrl.Hide;
    pcPageCtrl.Align := alNone;
    nbPages.Align    := alClient;

    for i := 0 to pcPageCtrl.PageCount - 1 do
      with pcPageCtrl.Pages[i] do
        begin
        nbPages.Pages.Add('npPage' + Name);
        nbPages.Page[i].ChildSizing.Assign(ChildSizing);

        for k := 0 to ControlCount - 1 do
          Controls[0].Parent := nbPages.Page[i];
        end;

    nbPages.PageIndex := 0;
  end;

procedure TfmSettings.OnLangChange(Sender: TObject);
  var
    i:        Integer;
    CEncList: TStringList;
  begin
    BeginFormUpdate;
    appLocalizerEx.Localize(cbTheme, TXT_THEME);

    // translate tree view tabs
    for i := 0 to pcPageCtrl.PageCount - 1 do
      if i < tvTabs.Items.Count then
        tvTabs.Items.Item[i].Text := pcPageCtrl.Pages[i].Caption;

      try
        try
        CEncList := TStringList.Create;
        EncodingsListUpdate;
        EncodingsListAssign(CEncList);
        appLocalizerEx.Localize(cbEncoding, CEncList.ToStringArray);
        finally
        CEncList.Free;
        end;
      except
      end;

    fmGen.OnLanguageChange;
    fmImportC.OnLanguageChange;

    StickingFormsEx.Rename(fmPreview, FM_PREV_EXAMPLE);
    StickingFormsEx.Rename(fmMap, FM_MAP_CAPTION);
    StickingFormsEx.Rename(fmGen, FM_GEN_CAPTION);

    appLocalizerEx.Localize(cbStickingSide, STFM_ANCHOR_TXT);
    appLocalizerEx.Localize(cbStickingSize, STFM_SIZE_TXT);

    EndFormUpdate;
  end;

procedure TfmSettings.StickingListUpdate;
  var
    i, s: Integer;
  begin
    s := clbSticking.ItemIndex;

    clbSticking.Clear;
    with StickingFormsEx do
      for i := 0 to Count - 1 do
        begin
        clbSticking.Items.Add(Format('%s: %s, %s', [
          FormByIndex[i].Name,
          UTF8LowerCase(STFM_ANCHOR_TXT[FormByIndex[i].Anchor]),
          UTF8LowerCase(STFM_SIZE_TXT[FormByIndex[i].FullSize])]));
        clbSticking.Checked[i] := FormByIndex[i].Enable;
        end;

    clbSticking.ItemIndex := s;
  end;



procedure TfmSettings.acOKExecute(Sender: TObject);
  begin
    Settings.SyncValues;
    appTunerEx.Theme := TAppTheme(cbTheme.ItemIndex);

    ModalResult := mrOk;
  end;

procedure TfmSettings.acCancelExecute(Sender: TObject);
  begin
    Settings.SyncComponents;
    cbLanguageChange(Sender);
    StickingFormsEx.Config := cfg.sticking.config;

    ModalResult := mrCancel;
  end;



procedure TfmSettings.clbStickingSelectionChange(Sender: TObject; User: Boolean);
  begin
      try
      with StickingFormsEx.FormByIndex[clbSticking.ItemIndex] do
        begin
        cbStickingSide.ItemIndex := Ord(Anchor);
        cbStickingSize.ItemIndex := Ord(FullSize);
        end;
      except
      end;
  end;

procedure TfmSettings.cbLanguageChange(Sender: TObject);
  begin
    BeginFormUpdate;
    appLocalizerEx.CurrentLanguage := cbLanguage.ItemIndex;
    StickingListUpdate;
    EndFormUpdate;
  end;

procedure TfmSettings.cbStickingCtrlChange(Sender: TObject);
  begin
    pStickingForms.Enabled := cbStickingEnable.Checked;
    clbSticking.Enabled    := cbStickingEnable.Checked;
    clbStickingSelectionChange(Sender, True);

    if clbSticking.ItemIndex >= 0 then
      with StickingFormsEx.FormByIndex[clbSticking.ItemIndex] do
        begin
        Enable   := clbSticking.Checked[clbSticking.ItemIndex];
        Anchor   := TAnchorKind(cbStickingSide.ItemIndex);
        FullSize := cbStickingSize.ItemIndex > 0;
        end;

    StickingListUpdate;
  end;

procedure TfmSettings.tvTabsSelectionChanged(Sender: TObject);
  begin
    nbPages.Page[tvTabs.Selected.AbsoluteIndex].Show;
  end;

procedure TfmSettings.cbCharNameClick(Sender: TObject);
  begin
    if (TCheckBox(Sender) = cbCharName) and not cbCharName.Checked then
      cbCodeName.Checked := True;

    if (TCheckBox(Sender) = cbCodeName) and not cbCodeName.Checked then
      cbCharName.Checked := True;

    cbCharNameFont.Enabled     := cbCharName.Checked;
    seCharNameFontSize.Enabled := cbCharName.Checked;
    cbCodeNameFont.Enabled     := cbCodeName.Checked;
    seCodeNameFontSize.Enabled := cbCodeName.Checked;
    cbCodeHex.Enabled          := cbCodeName.Checked;
  end;

procedure TfmSettings.udStickingOrderChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
  var
    i, j, c: Integer;
  begin
    c := clbSticking.Count - 1;
    i := clbSticking.ItemIndex;
    j := i;

    if (Direction = updUp) and (i > 0) then   j := i - 1;
    if (Direction = updDown) and (i < c) then j := i + 1;

    if (i < 0) or (i = j) then Exit;
    StickingFormsEx.Exchange(i, j);
    StickingListUpdate;

    clbSticking.ItemIndex := j;
  end;


end.
