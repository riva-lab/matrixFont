unit fm_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, Dialogs, ComCtrls, Spin, StdCtrls,
  ExtCtrls, ActnList, Controls, Buttons,
  LazFileUtils, LazUTF8, Math, AppLocalizer, AppTuner,
  fm_gen, fm_importc,
  u_encodings, u_utilities, u_strings, config_record;

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
    cbMagnetPreview:    TCheckBox;
    cbNaviInvert:       TCheckBox;
    cbNaviScroll:       TCheckBox;
    cbNaviTransparent:  TCheckBox;
    cbPreviewRefresh:   TCheckBox;
    cbTheme:            TComboBox;
    cbtnActive:         TColorButton;
    cbtnBackground:     TColorButton;
    cbtnGrid:           TColorButton;
    cbtnImportA:        TColorButton;
    cbtnImportBG:       TColorButton;
    cbtnNaviA:          TColorButton;
    cbtnNaviBG:         TColorButton;
    cbtnPreviewA:       TColorButton;
    cbtnPreviewBG:      TColorButton;
    edAuthor:           TEdit;
    edFontName:         TEdit;
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
    lbColorPreview:     TLabel;
    lbColorPreviewA:    TLabel;
    lbColorPreviewBG:   TLabel;
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
    lbTheme:            TLabel;
    pButtons:           TPanel;
    pColorEditor:       TPanel;
    pColorImport:       TPanel;
    pColorNavi:         TPanel;
    pColorPreview:      TPanel;
    pControls:          TPanel;
    pcPageCtrl:         TPageControl;
    pNaviColumns:       TPanel;
    pNewDefaults1:      TPanel;
    pNewDefaults2:      TPanel;
    pSpacer1:           TPanel;
    pSpacer2:           TPanel;
    pSpacer3:           TPanel;
    pSpacer4:           TPanel;
    pSpacer5:           TPanel;
    pSpacer6:           TPanel;
    pSpacer7:           TPanel;
    pTitle1:            TPanel;
    pTitle2:            TPanel;
    pTitle3:            TPanel;
    pTitle4:            TPanel;
    pTitle5:            TPanel;
    pTitle6:            TPanel;
    pTitle7:            TPanel;
    pTitle8:            TPanel;
    pTitle9:            TPanel;
    pValues1:           TPanel;
    pValues2:           TPanel;
    pValues3:           TPanel;
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
    tvTabs:             TTreeView;


    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure acOKExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);

    procedure cbLanguageChange(Sender: TObject);
    procedure cbCharNameClick(Sender: TObject);
    procedure tvTabsSelectionChanged(Sender: TObject);

  private
    procedure AdjustComponentsSizes;
    procedure FormAutosize;
    procedure InitConfig;

    procedure OnLangChange(Sender: TObject);
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

    pcPageCtrl.ActivePageIndex := 0;
    pcPageCtrl.ShowTabs        := False;

    // add chapter titles to treeview
    for i := 0 to pcPageCtrl.PageCount - 1 do
      tvTabs.Items.Add(TTreeNode.Create(nil), pcPageCtrl.Page[i].Caption);
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

    Position := poDefault;
    FormAutosize;
    Position := poMainFormCenter;
  end;

procedure TfmSettings.FormAutosize;
  var
    tmp, i: Integer;
  begin
    AdjustComponentsSizes;

    tmp := pcPageCtrl.ActivePageIndex;

    for i := 0 to pcPageCtrl.PageCount - 1 do
      begin
      pcPageCtrl.ActivePageIndex := i;
      AutoSize := True;

      Constraints.MinWidth  := Width;
      Constraints.MinHeight := Height;

      AutoSize := False;
      end;

    pcPageCtrl.ActivePageIndex := tmp;
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

    Settings.Add(cbMagnetPreview, @cfg.prev.magnet);
    Settings.Add(cbPreviewRefresh, @cfg.prev.refresh);

    Settings.Add(cbtnActive, @cfg.color.editor.active);
    Settings.Add(cbtnBackground, @cfg.color.editor.bg);
    Settings.Add(cbtnGrid, @cfg.color.editor.grid);
    Settings.Add(cbtnPreviewA, @cfg.color.prev.active);
    Settings.Add(cbtnPreviewBG, @cfg.color.prev.bg);
    Settings.Add(cbtnNaviA, @cfg.color.nav.active);
    Settings.Add(cbtnNaviBG, @cfg.color.nav.bg);
    Settings.Add(cbtnImportA, @cfg.color.import.active);
    Settings.Add(cbtnImportBG, @cfg.color.import.bg);

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

    EndFormUpdate;
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

    ModalResult := mrCancel;
  end;



procedure TfmSettings.cbLanguageChange(Sender: TObject);
  begin
    BeginFormUpdate;
    appLocalizerEx.CurrentLanguage := cbLanguage.ItemIndex;
    EndFormUpdate;
  end;

procedure TfmSettings.tvTabsSelectionChanged(Sender: TObject);
  begin
    pcPageCtrl.Page[tvTabs.Selected.AbsoluteIndex].Show;
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


end.
