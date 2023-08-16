unit fm_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, Dialogs, ComCtrls, Spin, StdCtrls,
  ExtCtrls, ActnList, Controls, IniPropStorage, LCLTranslator, Buttons,
  LazFileUtils, LazUTF8, u_encodings, u_utilities;

const
  LANGUAGE_DEFAULT = 'RU, Russian - Русский';
  LANGUAGES_DIR    = 'lang';
  LANGUAGE_FILE    = LANGUAGES_DIR + DirectorySeparator + 'matrixFont';
  LANGUAGES_FILE   = LANGUAGES_DIR + DirectorySeparator + 'languages.ini';
  SETTINGS_FILE    = DirectorySeparator + 'settings.ini';

type

  { TfmSettings }

  TfmSettings = class(TForm)
    IniStorageSettings: TIniPropStorage;
    SettingsActionList: TActionList;
    acOK:     TAction;
    acCancel: TAction;

    PageCtrl:      TPageControl;
    tsGeneral:     TTabSheet;
    tsNewDefaults: TTabSheet;
    tsColors:      TTabSheet;

    bbApply:    TBitBtn;
    bbCancel:   TBitBtn;
    bbDefaults: TBitBtn;

    cbtnGrid:       TColorButton;
    cbtnNaviA:      TColorButton;
    cbtnNaviBG:     TColorButton;
    cbtnPreviewA:   TColorButton;
    cbtnPreviewBG:  TColorButton;
    cbtnBackground: TColorButton;
    cbtnActive:     TColorButton;
    cbtnImportA:    TColorButton;
    cbtnImportBG:   TColorButton;

    cbNaviTransparent: TCheckBox;
    cbNaviInvert:      TCheckBox;
    cbNaviScroll:      TCheckBox;
    cbChessGrid:       TCheckBox;
    cbCharName:        TCheckBox;
    cbCodeName:        TCheckBox;
    cbCodeHex:         TCheckBox;
    cbMagnetPreview:   TCheckBox;
    cbPreviewRefresh:  TCheckBox;

    cbCharNameFont: TComboBox;
    cbCodeNameFont: TComboBox;
    cbLanguage:     TComboBox;
    cbEncoding:     TComboBox;

    pLanguage:      TPanel;
    lbNewDefaults2: TPanel;
    pControls:      TPanel;
    pNewDefaults1:  TPanel;
    pColorEditor:   TPanel;
    pValues:        TPanel;
    pNaviColumns:   TPanel;
    pColorPreview:  TPanel;
    pButtons:       TPanel;
    pColorNavi:     TPanel;

    lbColorImport:    TLabel;
    lbColorImportA:   TLabel;
    lbColorImportBG:  TLabel;
    lbEmpty1:         TLabel;
    lbFontEncoding:   TLabel;
    lbBWTreshold:     TLabel;
    pColorImport:     TPanel;
    lbGridThickness:  TLabel;
    lbColorGrid:      TLabel;
    lbColorBG:        TLabel;
    lbColorA:         TLabel;
    lbNewWidth:       TLabel;
    lbNewHeight:      TLabel;
    lbNewItemStart:   TLabel;
    lbNewItemLast:    TLabel;
    lbNaviHeight:     TLabel;
    lbColorPreviewA:  TLabel;
    lbColorPreviewBG: TLabel;
    lbColorNaviA:     TLabel;
    lbColorNaviBG:    TLabel;
    lbColorEditor:    TLabel;
    lbColorPreview:   TLabel;
    lbColorNavi:      TLabel;
    lbNaviColumns:    TLabel;
    lbNewDefaults:    TLabel;
    lbLanguage:       TLabel;
    lbFontName:       TLabel;
    lbAuthor:         TLabel;

    seBWTreshold:       TSpinEdit;
    seCodeNameFontSize: TSpinEdit;
    seNewHeight:        TSpinEdit;
    seNewItemLast:      TSpinEdit;
    seNewItemStart:     TSpinEdit;
    seNewWidth:         TSpinEdit;
    seGridThickness:    TSpinEdit;
    seCharNameFontSize: TSpinEdit;
    seNaviHeight:       TSpinEdit;

    edAuthor:   TEdit;
    edFontName: TEdit;


    procedure cbCharNameClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IniStorageSettingsRestore(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure acOKExecute(Sender: TObject);
    procedure IniStorageSettingsSavingProperties(Sender: TObject);
    procedure cbLanguageChange(Sender: TObject);

  private
    FLangList:       TStringList;
    FLanguage:       String;
    FLanguageIndex:  Integer;
    FChessGrid:      Boolean;
    FMagnetPreview:  Boolean;
    FPreviewRefresh: Boolean;
    FGridThickness:  Integer;
    FBWTreshold:     Integer;

    FColorActive:     TColor;
    FColorBackground: TColor;
    FColorGrid:       TColor;
    FColorNaviA:      TColor;
    FColorNaviBG:     TColor;
    FColorPreviewA:   TColor;
    FColorPreviewBG:  TColor;
    FColorImportA:    TColor;
    FColorImportBG:   TColor;

    FCharNameFont:     String;
    FCharNameFontSize: Integer;
    FCodeNameFont:     String;
    FCodeNameFontSize: Integer;
    FCodeHex:          Boolean;

    FNaviColName:     Boolean;
    FNaviColCode:     Boolean;
    FNaviHeight:      Integer;
    FNaviTransparent: Boolean;
    FNaviInvert:      Boolean;
    FNaviScroll:      Boolean;

    FNewName:      String;
    FNewAuthor:    String;
    FNewWidth:     Integer;
    FNewHeight:    Integer;
    FNewItemStart: Integer;
    FNewItemLast:  Integer;
    FNewEncoding:  Integer;

    procedure IniStorageLangLoad;
    procedure LoadComponentsFromFields;
    procedure LoadFieldsFromComponents;

  public
    property Language: String read FLanguage;
    property MagnetPreview: Boolean read FMagnetPreview;
    property PreviewRefresh: Boolean read FPreviewRefresh;
    property GridThickness: Integer read FGridThickness;
    property BWTreshold: Integer read FBWTreshold;
    property ChessGrid: Boolean read FChessGrid;

    property ColorActive: TColor read FColorActive;
    property ColorBackground: TColor read FColorBackground;
    property ColorGrid: TColor read FColorGrid;
    property ColorPreviewA: TColor read FColorPreviewA;
    property ColorPreviewBG: TColor read FColorPreviewBG;
    property ColorNaviA: TColor read FColorNaviA;
    property ColorNaviBG: TColor read FColorNaviBG;
    property ColorImportA: TColor read FColorImportA;
    property ColorImportBG: TColor read FColorImportBG;

    property CharNameFont: String read FCharNameFont;
    property CharNameFontSize: Integer read FCharNameFontSize;
    property CodeNameFont: String read FCodeNameFont;
    property CodeNameFontSize: Integer read FCodeNameFontSize;
    property CodeHex: Boolean read FCodeHex;

    property NaviColName: Boolean read FNaviColName;
    property NaviColCode: Boolean read FNaviColCode;
    property NaviHeight: Integer read FNaviHeight write FNaviHeight;
    property NaviTransparent: Boolean read FNaviTransparent;
    property NaviInvert: Boolean read FNaviInvert;
    property NaviScroll: Boolean read FNaviScroll;

    property NewName: String read FNewName;
    property NewAuthor: String read FNewAuthor;
    property NewWidth: Integer read FNewWidth;
    property NewHeight: Integer read FNewHeight;
    property NewItemStart: Integer read FNewItemStart;
    property NewItemLast: Integer read FNewItemLast;
    property NewEncoding: Integer read FNewEncoding;
  end;

var
  fmSettings:     TfmSettings;
  IniStorageLang: TIniPropStorage;



implementation

{$R *.lfm}

{ TfmSettings }

procedure TfmSettings.FormCreate(Sender: TObject);
  begin
    FLangList                := TStringList.Create;
    IniStorageSettings.IniFileName := ExtractFileDir(ParamStrUTF8(0)) + SETTINGS_FILE;
    PageCtrl.ActivePageIndex := 0;

    IniStorageLangLoad;
    EncodingsListAssign(cbEncoding.Items);
    cbEncoding.ItemIndex := 0;
  end;

procedure TfmSettings.FormShow(Sender: TObject);
  var
    i, tmp: Integer;
  begin
    if Sender <> nil then
      LoadComponentsFromFields;

    tmp      := PageCtrl.ActivePageIndex;
    Position := poDefault;

    for i := 1 to PageCtrl.PageCount do
      begin
      PageCtrl.ActivePageIndex := i - 1;
      AutoSize                 := True;

      Constraints.MinWidth  := Width;
      Constraints.MinHeight := Height;

      AutoSize := False;
      end;

    PageCtrl.ActivePageIndex := tmp;
    Position                 := poMainFormCenter;
    cbCharNameFont.Items     := screen.Fonts;
    cbCodeNameFont.Items     := screen.Fonts;
  end;


procedure TfmSettings.LoadComponentsFromFields;
  begin
    cbLanguage.ItemIndex     := FLanguageIndex;
    seGridThickness.Value    := FGridThickness;
    seBWTreshold.Value       := FBWTreshold;
    cbMagnetPreview.Checked  := FMagnetPreview;
    cbPreviewRefresh.Checked := FPreviewRefresh;
    cbChessGrid.Checked      := FChessGrid;

    cbtnActive.ButtonColor     := FColorActive;
    cbtnBackground.ButtonColor := FColorBackground;
    cbtnGrid.ButtonColor       := FColorGrid;
    cbtnPreviewA.ButtonColor   := FColorPreviewA;
    cbtnPreviewBG.ButtonColor  := FColorPreviewBG;
    cbtnNaviA.ButtonColor      := FColorNaviA;
    cbtnNaviBG.ButtonColor     := FColorNaviBG;
    cbtnImportA.ButtonColor    := FColorImportA;
    cbtnImportBG.ButtonColor   := FColorImportBG;

    cbCharNameFont.Text      := FCharNameFont;
    seCharNameFontSize.Value := FCharNameFontSize;
    cbCodeNameFont.Text      := FCodeNameFont;
    seCodeNameFontSize.Value := FCodeNameFontSize;
    cbCodeHex.Checked        := FCodeHex;

    cbCharName.Checked        := FNaviColName;
    cbCodeName.Checked        := FNaviColCode;
    seNaviHeight.Value        := FNaviHeight;
    cbNaviTransparent.Checked := FNaviTransparent;
    cbNaviInvert.Checked      := FNaviInvert;
    cbNaviScroll.Checked      := FNaviScroll;

    edFontName.Text      := FNewName;
    edAuthor.Text        := FNewAuthor;
    seNewWidth.Value     := FNewWidth;
    seNewHeight.Value    := FNewHeight;
    seNewItemStart.Value := FNewItemStart;
    seNewItemLast.Value  := FNewItemLast;
    cbEncoding.ItemIndex := FNewEncoding;

    cbCharNameClick(nil);
  end;

procedure TfmSettings.LoadFieldsFromComponents;
  begin
    FLanguageIndex  := cbLanguage.ItemIndex;
    FLanguage       := FLangList[cbLanguage.ItemIndex];
    FGridThickness  := seGridThickness.Value;
    FBWTreshold     := seBWTreshold.Value;
    FMagnetPreview  := cbMagnetPreview.Checked;
    FPreviewRefresh := cbPreviewRefresh.Checked;
    FChessGrid      := cbChessGrid.Checked;

    FColorActive     := cbtnActive.ButtonColor;
    FColorBackground := cbtnBackground.ButtonColor;
    FColorGrid       := cbtnGrid.ButtonColor;
    FColorPreviewA   := cbtnPreviewA.ButtonColor;
    FColorPreviewBG  := cbtnPreviewBG.ButtonColor;
    FColorNaviA      := cbtnNaviA.ButtonColor;
    FColorNaviBG     := cbtnNaviBG.ButtonColor;
    FColorImportA    := cbtnImportA.ButtonColor;
    FColorImportBG   := cbtnImportBG.ButtonColor;

    FCharNameFont     := cbCharNameFont.Text;
    FCharNameFontSize := seCharNameFontSize.Value;
    FCodeNameFont     := cbCodeNameFont.Text;
    FCodeNameFontSize := seCodeNameFontSize.Value;
    FCodeHex          := cbCodeHex.Checked;

    FNaviColName     := cbCharName.Checked;
    FNaviColCode     := cbCodeName.Checked;
    FNaviHeight      := seNaviHeight.Value;
    FNaviTransparent := cbNaviTransparent.Checked;
    FNaviInvert      := cbNaviInvert.Checked;
    FNaviScroll      := cbNaviScroll.Checked;

    FNewName      := edFontName.Text;
    FNewAuthor    := edAuthor.Text;
    FNewWidth     := seNewWidth.Value;
    FNewHeight    := seNewHeight.Value;
    FNewItemStart := seNewItemStart.Value;
    FNewItemLast  := seNewItemLast.Value;
    FNewEncoding  := cbEncoding.ItemIndex;

    if FNewName = '' then
      FNewName := 'New';

    if FNewAuthor = '' then
      FNewAuthor := GetEnvironmentVariable('ComputerName') + ' / ' +
        GetEnvironmentVariable('UserName');
  end;


procedure TfmSettings.IniStorageSettingsRestore(Sender: TObject);
  begin
    LoadFieldsFromComponents;
  end;

procedure TfmSettings.IniStorageSettingsSavingProperties(Sender: TObject);
  begin
    LoadComponentsFromFields;
  end;

procedure TfmSettings.IniStorageLangLoad;
  var
    i, cnt: Integer;
  begin
    cbLanguage.Clear;
    FLangList.Append(GetLangCode(LANGUAGE_DEFAULT));
    cbLanguage.Items.Append(GetLangCaption(LANGUAGE_DEFAULT));

    IniStorageLang := TIniPropStorage.Create(nil);
    with IniStorageLang do
      begin
      IniFileName := ExtractFilePath(ParamStrUTF8(0)) + LANGUAGES_FILE;
      Active      := True;
      IniSection  := 'Languages List';

      // если приложение не нашло файл со списком локализаций - создаем его
      if not FileExistsUTF8(IniFileName) then
        begin
        WriteInteger('Count', 1);
        WriteString('L-1', LANGUAGE_DEFAULT);
        end;

      // считываем список локализаций, кроме 1-го пункта (язык по умолчанию)
      cnt := ReadInteger('Count', 1);
      cbLanguage.ItemIndex := 0;

      if cnt > 1 then
        for i := 2 to cnt do
          begin
          FLangList.Append(GetLangCode(ReadString('L-' + i.ToString, '')));
          cbLanguage.Items.Append(GetLangCaption(ReadString('L-' + i.ToString, '')));
          end;
      end;
  end;


procedure TfmSettings.acOKExecute(Sender: TObject);
  begin
    LoadFieldsFromComponents;

    ModalResult := mrOk;
  end;

procedure TfmSettings.acCancelExecute(Sender: TObject);
  begin
    LoadComponentsFromFields;

    ModalResult := mrCancel;
    //Close;
  end;


procedure TfmSettings.cbLanguageChange(Sender: TObject);
  var
    _item: Integer;
  begin
    _item := cbEncoding.ItemIndex; // бекап индекса кодировки

    // применяем язык интерфейса не выходя из настроек
    SetDefaultLang(FLangList[cbLanguage.ItemIndex], '', LANGUAGE_FILE);

    // перерисовываем форму, чтобы более длинные метки полностью помещались
    FormShow(nil);
    cbEncoding.ItemIndex := _item; // восстан. индекса кодировки
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
