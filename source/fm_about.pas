unit fm_about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, LCLType, LCLIntf,
  app_ver;

resourcestring
  ABOUT_RIGHTS  = 'Все права защищены' + LineEnding + 'Свободное ПО';
  ABOUT_VERSION = 'версия';
  ABOUT_BIT     = '-битная';
  ABOUT_BUILD   = 'сборка';
  ABOUT_SITE    = 'сайт';

const

  // адрес сайта, пример https://example.site/home, пустая строка адреса отключает видимость ссылки
  APP_SITE_ADDRESS = 'https://gitlab.com/riva-lab/matrixFont';
  APP_SITE         = 'gitlab.com'; // отображаемое имя сайта

  FILE_LICENSE     = 'license.md';
  FILE_README      = 'readme.md';

type

  { TfmAbout }

  TfmAbout = class(TForm)
    imLogo:       TImage;
    lbLicense:    TLabel;
    lbInfo:       TLabel;
    lbSite:       TLabel;
    lbCopyRights: TLabel;
    pInfo:        TPanel;
    lbHardware:   TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure lbLicenseClick(Sender: TObject);
    procedure lbSiteClick(Sender: TObject);
    procedure lbCopyRightsClick(Sender: TObject);
  private
    FAppArc:           String;
    FAppAuthor:        String;
    FAppBrief:         String;
    FAppBuild:         String;
    FAppCopyright:     String;
    FAppDescription:   String;
    FAppIntName:       String;
    FAppSite:          String;
    FAppSiteAvailable: Boolean;
    FAppVersion:       String;
    FAppComments:      String;

    { private declarations }
  public
    { public declarations }
    procedure ShowSplash(IsShow: Boolean = True);
    procedure VisitSite;
    procedure UpdateInfo;

    property AppIntName: String read FAppIntName;
    property AppVersion: String read FAppVersion;
    property AppBuild: String read FAppBuild;
    property AppArc: String read FAppArc;
    property AppAuthor: String read FAppAuthor;
    property AppCopyright: String read FAppCopyright;
    property AppDescription: String read FAppDescription;
    property AppBrief: String read FAppBrief;
    property AppSite: String read FAppSite;
    property AppSiteAvailable: Boolean read FAppSiteAvailable;
  end;

var
  fmAbout: TfmAbout;

implementation

{$R *.lfm}

{ TfmAbout }

procedure TfmAbout.FormCreate(Sender: TObject);
  begin
    UpdateInfo;
    pInfo.ControlStyle := pInfo.ControlStyle - [csOpaque] + [csParentBackground];
    lbHardware.Caption := Format('Screen:' + LineEnding + '%d x %d' + LineEnding + '%d DPI',
      [Screen.Width, Screen.Height, Screen.PixelsPerInch]);
  end;

procedure TfmAbout.FormDeactivate(Sender: TObject);
  begin
    Close;
  end;

procedure TfmAbout.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  begin
    case UTF8Key of
      chr(13), chr(27), chr(32):
        Close;
      end;
  end;


procedure TfmAbout.lbCopyRightsClick(Sender: TObject);
  begin
    if not OpenDocument(FILE_README) then
      OpenDocument('..' + DirectorySeparator + FILE_README);
  end;

procedure TfmAbout.lbLicenseClick(Sender: TObject);
  begin
    if not OpenDocument(FILE_LICENSE) then
      OpenDocument('..' + DirectorySeparator + FILE_LICENSE);
  end;

procedure TfmAbout.lbSiteClick(Sender: TObject);
  begin
    VisitSite;
  end;

procedure TfmAbout.ShowSplash(IsShow: Boolean = True);
  var
    i: Integer;
  begin
    {$IfNDef Debug}
    Position       := poScreenCenter;
    FormStyle      := fsStayOnTop;
    OnDeactivate   := nil;
    OnUTF8KeyPress := nil;

    if IsShow then
      begin
      Show;

      for i := 0 to 120 do
        begin
        Sleep(10);
        Application.ProcessMessages;
        end;

      Close;
      end;
    {$EndIf}

    FormStyle      := fsNormal;
    BorderStyle    := bsNone;
    Position       := poMainFormCenter;
    OnDeactivate   := @FormDeactivate;
    OnUTF8KeyPress := @FormUTF8KeyPress;
  end;


procedure TfmAbout.VisitSite;
  begin
    OpenURL(APP_SITE_ADDRESS);
  end;

procedure TfmAbout.UpdateInfo;
  const
  {$IfDef WIN64}
    sys_arc = '64';
  {$Else}
    sys_arc = '32';
  {$EndIf}
  var
    info:          String;
    BuildDateTime: TDateTime;
  begin
    TryStrToDate({$INCLUDE %DATE%}, BuildDateTime, 'YYYY/MM/DD', '/');

    ReadAppInfo;
    FAppIntName       := app_info.InternalName;
    FAppArc           := sys_arc + ABOUT_BIT;
    FAppAuthor        := app_info.CompanyName;
    FAppBuild         := ABOUT_BUILD + ' ' + FormatDateTime('yyyy.mm.dd', BuildDateTime);
    FAppDescription   := app_info.FileDescription;
    FAppVersion       := ABOUT_VERSION + ' ' + app_info.FileVersion;
    FAppCopyright     := '© ' + app_info.LegalCopyright;
    FAppComments      := app_info.Comments;
    FAppBrief         := FAppIntName + ' ' + FAppVersion + ', ' + FAppArc + ', ' + FAppBuild;
    FAppSite          := APP_SITE;
    FAppSiteAvailable := Length(APP_SITE_ADDRESS) > 0;

    info := app_info.InternalName + LineEnding;
    info += FAppVersion + ', ' + FAppArc + LineEnding;
    info += FAppBuild + LineEnding + LineEnding;
    info += FAppComments + LineEnding + LineEnding;
    info += FAppCopyright + LineEnding;
    info += ABOUT_RIGHTS + LineEnding;
    info += FAppAuthor;

    lbInfo.Caption    := info;
    lbSite.Caption    := {ABOUT_SITE + ': ' +} APP_SITE;
    lbSite.Hint       := APP_SITE_ADDRESS;
    lbSite.Visible    := FAppSiteAvailable;
    lbLicense.Hint    := FILE_LICENSE;
    lbCopyRights.Hint := FILE_README;
  end;

end.
