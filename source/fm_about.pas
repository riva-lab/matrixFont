unit fm_about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, LCLType, LCLIntf, ExtCtrls, Math,
  BCSVGViewer, u_utilities, u_helpers, appAbout;

const
  COLOR_SVG_BOTTOM = $00153103; // background color of the SVG bottom

type

  { TfmAbout }

  TfmAbout = class(TForm)
    BCSVGLogo:    TBCSVGViewer;
    lbCopyRights: TLabel;
    lbHomepage:   TLabel;
    lbInfo:       TLabel;
    lbLicense:    TLabel;
    lbRepo:       TLabel;
    pLinks:       TPanel;
    pLinksX:      TPanel;

    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);

    procedure OnLinkClick(Sender: TObject);

  public
    procedure ShowSplash(IsShow: Boolean = True);

  end;

var
  fmAbout: TfmAbout;

implementation

{$R *.lfm}

{ TfmAbout }

procedure TfmAbout.FormCreate(Sender: TObject);
  begin
    BCSVGLogo.SVGString := GetResourceAsString('LOGO');
  end;

procedure TfmAbout.FormDeactivate(Sender: TObject);
  begin
    Close;
  end;

procedure TfmAbout.FormShow(Sender: TObject);

  procedure CreateLinks(ALabel: array of TLabel; AURL: array of String);
    var
      i: Integer;
    begin
      if Length(ALabel) <> Length(AURL) then Exit;
      if Length(ALabel) = 0 then Exit;
      for i := 0 to High(ALabel) do
        begin
        ALabel[i].Hint    := AURL[i];
        ALabel[i].OnClick := @OnLinkClick;
        end;
    end;

  begin
    lbInfo.Caption := appAbout.UpdateAboutAppInfo
      + Format('Screen: %d x %d, %d DPI' + LineEnding, [Screen.Width, Screen.Height, Screen.PixelsPerInch]);

    CreateLinks(
      [lbLicense, lbCopyRights, lbHomepage, lbRepo],
      [FILE_LICENSE, FILE_README, APP_URL_HOME, APP_URL_REPO]);

    with BCSVGLogo.Constraints do
      begin
      MinWidth  := Max(Scale96ToScreen(360), pLinks.Width);
      MinHeight := Max(MinWidth * 2 div 3, lbInfo.Height);
      MinWidth  := MinHeight * 3 div 2;
      end;

    Color    := COLOR_SVG_BOTTOM;
    Position := poMainFormCenter;
  end;

procedure TfmAbout.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  begin
    if Ord(UTF8Key[1]) in [13, 27, 32] then Close;
  end;

procedure TfmAbout.OnLinkClick(Sender: TObject);
  begin
    appAbout.linkClick(Sender);
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


end.
