unit fm_update;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls,
  DateUtils, OnlineUpdater,
  u_strings, u_helpers, config_record;

const
  PROJECT_GITLAB_ID = '29170126';
  PROJECT_ROOT      = '..' + DirectorySeparator;

  {$IfDef WINDOWS}
    {$IfDef WIN64}
    PROJECT_ZIP_NAME = 'matrixFont-(v(\d+\.)(\d+\.)(\d+\.)(\d+)-)?(x|win)64-portable';
    {$EndIf}
    {$IfDef WIN32}
    PROJECT_ZIP_NAME = 'matrixFont-(v(\d+\.)(\d+\.)(\d+\.)(\d+)-)?(x|win)32-portable';
    {$EndIf}
  {$EndIf}

type

  { TfmUpdate }

  TfmUpdate = class(TForm)
    btnCheck:   TButton;
    btnRestart: TButton;
    btnUpdate:  TButton;
    lbSize:     TLabel;
    mmLog:      TMemo;
    pbBar:      TProgressBar;
    pCtrl:      TPanel;
    tmrWork:    TTimer;

    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmrWorkTimer(Sender: TObject);

    procedure Log(s: String);
    procedure Log(s: String; args: array of const);
    procedure Progress(AStyle: TProgressBarStyle; APos, AMax: Integer);

    procedure btnCheckClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure btnRestartClick(Sender: TObject);

    procedure ouAvailable;
    procedure ouDownloadBegin;
    procedure ouDownloading(Sender: TObject; const ContentLength, CurrentPos: Int64);
    procedure ouDownloadDone;
    procedure ouUnzipping(Sender: TObject; const ATotPos, ATotSize: Int64);
    procedure ouUnzip;
    procedure ouError;

  private
    FSize:        String;
    FChecked:     Boolean;
    FAvailable:   Boolean;
    FDownloading: Boolean;
    FLater:       Boolean;

  public
    function IsDownloading: Boolean;
    function IsNotify: Boolean;
    procedure Later;
  end;

var
  fmUpdate: TfmUpdate;


function IsUpdaterReplaceActivated: Boolean;


implementation

{$R *.lfm}

var
  ou: TGitlabUpdater;

procedure InitUpdater;
  begin
    ou.ZipName      := PROJECT_ZIP_NAME;
    ou.UseRegex     := True;
    ou.RootPath     := ExtractFilePath(ParamStr(0)) + PROJECT_ROOT;
    ou.WhatsNewNeed := True;
  end;

function IsUpdaterReplaceActivated: Boolean;
  begin
    InitUpdater;

    // update application if unzipped update dir is available
    if ou.StartReplacing then
      begin
      if ou.IsReplacing then Exit(True);
      Sleep(100);
      while ou.Status <> usOK do Sleep(100);
      Exit(True);
      end;

    Result := False;
  end;

{ TfmUpdate }

procedure TfmUpdate.FormCreate(Sender: TObject);
  begin
    InitUpdater;
    ou.OnAvailable     := @ouAvailable;
    ou.OnDownloadBegin := @ouDownloadBegin;
    ou.OnDownloading   := @ouDownloading;
    ou.OnDownloadDone  := @ouDownloadDone;
    ou.OnUnzipping     := @ouUnzipping;
    ou.OnUnzip         := @ouUnzip;
    ou.OnError         := @ouError;

    FChecked     := False;
    FAvailable   := False;
    FDownloading := False;
    FLater       := False;

    cfg.app.update.lastTime := DateTimeToUnix(IncYear(Now, -1));

    lbSize.Constraints.MinWidth := lbSize.Width;
  end;

procedure TfmUpdate.FormShow(Sender: TObject);
  begin
    if IsDownloading then Exit;

    if FChecked then
      Later
    else
      begin
      btnCheck.Visible   := False;
      btnUpdate.Visible  := False;
      btnRestart.Visible := False;
      btnUpdate.Enabled  := False;
      btnRestart.Enabled := False;
      btnCheck.Click;
      end;
  end;

procedure TfmUpdate.tmrWorkTimer(Sender: TObject);
  begin
    tmrWork.Interval := 10 * 60 * 1000; // once per 10min

    if TAppUpdateWay(cfg.app.update.wayIndex) = uwManual then
      tmrWork.Enabled := False
    else
    if not FChecked then
      if DaysBetween(Now, UnixToDateTime(cfg.app.update.lastTime)) >=
        CAppUpdateDays[TAppUpdateFreq(cfg.app.update.freqIndex)] then
        btnCheck.Click;
  end;


procedure TfmUpdate.Log(s: String);
  begin
    mmLog.Append(s);
  end;

procedure TfmUpdate.Log(s: String; args: array of const);
  begin
    Log(Format(s, args));
  end;

procedure TfmUpdate.Progress(AStyle: TProgressBarStyle; APos, AMax: Integer);
  begin
    pbBar.Style    := AStyle;
    pbBar.Max      := AMax;
    pbBar.Position := APos;
  end;


procedure TfmUpdate.btnCheckClick(Sender: TObject);
  begin
    cfg.app.update.lastTime := DateTimeToUnix(Now);

    FChecked         := True;
    tmrWork.Enabled  := False;
    btnCheck.Enabled := False;
    mmLog.Text       := TXT_UPD_CHECKING;
    Progress(pbstMarquee, 0, 1);
    ou.StartChecking;
  end;

procedure TfmUpdate.btnUpdateClick(Sender: TObject);
  begin
    mmLog.Clear;
    ou.StartDownloading;
  end;

procedure TfmUpdate.btnRestartClick(Sender: TObject);
  begin
    if ou.Status = usOK then
      if ou.IsReplacing then
        Application.MainForm.Close;
  end;


procedure TfmUpdate.ouAvailable;
  begin
    Progress(pbstNormal, 0, 1);
    mmLog.Clear;
    btnCheck.Visible  := False;
    btnUpdate.Visible := True;
    btnUpdate.Enabled := True;

    FSize := ou.DownloadSize.SizeInBytes(
      TXT_BYTE_SHORT, TXT_BYTE_KB, TXT_BYTE_MB, TXT_BYTE_GB, False);

    with ou.LatestVer do
      Log(TXT_UPD_NEWVER, [Major, Minor, Revision, Build, FSize]);

    if ou.WhatsNewNeed and (ou.WhatsNewData <> '') then
      Log(LineEnding + ou.WhatsNewData.Replace(#10, LineEnding));

    if Visible then Later;

    if TAppUpdateWay(cfg.app.update.wayIndex) = uwAuto then
      btnUpdate.Click
    else
      FAvailable := True;
  end;

procedure TfmUpdate.ouDownloadBegin;
  begin
    Progress(pbstNormal, 0, 1);
    Log(TXT_UPD_DOWNLOADING, [FSize]);
    btnUpdate.Enabled := False;
    FDownloading      := True;
  end;

procedure TfmUpdate.ouDownloading(Sender: TObject; const ContentLength,
  CurrentPos: Int64);
  begin
    if ContentLength - pbBar.Position < 100000 then Exit;

    Progress(pbstNormal, CurrentPos, ContentLength);
    btnUpdate.Visible := False;
    lbSize.Caption    := Format('%d / %d %s', [CurrentPos div 1024, ContentLength div 1024, TXT_BYTE_KB]);
    lbSize.Visible    := True;
  end;

procedure TfmUpdate.ouDownloadDone;
  begin
    Progress(pbstNormal, 0, 1);
    Log(TXT_UPD_UNZIPPING);
  end;

procedure TfmUpdate.ouUnzipping(Sender: TObject; const ATotPos, ATotSize: Int64);
  begin
    if ATotPos - pbBar.Position < ATotSize div 100 then Exit;

    Progress(pbstNormal, ATotPos, ATotSize);
    lbSize.Caption := Format('%d / %d %s', [ATotPos div 1024, ATotSize div 1024, TXT_BYTE_KB]);
  end;

procedure TfmUpdate.ouUnzip;
  begin
    Progress(pbstNormal, 1, 1);
    btnUpdate.Enabled := False;
    lbSize.Visible    := False;
    FDownloading      := False;

    if ou.Status = usOK then
      begin
      Log(TXT_UPD_READY);
      btnUpdate.Visible  := False;
      btnRestart.Visible := True;
      end;
  end;

procedure TfmUpdate.ouError;
  begin
    Progress(pbstNormal, 0, 1);
    btnUpdate.Visible := False;
    btnCheck.Visible  := True;
    btnCheck.Enabled  := True;
    FChecked          := False;

    case ou.Status of
      usNoUpdates:
        Log(TXT_UPD_UPTODATE);
      else
        Log(TXT_UPD_ERROR);
      end;
  end;


function TfmUpdate.IsDownloading: Boolean;
  begin
    Result := FDownloading;
  end;

function TfmUpdate.IsNotify: Boolean;
  begin
    Result := FAvailable and not FLater;
  end;

procedure TfmUpdate.Later;
  begin
    FLater := True;
  end;


initialization
  ou := TGitlabUpdater.Create(PROJECT_GITLAB_ID);

finalization
  ou.Terminate;

end.
