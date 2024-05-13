unit appAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, LCLType, LCLIntf, LazFileUtils, Registry,
  AppLocalizer, OnlineUpdater, ouVersion,
  u_helpers, u_utilities;


resourcestring
  ABOUT_VERSION = 'версия %s';
  ABOUT_BIT     = '%s-битная';
  ABOUT_BUILD   = 'сборка #%d, %s';


const
  ABOUT_RIGHTS  = 'Modified FreeBSD License';
  ABOUT_OPENSRC = 'Free Open Source Software';

  FILE_LICENSE  = 'license.md';
  FILE_README   = 'readme.md';
  FILE_HELP     = 'help' + DirectorySeparator + 'matrixFont-help.md';

  // homepage url, e.g. https://example.site/home
  APP_URL_HOME  = 'https://riva-lab.gitlab.io/apps/matrixFont/';

  // project repository url
  APP_URL_REPO  = 'https://gitlab.com/riva-lab/matrixFont';

  // project online help url
  APP_URL_HELP  = 'https://riva-lab.gitlab.io/apps/matrixFont/help.html';


procedure linkClick(Sender: TObject);
function UpdateAboutAppShort: String;
function UpdateAboutAppInfo: String;

function GetAppName: String;
function GetAppVersion: String;
function GetAppNameVersion: String;
function GetAppCompanyName: String;
function GetAppNameAuthor: String;


var
  AppVersionStr: String = '';
  AppProductStr: String = '';
  AppSystemStr:  String = '';


implementation

var
  fvi: TFileVersionInfoSimple;


procedure linkClick(Sender: TObject);

  function TryOpenFile(AFilename: String): Boolean;
    begin
      Result := False;
      if OpenDocument(AFilename) then Exit(True);
      if OpenDocument('..' + DirectorySeparator + AFilename) then Exit(True);
    end;

  function GetFromResources(ARes: String; AFilename: String): Boolean;
    begin
        try
        with TResourceStream.Create(HINSTANCE, ARes, RT_RCDATA) do
          try
          SaveToFile(AFilename);
          finally
          Free;
          OpenDocument(AFilename); // open file
          Sleep(2000);             // wait for 2s while file is opening
          DeleteFile(AFilename);   // delete file
          end;
        Result := True;
        except
        Result := False;
        end;
    end;
  var
    url, id: String;
  begin
    if TComponent(Sender).ClassName <> 'TLabel' then Exit;
    url := TLabel(Sender).Hint;
    id  := ExtractFileNameWithoutExt(url);

    if url.StartsWith('https://') then
      OpenURL(url)
    else
    if not TryOpenFile(id + '.' + appLocalizerEx.CurrentLangCode + '.html') then
      if not TryOpenFile(id + '.' + appLocalizerEx.CurrentLangCode + '.md') then
        if not TryOpenFile(id + '.html') then
          if not TryOpenFile(id + '.md') then
            GetFromResources(id, url);
  end;

function UpdateAboutAppShort: String;
  var
    _bldTime: TDateTime;
    _info:    TStringList;
    _arc:     String;
  begin
    _info  := TStringList.Create;
    _arc   := '';
    Result := '';

    {$IfDef WINDOWS}
      {$IfDef WIN64}
      _arc := Format(ABOUT_BIT, ['64']);
      {$EndIf}
      {$IfDef WIN32}
      _arc := Format(ABOUT_BIT, ['32']);
      {$EndIf}

      with TRegistry.Create(KEY_READ) do
        begin
          try
          RootKey      := HKEY_LOCAL_MACHINE;
          OpenKeyReadOnly('SYSTEM\CurrentControlSet\Control\Session Manager\Environment');
          AppSystemStr := ReadString('PROCESSOR_ARCHITECTURE').Contains('64').Select('x64', 'x32');
          CloseKey;
          OpenKeyReadOnly('SOFTWARE\Microsoft\Windows NT\CurrentVersion');
          AppSystemStr := Format('%s %s Version %s Build %s.%d', [
            ReadString('ProductName'),
            AppSystemStr,
            ReadString('DisplayVersion').IsEmpty.Select(
              ReadString('ReleaseId'),
              ReadString('DisplayVersion') + ' (' + ReadString('ReleaseId') + ')'),
            ReadString('CurrentBuild'),
            ReadInteger('UBR')]);
          except
          end;
        Free;
        end;
    {$EndIf}

    with fvi do
      begin
      _info.Add(InternalName);

      with ParseVersion(FileVersion) do
        begin
        TryStrToDate({$INCLUDE %DATE%}, _bldTime, 'YYYY/MM/DD', '/');
        AppVersionStr := Format('%d.%d.%d', [Major, Minor, Revision]);
        _info.Add(Format(ABOUT_VERSION, [AppVersionStr]) + (_arc <> '').Select(', ' + _arc, ''));
        _info.Add(Format(ABOUT_BUILD, [Build, FormatDateTime('yyyy.mm.dd', _bldTime)]));
        end;

      Result := _info.Text;
      end;

    _info.Free;
  end;

function UpdateAboutAppInfo: String;
  var
    _info: TStringList;
  begin
    _info  := TStringList.Create;
    Result := '';

    with fvi do
      begin
      _info.Add('');
      _info.Add(Comments);
      _info.Add('');
      _info.Add('© ' + LegalCopyright);
      _info.Add(CompanyName);
      _info.Add('');
      _info.Add(ABOUT_RIGHTS);
      _info.Add(ABOUT_OPENSRC);
      _info.Add('');

      {$IfDef DEBUG}
      _info.Add('DEBUG VERSION');
      {$EndIf}

      Result := UpdateAboutAppShort + _info.Text;

      AppProductStr := ProductName;
      end;

    _info.Free;
  end;


function GetAppName: String;
  begin
    Result := fvi.ProductName;
  end;

function GetAppVersion: String;
  begin
    Result := fvi.FileVersion;
  end;

function GetAppNameVersion: String;
  begin
    Result := Format('%s v%s', [fvi.ProductName, fvi.FileVersion]);
  end;

function GetAppCompanyName: String;
  begin
    Result := fvi.CompanyName;
  end;

function GetAppNameAuthor: String;
  begin
    Result := Format('%s, © %s', [fvi.InternalName, GetAuthorName(fvi.LegalCopyright)]);
  end;


initialization
  fvi := TFileVersionInfoSimple.Create;
  fvi.ReadVersionInfo;

end.
