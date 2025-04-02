; This script isn't full and therefore not compiled.
; This script provides common part of full work script.
; Include this file via #include directive in work script.


; Project data
#define appPath         "..\.."
#define appName         "matrixFont"
#define appNameShort    "matrixFont"
#define appCopyright    "Copyright (c) 2015-2024 Riva"
#define appPublisher    "Riva Lab"
#define appURL          "https://riva-lab.gitlab.io/en/apps/matrixFont/"
#define appUpdatesURL   "https://gitlab.com/riva-lab/matrixFont/-/releases"
#define appExeName      "matrixFont-win" + appArch + "-Release.exe"

#define _version        0
#define _vMajor         0
#define _vMinor         0
#define _vRev           0
#define _vBuild         0
#define _getVer         GetPackedVersion(appPath + '\bin\' + appExeName, _version)
#define _unpackVer      UnpackVersionComponents(_version, _vMajor, _vMinor, _vRev, _vBuild)
#define appVer3         Str(_vMajor) + "." + Str(_vMinor) + "." + Str(_vRev)
#define appVersion      GetVersionNumbersString(appPath + '\bin\' + appExeName)

#define appOutputDir    "\install\v" + appVersion
#define appOutputFile   "matrixFont-v"+ appVersion + "-win" + appArch + "-Setup"


[Setup]
AppId               = {#appPublisher}_{#appName}_desktop-win
AppName             = {#appName}
AppVersion          = {#appVersion}
AppVerName          = {#appName} {#appVer3}
AppPublisher        = {#appPublisher}
AppPublisherURL     = {#appURL}
AppSupportURL       = {#appURL}
AppUpdatesURL       = {#appUpdatesURL}
AppCopyright        = {#appCopyright}
AppReadmeFile       = {#appPath}\readme.html
LicenseFile         = {#appPath}\license.md
;InfoBeforeFile      = info.txt

WizardStyle         = modern
DisableWelcomePage  = no
UsePreviousAppDir   = yes
AllowNoIcons        = yes
DefaultDirName      = {autopf}\{#appName}
DefaultGroupName    = {#appName}

SetupIconFile       = {#appPath}\resources\icon\install.ico
UninstallDisplayIcon= {app}\bin\{#appExeName}
OutputDir           = {#appPath}\{#appOutputDir}
OutputBaseFilename  = {#appOutputFile}
Compression         = lzma
SolidCompression    = yes
VersionInfoVersion  = {#appVersion}

PrivilegesRequired                  = lowest
PrivilegesRequiredOverridesAllowed  = dialog


[Languages]
Name: "english";                MessagesFile: "compiler:Default.isl"; InfoBeforeFile: "info-en.rtf"
; Name: "armenian";               MessagesFile: "compiler:Languages\Armenian.isl"
; Name: "brazilianportuguese";    MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"
; Name: "bulgarian";              MessagesFile: "compiler:Languages\Bulgarian.isl"
; Name: "catalan";                MessagesFile: "compiler:Languages\Catalan.isl"
; Name: "corsican";               MessagesFile: "compiler:Languages\Corsican.isl"
; Name: "czech";                  MessagesFile: "compiler:Languages\Czech.isl"
; Name: "danish";                 MessagesFile: "compiler:Languages\Danish.isl"
; Name: "dutch";                  MessagesFile: "compiler:Languages\Dutch.isl"
; Name: "finnish";                MessagesFile: "compiler:Languages\Finnish.isl"
; Name: "french";                 MessagesFile: "compiler:Languages\French.isl"
; Name: "german";                 MessagesFile: "compiler:Languages\German.isl"
; Name: "hebrew";                 MessagesFile: "compiler:Languages\Hebrew.isl"
; Name: "hungarian";              MessagesFile: "compiler:Languages\Hungarian.isl"
; Name: "icelandic";              MessagesFile: "compiler:Languages\Icelandic.isl"
; Name: "italian";                MessagesFile: "compiler:Languages\Italian.isl"
; Name: "japanese";               MessagesFile: "compiler:Languages\Japanese.isl"
; Name: "norwegian";              MessagesFile: "compiler:Languages\Norwegian.isl"
; Name: "polish";                 MessagesFile: "compiler:Languages\Polish.isl"
; Name: "portuguese";             MessagesFile: "compiler:Languages\Portuguese.isl"
Name: "russian";                MessagesFile: "compiler:Languages\Russian.isl"; InfoBeforeFile: "info-ru.rtf"
; Name: "slovak";                 MessagesFile: "compiler:Languages\Slovak.isl"
; Name: "slovenian";              MessagesFile: "compiler:Languages\Slovenian.isl"
; Name: "spanish";                MessagesFile: "compiler:Languages\Spanish.isl"
; Name: "turkish";                MessagesFile: "compiler:Languages\Turkish.isl"
; Name: "ukrainian";              MessagesFile: "compiler:Languages\Ukrainian.isl"


[Components]
Name: "Application";            Description: "Application";     Types: full compact custom; Flags: fixed
Name: "Help";                   Description: "Help";            Types: full
Name: "Translations";           Description: "Localizations";   Types: full
Name: "Translations\English";   Description: "English";         Types: full
Name: "Translations\Russian";   Description: "Russian";         Types: full compact custom; Flags: fixed


[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"


[Dirs]
Name: "{app}\bin\lang"; Permissions: users-modify


[Files]

; Application files
Source: "{#appPath}\bin\{#appExeName}";         DestDir: "{app}\bin"; Flags: ignoreversion
Source: "{#appPath}\bin\openssl-license.txt";   DestDir: "{app}\bin"; Flags: ignoreversion
Source: "settings.ini";                         DestDir: "{app}\bin"; Flags: ignoreversion; Permissions: users-modify

; Localization files
Source: "{#appPath}\bin\lang\*.ini";            DestDir: "{app}\bin\lang"; Flags: ignoreversion
Source: "{#appPath}\bin\lang\{#appName}.pot";   DestDir: "{app}\bin\lang"; Flags: ignoreversion
Source: "{#appPath}\bin\lang\{#appName}.en.po"; DestDir: "{app}\bin\lang"; Flags: ignoreversion; Components: Translations\English
Source: "{#appPath}\bin\lang\{#appName}.ru.po"; DestDir: "{app}\bin\lang"; Flags: ignoreversion; Components: Translations\Russian

; Common files for readme and help
Source: "{#appPath}\help\css\*";                            DestDir: "{app}\help\css";                  Flags: ignoreversion recursesubdirs
Source: "{#appPath}\help\js\*";                             DestDir: "{app}\help\js";                   Flags: ignoreversion recursesubdirs
Source: "{#appPath}\help\light\screenshots\matrixFont.png"; DestDir: "{app}\help\light\screenshots";    Flags: ignoreversion recursesubdirs
Source: "{#appPath}\help\dark\screenshots\matrixFont.png";  DestDir: "{app}\help\dark\screenshots";     Flags: ignoreversion recursesubdirs

; Help files
Source: "{#appPath}\help\*";        Excludes: "*template.html"; DestDir: "{app}\help"; Flags: ignoreversion recursesubdirs; Components: Help

; Info files
Source: "{#appPath}\readme.md";     DestDir: "{app}"; Flags: ignoreversion
Source: "{#appPath}\readme.html";   DestDir: "{app}"; Flags: ignoreversion
Source: "{#appPath}\readme.en.*";   DestDir: "{app}"; Flags: ignoreversion; Components: Translations\English
Source: "{#appPath}\license.*";     DestDir: "{app}"; Flags: ignoreversion
Source: "{#appPath}\versions.*";    DestDir: "{app}"; Flags: ignoreversion


[Icons]
Name: "{group}\{#appName}";                         Filename: "{app}\bin\{#appExeName}"
Name: "{group}\Help (Rus)";                         Filename: "{app}\help\{#appNameShort}-help.html"; Components: Help
Name: "{group}\Readme";                             Filename: "{app}\readme.html"
Name: "{group}\{cm:UninstallProgram,{#appName}}";   Filename: "{uninstallexe}"
Name: "{autodesktop}\{#appName}";                   Filename: "{app}\bin\{#appExeName}"; Tasks: desktopicon


[Run]
Filename: "{app}\bin\{#appExeName}";    Description: "{cm:LaunchProgram,{#StringChange(appName, '&', '&&')}}";      Flags: nowait postinstall skipifsilent
Filename: "{#appURL}";                  Description: "{cm:ProgramOnTheWeb,{#StringChange(appName, '&', '&&')}}";    Flags: nowait postinstall skipifsilent shellexec unchecked
Filename: "{app}\readme.html";          Description: "Readme";                                                      Flags: nowait postinstall skipifsilent shellexec unchecked


[UninstallDelete]
Type: files;        Name: "{app}\bin\lang\*.po"
Type: files;        Name: "{app}\bin\lang\*.pot"
Type: files;        Name: "{app}\bin\lang\*.ini"
Type: dirifempty;   Name: "{app}\bin\lang"
Type: files;        Name: "{app}\bin\settings.ini"
Type: dirifempty;   Name: "{app}"