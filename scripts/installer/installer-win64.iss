#define appArch "64"

[Setup]
ArchitecturesInstallIn64BitMode = x64
ArchitecturesAllowed            = x64

#include "installer-common.iss"

[Files]

; Application files
Source: "{#appPath}\bin\libcrypto-1_1-x64.dll";     DestDir: "{app}\bin"; Flags: ignoreversion
Source: "{#appPath}\bin\libssl-1_1-x64.dll";        DestDir: "{app}\bin"; Flags: ignoreversion
