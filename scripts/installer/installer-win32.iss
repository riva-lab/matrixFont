#define appArch "32"

#include "installer-common.iss"

[Files]

; Application files
Source: "{#appPath}\bin\libcrypto-1_1.dll";     DestDir: "{app}\bin"; Flags: ignoreversion
Source: "{#appPath}\bin\libssl-1_1.dll";        DestDir: "{app}\bin"; Flags: ignoreversion
