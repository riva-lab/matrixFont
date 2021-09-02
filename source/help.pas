unit help;

{$mode objfpc}{$H+}

interface

uses
  Windows, LCLIntf, ShellApi;

const
  HELP_FILE   = 'matrixFont-help.html';
  HELP_ENGINE = 'hh.exe';

procedure HelpOpenDefault;
procedure HelpOpenTopic(TopicName: String);

implementation

procedure HelpOpenDefault;
  begin
    OpenURL(HELP_FILE);
  end;

procedure HelpOpenTopic(TopicName: String);
  begin
    ShellExecute(0, 'open', StringToOleStr(HELP_ENGINE),
      StringToOleStr(HELP_FILE + '::/' + TopicName), nil, SW_SHOWNORMAL);
  end;

end.

