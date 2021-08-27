unit cOpenFileList;

{$mode objfpc}{$H+}

interface

uses
  LazFileUtils;

const
  LAST_FILES_LIST_SIZE = 10; // кол-во элементов в списке последних открытых файлов

type

  { TOpenFileList }
  { Класс-список последних открытых файлов, без дублирования записей }

  TOpenFileList = class
  PRIVATE
    FFileName: array [0..LAST_FILES_LIST_SIZE - 1] of String;
    FFilePath: array [0..LAST_FILES_LIST_SIZE - 1] of String;
    FCount:    Integer;

  PRIVATE
    function GetFileName(Index: Integer): String;
    function GetFilePath(Index: Integer): String;
    procedure SetFilePath(Index: Integer; AValue: String);

  PUBLIC
    procedure Clear;

    constructor Create;
    destructor Destroy; OVERRIDE;

  PUBLIC
    property FileName[Index: Integer]: String read GetFileName;
    property FilePath[Index: Integer]: String read GetFilePath write SetFilePath;
  end;



implementation


{ TOpenFileList }

function TOpenFileList.GetFileName(Index: Integer): String;
  begin
    if Index < LAST_FILES_LIST_SIZE then
      Result := FFileName[Index] else
      Result := '';
  end;

function TOpenFileList.GetFilePath(Index: Integer): String;
  begin
    if Index < LAST_FILES_LIST_SIZE then
      Result := FFilePath[Index] else
      Result := '';
  end;

procedure TOpenFileList.SetFilePath(Index: Integer; AValue: String);
  var
    i, cnt: Integer;
  begin
    if AValue = '' then Exit;
    if not FileExistsUTF8(AValue) then Exit;

    for cnt := 0 to LAST_FILES_LIST_SIZE - 1 do
      if AValue = FFilePath[cnt] then
        break;

    for i := cnt downto 1 do
      FFilePath[i] := FFilePath[i - 1];

    FFilePath[0] := AValue;

    for i := 0 to LAST_FILES_LIST_SIZE - 1 do
      FFileName[i] := ExtractFileNameOnly(FFilePath[i]);
  end;

procedure TOpenFileList.Clear;
  var
    i: Integer;
  begin
    FCount := 0;
    for i := 0 to LAST_FILES_LIST_SIZE - 1 do
      begin
      FFileName[i] := '';
      FFilePath[i] := '';
      end;
  end;

constructor TOpenFileList.Create;
  begin
    Clear;
  end;

destructor TOpenFileList.Destroy;
  begin
    inherited Destroy;
  end;


end.




