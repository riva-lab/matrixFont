unit u_utilities;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, LazFileUtils, LazUTF8, ShellApi;


 // удаление указанного файла, по умолчанию - в корзину
 //function FileRemove(AFileName: String; toTrash: Boolean = True): Boolean;

// открытие указанного пути в проводнике
procedure RootOpenInExplorer(ARoot: String);

// вывод массива байт в строку в HEX виде
function BytesToHex(AData: TBytes; ABefore: String = '0x'; AAfter: String = ' ';
  ABytesPerString: Byte = 8): String;

function CheckBoolean(ABool: Boolean; ValueTrue, ValueFalse: Variant): Variant;

// проверяет значение переменной на вхождение в диапазон
function InRange(AValue, AMin, AMax: Variant): Boolean;

// проверка расширения файла, регистронезависимая
function FileExtCheck(AFilename, AExtList: String): Boolean;


implementation

 //function FileRemove(AFileName: String; toTrash: Boolean): Boolean;
 //  var
 //    FileOp: TSHFileOpStruct;
 //  begin
 //    Result := False;
 //    if not FileExistsUTF8(AFileName) then Exit;
 //    if not toTrash then Exit(DeleteFileUTF8(AFileName));
 //
 //    if AFileName <> '' then
 //      begin
 //      FillChar(FileOp, SizeOf(FileOp), 0);
 //      FileOp.Wnd    := 0;
 //      FileOp.wFunc  := FO_DELETE;
 //      FileOp.pFrom  := PChar(UTF8ToWinCP(AFileName) + #0#0);
 //      FileOp.pTo    := nil;
 //      FileOp.fFlags := FOF_ALLOWUNDO or FOF_NOERRORUI or FOF_SILENT; // or FOF_NOCONFIRMATION;
 //      Result        := (SHFileOperation(FileOp) = 0) and (not
 //        FileOp.fAnyOperationsAborted);
 //      end;
 //  end;

procedure RootOpenInExplorer(ARoot: String);
  begin
    if ARoot = '' then
      ARoot := ParamStrUTF8(0);

    if not DirectoryExistsUTF8(ARoot) then
      ARoot := ExtractFileDir(ARoot);

    if DirectoryExistsUTF8(ARoot) then
    {$IfDef WINDOWS}
      ExecuteProcess('explorer.exe', UTF8ToWinCP(ARoot + DirectorySeparator), []);
    {$Else}
    ;
    {$EndIf}
  end;

function BytesToHex(AData: TBytes; ABefore: String; AAfter: String; ABytesPerString: Byte): String;
  var
    i: Integer;
  begin
    Result := '';

    if Length(AData) > 0 then
      for i := 0 to High(AData) do
        begin
        Result += ABefore + IntToHex(AData[i], 2) + AAfter;

        if (ABytesPerString > 0) and (i mod ABytesPerString = ABytesPerString - 1) then
          Result += #13#10;
        end;
  end;

function CheckBoolean(ABool: Boolean; ValueTrue, ValueFalse: Variant): Variant;
  begin
    if ABool then
      Result := ValueTrue else
      Result := ValueFalse;
  end;

function InRange(AValue, AMin, AMax: Variant): Boolean;
  begin
    Result := (AValue >= AMin) and (AValue <= AMax);
  end;

function FileExtCheck(AFilename, AExtList: String): Boolean;
  var
    i: Integer;
    s: String = '';
  begin
    AExtList += ' ';

    for i := 1 to Length(AExtList) do
      if AExtList[i] in [' ', ';', ','] then
        begin
        if CompareFileExt(AFileName, s, False) = 0 then Exit(True);
        s := '';
        end
      else
        s += AExtList[i];

    Result := False;
  end;

end.
