unit u_utilities;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8, ShellApi, StdCtrls, Spin;


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

 // извлечение названия языка из языковой строки
function GetLangCaption(ALangStr: String; ADelimiter: Char = ','): String;

 // извлечение кода языка (ex: en_en) из языковой строки
function GetLangCode(ALangStr: String; ADelimiter: Char = ','): String;

 // убирает из строки копирайта даты
function GetAuthorName(ACopyrightStr: String): String;

// обновление выпадающего списка значениями из массива
procedure UpdateComboBox(AComponent: TComboBox; AList: array of String);

// установка значения в компонент без вызова обработчика по изменению
procedure SetValueWithoutAction(AComponent: TComponent; AValue: Integer);


implementation

{
 function FileRemove(AFileName: String; toTrash: Boolean): Boolean;
   var
     FileOp: TSHFileOpStruct;
   begin
     Result := False;
     if not FileExistsUTF8(AFileName) then Exit;
     if not toTrash then Exit(DeleteFileUTF8(AFileName));

     if AFileName <> '' then
       begin
       FillChar(FileOp, SizeOf(FileOp), 0);
       FileOp.Wnd    := 0;
       FileOp.wFunc  := FO_DELETE;
       FileOp.pFrom  := PChar(UTF8ToWinCP(AFileName) + #0#0);
       FileOp.pTo    := nil;
       FileOp.fFlags := FOF_ALLOWUNDO or FOF_NOERRORUI or FOF_SILENT; // or FOF_NOCONFIRMATION;
       Result        := (SHFileOperation(FileOp) = 0) and (not
         FileOp.fAnyOperationsAborted);
       end;
   end;
}

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

function GetLangCaption(ALangStr: String; ADelimiter: Char): String;
  begin
    Result := ALangStr.Remove(0, ALangStr.IndexOf(ADelimiter) + 1).Trim;
  end;

function GetLangCode(ALangStr: String; ADelimiter: Char): String;
  begin
    Result := ALangStr.Remove(ALangStr.IndexOf(ADelimiter)).ToLower;
  end;

function GetAuthorName(ACopyrightStr: String): String;
  var
    i: Integer;
  begin
    for i := 1 to ACopyrightStr.Length do
      if not (ACopyrightStr[i] in ['0'..'9']) then
        Result += ACopyrightStr[i];

    for i := ACopyrightStr.Length downto 1 do
      if LowerCase(ACopyrightStr[i]) in ['a'..'z'] then
        Exit(Result.Remove(i));
  end;

// обновление выпадающего списка значениями из массива
procedure UpdateComboBox(AComponent: TComboBox; AList: array of String);
  var
    i, index: Integer;
  begin
    with AComponent do
      begin
      index := ItemIndex;
      if index < 0 then index := 0;
      Clear;
      for i := 0 to High(AList) do
        Items.Append(AList[i]);
      ItemIndex := index;
      end;
  end;

// установка значения в компонент без вызова обработчика по изменению
procedure SetValueWithoutAction(AComponent: TComponent; AValue: Integer);
  var
    tmp: TNotifyEvent;
  begin
    case AComponent.ClassName of

      'TComboBox':
        with TComboBox(AComponent) do
          begin
          tmp       := OnChange;
          OnChange  := nil;
          ItemIndex := AValue;
          OnChange  := tmp;
          end;

      'TSpinEdit':
        with TSpinEdit(AComponent) do
          begin
          tmp      := OnChange;
          OnChange := nil;
          Value    := AValue;
          OnChange := tmp;
          end;
      end;
  end;

end.
