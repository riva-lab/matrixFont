unit u_utilities;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8, StdCtrls, Spin, LCLType, LResources;


 // проверка расширения файла, регистронезависимая
function FileExtCheck(AFilename, AExtList: String): Boolean;

 // убирает из строки копирайта даты
function GetAuthorName(ACopyrightStr: String): String;

// установка значения в компонент без вызова обработчика по изменению
procedure SetValueWithoutAction(AComponent: TComponent; AValue: Integer);

 // get the specified resource RCDATA as a string
function GetResourceAsString(AResName: String): String;


implementation


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

function GetResourceAsString(AResName: String): String;
  var
    resStream: TResourceStream;
  begin
    // create a resource stream which points to our resource
    resStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);

      try
      with TStringStream.Create do
        try
        LoadFromStream(resStream);
        Result := DataString;
        Free;
        except
        Result := '';
        end;

      finally
      resStream.Free; // destroy the resource stream
      end;
  end;


end.
