unit u_helpers;

{$mode objfpc}{$H+}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8, ShellApi, FPImage,
  StrUtils, Graphics, StdCtrls, LResources, LCLType, Controls;


type

  { TBooleanHelperCustom }

  TBooleanHelperCustom = type Helper(TBooleanHelper) for Boolean
  public
    function Select(ValueIfTrue, ValueIfFalse: Variant): Variant; inline;
    function Select(ValueIfTrue, ValueIfFalse: Pointer): Pointer; inline;
  end;


  { TStringHelperCustom }

  TStringHelperCustom = type helper(TStringHelper) for string
  public
    function ToHex(ALineBytes: Integer = 8; ABlockBytes: Integer = 4): String;
    function ToCodes(ASysOut: Integer; ALineChars: Integer): String;
    function FromToCodes(ASysIn: Integer; ASysOut: Integer = 0): String;
    function FirstLowCase: String;
  end;


  { TIntegerHelperCustom }

  TIntegerHelperCustom = type Helper(TIntegerHelper) for Integer
  public
    function SizeInBytes(ASingle, AMul1: String; AMul2to4: String = ''; AMul5to10: String = ''): String;
    function SizeInBytes(AByte, AKilo, AMega, AGiga: String; ADecimal: Boolean): String;
    function PostInc: Integer; inline;
    function PreInc: Integer; inline;
  end;


  { TInt64HelperCustom }

  TInt64HelperCustom = type Helper(TInt64Helper) for Int64
  public
    function SizeInBytes(ASingle, AMul1: String; AMul2to4: String = ''; AMul5to10: String = ''): String;
    function SizeInBytes(AByte, AKilo, AMega, AGiga: String; ADecimal: Boolean): String;
  end;


implementation


{ TBooleanHelperCustom }

function TBooleanHelperCustom.Select(ValueIfTrue, ValueIfFalse: Variant): Variant;
  begin
    if Self then
      Result := ValueIfTrue
    else
      Result := ValueIfFalse;
  end;

function TBooleanHelperCustom.Select(ValueIfTrue, ValueIfFalse: Pointer): Pointer;
  begin
    if Self then
      Result := ValueIfTrue
    else
      Result := ValueIfFalse;
  end;

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


{ TStringHelperCustom }

function TStringHelperCustom.ToHex(ALineBytes: Integer; ABlockBytes: Integer): String;
  var
    i, m, len: Integer;
  begin
    if ALineBytes = 0 then Exit(Self);
    if ABlockBytes = 0 then Exit(Self);

    Result := '';
    len    := Length;
    if len > 0 then
      for i := 1 to len do
          try
          Result += Ord(Self[i]).ToHexString(2);
          finally
          m      := i mod ALineBytes;

          if m = 0 then
            Result += LineEnding
          else
          if i < len then
            begin
            Result   += ' ';
            if m mod ABlockBytes = 0 then
              Result += ' ';
            end;
          end;
  end;

function TStringHelperCustom.ToCodes(ASysOut: Integer; ALineChars: Integer): String;
  var
    i, l, cnt, tmp: Integer;
  begin
    cnt    := 0;
    l      := Length;
    Result := '';

    if l > 0 then
      begin
      for i := 1 to l do
          try

          case ASysOut of
            0:
              begin
              Result += Self[i];
              cnt    += 1;
              end;
            2:
              begin
              Result += IntToBin(Ord(Self[i]), 8);
              cnt    += 9;
              end;
            10:
              begin
              tmp    := Ord(Self[i]);
              Result += tmp.ToString;
              cnt    += 1;
              while tmp > 0 do
                begin
                cnt += 1;
                tmp := tmp div 10;
                end;
              end;
            16:
              begin
              Result += Ord(Self[i]).ToHexString(2);
              cnt    += 3;
              end;
            end;

          finally

          //if (ASysOut > 0) and (ALineChars > 0) then
          if cnt >= ALineChars - 1 then
            begin
            cnt    := 0;
            Result += LineEnding;
            end
          else
            Result += ' ';

          end;

      if (Result.Length > 0) and (ASysOut > 0) then
        while Result[Result.Length] = ' ' do
          Delete(Result, Result.Length, 1);
      end;
  end;

function TStringHelperCustom.FromToCodes(ASysIn: Integer; ASysOut: Integer): String;
  var
    i, l, ch_out: Integer;
    ch:           Char;
    prefix, tmp:  String;
    symbols:      set of Char;
    delimiters:   set of Char = [' ', ',', ';', '-', #13, #10];
  begin
    case ASysIn of
      2:
        begin
        prefix  := '%';
        symbols := ['0'..'1'];
        end;

      10:
        begin
        prefix  := '';
        symbols := ['0'..'9'];
        end;

      16:
        begin
        prefix  := '$';
        symbols := ['0'..'9', 'A'..'F'];
        end;
      end;

    Self   := UpperCase(Self);
    l      := Length;
    Result := '';
    tmp    := '';

    if l > 0 then
      begin
      for i := 1 to l do
        begin
        ch := Self[i];

        if ch in symbols then
          tmp += ch;

        if ((ch in delimiters) or (i = l)) and (tmp <> '') then
            try
            ch_out       := (prefix + tmp).ToInteger;
            case ASysOut of
              2: Result  += IntToBin(ch_out, 8) + ' ';
              10: Result += ch_out.ToString + ' ';
              16: Result += ch_out.ToHexString(2) + ' ';
              else
                Result   += chr(ch_out);
              end;
            finally
            tmp := '';
            end;
        end;

      if (Result.Length > 0) and (ASysOut > 0) then
        while Result[Result.Length] = ' ' do
          Delete(Result, Result.Length, 1);
      end;
  end;

function TStringHelperCustom.FirstLowCase: String;
  begin
    if Length = 0 then
      Result := ''
    else
      Result := UTF8LowerCase(UTF8Copy(Self, 1, 1)) + UTF8Copy(Self, 2, Length - 1);
  end;


{ TIntegerHelperCustom }

function TIntegerHelperCustom.SizeInBytes(ASingle, AMul1: String;
  AMul2to4: String; AMul5to10: String): String;
  begin
    Result := Int64(Self).SizeInBytes(ASingle, AMul1, AMul2to4, AMul5to10);
  end;

function TIntegerHelperCustom.SizeInBytes(AByte, AKilo, AMega, AGiga: String;
  ADecimal: Boolean): String;
  begin
    Result := Int64(Self).SizeInBytes(AByte, AKilo, AMega, AGiga, ADecimal);
  end;

function TIntegerHelperCustom.PostInc: Integer;
  begin
    Result := Self;
    Self   += 1;
  end;

function TIntegerHelperCustom.PreInc: Integer;
  begin
    Self   += 1;
    Result := Self;
  end;


{ TInt64HelperCustom }

function TInt64HelperCustom.SizeInBytes(ASingle, AMul1: String;
  AMul2to4: String; AMul5to10: String): String;
  begin
    Result := ToString + ' ';

    if Self > 1000000 then
      Result := Result.Insert(Result.Length - 7, ' ');

    if Self > 1000 then
      Result := Result.Insert(Result.Length - 4, ' ');

    if AMul2to4 = '' then AMul2to4   := AMul1;
    if AMul5to10 = '' then AMul5to10 := AMul1;

    if Self = 1 then
      Result += ASingle
    else
    if (Self mod 100) in [10..20] then
      Result += AMul5to10
    else
      case Self mod 10 of
        1:
          Result += AMul1;
        2..4:
          Result += AMul2to4
        else
          Result += AMul5to10;
        end;
  end;

function TInt64HelperCustom.SizeInBytes(AByte, AKilo, AMega, AGiga: String;
  ADecimal: Boolean): String;
  var
    p:    Integer = 0;
    base: Integer;
    r:    Int64;
  begin
    base := ADecimal.Select(1000, 1024);

    while Self >= base do
      begin
      r    := Self mod base;
      Self := Self div base;
      p.PostInc;
      end;

    Result := Self.ToString;

    if (p > 0) and (Self < 100) then
      Result += DefaultFormatSettings.DecimalSeparator + (10 * r div base).ToString;

    Result += ' ';

    case p of
      0: Result += AByte;
      1: Result += AKilo;
      2: Result += AMega;
      3: Result += AGiga;
      end;
  end;

end.
