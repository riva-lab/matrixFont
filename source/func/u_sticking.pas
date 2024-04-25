unit u_sticking;
{ Class TStickingForms. Precreated instance StickingFormsEx.
  Used for convenient manage of sticking forms to main form.
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Windows, Types, u_helpers;

resourcestring
  STFM_ANCHOR_L  = 'Слева';
  STFM_ANCHOR_R  = 'Справа';
  STFM_ANCHOR_T  = 'Сверху';
  STFM_ANCHOR_B  = 'Снизу';

  STFM_SIZE_NORM = 'По окну';
  STFM_SIZE_FULL = 'Полностью';

type

  { TStickingForm }

  TStickingForm = class
  private
    function GetConfig: String;

    procedure SetConfig(AValue: String);

  public
    Form:     TForm;
    Name:     String;
    Anchor:   TAnchorKind;
    FullSize: Boolean;
    Enable:   Boolean;

    property Config: String read GetConfig write SetConfig;

    constructor Create(AForm: TForm = nil; AName: String = '');
    destructor Destroy; override;

    procedure Init(AAnchor: TAnchorKind; AFullSize, AEnable: Boolean);
    procedure Stick(ABorder, W0, H0: Integer; AOrigin: TRect; var X: TRect);
    procedure StickToRightBottomEdge(ABorder: Integer; AOrigin: TRect);
  end;

  { TStickingForms }

  TStickingForms = class(TList)
  private
    FMainForm: TForm;
    FBorder:   Integer;

    function FindForm(AForm: TForm): Integer;
    function FindForm(AFormName: String): Integer;

    function GetConfig: String;
    function GetForm(Index: Integer): TStickingForm;
    function GetForm(AForm: TForm): TStickingForm;
    function GetFormShadowCorrectionValue: Integer;

    procedure SetConfig(AValue: String);
    procedure SetMainForm(AValue: TForm);

  public
    UseBorderCorrection: Boolean;

    property FormByIndex[Index: Integer]: TStickingForm read GetForm;
    property Form[AForm: TForm]: TStickingForm read GetForm;
    property MainForm: TForm read FMainForm write SetMainForm;
    property Config: String read GetConfig write SetConfig;

    constructor Create;
    destructor Destroy; override;

    procedure Add(AForm: TForm; AName: String = '');
    procedure Rename(AForm: TForm; ANewName: String);
    procedure Order(AForm: TForm; AOrder: Integer);
    procedure Stick;
    procedure StickToRightBottomEdge;
  end;


const
  STFM_ANCHOR_TXT: array[TAnchorKind] of String =
    (STFM_ANCHOR_T, STFM_ANCHOR_L, STFM_ANCHOR_R, STFM_ANCHOR_B);

  STFM_SIZE_TXT: array[Boolean] of String =
    (STFM_SIZE_NORM, STFM_SIZE_FULL);


var
  StickingFormsEx: TStickingForms;


implementation


{ TStickingForm }

function TStickingForm.GetConfig: String;
  begin
    Result := '';

    with TStringList.Create do
      begin
      Add(IntToStr(Ord(Anchor)));
      Add(FullSize.Select('1', '0'));
      Add(Enable.Select('1', '0'));
      Result := CommaText;
      Free;
      end;
  end;

procedure TStickingForm.SetConfig(AValue: String);
  begin
    with TStringList.Create do
      begin
      CommaText := AValue;
      if Count = 3 then
        try
        Anchor   := TAnchorKind(StrToInt64Def(Strings[0], 0));
        FullSize := Strings[1] <> '0';
        Enable   := Strings[2] <> '0';
        except
        end;
      Free;
      end;
  end;

constructor TStickingForm.Create(AForm: TForm; AName: String);
  begin
    Form := AForm;
    Name := AName;
    Init(akRight, False, False);
  end;

destructor TStickingForm.Destroy;
  begin
    inherited Destroy;
  end;

procedure TStickingForm.Init(AAnchor: TAnchorKind; AFullSize, AEnable: Boolean);
  begin
    Anchor   := AAnchor;
    FullSize := AFullSize;
    Enable   := AEnable;
  end;

procedure TStickingForm.Stick(ABorder, W0, H0: Integer; AOrigin: TRect; var X: TRect);
  var
    FR: TRect;
  begin
    if not Enable or not Assigned(Form) or not Form.Visible then Exit;

    with Form do
      begin
      GetWindowRect(Handle, FR);

      if Anchor in [akLeft, akRight] then
        begin
        Top    := AOrigin.Top - X.Top + ABorder;
        Height := H0 + FullSize.Select(X.Top + X.Bottom, 0);
        end
      else
        begin
        Left  := AOrigin.Left - X.Left + ABorder;
        Width := W0 + FullSize.Select(X.Left + X.Right, 0);
        end;

      case Anchor of

        akLeft:
          begin
          Left   := AOrigin.Left - X.Left - FR.Width + 3 * ABorder;
          X.Left := X.Left + FR.Width - 2 * ABorder;
          end;

        akRight:
          begin
          Left    := AOrigin.Right + X.Right - ABorder;
          X.Right := X.Right + FR.Width - 2 * ABorder;
          end;

        akTop:
          begin
          Top   := AOrigin.Top - X.Top - FR.Height + 2 * ABorder;
          X.Top := X.Top + FR.Height - ABorder;
          end;

        akBottom:
          begin
          Top      := AOrigin.Bottom + X.Bottom;
          X.Bottom := X.Bottom + FR.Height - ABorder;
          end;
        end;
      end;
  end;

procedure TStickingForm.StickToRightBottomEdge(ABorder: Integer; AOrigin: TRect);
  var
    FR: TRect;
  begin
    if not Enable or not Assigned(Form) or not Form.Visible then Exit;

    with Form do
      begin
      GetWindowRect(Handle, FR);
      Top  := AOrigin.Bottom + AOrigin.Top - FR.Height + ABorder;
      Left := AOrigin.Right + AOrigin.Left - FR.Width + ABorder;
      end;
  end;


{ TStickingForms }

function TStickingForms.FindForm(AForm: TForm): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    if Count > 0 then
      for i := 0 to Count - 1 do
        if Form[i].Form = AForm then Exit(i);
  end;

function TStickingForms.FindForm(AFormName: String): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    if Count > 0 then
      for i := 0 to Count - 1 do
        if Form[i].Form.Name = AFormName then Exit(i);
  end;

function TStickingForms.GetConfig: String;
  var
    i: Integer;
  begin
    Result := '';

    if Count = 0 then Exit;
    for i := 0 to Count - 1 do
      Result +=
        TStickingForm(Items[i]).Form.Name + '=' +
        TStickingForm(Items[i]).Config + '|';
  end;

function TStickingForms.GetForm(Index: Integer): TStickingForm;
  begin
    if Count > 0 then
      if Index in [0..Count - 1] then
        Result := TStickingForm(Items[Index]) else
        Result := nil;
  end;

function TStickingForms.GetForm(AForm: TForm): TStickingForm;
  var
    i: Integer;
  begin
    i := FindForm(AForm);

    if i < 0 then
      Result := nil else
      Result := Form[i];
  end;

function TStickingForms.GetFormShadowCorrectionValue: Integer;
  var
    pvParam: BOOL;
  begin
    SystemParametersInfo(SPI_GETDROPSHADOW, 0, @pvParam, 0);
    Result := Boolean(UseBorderCorrection xor (pvParam or (Win32BuildNumber < 6000 {WinVista or lower})))
      .Select(0, GetSystemMetrics(SM_CXSIZEFRAME));
  end;

procedure TStickingForms.SetConfig(AValue: String);
  var
    a, b: TStringArray;
    i:    Integer;
  begin
    if Count = 0 then Exit;
    if AValue.IsEmpty then Exit;
    a := AValue.Split('|');

    for i := 0 to Count - 1 do
      if i < Length(a) then
        begin
        b := a[i].Split('=');
        if Length(b) < 2 then Continue;
        Order(TStickingForm(Items[FindForm(b[0])]).Form, i);
        TStickingForm(Items[i]).Config := b[1];
        end;
  end;

procedure TStickingForms.SetMainForm(AValue: TForm);
  begin
    if FMainForm = AValue then Exit;
    FMainForm := AValue;
  end;

constructor TStickingForms.Create;
  begin
    inherited Create;

    UseBorderCorrection := False;
  end;

destructor TStickingForms.Destroy;
  var
    i: Integer;
  begin
    if Count > 0 then
      for i := 0 to Count - 1 do
        TStickingForm(Items[i]).Free;

    inherited Destroy;
  end;

procedure TStickingForms.Add(AForm: TForm; AName: String);
  var
    item: TStickingForm;
  begin
    if FindForm(AForm) > 0 then Exit;
    item := TStickingForm.Create(AForm, AName);
    inherited Add(item);
  end;

procedure TStickingForms.Rename(AForm: TForm; ANewName: String);
  var
    i: Integer;
  begin
    i := FindForm(AForm);
    if i >= 0 then
      TStickingForm(Items[i]).Name := ANewName;
  end;

procedure TStickingForms.Order(AForm: TForm; AOrder: Integer);
  var
    i:   Integer;
    tmp: TForm;
  begin
    if Count = 0 then Exit;
    if AOrder < 0 then AOrder      := 0;
    if AOrder >= Count then AOrder := Count - 1;
    Exchange(FindForm(AForm), AOrder);
  end;

procedure TStickingForms.Stick;
  var
    i, W0, H0: Integer;
    R, X:      TRect;
  begin
    if not Assigned(FMainForm) then Exit;
    if Count = 0 then Exit;
    FBorder := GetFormShadowCorrectionValue;

    with FMainForm do
      begin
      GetWindowRect(Handle, R);
      W0 := Width;
      H0 := Height + Assigned(Menu).Select(Menu.Height, 0);
      X  := Rect(FBorder, FBorder, -FBorder, -FBorder);
      end;

    for i := 0 to Count - 1 do
      TStickingForm(Items[i]).Stick(FBorder, W0, H0, R, X);
  end;

procedure TStickingForms.StickToRightBottomEdge;
  var
    i: Integer;
    R: TRect;
  begin
    if not Assigned(FMainForm) then Exit;
    if Count = 0 then Exit;
    FBorder := GetFormShadowCorrectionValue;
    GetWindowRect(FMainForm.Handle, R);

    for i := 0 to Count - 1 do
      TStickingForm(Items[i]).StickToRightBottomEdge(FBorder, R);
  end;


initialization
  StickingFormsEx := TStickingForms.Create;

end.
