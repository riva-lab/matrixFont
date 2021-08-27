unit fm_confirm;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, Buttons, ExtCtrls, Dialogs, u_strings;

type

  { TfmConfirm }

  TfmConfirm = class(TForm)
    bbYes:     TBitBtn;
    bbNo:      TBitBtn;
    bbCancel:  TBitBtn;
    Label1:    TLabel;
    lbMessage: TLabel;
    pButtons:  TPanel;
    pMessage:  TPanel;
    pValueR:   TPanel;

    procedure FormShow(Sender: TObject);

  PRIVATE
    FButtons:       TMsgDlgButtons;
    FWindowCaption: String;
    FWindowParent:  TForm;
    FWindowText:    String;

    procedure SetButtons(AValue: TMsgDlgButtons);
    procedure SetWindowCaption(AValue: String);
    procedure SetWindowText(AValue: String);

  PUBLIC
    property Buttons: TMsgDlgButtons read FButtons write SetButtons;
    property WindowCaption: String read FWindowCaption write SetWindowCaption;
    property WindowText: String read FWindowText write SetWindowText;
    property WindowParent: TForm read FWindowParent write FWindowParent;

    // быстрый вариант вызова окна подтверждения, AButtons = [mbYes, mbNo, mbCancel]
    function Show(ACaption, AText: String; AButtons: TMsgDlgButtons;
      AParent: TForm): TModalResult;
  end;

var
  fmConfirm: TfmConfirm;

implementation

{$R *.lfm}

{ TfmConfirm }

procedure TfmConfirm.SetWindowCaption(AValue: String);
  begin
    FWindowCaption := AValue;
    Caption        := FWindowCaption;
  end;

procedure TfmConfirm.SetWindowText(AValue: String);
  begin
    FWindowText       := AValue;
    lbMessage.Caption := FWindowText;
  end;

procedure TfmConfirm.SetButtons(AValue: TMsgDlgButtons);
  begin
    FButtons := AValue;

    bbYes.Visible    := mbYes in FButtons;
    bbNo.Visible     := mbNo in FButtons;
    bbCancel.Visible := mbCancel in FButtons;
  end;


procedure TfmConfirm.FormShow(Sender: TObject);
  begin
    lbMessage.Constraints.MinWidth := pButtons.Width;
    if FWindowParent = nil then
      begin
      Top  := (Screen.Height - Height) div 2;
      Left := (Screen.Width - Width) div 2;
      end
    else
      begin
      Top  := FWindowParent.Top + (FWindowParent.Height - Height) div 2;
      Left := FWindowParent.Left + (FWindowParent.Width - Width) div 2;
      end;
  end;

function TfmConfirm.Show(ACaption, AText: String;
  AButtons: TMsgDlgButtons; AParent: TForm): TModalResult;
  begin
    WindowParent  := AParent;
    WindowText    := MultiString(AText);
    WindowCaption := MultiString(ACaption);
    Buttons       := AButtons;
    Result        := fmConfirm.ShowModal;

    lbMessage.Constraints.MinWidth := 0;
  end;

end.
