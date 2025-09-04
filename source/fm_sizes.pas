unit fm_sizes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls, StdCtrls, Spin, Buttons, Math,
  u_strings, font, symbol;

type

  TSideControls = record
    Edit:  TSpinEdit;
    Btn:   TSpeedButton;
    Panel: TPanel;
    optSz: Integer;
  end;

  { TfmSizes }

  TfmSizes = class(TForm)
    bbApply:    TBitBtn;
    pControls:  TPanel;
    sbReset:    TSpeedButton;
    gbLimits:   TGroupBox;
    pResult:    TPanel;
    pValueU:    TPanel;
    pValueL:    TPanel;
    pValueR:    TPanel;
    pValueD:    TPanel;
    pMain:      TPanel;
    lbEmpty1:   TLabel;
    lbEmpty2:   TLabel;
    lbEmpty3:   TLabel;
    lbEmpty4:   TLabel;
    lbHint:     TLabel;
    lbWarning:  TLabel;
    lbOld:      TLabel;
    lbNew:      TLabel;
    lbOldValue: TLabel;
    lbNewValue: TLabel;
    sbUp:       TSpeedButton;
    sbDown:     TSpeedButton;
    sbLeft:     TSpeedButton;
    sbRight:    TSpeedButton;
    seUp:       TSpinEdit;
    seRight:    TSpinEdit;
    seLeft:     TSpinEdit;
    seDown:     TSpinEdit;
    rgResult:   TRadioGroup;
    rgMode:     TRadioGroup;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure seUpChange(Sender: TObject);
    procedure sbResetClick(Sender: TObject);

  private
    FOldW, FOldH:  Integer;
    FSideControls: array [TDirection] of TSideControls;

    procedure SetSideControls(ADir: TDirection; ASpEd: TSpinEdit; ASpBtn: TSpeedButton; APan: TPanel);

  public
    procedure SetFont(AFont: TMatrixFont);
  end;

var
  fmSizes: TfmSizes;


implementation

{$R *.lfm}

{ TfmSizes }

procedure TfmSizes.FormCreate(Sender: TObject);
  begin
    SetSideControls(dirUp, seUp, sbUp, pValueU);
    SetSideControls(dirDown, seDown, sbDown, pValueD);
    SetSideControls(dirLeft, seLeft, sbLeft, pValueL);
    SetSideControls(dirRight, seRight, sbRight, pValueR);
    FormShow(Sender);
  end;

procedure TfmSizes.FormShow(Sender: TObject);
  var
    ctrl: TSideControls;
  begin
    lbWarning.Caption               := WARN_NOREDO;
    lbOldValue.Constraints.MinWidth := Canvas.GetTextWidth('000 x 000');

    with sbReset do
      begin
      Constraints.MinHeight := Images.Width + Scale96ToScreen(16);
      Constraints.MinWidth  := Constraints.MinHeight;
      end;

    for ctrl in FSideControls do
      begin
      ctrl.Edit.Constraints.MinWidth := ctrl.Edit.Height + Canvas.GetTextWidth('000000');
      ctrl.Edit.Constraints.MaxWidth := ctrl.Edit.Constraints.MinWidth;

      Tag := Max(Tag, ctrl.Panel.Height);
      Tag := Max(Tag, ctrl.Panel.Width);
      end;

    for ctrl in FSideControls do
      begin
      ctrl.Panel.Constraints.MinWidth  := Tag;
      ctrl.Panel.Constraints.MinHeight := Tag;
      ctrl.Btn.Constraints.MinHeight   := ctrl.Panel.Height - ctrl.Edit.Height - 4;
      ctrl.Btn.BorderSpacing.Top       := 2;
      end;

    seUpChange(Sender);
  end;


procedure TfmSizes.sbResetClick(Sender: TObject);
  var
    ctrl: TSideControls;
  begin
    for ctrl in FSideControls do
      if ctrl.Edit.Enabled then ctrl.Edit.Value := 0;
    seUpChange(Sender);
  end;

procedure TfmSizes.seUpChange(Sender: TObject);
  var
    ctrl: TSideControls;
  begin
    for ctrl in FSideControls do
      begin
      ctrl.Edit.Enabled := not ctrl.Btn.Down;
      if ctrl.Btn.Down then ctrl.Edit.Value := ctrl.Btn.Down.ToInteger * ctrl.optSz;
      end;

    lbOldValue.Caption := Format('%d x %d', [FOldW, FOldH]);
    lbNewValue.Caption := Format('%d x %d', [
      FOldW + seLeft.Value + seRight.Value,
      FOldH + seUp.Value + seDown.Value]);

    bbApply.Enabled := (
      ((seLeft.Value + seRight.Value > -FOldW) and (seUp.Value + seDown.Value > -FOldH))) and
      ((seLeft.Value <> -seRight.Value) or (seUp.Value <> -seDown.Value)) and
      ((seLeft.Value <> 0) or (seRight.Value <> 0) or (seUp.Value <> 0) or (seDown.Value <> 0));
  end;

procedure TfmSizes.SetSideControls(ADir: TDirection; ASpEd: TSpinEdit; ASpBtn: TSpeedButton; APan: TPanel);
  begin
    FSideControls[ADir].Edit  := ASpEd;
    FSideControls[ADir].Btn   := ASpBtn;
    FSideControls[ADir].Panel := APan;

    ASpBtn.Down := False;
  end;

procedure TfmSizes.SetFont(AFont: TMatrixFont);
  begin
    FOldW := AFont.Width;
    FOldH := AFont.Height;

    FSideControls[dirUp].optSz    := -AFont.CanOptimize(dirUp);
    FSideControls[dirDown].optSz  := -AFont.CanOptimize(dirDown);
    FSideControls[dirLeft].optSz  := -AFont.CanOptimize(dirLeft);
    FSideControls[dirRight].optSz := -AFont.CanOptimize(dirRight);
  end;

end.
