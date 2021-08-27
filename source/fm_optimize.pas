unit fm_optimize;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Buttons, StdCtrls, ExtCtrls, u_strings, Classes;

type

  { TfmOptimize }

  TfmOptimize = class(TForm)
    bbApply:      TBitBtn;
    gbBorders:    TGroupBox;
    lbEmpty1:     TLabel;
    lbEmpty2:     TLabel;
    lbEmpty3:     TLabel;
    lbEmpty4:     TLabel;
    lbWarning:    TLabel;
    lbNew:        TLabel;
    lbNewValue:   TLabel;
    lbOld:        TLabel;
    lbOldValue:   TLabel;
    lbUpValue:    TLabel;
    lbRightValue: TLabel;
    lbRight:      TLabel;
    lbUp:         TLabel;
    lbLeft:       TLabel;
    lbLeftValue:  TLabel;
    lbDown:       TLabel;
    lbDownValue:  TLabel;
    pControls1:   TPanel;
    pResult:      TPanel;
    pMain:        TPanel;
    pControls:    TPanel;
    rgDelete:     TRadioGroup;
    rgResult:     TRadioGroup;
    sbLeft:       TSpeedButton;
    sbUp:         TSpeedButton;
    sbDown:       TSpeedButton;
    sbRight:      TSpeedButton;
    sbReset:      TSpeedButton;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbResetClick(Sender: TObject);
    procedure sbArrowClick(Sender: TObject);
  end;

var
  fmOptimize: TfmOptimize;
  opt_oldWidth, opt_oldHeight: Integer;

  opt_up, opt_down, opt_left, opt_right: Integer;
  res_up, res_down, res_left, res_right: Integer;

implementation

{$R *.lfm}

{ TfmOptimize }

procedure TfmOptimize.FormCreate(Sender: TObject);
  begin
  end;

procedure TfmOptimize.FormShow(Sender: TObject);
  begin
    lbWarning.Caption := MultiString(WARN_NOREDO);
    sbUp.Down         := False;
    sbReset.Click;
  end;

procedure TfmOptimize.sbResetClick(Sender: TObject);
  var
    Value: Boolean;
  begin
    Value        := not (sbUp.Down and sbDown.Down and sbLeft.Down and sbRight.Down);
    sbUp.Down    := Value;
    sbDown.Down  := Value;
    sbLeft.Down  := Value;
    sbRight.Down := Value;
    sbArrowClick(Sender);
  end;

procedure TfmOptimize.sbArrowClick(Sender: TObject);
  begin
    res_up    := sbUp.Down.ToInteger * opt_up;
    res_down  := sbDown.Down.ToInteger * opt_down;
    res_left  := sbLeft.Down.ToInteger * opt_left;
    res_right := sbRight.Down.ToInteger * opt_right;

    lbUpValue.Caption    := IntToStr(res_up);
    lbDownValue.Caption  := IntToStr(res_down);
    lbLeftValue.Caption  := IntToStr(res_left);
    lbRightValue.Caption := IntToStr(res_right);

    lbOldValue.Caption := IntToStr(opt_oldWidth) + ' x ' + IntToStr(opt_oldHeight);
    lbNewValue.Caption := IntToStr(opt_oldWidth - res_left - res_right) +
      ' x ' + IntToStr(opt_oldHeight - res_up - res_down);

    // отслеживание корректности новых размеров символа
    bbApply.Enabled := ((opt_oldHeight - res_up - res_down > 0) and
      (opt_oldWidth - res_left - res_right > 0)) and
      ((res_up > 0) or (res_down > 0) or (res_left > 0) or (res_right > 0));
  end;

end.
