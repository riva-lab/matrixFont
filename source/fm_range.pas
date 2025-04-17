unit fm_range;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Buttons, StdCtrls, Spin, ExtCtrls, SysUtils,
  font, u_helpers, u_strings;

type

  { TfmRange }

  TfmRange = class(TForm)
    bbApply:   TBitBtn;
    gbRange:   TGroupBox;
    gbResult:  TGroupBox;
    lbDiffE:   TLabel;
    lbDiffEL:  TLabel;
    lbDiffS:   TLabel;
    lbDiffSL:  TLabel;
    lbEnd:     TLabel;
    lbStart:   TLabel;
    lbWarning: TLabel;
    pControls: TPanel;
    pMain:     TPanel;
    seEnd:     TSpinEdit;
    seStart:   TSpinEdit;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure seValueChange(Sender: TObject);
  end;

var
  fmRange: TfmRange;

implementation

{$R *.lfm}

function IntWithSign(AValue: Integer): String;
  begin
    Result := Format('%s%d', [(AValue > 0).Select('+', ''), AValue]);
  end;

{ TfmRange }


procedure TfmRange.FormCreate(Sender: TObject);
  begin
  end;

procedure TfmRange.FormShow(Sender: TObject);
  begin
    lbDiffS.Constraints.MinWidth := Canvas.TextWidth('0000');
    lbDiffE.Constraints.MinWidth := Canvas.TextWidth('0000');
  end;

procedure TfmRange.seValueChange(Sender: TObject);
  begin
    seEnd.MinValue   := seStart.Value;
    seStart.MaxValue := seEnd.Value;

    lbDiffS.Caption := IntWithSign(mxFont.FontStartItem - seStart.Value);
    lbDiffE.Caption := IntWithSign(1 - mxFont.FontStartItem - mxFont.FontLength + seEnd.Value);
  end;

end.
