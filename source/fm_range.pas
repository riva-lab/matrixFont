unit fm_range;

{$mode objfpc}{$H+}

interface

uses
  Forms, Buttons, StdCtrls, Spin, ExtCtrls, u_strings, Classes;

type

  { TfmRange }

  TfmRange = class(TForm)
    bbApply:   TBitBtn;
    gbRange:   TGroupBox;
    lbStart:   TLabel;
    lbEnd:     TLabel;
    lbWarning: TLabel;
    pControls: TPanel;
    pMain:     TPanel;
    seStart:   TSpinEdit;
    seEnd:     TSpinEdit;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure seEndChange(Sender: TObject);
    procedure seStartChange(Sender: TObject);
  end;

var
  fmRange: TfmRange;

implementation

{$R *.lfm}

{ TfmRange }


procedure TfmRange.FormCreate(Sender: TObject);
  begin
  end;

procedure TfmRange.FormShow(Sender: TObject);
  begin
    lbWarning.Caption := MultiString(WARN_NOREDO);
  end;

procedure TfmRange.seStartChange(Sender: TObject);
  begin
    seEnd.MinValue := seStart.Value;
  end;

procedure TfmRange.seEndChange(Sender: TObject);
  begin
    seStart.MaxValue := seEnd.Value;
  end;

end.

