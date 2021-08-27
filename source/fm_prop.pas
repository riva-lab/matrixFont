unit fm_prop;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, StdCtrls, Buttons, ExtCtrls, u_encodings, fm_settings,
  Classes;

type

  { TfmProp }

  TfmProp = class(TForm)
    bbApply:        TBitBtn;
    cbEncoding:     TComboBox;
    edAppChange:    TEdit;
    edPath:         TEdit;
    edAppCreate:    TEdit;
    edDateCreate:   TEdit;
    edDateChange:   TEdit;
    edStart:        TEdit;
    edLast:         TEdit;
    edHeight:       TEdit;
    edWidth:        TEdit;
    edAuthor:       TEdit;
    edFontName:     TEdit;
    lbDateCreate:   TLabel;
    lbFontEncoding: TLabel;
    lbPath:         TLabel;
    lbDateChange:   TLabel;
    lbName:         TLabel;
    lbAuthor:       TLabel;
    lbStart:        TLabel;
    lbLast:         TLabel;
    lbHeight:       TLabel;
    lbWidth:        TLabel;
    pBottom:        TPanel;
    pEdits:         TPanel;
    pCreate:        TPanel;
    pChange:        TPanel;
    pMain:          TPanel;
    pValues:        TPanel;

    procedure edPathClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  PUBLIC
    prName, prAuthor, prDTCreate, prDTChange: String;
    prFirst, prLast, prW, prH, prEnc:         Integer;
    prAppCreate, prAppChange, prPath:         String;
  end;

var
  fmProp: TfmProp;

implementation

{$R *.lfm}

{ TfmProp }

procedure TfmProp.FormShow(Sender: TObject);
  begin
    cbEncoding.Items.Assign(fmSettings.cbEncoding.Items);
    cbEncoding.ItemIndex := prEnc;
    edPath.Text          := prPath;
    edFontName.Text      := prName;
    edAuthor.Text        := prAuthor;
    edDateCreate.Text    := prDTCreate;
    edDateChange.Text    := prDTChange;
    edAppCreate.Text     := prAppCreate;
    edAppChange.Text     := prAppChange;
    edStart.Text         := IntToStr(prFirst);
    edLast.Text          := IntToStr(prLast);
    edHeight.Text        := IntToStr(prH);
    edWidth.Text         := IntToStr(prW);
  end;

procedure TfmProp.edPathClick(Sender: TObject);
  begin
    edPath.SelectAll;
  end;

end.

