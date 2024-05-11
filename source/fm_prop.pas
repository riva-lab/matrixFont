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
    edAppCreate:    TEdit;
    edAuthor:       TEdit;
    edDateChange:   TEdit;
    edDateCreate:   TEdit;
    edFontName:     TEdit;
    edPath:         TEdit;
    edRange:        TEdit;
    edSize:         TEdit;
    lbAuthor:       TLabel;
    lbDateChange:   TLabel;
    lbDateCreate:   TLabel;
    lbFont:         TLabel;
    lbFontEncoding: TLabel;
    lbName:         TLabel;
    lbPath:         TLabel;
    pBottom:        TPanel;
    pChange:        TPanel;
    pCreate:        TPanel;
    pEdits:         TPanel;
    pFont:          TPanel;
    pMain:          TPanel;

    procedure edPathClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
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
    edSize.Text          := Format(edSize.Hint, [prW, prH]);
    edRange.Text         := Format(edRange.Hint, [prFirst, prLast]);

    // fix image of button
    bbApply.ImageIndex := 0;
    bbApply.ImageIndex := 44;
  end;

procedure TfmProp.edPathClick(Sender: TObject);
  begin
    edPath.SelectAll;
  end;

end.
