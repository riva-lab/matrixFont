unit fm_rbf;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, StdCtrls, Buttons, Spin, ExtCtrls, LCLIntf,
  u_rbf;

type

  { TfmRbf }

  TfmRbf = class(TForm)
    bbOK:        TBitBtn;
    cbMonospace: TCheckBox;
    edName:      TEdit;
    lbBaseline:  TLabel;
    lbInterline: TLabel;
    lbMonospace: TLabel;
    lbName:      TLabel;
    lbOptions:   TLabel;
    lbSpacing:   TLabel;
    lbSpaceW:    TLabel;
    pControls:   TPanel;
    pMain:       TPanel;
    pMonospace:  TPanel;
    pName:       TPanel;
    pOptions:    TPanel;
    seBaseline:  TSpinEdit;
    seInterline: TSpinEdit;
    seSpacing:   TSpinEdit;
    seSpaceW:    TSpinEdit;
    lbFontWiki:  TLabel;

    procedure FormShow(Sender: TObject);
    procedure bbOKClick(Sender: TObject);
    procedure lbFontWikiClick(Sender: TObject);
    procedure OptionChange(Sender: TObject);
  end;

var
  fmRbf: TfmRbf;

implementation

{$R *.lfm}

{ TfmRbf }

procedure TfmRbf.FormShow(Sender: TObject);
  begin
    edName.Constraints.MinWidth :=
      Canvas.GetTextWidth('0') * edName.MaxLength + 20;

    with rbfConverter do
      begin
      edName.Text         := Name;
      seSpacing.Value     := Spacing;
      seSpaceW.Value      := SpaceWdth;
      seInterline.Value   := Interline;
      seBaseline.Value    := Baseline;
      cbMonospace.Checked := Monospace;
      end;

    // fix image of button
    bbOK.ImageIndex := 0;
    bbOK.ImageIndex := 44;
  end;

procedure TfmRbf.bbOKClick(Sender: TObject);
  begin
    with rbfConverter do
      begin
      Name      := edName.Text;
      Spacing   := seSpacing.Value;
      SpaceWdth := seSpaceW.Value;
      Interline := seInterline.Value;
      Baseline  := seBaseline.Value;
      Monospace := cbMonospace.Checked;
      end;
  end;

procedure TfmRbf.lbFontWikiClick(Sender: TObject);
  begin
    OpenURL(lbFontWiki.Hint);
  end;

procedure TfmRbf.OptionChange(Sender: TObject);
  begin
    seSpaceW.Enabled := not cbMonospace.Checked;
  end;

end.
