unit fm_bdf;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, StdCtrls, Buttons, Spin, ExtCtrls, LCLIntf,
  u_bdf;

type

  { TfmBdf }

  TfmBdf = class(TForm)
    bbOK:        TBitBtn;
    cbMonospace: TCheckBox;
    edName:      TEdit;
    lbBaseline:  TLabel;
    lbDefChar:   TLabel;
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
    seDefChar:   TSpinEdit;
    seSpacing:   TSpinEdit;
    seSpaceW:    TSpinEdit;
    lbBDFWiki:   TLabel;
    lbFontWiki:  TLabel;

    procedure FormShow(Sender: TObject);
    procedure bbOKClick(Sender: TObject);
    procedure lbFontWikiClick(Sender: TObject);
    procedure OptionChange(Sender: TObject);
  end;

var
  fmBdf: TfmBdf;

implementation

{$R *.lfm}

{ TfmBdf }

procedure TfmBdf.FormShow(Sender: TObject);
  begin
    edName.Constraints.MinWidth :=
      Canvas.GetTextWidth('0') * edName.MaxLength + 20;

    with bdfConverter do
      begin
      edName.Text         := Name;
      seSpacing.Value     := Spacing;
      seSpaceW.Value      := SpaceWdth;
      seDefChar.Value     := DefChar;
      seBaseline.Value    := Baseline;
      cbMonospace.Checked := Monospace;
      end;

    // fix image of button
    bbOK.ImageIndex := 0;
    bbOK.ImageIndex := 44;
  end;

procedure TfmBdf.bbOKClick(Sender: TObject);
  begin
    with bdfConverter do
      begin
      Name      := edName.Text;
      Spacing   := seSpacing.Value;
      SpaceWdth := seSpaceW.Value;
      DefChar   := seDefChar.Value;
      Baseline  := seBaseline.Value;
      Monospace := cbMonospace.Checked;
      end;
  end;

procedure TfmBdf.lbFontWikiClick(Sender: TObject);
  begin
    OpenURL(lbFontWiki.Hint);
  end;

procedure TfmBdf.OptionChange(Sender: TObject);
  begin
    seSpaceW.Enabled := not cbMonospace.Checked;
  end;

end.
