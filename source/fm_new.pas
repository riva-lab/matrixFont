unit fm_new;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, Buttons, Spin, ExtCtrls, fm_settings, u_strings;

type

  { TfmNew }

  TfmNew = class(TForm)
    bbOK:        TBitBtn;
    cbEncoding:  TComboBox;
    edFontName:  TEdit;
    edAuthor:    TEdit;
    lbEncoding:  TLabel;
    lbFontName1: TLabel;
    lbFontName2: TLabel;
    lbWidth:     TLabel;
    lbHeight:    TLabel;
    lbFontName:  TLabel;
    lbAuthor:    TLabel;
    lbStartItem: TLabel;
    lbLastItem:  TLabel;
    pEncoding:   TPanel;
    pValues:     TPanel;
    pRange:      TPanel;
    pSizes:      TPanel;
    pMain:       TPanel;
    pControls:   TPanel;
    pName:       TPanel;
    pAuthor:     TPanel;
    seLastItem:  TSpinEdit;
    seWidth:     TSpinEdit;
    seHeight:    TSpinEdit;
    seStartItem: TSpinEdit;

    procedure FormShow(Sender: TObject);
    procedure seLastItemChange(Sender: TObject);
    procedure seStartItemChange(Sender: TObject);
  end;

var
  fmNew: TfmNew;

implementation

{$R *.lfm}

{ TfmNew }

procedure TfmNew.FormShow(Sender: TObject);
  begin
    cbEncoding.Items.Assign(fmSettings.cbEncoding.Items);
    cbEncoding.ItemIndex := fmSettings.NewEncoding;
    seWidth.Value        := fmSettings.NewWidth;
    seHeight.Value       := fmSettings.NewHeight;
    seStartItem.Value    := fmSettings.NewItemStart;
    seLastItem.Value     := fmSettings.NewItemLast;
    edFontName.Text      := fmSettings.NewName;
    edAuthor.Text        := fmSettings.NewAuthor;
    edFontName.TextHint  := lbFontName.Caption;
    edAuthor.TextHint    := lbAuthor.Caption;
  end;

procedure TfmNew.seLastItemChange(Sender: TObject);
  begin
    seStartItem.MaxValue := seLastItem.Value;
  end;

procedure TfmNew.seStartItemChange(Sender: TObject);
  begin
    seLastItem.MinValue := seStartItem.Value;
  end;

end.
