unit fm_new;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, Buttons, Spin, ExtCtrls,
  fm_settings,
  u_strings, config_record;

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
    cbEncoding.ItemIndex := cfg.new.enc;
    seWidth.Value        := cfg.new.w;
    seHeight.Value       := cfg.new.h;
    seStartItem.Value    := cfg.new.start;
    seLastItem.Value     := cfg.new.last;
    edFontName.Text      := cfg.new.title;
    edAuthor.Text        := cfg.new.author;
    edFontName.TextHint  := lbFontName.Caption;
    edAuthor.TextHint    := lbAuthor.Caption;

    // fix image of button
    bbOK.ImageIndex := 0;
    bbOK.ImageIndex := 44;
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
