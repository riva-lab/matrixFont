unit fm_sizes;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, ExtCtrls, StdCtrls, Spin, Buttons, u_strings, Classes;

resourcestring
  FM_SIZES_ADD    = 'Добавить строки и столбцы';
  FM_SIZES_DEL    = 'Удалить строки и столбцы';
  FM_SIZES_EXPAND = 'Расширить холст';
  FM_SIZES_CROP   = 'Обрезать холст';




type

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
    lbEmpty5:   TLabel;
    lbEmpty6:   TLabel;
    lbWarning:  TLabel;
    lbOld:      TLabel;
    lbNew:      TLabel;
    lbOldValue: TLabel;
    lbNewValue: TLabel;
    lbUp:       TLabel;
    lbLeft:     TLabel;
    lbDown:     TLabel;
    lbRight:    TLabel;
    seUp:       TSpinEdit;
    seRight:    TSpinEdit;
    seLeft:     TSpinEdit;
    seDown:     TSpinEdit;
    rgResult:   TRadioGroup;
    rgMode:     TRadioGroup;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rgModeClick(Sender: TObject);
    procedure seUpChange(Sender: TObject);
    procedure sbResetClick(Sender: TObject);
  end;

var
  fmSizes:             TfmSizes;
  oldWidth, oldHeight: Integer;

implementation

{$R *.lfm}

{ TfmSizes }

procedure TfmSizes.FormCreate(Sender: TObject);
  begin
    FormShow(Sender);
  end;

procedure TfmSizes.FormShow(Sender: TObject);
  begin
    lbWarning.Caption               := MultiString(WARN_NOREDO);
    lbOldValue.Constraints.MinWidth := Canvas.GetTextWidth('000 x 000');

    rgMode.Items.Clear;
    rgMode.Items.Add(FM_SIZES_EXPAND);
    rgMode.Items.Add(FM_SIZES_CROP);
    rgModeClick(Sender);
  end;


procedure TfmSizes.sbResetClick(Sender: TObject);
  begin
    seUp.Value    := 0;
    seRight.Value := 0;
    seLeft.Value  := 0;
    seDown.Value  := 0;
    seUpChange(Sender);
  end;

procedure TfmSizes.rgModeClick(Sender: TObject);
  begin
    if rgMode.ItemIndex = 0 then
      gbLimits.Caption := FM_SIZES_ADD else
      gbLimits.Caption := FM_SIZES_DEL;
    gbLimits.Refresh;
    seUpChange(Sender);
  end;

procedure TfmSizes.seUpChange(Sender: TObject);
  begin
    lbOldValue.Caption := IntToStr(oldWidth) + ' x ' + IntToStr(oldHeight);
    if rgMode.ItemIndex = 0 then
      lbNewValue.Caption :=
        IntToStr(oldWidth + seLeft.Value + seRight.Value) + ' x ' +
        IntToStr(oldHeight + seUp.Value + seDown.Value)
    else
      lbNewValue.Caption :=
        IntToStr(oldWidth - seLeft.Value - seRight.Value) + ' x ' +
        IntToStr(oldHeight - seUp.Value - seDown.Value);

    // отслеживание корректности новых размеров символа
    bbApply.Enabled := ((rgMode.ItemIndex = 0) or
      ((oldWidth - seLeft.Value - seRight.Value > 0) and
      (oldHeight - seUp.Value - seDown.Value > 0))) and
      ((seLeft.Value > 0) or (seRight.Value > 0) or
      (seUp.Value > 0) or (seDown.Value > 0));
  end;

end.
