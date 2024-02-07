program _matrixFont;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,

  // project forms
  fm_about, fm_confirm, fm_gen, fm_import, fm_importc, fm_main, fm_new,
  fm_optimize, fm_preview, fm_prop, fm_range, fm_settings, fm_sizes,

  // functional units
  font, symbol, app_ver, help, cOpenFileList,

  // additional units
  u_strings, u_utilities, u_encodings, u_helpers;

{$R *.res}

begin
  Application.Scaled         := True;
  Application.Title          := 'matrixFont';
  RequireDerivedFormResource := True;

  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmGen, fmGen);
  Application.CreateForm(TfmNew, fmNew);
  Application.CreateForm(TfmProp, fmProp);
  Application.CreateForm(TfmConfirm, fmConfirm);
  Application.CreateForm(TfmImport, fmImport);
  Application.CreateForm(TfmPreview, fmPreview);
  Application.CreateForm(TfmSizes, fmSizes);
  Application.CreateForm(TfmOptimize, fmOptimize);
  Application.CreateForm(TfmRange, fmRange);
  Application.CreateForm(TfmAbout, fmAbout);
  Application.CreateForm(TfmSettings, fmSettings);
  Application.CreateForm(TfmImportC, fmImportC);
  Application.Run;
end.

