program _matrixFont;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, fm_main, fm_gen, fm_new, symbol, font, fm_prop, fm_confirm, app_ver,
  fm_import, fm_preview, fm_sizes, fm_optimize, fm_range, fm_about, fm_settings,
  lhelpcontrolpkg, help, u_strings, cOpenFileList, u_utilities, u_encodings;

{$R *.res}

begin
  Application.Title:='matrixFont';
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
  Application.Run;
end.

