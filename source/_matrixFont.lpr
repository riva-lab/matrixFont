program _matrixFont;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, SysUtils, LazUTF8, AppTuner, config_record,

  // project forms
  fm_about, fm_confirm, fm_gen, fm_import, fm_importc, fm_main, fm_new,
  fm_optimize, fm_preview, fm_prop, fm_range, fm_settings, fm_sizes, fm_map;

{$R *.res}

begin            
  { CRITICAL! Load INI file as soon as possible to support dark theme.
    INI file should be loaded before Application.Initialize method! }
  appTunerEx.IniFile := ExtractFilePath(ParamStrUTF8(0)) + SETTINGS_FILE;

  Application.Scaled := True;
  Application.Title := 'matrixFont';
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
  Application.CreateForm(TfmMap, fmMap);
  Application.Run;
end.

