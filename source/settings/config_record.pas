unit config_record;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, AppSettings, Graphics, cOpenFileList;


type

  { Colors (used to support for different themes)
  }
  TAppCfgColors = record

    editor: record
      active: TColor;
      bg:     TColor;
      grid:   TColor;
      end;

    prev: record
      active: TColor;
      bg:     TColor;
      end;

    nav: record
      active: TColor;
      bg:     TColor;
      txt:    TColor;
      end;

    import: record
      active: TColor;
      bg:     TColor;
      end;
  end;

  { Project settings.
    Saved and restored from INI file by TAppSettings class.
  }
  TAppConfig = record

    app: record
      lang:      Integer;
      iconscale: Integer;
      splitter:  Integer;
      rollover:  Boolean;
      lastfiles: array[0..LAST_FILES_LIST_SIZE - 1] of String;
      end;

    gen: record
      fontsize: Integer;
      end;

    import: record
      bwlevel:  Integer;
      optimize: Boolean;
      snapleft: Boolean;
      example:  String;
      end;

    importc: record
      optimize: Boolean;
      snapleft: Boolean;

      metrics: record
        nbits:    Integer;
        order:    Integer;
        codetype: Integer;
        bitorder: Integer;
        w:        Integer;
        h:        Integer;
        start:    Integer;
        last:     Integer;
        offset:   Integer;
        skip:     Integer;
        end;

      example: record
        enable: Boolean;
        str:    String;
        char:   Integer;
        end;

      tab: record
        code:   Boolean;
        params: Boolean;
        end;
      end;

    map: record
      init:   Boolean;
      cols:   Integer;
      w:      Integer;
      h:      Integer;
      spacex: Integer;
      spacey: Integer;
      end;

    grid: record
      enable: Boolean;
      size:   Integer;
      chess:  Boolean;
      end;

    prev: record
      enable:  Boolean;
      magnet:  Boolean;
      refresh: Boolean;
      example: String;
      scale:   Integer;
      delta:   Integer;
      space:   Integer;
      mono:    Boolean;
      prop:    Boolean;
      end;

    color:  TAppCfgColors;
    colorl: TAppCfgColors;
    colord: TAppCfgColors;

    nav: record
      rowheight:   Integer;
      transparent: Boolean;
      invert:      Boolean;
      scroll:      Boolean;

      char: record
        enable:   Boolean;
        font:     Integer;
        fontsize: Integer;
        end;
      code: record
        enable:   Boolean;
        font:     Integer;
        fontsize: Integer;
        hex:      Boolean;
        end;
      end;

    new: record
      title:  String;
      author: String;
      w:      Integer;
      h:      Integer;
      start:  Integer;
      last:   Integer;
      enc:    Integer;
      end;

    toolbar: record
      chars: Boolean;
      fonts: Boolean;
      files: Boolean;
      tools: Boolean;
      end;
  end;


const
  SETTINGS_FILE = 'settings.ini';


var
  Settings: TAppSettings; // class for work with settings
  cfg:      TAppConfig;   // configuration record with project settings


implementation


initialization
  Settings := TAppSettings.Create;

end.
