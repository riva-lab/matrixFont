 // чтение сведений о версии и копирайте приложения
 // http://wiki.freepascal.org/Show_Application_Title,_Version,_and_Company

unit app_ver;

{$mode objfpc}{$H+}

interface

uses
  fileinfo;

type
  TFileInfoCustom = record
    CompanyName:      String;
    FileDescription:  String;
    FileVersion:      String;
    InternalName:     String;
    LegalCopyright:   String;
    OriginalFilename: String;
    ProductName:      String;
    ProductVersion:   String;
    Comments:         String;
    //FileVersionMajor: Integer;
    //FileVersionMinor: Integer;
    //FileVersionRev:   Integer;
    //FileVersionBuild: Integer;
  end;

// получение сведений о версии и т.п. запущенного приложения
procedure ReadAppInfo;

var
  app_info: TFileInfoCustom;

implementation

var
  FileVerInfo: TFileVersionInfo;

// получение сведений о версии и т.п. запущенного приложения
procedure ReadAppInfo;
  begin
    FileVerInfo := TFileVersionInfo.Create(nil);
      try
      FileVerInfo.FileName := ParamStr(0);
      FileVerInfo.ReadFileInfo;

      app_info.CompanyName      := FileVerInfo.VersionStrings.Values['CompanyName'];
      app_info.FileDescription  := FileVerInfo.VersionStrings.Values['FileDescription'];
      app_info.FileVersion      := FileVerInfo.VersionStrings.Values['FileVersion'];
      app_info.InternalName     := FileVerInfo.VersionStrings.Values['InternalName'];
      app_info.LegalCopyright   := FileVerInfo.VersionStrings.Values['LegalCopyright'];
      app_info.OriginalFilename := FileVerInfo.VersionStrings.Values['OriginalFilename'];
      app_info.ProductName      := FileVerInfo.VersionStrings.Values['ProductName'];
      app_info.ProductVersion   := FileVerInfo.VersionStrings.Values['ProductVersion'];
      app_info.Comments         := FileVerInfo.VersionStrings.Values['Comments'];

      finally
      FileVerInfo.Free;
      end;
  end;

end.
