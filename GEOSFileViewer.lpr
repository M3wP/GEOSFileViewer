program GEOSFileViewer;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, lazcontrols, FormGEOSFileViewerMain, DModGEOSFileViewerMain,
    FormGEOSWriteText, GEOSWriteFile, FormGEOSFontView, GEOSTypes,
    GEOSFont, FormGEOSPaintImage, GEOSPaintFile
    { you can add units after this };

{$R *.res}

begin
    RequireDerivedFormResource:=True;
    Application.Initialize;
    Application.CreateForm(TGEOSFileViewerMainForm, GEOSFileViewerMainForm);
    Application.CreateForm(TGEOSFileViewerMainDMod, GEOSFileViewerMainDMod);
    Application.Run;
end.

