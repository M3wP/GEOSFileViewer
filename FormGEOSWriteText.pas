//------------------------------------------------------------------------------
//FormGEOSWriteText
//=================
//Text viewer for a geoWrite document.
//
//
//Please note:
//------------
//Presently, only FPC/Lazarus is supported.  Delphi support is incomplete.
//
//
//Copyright (C) 2016, Daniel England.
//All Rights Reserved.  Released under the GPL.
//
//This program is free software: you can redistribute it and/or modify it under
//the terms of the GNU General Public License as published by the Free Software
//Foundation, either version 3 of the License, or (at your option) any later
//version.
//
//This program is distributed in the hope that it will be useful, but WITHOUT
//ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
//FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
//details.
//
//You should have received a copy of the GNU General Public License along with
//this program.  If not, see <http://www.gnu.org/licenses/>.
//
//------------------------------------------------------------------------------
unit FormGEOSWriteText;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ComCtrls, ExtCtrls, GEOSWriteFile;

type

{ TGEOSWriteTextForm }

    TGEOSWriteTextForm = class(TForm)
        Button1: TButton;
        MemText: TMemo;
        Panel1: TPanel;
        ToolBar1: TToolBar;
        procedure Button1Click(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    private
        FGEOSWriteFile: TGEOSWriteFile;

        procedure ClearDisplay;

    public
        procedure InitialiseDisplay;
    end;

var
    GEOSWriteTextForm: TGEOSWriteTextForm;

implementation

{$R *.lfm}

uses
    DModGEOSFileViewerMain;


{ TGEOSWriteTextForm }

procedure TGEOSWriteTextForm.Button1Click(Sender: TObject);
    begin
    Close;
    end;

procedure TGEOSWriteTextForm.FormClose(Sender: TObject;
        var CloseAction: TCloseAction);
    begin
    ClearDisplay;
    end;

procedure TGEOSWriteTextForm.ClearDisplay;
    begin
    if  Assigned(FGEOSWriteFile) then
        FreeAndNil(FGEOSWriteFile);

    MemText.Lines.Clear;
    end;

procedure TGEOSWriteTextForm.InitialiseDisplay;
    begin
    ClearDisplay;

    FGEOSWriteFile:= TGEOSWriteFile.Create(
            GEOSFileViewerMainDMod.D64Image,
            GEOSFileViewerMainDMod.Entries[GEOSFileViewerMainDMod.Selected]);

    FGEOSWriteFile.ConvertDocumentToStrings(MemText.Lines);
    end;

end.

