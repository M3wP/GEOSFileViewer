//------------------------------------------------------------------------------
//FormGEOSFileViewerMain
//======================
//Application main form for the GEOS File Viewer application.
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
unit FormGEOSFileViewerMain;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type

    { TGEOSFileViewerMainForm }

    TGEOSFileViewerMainForm = class(TForm)
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    private
        { private declarations }
    public
        { public declarations }
    end;

var
    GEOSFileViewerMainForm: TGEOSFileViewerMainForm;

implementation

{$R *.lfm}

uses
    DModGEOSFileViewerMain;


{ TGEOSFileViewerMainForm }

procedure TGEOSFileViewerMainForm.FormClose(Sender: TObject;
        var CloseAction: TCloseAction);
    begin
    GEOSFileViewerMainDMod.ApplicationClose;
    end;

end.

