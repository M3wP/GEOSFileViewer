//------------------------------------------------------------------------------
//FormGEOSPaintImage
//==================
//Viewer form for a geoPaint image.
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
unit FormGEOSPaintImage;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
    GEOSPaintFile;

type

{ TGEOSPaintImageForm }

    TGEOSPaintImageForm = class(TForm)
        ImgPaint: TImage;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    private
        FGEOSPaintFile: TGEOSPaintFile;

        procedure ClearDisplay;

    public
        procedure InitialiseDisplay;
    end;

var
    GEOSPaintImageForm: TGEOSPaintImageForm;

implementation

{$R *.lfm}

uses
    GEOSTypes, DModGEOSFileViewerMain;


{ TGEOSPaintImageForm }

procedure TGEOSPaintImageForm.FormClose(Sender: TObject;
        var CloseAction: TCloseAction);
    begin
    ClearDisplay;
    end;

procedure TGEOSPaintImageForm.ClearDisplay;
    begin
    if  Assigned(FGEOSPaintFile) then
        FGEOSPaintFile.Free;

    if  not Assigned(ImgPaint.Picture.Bitmap) then
        ImgPaint.Picture.Bitmap:= TBitmap.Create;

    ImgPaint.Picture.Bitmap.SetSize(640, 720);
    ImgPaint.Picture.Bitmap.Canvas.Brush.Color:= clC64LightGrey;
    ImgPaint.Picture.Bitmap.Canvas.Brush.Style:= bsSolid;
    ImgPaint.Picture.Bitmap.Canvas.FillRect(0, 0, 640, 720);
    end;

procedure TGEOSPaintImageForm.InitialiseDisplay;
    begin
    ClearDisplay;

    FGEOSPaintFile:= TGEOSPaintFile.Create(
            GEOSFileViewerMainDMod.D64Image,
            GEOSFileViewerMainDMod.Entries[GEOSFileViewerMainDMod.Selected]);

    ImgPaint.Picture.Bitmap.Canvas.CopyRect(Rect(0, 0, 640, 720),
            FGEOSPaintFile.Bitmap.Canvas, Rect(0, 0, 640, 720));
    end;

end.

