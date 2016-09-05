//------------------------------------------------------------------------------
//DModGEOSFileViewerMain
//======================
//Application main data module for the GEOS File Viewer application.  Controls
//most aspects of the application behaviour.
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
unit DModGEOSFileViewerMain;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Menus, ActnList, Dialogs, C64D64Image,
    FrameGEOSFileViewerMain;

type

    { TGEOSFileViewerMainDMod }

    TGEOSFileViewerMainDMod = class(TDataModule)
        ActFileExit: TAction;
        ActFileOpen: TAction;
        ActViewAsImage: TAction;
        ActViewFontView: TAction;
        ActViewAsText: TAction;
        ActionList1: TActionList;
        MainMenu1: TMainMenu;
        MenuItem1: TMenuItem;
        MenuItem2: TMenuItem;
        MenuItem3: TMenuItem;
        MenuItem4: TMenuItem;
        MenuItem5: TMenuItem;
        MenuItem6: TMenuItem;
        MenuItem7: TMenuItem;
        MenuItem8: TMenuItem;
        OpenDialog1: TOpenDialog;
        procedure ActFileExitExecute(Sender: TObject);
        procedure ActFileOpenExecute(Sender: TObject);
        procedure ActViewAsImageExecute(Sender: TObject);
        procedure ActViewFontViewExecute(Sender: TObject);
        procedure ActionList1Update(AAction: TBasicAction;
                var Handled: Boolean);
        procedure ActViewAsTextExecute(Sender: TObject);
        procedure DataModuleDestroy(Sender: TObject);
    private
    	FD64Image: TD64Image;
        FMainFrame: TGEOSFileViewerMainFrame;
        FEntries: TD64DirEntries;
        FSelected: Integer;

        procedure ClearMainForm;
        procedure InitialiseMainForm;

        procedure SetSelected(const AValue: Integer);

    public
        procedure ApplicationClose;

        property D64Image: TD64Image read FD64Image;
        property Entries: TD64DirEntries read FEntries;
        property Selected: Integer read FSelected write SetSelected;
    end;

var
    GEOSFileViewerMainDMod: TGEOSFileViewerMainDMod;

implementation

{$R *.lfm}

uses
    C64D64ImageStrs, Controls, Forms, FormGEOSFileViewerMain,
    FormGEOSWriteText, FormGEOSPaintImage, FormGEOSFontView;

resourcestring
    STR_CAP_GEOSFILEVW = ' - GEOS File Viewer';

{ TGEOSFileViewerMainDMod }

procedure TGEOSFileViewerMainDMod.ActFileOpenExecute(Sender: TObject);
    var
    e: TD64DirEntries;
    i: Integer;
    g: Boolean;

    begin
 	if  OpenDialog1.Execute then
       	begin
        ClearMainForm;

        if  Assigned(FD64Image) then
            FreeAndNil(FD64Image);

        try
//dengland  If I use the file open version of the constructor and the file is
//          share locked, then the runtime fails and the program aborts.  This
//          is despite 3 layers of trying to protect the application from errors.
            FD64Image:= TD64Image.Create;
            FD64Image.LoadFromFile(OpenDialog1.FileName);

            SetLength(FEntries, 0);
            FD64Image.GetFileEntries(e);

            for i:= 0 to High(e) do
                begin
                g:= FD64Image.GEOSDisk and
                        ((e[i].FileType and $07) in [1..3]) and
                        (e[i].EntryData[$18] <> 0);

                if  g then
                    begin
                    SetLength(FEntries, Length(FEntries) + 1);
                    FEntries[High(FEntries)]:= e[i];
                    end;
                end;

            InitialiseMainForm;

            Application.Title:= ExtractFileName(OpenDialog1.FileName) +
                    STR_CAP_GEOSFILEVW;
            GEOSFileViewerMainForm.Caption:=
                    ExtractFileName(OpenDialog1.FileName)+
                    STR_CAP_GEOSFILEVW;

            except
            on E: Exception do
                MessageDlg(STR_CAP_D64EXCEPTION, E.Message, mtError, [mbOk], -1);
            end;
       end;
    end;

procedure TGEOSFileViewerMainDMod.ActViewAsImageExecute(Sender: TObject);
    begin
    if  not Assigned(GEOSPaintImageForm) then
        Application.CreateForm(TGEOSPaintImageForm, GEOSPaintImageForm);

    GEOSPaintImageForm.InitialiseDisplay;
    GEOSPaintImageForm.ShowModal;
    end;

procedure TGEOSFileViewerMainDMod.ActViewFontViewExecute(Sender: TObject);
    begin
    if  not Assigned(GEOSFontViewForm) then
        Application.CreateForm(TGEOSFontViewForm, GEOSFontViewForm);

    GEOSFontViewForm.InitialiseDisplay;
    GEOSFontViewForm.ShowModal;
    end;

procedure TGEOSFileViewerMainDMod.ActionList1Update(AAction: TBasicAction;
        var Handled: Boolean);
    begin
    ActViewAsText.Enabled:= Assigned(FMainFrame) and
            (FMainFrame.FileType = gftWrite);
    ActViewAsText.Visible:= ActViewAsText.Enabled;

    ActViewFontView.Enabled:= Assigned(FMainFrame) and
            (FMainFrame.FileType = gftFont);
    ActViewFontView.Visible:= ActViewFontView.Enabled;

    ActViewAsImage.Enabled:= Assigned(FMainFrame) and
            (FMainFrame.FileType = gftPaint);
    ActViewAsImage.Visible:= ActViewAsImage.Enabled;
    end;

procedure TGEOSFileViewerMainDMod.ActViewAsTextExecute(Sender: TObject);
    begin
    if  not Assigned(GEOSWriteTextForm) then
        Application.CreateForm(TGEOSWriteTextForm, GEOSWriteTextForm);

    GEOSWriteTextForm.InitialiseDisplay;
    GEOSWriteTextForm.ShowModal;
    end;

procedure TGEOSFileViewerMainDMod.DataModuleDestroy(Sender: TObject);
    begin
    if  Assigned(FD64Image) then
    	FD64Image.Free;
    end;

procedure TGEOSFileViewerMainDMod.ActFileExitExecute(Sender: TObject);
    begin
    Application.Terminate;
    end;

procedure TGEOSFileViewerMainDMod.ClearMainForm;
    begin
    FSelected:= -1;

    if  Assigned(FMainFrame) then
        begin
        FMainFrame.Visible:= False;
        FMainFrame.Parent:= nil;
        FMainFrame.Free;
        FMainFrame:= nil;
        end;
    end;

procedure TGEOSFileViewerMainDMod.InitialiseMainForm;
    begin
    FMainFrame:= TGEOSFileViewerMainFrame.Create(Self);
    FMainFrame.InitialiseDisplay;
    FMainFrame.Parent:= GEOSFileViewerMainForm;
    FMainFrame.Align:= alClient;

    if  Length(FEntries) > 0 then
        begin
        FMainFrame.LstBxFiles.Selected[0]:= True;
        SetSelected(0);
        end;
    end;

procedure TGEOSFileViewerMainDMod.SetSelected(const AValue: Integer);
    var
    h: Boolean;

    begin
    if  AValue <> FSelected then
        begin
        FSelected:= AValue;
        FMainFrame.InitialiseFileView(FSelected);
        end;

//dengland I seem to need this here.  *sigh*
    h:= False;
    ActionList1Update(nil, h);
    end;

procedure TGEOSFileViewerMainDMod.ApplicationClose;
    begin
    if  Assigned(GEOSWriteTextForm) then
        GEOSWriteTextForm.Release;

    if  Assigned(FMainFrame) then
        begin
//dengland Doing this causes nasty things to happen so I'm avoiding it and the
//      control seems to have a bitmap by default, anyway.
//      if  Assigned(FMainFrame.ImgGEOSIcon.Picture.Bitmap) then
//          FMainFrame.ImgGEOSIcon.Picture.Clear;

        FMainFrame.Visible:= False;
        FMainFrame.Parent:= nil;
        FreeAndNil(FMainFrame);
        end;
    end;

end.

