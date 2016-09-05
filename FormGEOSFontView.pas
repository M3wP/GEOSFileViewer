//------------------------------------------------------------------------------
//FormGEOSFontView
//================
//Font viewer form.
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
unit FormGEOSFontView;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics,
    Dialogs, StdCtrls, ExtCtrls, GEOSFont;

type

{ TGEOSFontViewForm }

    TGEOSFontViewForm = class(TForm)
        Button1: TButton;
        CmbSizes: TComboBox;
        CmbColours: TComboBox;
        CmbScale: TComboBox;
        DividerBevel1: TDividerBevel;
        DividerBevel2: TDividerBevel;
        EditSample: TEdit;
        ImgDisplay: TImage;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        LstBxReport: TListBox;
        Panel1: TPanel;
        Panel2: TPanel;
        procedure Button1Click(Sender: TObject);
        procedure CmbColoursChange(Sender: TObject);
        procedure CmbScaleChange(Sender: TObject);
        procedure CmbSizesChange(Sender: TObject);
        procedure EditSampleKeyDown(Sender: TObject; var Key: Word;
            Shift: TShiftState);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormResize(Sender: TObject);
    private
        FChanging: Boolean;
        FGEOSFont: TGEOSFont;
        FYPos: Integer;

        procedure ClearDisplay;

        procedure DoIntitialiseFont;
        procedure DoResetSampleText;
        procedure DoDisplayText;

    public
        procedure InitialiseDisplay;
    end;

var
    GEOSFontViewForm: TGEOSFontViewForm;

implementation

{$R *.lfm}

uses
    GEOSTypes, DModGEOSFileViewerMain;


const
    LIT_CAP_SAMPLETEXT = 'The quick brown fox jumped over the lazy dog.';

{ TGEOSFontViewForm }

procedure TGEOSFontViewForm.FormClose(Sender: TObject;
        var CloseAction: TCloseAction);
    begin
    ClearDisplay;
    end;

procedure TGEOSFontViewForm.CmbSizesChange(Sender: TObject);
    begin
    if  not FChanging then
        begin
        FGEOSFont.Size:= FGEOSFont.Sizes[CmbSizes.ItemIndex];
        DoDisplayText;
        end;
    end;

procedure TGEOSFontViewForm.EditSampleKeyDown(Sender: TObject;
        var Key: Word; Shift: TShiftState);
    begin
    if  Key = 13 then
        begin
        Key:= 0;

        if  not FChanging then
            DoDisplayText;
        end;
    end;

procedure TGEOSFontViewForm.CmbScaleChange(Sender: TObject);
    begin
    if  not FChanging then
        DoDisplayText;
    end;

procedure TGEOSFontViewForm.CmbColoursChange(Sender: TObject);
    begin
    if  not FChanging then
        begin
        FGEOSFont.Lock;
        try
            if  CmbColours.ItemIndex = 0 then
                begin
                FGEOSFont.BrushColor:= clC64LightGrey;
                FGEOSFont.PenColor:= clC64DarkGrey;
                end
            else if CmbColours.ItemIndex = 1 then
                begin
                FGEOSFont.BrushColor:= TColor($C6C6C6);
                FGEOSFont.PenColor:= clBlack;
                end
            else if CmbColours.ItemIndex = 2 then
                begin
                FGEOSFont.BrushColor:= clWhite;
                FGEOSFont.PenColor:= clBlack;
                end
            else
                begin
                FGEOSFont.BrushColor:= clWindow;
                FGEOSFont.PenColor:= clWindowText;
                end;

            finally
            FGEOSFont.Unlock;
            end;

        DoDisplayText;
        end;
    end;

procedure TGEOSFontViewForm.Button1Click(Sender: TObject);
    begin
    DoResetSampleText;
    DoDisplayText;
    end;

procedure TGEOSFontViewForm.FormResize(Sender: TObject);
    begin
    if  Assigned(ImgDisplay.Picture.Bitmap)
    and Assigned(FGEOSFont) then
        DoDisplayText;
    end;

procedure TGEOSFontViewForm.ClearDisplay;
    begin
    if  Assigned(FGEOSFont) then
        FreeAndNil(FGEOSFont);

    FChanging:= True;
    try

        LstBxReport.Items.Clear;

        CmbColours.ItemIndex:= 0;
        CmbSizes.Items.Clear;
        CmbScale.ItemIndex:= 0;

        DoResetSampleText;

        finally
        FChanging:= False;
        end;
    end;

procedure TGEOSFontViewForm.DoIntitialiseFont;
    var
    i: Integer;

    begin
    FChanging:= True;
    try
        CmbSizes.Items.BeginUpdate;
        try
            CmbSizes.Items.Clear;

            for i:= 0 to FGEOSFont.SizesCount - 1 do
                CmbSizes.Items.Add(IntToStr(FGEOSFont.Sizes[i]) + ' point');

            finally
            CmbSizes.Items.EndUpdate;
            end;

        CmbSizes.ItemIndex:= 0;

        finally
        FChanging:= False;
        end;
    end;

procedure TGEOSFontViewForm.DoResetSampleText;
    begin
    EditSample.Text:= LIT_CAP_SAMPLETEXT;
    end;

procedure TGEOSFontViewForm.DoDisplayText;
    var
    s: string;
    i: Byte;
    c: Integer;
    w,
    h: Integer;

    begin
    if  CmbScale.ItemIndex = 0 then
        begin
        w:= ImgDisplay.Width;
        h:= ImgDisplay.Height;
        end
    else if CmbScale.ItemIndex = 1 then
        begin
        w:= ImgDisplay.Width div 2;
        h:= ImgDisplay.Height div 2;
        end
    else
        begin
        w:= ImgDisplay.Width div 4;
        h:= ImgDisplay.Height div 4;
        end;

    ImgDisplay.Picture.Bitmap.SetSize(w, h);

    if  CmbColours.ItemIndex = 0 then
        ImgDisplay.Picture.Bitmap.Canvas.Brush.Color:= clC64LightGrey
    else if CmbColours.ItemIndex = 1 then
        ImgDisplay.Picture.Bitmap.Canvas.Brush.Color:= TColor($C6C6C6)
    else if CmbColours.ItemIndex = 2 then
        ImgDisplay.Picture.Bitmap.Canvas.Brush.Color:= clWhite
    else
        ImgDisplay.Picture.Bitmap.Canvas.Brush.Color:= clWindow;

    ImgDisplay.Picture.Bitmap.Canvas.Brush.Style:= bsSolid;

    ImgDisplay.Picture.Bitmap.Canvas.FillRect(Rect(0, 0, w, h));

    s:= EmptyStr;
    for i:= $20 to FGEOSFont.GlyphCount + $1F do
        s:= s + string(AnsiChar(i));

    FYPos:= 0;
    while Length(s) > 0 do
        begin
        c:= FGEOSFont.TextFitInfo(s, w);

        FGEOSFont.TextOut(ImgDisplay.Picture.Bitmap.Canvas, 0, FYPos,
                Copy(s, 1, c));

        s:= Copy(s, c + 1, MaxInt);
        Inc(FYPos, FGEOSFont.Size);
        end;

    Inc(FYPos, FGEOSFont.Size);
    s:= EditSample.Text;
    while Length(s) > 0 do
        begin
        c:= FGEOSFont.TextFitInfo(s, w);

        FGEOSFont.TextOut(ImgDisplay.Picture.Bitmap.Canvas, 0, FYPos,
                Copy(s, 1, c));

        s:= Copy(s, c + 1, MaxInt);
        Inc(FYPos, FGEOSFont.Size);
        end;
    end;

procedure TGEOSFontViewForm.InitialiseDisplay;
    begin
    ClearDisplay;

    FGEOSFont:= TGEOSFont.Create(
            GEOSFileViewerMainDMod.D64Image,
            GEOSFileViewerMainDMod.Entries[GEOSFileViewerMainDMod.Selected],
            LstBxReport.Items, clC64DarkGrey, clC64LightGrey);

    if  not Assigned(ImgDisplay.Picture.Bitmap) then
        ImgDisplay.Picture.Bitmap:= TBitmap.Create;

    DoIntitialiseFont;
    DoResetSampleText;

    DoDisplayText;
    end;

end.

