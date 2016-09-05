//------------------------------------------------------------------------------
//FrameGEOSFileViewerMain
//=======================
//Main frame and UI for the GEOS File Viewer application.
//
//
//Please note:
//------------
//Presently, only FPC/Lazarus is supported.
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
unit FrameGEOSFileViewerMain;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, StdCtrls,
    ExtCtrls, ComCtrls, C64D64Image;

type
    TGEOSFileType = (gftOther, gftFont, gftWrite, gftPaint);


{ TGEOSFileViewerMainFrame }

    TGEOSFileViewerMainFrame = class(TFrame)
        Button1: TButton;
        CmbGEOSVLIRRec: TComboBox;
        DividerBevel1: TDividerBevel;
        DividerBevel2: TDividerBevel;
        ImgGEOSIcon: TImage;
        Label1: TLabel;
        Label10: TLabel;
        Label11: TLabel;
        Label12: TLabel;
        Label14: TLabel;
        Label15: TLabel;
        Label16: TLabel;
        Label17: TLabel;
        Label18: TLabel;
        Label19: TLabel;
        LblGEOS4080: TLabel;
        Label20: TLabel;
        Label21: TLabel;
        Label22: TLabel;
        Label23: TLabel;
        Label25: TLabel;
        Label26: TLabel;
        Label27: TLabel;
        Label28: TLabel;
        Label29: TLabel;
        Label30: TLabel;
        Label5: TLabel;
        Label7: TLabel;
        Label8: TLabel;
        Label9: TLabel;
        LblFileActual: TLabel;
        LblFileData: TLabel;
        LblFileSize: TLabel;
        LblGEOSVLIRActual: TLabel;
        LblGEOSClass: TLabel;
        LblGEOSInfoLoc: TLabel;
        LblGEOSAppData: TLabel;
        LblGEOSDesc: TLabel;
        LblGEOSParent: TLabel;
        LblGEOSAuthor: TLabel;
        Label24: TLabel;
        LblGEOSInit: TLabel;
        LblGEOSEnd: TLabel;
        LblGEOSLoad: TLabel;
        LblGEOSIDBytes: TLabel;
        LblGEOSC64Type: TLabel;
        LblGEOSDate: TLabel;
        Label13: TLabel;
        LblGEOSStructInf: TLabel;
        LblGEOSType: TLabel;
        LblGEOSStruct: TLabel;
        LblGEOSTypeInf: TLabel;
        LstBxFileData: TListBox;
        LstBxGEOSVLIRRec: TListBox;
        LstBxGEOSData: TListBox;
        LstBxFiles: TListBox;
        PgCtrlFile: TPageControl;
        Splitter1: TSplitter;
        TbShtGEOSSeq: TTabSheet;
        TbShtGEOSInfo: TTabSheet;
        TbShtGEOSVLIR: TTabSheet;
        procedure Button1Click(Sender: TObject);
        procedure CmbGEOSVLIRRecChange(Sender: TObject);
        procedure LstBxFilesSelectionChange(Sender: TObject; User: boolean);
    private
        FChanging: Boolean;
//      FEntries: TD64DirEntries;
//      FEntryData: array of Byte;
        FVLIRRecs: array of Word;
        FFileType: TGEOSFileType;
        FGEOSFileVersion: Word;

        procedure HexDumpStreamToLstBx(const AStream: TStream;
                const ALstBx: TListBox);

        procedure ClearGEOSInfo;

        procedure DoPrepareGEOSInfo(const AEntry: Integer;
                const AStream: TMemoryStream);
        procedure DoPrepareGEOSVLIR(const AEntry: Integer;
                const AStream: TMemoryStream);
        procedure DoPrepareGEOSSeq(const AEntry: Integer;
                const AStream: TMemoryStream);
        procedure DoPrepareGEOSVLIRRec(const ARecord: Integer);

    public
        procedure InitialiseDisplay;
        procedure InitialiseFileView(const AEntry: Integer);

        property  FileType: TGEOSFileType read FFileType;
        property  GEOSFileVersion: Word read FGEOSFileVersion;
    end;

implementation

{$R *.lfm}

uses
    DateUtils, Graphics, Clipbrd, C64D64ImageStrs, DModGEOSFileViewerMain;


const
    LIT_TOK_GEOSWRITECLS = 'Write Image ';
    LIT_TOK_GEOSPAINTCLS = 'Paint Image ';

{ TGEOSFileViewerMainFrame }

procedure TGEOSFileViewerMainFrame.LstBxFilesSelectionChange(Sender: TObject;
        User: boolean);
    var
    i: Integer;

    begin
    if not FChanging then
        for i:= 0 to LstBxFiles.Items.Count - 1 do
            if  LstBxFiles.Selected[i] then
                begin
                GEOSFileViewerMainDMod.Selected:= i;
                Break;
                end;
    end;

procedure TGEOSFileViewerMainFrame.CmbGEOSVLIRRecChange(Sender: TObject);
    begin
    if  not FChanging then
        DoPrepareGEOSVLIRRec(CmbGEOSVLIRRec.ItemIndex);
    end;

procedure TGEOSFileViewerMainFrame.Button1Click(Sender: TObject);
    begin
    Clipboard.AsText:= LstBxGEOSVLIRRec.Items.Text;
    end;

procedure TGEOSFileViewerMainFrame.HexDumpStreamToLstBx(const AStream: TStream;
        const ALstBx: TListBox);
    var
    i,
    j,
    l: Integer;
    s: string;
    b: Byte;
    d: array[0..$0F] of Byte;

    begin
    i:= 0;
    l:= 0;
//dengland      Stop the compiler complaining
    d[0]:= $00;
    s:= Format('%6.6x   ', [l]);
    while AStream.Position < AStream.Size do
        begin
        if  (i > 0)
        and (i mod 16 = 0) then
            begin
            s:= s + '  ';
            for j:= 0 to $0F do
                if  d[j] in [$20..$7E] then
                    s:= s + string(AnsiChar(d[j]))
                else
                    s:= s + ' ';
            ALstBx.Items.Add(s);

            Inc(l, i);
            i:= 0;
            s:= Format('%6.6x   ', [l]);
            end
        else if (i > 0)
        and (i mod 4 = 0) then
            s:= s + ' ';

        b:= AStream.ReadByte;
        s:= s + Format('%2.2x ', [b]);

        d[i]:= b;
        Inc(i);
        end;

    while i < 16 do
        begin
        if  (i > 0)
        and (i mod 4 = 0) then
            s:= s + ' ';

        s:= s + '   ';
        d[i]:= $00;

        Inc(i);
        end;

    s:= s + '  ';
    for j:= 0 to $0F do
        if  d[j] in [$20..$7E] then
            s:= s + string(AnsiChar(d[j]))
        else
            s:= s + ' ';

    ALstBx.Items.Add(s);
    end;

procedure TGEOSFileViewerMainFrame.DoPrepareGEOSInfo(const AEntry: Integer;
        const AStream: TMemoryStream);
    var
    d: TDateTime;
    b: Byte;
    s: string;
    i: Integer;
    v: Byte;
    f: Boolean;
    fv: Word;

    procedure DoDecodeGEOSIcon;
        var
        b: Byte;
        i,
        k,
        x,
        y: Integer;

        begin
        for y:= 0 to 20 do
            begin
            for i:= 0 to 2 do
                begin
                x:= i * 8;
                b:= AStream.ReadByte;
                k:= 128;
                while k >= 1 do
                    begin
                    if  (b and k) <> 0 then
                        ImgGEOSIcon.Picture.Bitmap.Canvas.Pixels[x, y]:= clBlack;

                    Inc(x);
                    if k = 1 then
                        k:= 0
                    else
                        k:= k shr 1;
                    end;
                end;
            end;
        end;

    function DoReadWord: Word;
        var
        b: Byte;

        begin
        b:= AStream.ReadByte;
        Result:= b;
        b:= AStream.ReadByte;
        Result:= Result or (b shl 8);
        end;

    function DoReadStringData(const ALen: Byte): string;
        var
        b: Byte;
        i: Integer;

        begin
        Result:= EmptyStr;
        for i:= 0 to ALen - 1 do
            begin
            b:= AStream.ReadByte;
            if  b in [$20..$7E] then
                begin
                Result:= Result + string(AnsiChar(b));
//dengland      These strings are going to labels which will use '&' characters
//              as shortcut references unless we prevent it with '&&';
                if  b = $26 then
                    Result:= Result + string(AnsiChar(b));
                end
            else
                Result:= Result + ' ';
            end;
        end;

    begin
    fv:= $0000;

    ClearGEOSInfo;

    LblGEOSStruct.Caption:= D64GEOSStructToStr(
            GEOSFileViewerMainDMod.Entries[AEntry].EntryData[$17]);
    LblGEOSType.Caption:= D64GEOSFileTypeToStr(
            GEOSFileViewerMainDMod.Entries[AEntry].EntryData[$18]);
    LblGEOSInfoLoc.Caption:= Format('$%2.2x $%2.2x', [
            GEOSFileViewerMainDMod.Entries[AEntry].EntryData[$15],
            GEOSFileViewerMainDMod.Entries[AEntry].EntryData[$16]]);

    d:= EncodeDateTime(1900 +
            GEOSFileViewerMainDMod.Entries[AEntry].EntryData[$19],
            GEOSFileViewerMainDMod.Entries[AEntry].EntryData[$1A],
            GEOSFileViewerMainDMod.Entries[AEntry].EntryData[$1B],
            GEOSFileViewerMainDMod.Entries[AEntry].EntryData[$1C],
            GEOSFileViewerMainDMod.Entries[AEntry].EntryData[$1D],
            0, 0);
    LblGEOSDate.Caption:= DateTimeToStr(d);

    if  GEOSFileViewerMainDMod.Entries[AEntry].EntryData[$15] > 0 then
        begin
        AStream.Clear;
        GEOSFileViewerMainDMod.D64Image.GetRawSector(
                GEOSFileViewerMainDMod.Entries[AEntry].EntryData[$15],
                GEOSFileViewerMainDMod.Entries[AEntry].EntryData[$16],
                AStream);

        AStream.Position:= 2;
        s:= EmptyStr;
        for i:= 0 to 2 do
            begin
            b:= AStream.ReadByte;
            s:= s + Format('$%2.2x ', [b]);
            end;
        LblGEOSIDBytes.Caption:= s;

        DoDecodeGEOSIcon;

        LblGEOSC64Type.Caption:= D64FileTypeToStr(AStream.ReadByte, False);

        b:= AStream.ReadByte;
        LblGEOSTypeInf.Caption:= D64GEOSFileTypeToStr(b);

        if  b = $08 then
            FFileType:= gftFont;

        v:= AStream.ReadByte;
        LblGEOSStructInf.Caption:= D64GEOSStructToStr(v);

        f:= v = $01;

        LblGEOSLoad.Caption:= Format('$%4.4x', [DoReadWord]);
        LblGEOSEnd.Caption:= Format('$%4.4x', [DoReadWord]);
        LblGEOSInit.Caption:= Format('$%4.4x', [DoReadWord]);

        s:= DoReadStringData($13);
        LblGEOSClass.Caption:= s;

        if  f
        and (CompareStr(LIT_TOK_GEOSWRITECLS, Copy(s, 1, 12)) = 0) then
            FFileType:= gftWrite;

        if  f
        and (CompareStr(LIT_TOK_GEOSPAINTCLS, Copy(s, 1, 12)) = 0) then
            FFileType:= gftPaint;

        if  FFileType <> gftOther then
            begin
            fv:= StrToInt(Copy(s, 16, 1));
            fv:= fv or (StrToInt(Copy(s, 14, 1))) shl 8;
            end;

        LblGEOS4080.Caption:= Format('$%2.2x', [AStream.ReadByte]);

        LblGEOSAuthor.Caption:= DoReadStringData($14);
        LblGEOSParent.Caption:= DoReadStringData($14);

        s:= EmptyStr;
        for i:= 0 to $16 do
            begin
            b:= AStream.ReadByte;
            s:= s + Format('$%2.2x ', [b]);
            end;
        LblGEOSAppData.Caption:= s;

        LblGEOSDesc.Caption:= DoReadStringData($60);

        AStream.Position:= $4D;
        HexDumpStreamToLstBx(AStream, LstBxGEOSData);
        end;

    FGEOSFileVersion:= fv;
    end;

procedure TGEOSFileViewerMainFrame.DoPrepareGEOSVLIR(const AEntry: Integer;
        const AStream: TMemoryStream);
    var
    c: Boolean;
    t,
    s: Byte;
    i: Integer;

    begin
    c:= FChanging;
    FChanging:= True;
    try
        AStream.Clear;
        GEOSFileViewerMainDMod.D64Image.GetRawSector(
                GEOSFileViewerMainDMod.Entries[AEntry].EntryData[$03],
                GEOSFileViewerMainDMod.Entries[AEntry].EntryData[$04], AStream);

        CmbGEOSVLIRRec.Items.BeginUpdate;
        try
            CmbGEOSVLIRRec.Items.Clear;

            SetLength(FVLIRRecs, 127);
            for i:= 0 to 126 do
                FVLIRRecs[i]:= $0000;

            AStream.Position:= $02;
            i:= 0;
            while AStream.Position < AStream.Size do
                begin
                t:= AStream.ReadByte;
                s:= AStream.ReadByte;

                FVLIRRecs[i]:= t or (s shl 8);

                if  t = $00 then
                    CmbGEOSVLIRRec.Items.Add('%3.3d:  %s ($%2.2x $%2.2x)', [i,
                            ChooseString(s = 0, STR_LBL_D64GEOSVREND,
                            STR_LBL_D64GEOSVRNON), t, s])
                else
                    CmbGEOSVLIRRec.Items.Add('%3.3d:  $%2.2x $%2.2x', [i, t, s]);

                Inc(i);
                end;

            finally
            CmbGEOSVLIRRec.Items.EndUpdate;
            end;

        finally
        FChanging:= c;
        end;

    CmbGEOSVLIRRec.ItemIndex:= 0;
    DoPrepareGEOSVLIRRec(0);
    end;

procedure TGEOSFileViewerMainFrame.DoPrepareGEOSSeq(const AEntry: Integer;
        const AStream: TMemoryStream);
    var
    sz: Cardinal;

    begin
    LblFileData.Caption:=
            Format('$%2.2x $%2.2x', [
            GEOSFileViewerMainDMod.Entries[AEntry].EntryData[$03],
            GEOSFileViewerMainDMod.Entries[AEntry].EntryData[$04]]);
    LblFileSize.Caption:= Format('$%4.4x (%0:d)',
            [GEOSFileViewerMainDMod.Entries[AEntry].FileSize]);

    LstBxFileData.Items.BeginUpdate;
    try
        LstBxFileData.Clear;

//dengland Not going to try reading scratched file data because if it was a CBM
//      partition, nasty things could happen.  Viewing scratched files is a
//      task for another routine.
        if  (GEOSFileViewerMainDMod.Entries[AEntry].EntryData[$03] <> 0)
        and ((GEOSFileViewerMainDMod.Entries[AEntry].FileType and $7) > 0) then
            begin
            AStream.Clear;

            GEOSFileViewerMainDMod.D64Image.GetDataChain(
                    GEOSFileViewerMainDMod.Entries[AEntry].EntryData[$03],
                    GEOSFileViewerMainDMod.Entries[AEntry].EntryData[$04],
                    AStream, sz);

            LblFileActual.Caption:= Format('$%6.6x (%0:d)', [sz]);

            AStream.Position:= 0;
            HexDumpStreamToLstBx(AStream, LstBxFileData);
            end
        else
            LblFileActual.Caption:= EmptyStr;

        finally
        LstBxFileData.Items.EndUpdate;
        end;
    end;

procedure TGEOSFileViewerMainFrame.DoPrepareGEOSVLIRRec(const ARecord: Integer);
    var
    t,
    s: Byte;
    m: TMemoryStream;
    sz: Cardinal;

    begin
    t:= FVLIRRecs[ARecord] and $00FF;
    s:= (FVLIRRecs[ARecord] and $FF00) shr 8;

    LstBxGEOSVLIRRec.Clear;
    LblGEOSVLIRActual.Caption:= EmptyStr;

    if  t = 0 then
        Exit;

    m:= TMemoryStream.Create;
    try
        GEOSFileViewerMainDMod.D64Image.GetDataChain(t, s, m, sz);

        m.Position:= 0;
        HexDumpStreamToLstBx(m, LstBxGEOSVLIRRec);

        LblGEOSVLIRActual.Caption:= Format('$%6.6x (%0:d)', [sz]);

        finally
        m.Free;
        end;
    end;

procedure TGEOSFileViewerMainFrame.ClearGEOSInfo;
    begin
    FFileType:= gftOther;
    FGEOSFileVersion:= $0000;

    LblGEOSStruct.Caption:= EmptyStr;
    LblGEOSType.Caption:= EmptyStr;
    LblGEOSInfoLoc.Caption:= EmptyStr;
    LblGEOSDate.Caption:= EmptyStr;
    LblGEOSIDBytes.Caption:= EmptyStr;

    if  not Assigned(ImgGEOSIcon.Picture.Bitmap) then
//dengland  Strangely, this isn't being called...
        ImgGEOSIcon.Picture.Bitmap:= TBitmap.Create;

    ImgGEOSIcon.Picture.Bitmap.Width:= 24;
    ImgGEOSIcon.Picture.Bitmap.Height:= 21;
    ImgGEOSIcon.Picture.Bitmap.Canvas.Brush.Color:= clNone;
    ImgGEOSIcon.Picture.Bitmap.Canvas.Pen.Color:= clBlack;
    ImgGEOSIcon.Picture.Bitmap.Canvas.FillRect(0, 0, 24, 21);

    LblGEOSC64Type.Caption:= EmptyStr;
    LblGEOSTypeInf.Caption:= EmptyStr;
    LblGEOSStructInf.Caption:= EmptyStr;
    LblGEOSLoad.Caption:= EmptyStr;
    LblGEOSEnd.Caption:= EmptyStr;
    LblGEOSInit.Caption:= EmptyStr;
    LblGEOSClass.Caption:= EmptyStr;
    LblGEOS4080.Caption:= EmptyStr;
    LblGEOSAuthor.Caption:= EmptyStr;
    LblGEOSParent.Caption:= EmptyStr;
    LblGEOSAppData.Caption:= EmptyStr;
    LblGEOSDesc.Caption:= EmptyStr;

    LstBxGEOSData.Items.Clear;
    end;

procedure TGEOSFileViewerMainFrame.InitialiseFileView(const AEntry: Integer);
    var
    m: TMemoryStream;

    begin
    m:= TMemoryStream.Create;
    try
        DoPrepareGEOSInfo(AEntry, m);

        if  GEOSFileViewerMainDMod.Entries[AEntry].EntryData[$17] = 1 then
            begin
            TbShtGEOSVLIR.TabVisible:= True;
            TbShtGEOSSeq.TabVisible:= False;

            DoPrepareGEOSVLIR(AEntry, m);
            end
        else
            begin
            TbShtGEOSVLIR.TabVisible:= False;
            TbShtGEOSSeq.TabVisible:= True;

            DoPrepareGEOSSeq(AEntry, m);
            end;

        PgCtrlFile.ActivePage:= TbShtGEOSInfo;

        finally
        m.Free;
        end;
    end;

procedure TGEOSFileViewerMainFrame.InitialiseDisplay;
    var
    i: Integer;

    begin
    FChanging:= True;
    try
        LstBxFiles.Items.BeginUpdate;
        try
            for i:= 0 to High(GEOSFileViewerMainDMod.Entries) do
                LstBxFiles.Items.Add(
                        GEOSFileViewerMainDMod.Entries[i].FileName);

            finally
            LstBxFiles.Items.EndUpdate;
            end;
        finally
        FChanging:= False;
        end;
    end;

end.

