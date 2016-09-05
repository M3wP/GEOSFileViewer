//------------------------------------------------------------------------------
//GEOSWriteFile
//=============
//Class for manipulating a geoWrite document file.
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
unit GEOSWriteFile;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, C64D64Image, GEOSTypes;

type
    TGEOSWritePage = 1..63;
    TGEOSWriteImage = 1..64;

const
    VAL_IDX_GEOSWRITEHDRPG = 62;
    VAL_IDX_GEOSWRITEFTRPG = 63;

type

{ TGEOSWriteFile }

    TGEOSWriteFile = class(TObject)
    private
        FD64Image: TD64Image;
        FEntryData: array[$00..$1F] of Byte;
        FVersion: TGEOSVersion;
        FInfoFork: array[$00..$FF] of Byte;
        FVLIRFork: array[$00..$7F] of TD64TrackSectorRef;
        FPageHeight,
        FHeaderHeight,
        FFooterHeight,
        FPageWidth: Word;
        FTitlePage,
        FHasHeader,
        FHasFooter: Boolean;
        FPageCount: Byte;
        FStartPage: Word;
        FImageCount: Byte;

        procedure DoLoadFromImage;

        procedure DoInitialiseVersion1;
        procedure DoInitialiseVersion2;

        procedure DoConvertVersion1Page(const AStreamIn,
                AStreamOut: TMemoryStream; const ASize: Cardinal);
        procedure DoLoadPageToStream(const APageIdx: Integer;
                const AStream: TMemoryStream; out AActualSize: Cardinal);

    public
        constructor Create(const AD64Image: TD64Image;
                const AEntry: TD64DirEntry);

        procedure ConvertDocumentToStrings(const AStrings: TStrings);
    end;

    EGEOSWriteFileUnkVer = class(Exception);


implementation

resourcestring
    STR_MSG_GEOSWRTUNKVER = 'geoWrite file version unsupported.';

{ TGEOSWriteFile }

procedure TGEOSWriteFile.DoLoadFromImage;
    var
    m: TMemoryStream;

    begin
    m:= TMemoryStream.Create;
    try
        FD64Image.GetRawSector(FEntryData[$15], FEntryData[$16], m);
        m.Position:= 0;
        m.ReadBuffer(FInfoFork[0], $100);

        m.Clear;
        FD64Image.GetRawSector(FEntryData[$03], FEntryData[$04], m);
        m.Position:= 2;
        m.ReadBuffer(FVLIRFork[0], $FE);

        FVersion.Minor:= FInfoFork[$5C] - Ord('0');
        FVersion.Major:= FInfoFork[$5A] - Ord('0');

        finally
        m.Free;
        end;

    if  FVersion.Major = 1 then
        DoInitialiseVersion1
    else if FVersion.Major = 2 then
        DoInitialiseVersion2
    else
        raise EGEOSWriteFileUnkVer.Create(STR_MSG_GEOSWRTUNKVER);
    end;

procedure TGEOSWriteFile.DoInitialiseVersion1;
    begin

    end;

procedure TGEOSWriteFile.DoInitialiseVersion2;
    begin

    end;

procedure TGEOSWriteFile.DoConvertVersion1Page(const AStreamIn,
        AStreamOut: TMemoryStream; const ASize: Cardinal);
    var
    i: Integer;
    b: Byte;

    begin
//  Convert version 1 header to a proper ruler escape
    AStreamOut.WriteByte($11);
    for i:= 0 to 19 do
        begin
        b:= AStreamIn.ReadByte;
        AStreamOut.WriteByte(b);
        end;
    for i:= 0 to 5 do
        AStreamOut.WriteByte(0);

    AStreamOut.CopyFrom(AStreamIn, ASize - 20);
    end;

procedure TGEOSWriteFile.DoLoadPageToStream(const APageIdx: Integer;
        const AStream: TMemoryStream; out AActualSize: Cardinal);
    var
    m: TMemoryStream;
    sz: Cardinal;

    begin
    if  FVersion.Major = 1 then
        m:= TMemoryStream.Create
    else
        m:= AStream;
    try
        m.Clear;
        FD64Image.GetDataChain(FVLIRFork[APageIdx].Track,
                FVLIRFork[APageIdx].Sector, m, sz);

        if  FVersion.Major = 1 then
            begin
            m.Position:= 0;
            AStream.Clear;
            DoConvertVersion1Page(m, AStream, sz);
            sz:= AStream.Size;
            end;

        AStream.Position:= 0;
        AActualSize:= sz;

        finally
        if  FVersion.Major = 1 then
            m.Free;
        end;
    end;

constructor TGEOSWriteFile.Create(const AD64Image: TD64Image;
        const AEntry: TD64DirEntry);
    begin
    FD64Image:= AD64Image;
    Move(AEntry.EntryData[0], FEntryData[0], SizeOf(FEntryData));

    DoLoadFromImage;
    end;

procedure TGEOSWriteFile.ConvertDocumentToStrings(const AStrings: TStrings);
    var
    m: TMemoryStream;
    i: Integer;
    sz: Cardinal;
    b: Byte;
    s: string;

    begin
    AStrings.BeginUpdate;
    try
        AStrings.Clear;

        m:= TMemoryStream.Create;
        try
            i:= 0;
            while (i < 61) and (FVLIRFork[i].Track > $00) do
                begin
                DoLoadPageToStream(i, m, sz);

                s:= EmptyStr;
                while m.Position < sz do
                    begin
                    b:= m.ReadByte;
//                  Graphics escape
                    if b = $10 then
                        begin
                        m.Position:= m.Position + 4;
                        s:= s + '<!-- @image -->';
                        AStrings.Add(s);
                        s:= EmptyStr;
                        end
//                  Ruler escape
                    else if  b = $11 then
                        m.Position:= m.Position + 26
//                  Font/style escape
                    else if b = $17 then
                        m.Position:= m.Position + 3
//                  End of document/page/paragraph.
//dengland          Apparently v1 uses $0C, as well for page breaks.
                    else if b in [$00, $01, $0C, $0D] then
                        begin
                        if  b in [$00, $01, $0C] then
                            s:= s + #$0C;

                        AStrings.Add(s);
                        s:= EmptyStr;
                        end
//                  "Tab escape"
                    else if b = $09 then
                        s:= s + #$09
                    else if b in [$20..$7E] then
                        s:= s + string(AnsiChar(b))
                    else
                        s:= s + '<!-- &unk; -->';
                    end;

                Inc(i);
                end;

            finally
            m.Free;
            end;

        finally
        AStrings.EndUpdate;
        end;
    end;

end.

