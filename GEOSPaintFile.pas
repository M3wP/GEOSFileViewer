//------------------------------------------------------------------------------
//GEOSPainFile
//============
//Class for manipulating a geoPaint file.
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
unit GEOSPaintFile;

{$mode objfpc}{$H+}

interface

uses
    Graphics, Classes, SysUtils, GEOSTypes, C64D64Image;

type

{ TGEOSPaintFile }

    TGEOSPaintFile = class(TObject)
    private
        FD64Image: TD64Image;
        FEntryData: array[$00..$1F] of Byte;
        FVersion: TGEOSVersion;
        FBitmap: TBitmap;

        procedure DoLoadFromImage;

    public
        constructor Create(const AD64Image: TD64Image;
                const AEntry: TD64DirEntry);
        destructor  Destroy; override;

        property  Bitmap: TBitmap read FBitmap;
    end;


implementation


{ TGEOSPaintFile }
{$IFOPT R-}
    {$DEFINE DEF_FLG_GEOSPNTUNDOR}
{$ENDIF}
{$R+}
procedure TGEOSPaintFile.DoLoadFromImage;
    const
    VAL_COL_DEFAULTFGBG = $BF;

    var
    i,
    y: Integer;
    m: TMemoryStream;
    sz: Cardinal;
    infoFork: array[$00..$FF] of Byte;
    VLIRFork: array[$00..$7F] of TD64TrackSectorRef;
//dengland Apparently, there was an "oversight" in geoPaint 1.0 so there is an
//      extra byte.
    rowBits: array[0..160, 0..7] of Byte;
    rowClrs: array[0..159] of Byte;
    bm: TBitmap;

    procedure DoGetHeaderInfo;
        begin
        FD64Image.GetRawSector(FEntryData[$15], FEntryData[$16], m);
        m.Position:= 0;
        m.ReadBuffer(infoFork[0], $100);

        m.Clear;
        FD64Image.GetRawSector(FEntryData[$03], FEntryData[$04], m);
        m.Position:= 2;
        m.ReadBuffer(VLIRFork[0], $FE);

        FVersion.Minor:= infoFork[$5C] - Ord('0');
        FVersion.Major:= infoFork[$5A] - Ord('0');
        end;

    procedure DoClearRowClrs; inline;
        begin
        FillChar(rowClrs[0], 160, VAL_COL_DEFAULTFGBG);
        end;

    procedure DoClearRowBits; inline;
        begin
        FillChar(rowBits[0], 160 * 8, 0);
        end;

    procedure DoDecodeDataBlock;
        var
        x,
        y: Integer;
        b: Byte;
        i,
        j: Integer;
        c: array[0..7] of Byte;

        procedure DoIncrementBitsPos; inline;
            begin
            Inc(y);
            if  y = 8 then
                begin
                Inc(x);
                y:= 0;
                end;
            end;

        begin
        x:= 0;
        y:= 0;
        while m.Position < sz do
            begin
//dengland  Hopefully the bitmap and colour data is compressed separately all
//          the time.  We need this, apparently.
            if  x >= 161 then
                Break;

            b:= m.ReadByte;

            if  b = 0 then
                Break
            else if b in [$01..$3F] then
                for i:= 0 to b - 1 do
                    begin
                    rowBits[x, y]:= m.ReadByte;
                    DoIncrementBitsPos;
                    end
            else if b in [$41..$7F] then
                begin
                Dec(b, $41);

                for i:= 0 to 7 do
                    c[i]:= m.ReadByte;

                for i:= 0 to b do
                    for j:= 0 to 7 do
                        begin
                        rowBits[x, y]:= c[j];
                        DoIncrementBitsPos;
                        end
                end
            else if b in [$81..$FF] then
                begin
                Dec(b, $81);

                c[0]:= m.ReadByte;

                for i:= 0 to b do
                    begin
                    rowBits[x, y]:= c[0];
                    DoIncrementBitsPos;
                    end
                end
            else
                raise Exception.Create('Invalid command byte in stream.');
            end;

        x:= 0;
        while m.Position < sz do
            begin
//dengland  It seems that some files have the incorrect size...
            if  x >= 160 then
                Break;

            b:= m.ReadByte;

            if  b = 0 then
                Break
            else if b in [$01..$3F] then
                for i:= 0 to b - 1 do
                    begin
                    rowClrs[x]:= m.ReadByte;
                    Inc(x);
                    end
            else if b in [$41..$7F] then
                begin
                Dec(b, $41);

                for i:= 0 to 7 do
                    c[i]:= m.ReadByte;

                for i:= 0 to b do
                    for j:= 0 to 7 do
                        begin
                        rowClrs[x]:= c[j];
                        Inc(x);
                        end
                end
            else if b in [$81..$FF] then
                begin
                Dec(b, $81);

                c[0]:= m.ReadByte;

                for i:= 0 to b do
                    begin
                    rowClrs[x]:= c[0];
                    Inc(x);
                    end
                end
            else
                raise Exception.Create('Invalid command byte in stream.');
            end;
        end;

    procedure DoPaintRows;
        var
        r,
        i,
        j,
        k: Integer;
        d: Byte;
        o,
        x,
        y: Integer;
        pc,
        bc: TColor;

        begin
        bm.Canvas.Brush.Color:= clC64LightGrey;
        bm.Canvas.Brush.Style:= bsSolid;
        bm.Canvas.FillRect(0, 0, 620, 16);

        for r:= 0 to 1 do
            begin
            y:= r * 8;
            o:= r * 80;

            for i:= 0 to 79 do
                begin
                pc:= ARR_COL_C64MEWPALETTE[(rowClrs[i + o] and $F0) shr 4];
                bc:= ARR_COL_C64MEWPALETTE[rowClrs[i + o] and $0F];

                for j:= 0 to 7 do
                    begin
                    d:= 128;
                    x:= i * 8;
                    for k:= 7 downto 0 do
                        begin
                        if  (rowBits[i + o, j] and d) <> 0 then
                            bm.Canvas.Pixels[x, y + j]:= pc
                        else
                            bm.Canvas.Pixels[x, y + j]:= bc;

                        d:= d shr 1;
                        Inc(x);
                        end;
                    end;
                end;
            end;
        end;

    procedure DoCopyRows;
        begin
        FBitmap.Canvas.CopyRect(Rect(0, y, 640, y + 16), bm.Canvas,
                Rect(0, 0, 640, 16));

        Inc(y, 16);
        end;

    begin
    y:= 0;
    bm:= TBitmap.Create;
    m:= TMemoryStream.Create;
    try
        bm.SetSize(640, 16);

        DoGetHeaderInfo;

        for i:= 0 to 44 do
            begin
            DoClearRowClrs;
            DoClearRowBits;

            try
                if  VLIRFork[i].Track > 0 then
                    begin
                    m.Clear;
                    FD64Image.GetDataChain(VLIRFork[i].Track, VLIRFork[i].Sector,
                            m, sz);
                    m.Position:= 0;
                    DoDecodeDataBlock;
                    end;
                except
//              WriteLn(i);
                end;
            DoPaintRows;
            DoCopyRows;
            end;

        finally
        m.Free;
        bm.Free;
        end;
    end;
{$IFDEF DEFINE DEF_FLG_GEOSPNTUNDOR}
    {$R-}
{$ENDIF}


constructor TGEOSPaintFile.Create(const AD64Image: TD64Image;
        const AEntry: TD64DirEntry);
    begin
    FBitmap:= TBitmap.Create;
    FBitmap.SetSize(640, 720);
    FBitmap.Canvas.Brush.Color:= clC64LightGrey;
    FBitmap.Canvas.Brush.Style:= bsSolid;
    FBitmap.Canvas.FillRect(0, 0, 640, 720);

    FD64Image:= AD64Image;
    Move(AEntry.EntryData[0], FEntryData[0], SizeOf(FEntryData));

    DoLoadFromImage;
    end;

destructor TGEOSPaintFile.Destroy;
    begin
    if  Assigned(FBitmap) then
        FBitmap.Free;

    inherited Destroy;
    end;




end.

