//------------------------------------------------------------------------------
//GEOSFont
//========
//Class for manipulating a GEOS font.
//
//
//Please note:
//------------
//Presently, only FPC/Lazarus is supported.  Delphi support is incomplete.
//
//
//TODO:
//-----
//Move in style support from GEOS Designer and expose to user.
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
unit GEOSFont;

{$mode objfpc}{$H+}

interface

uses
    Graphics, Classes, SysUtils, GEOSTypes, C64D64Image;

type

{ TGEOSFont }

    TGEOSFont = class(TObject)
    private type
        TCharWidths = array of Byte;
        TCharOffsets = array of Word;
        TFontDetail = record
            Baseline: Byte;
            BytesPerLine: Word;
            Size: TGEOSFontSize;
//          OffsetsOffset: Word;
            PixelsOffset: Word;
            Offsets: TCharOffsets;
//dengland  Widths is actually not stored in the font data, I've calculated them
//              here for convienience.
            Widths: TCharWidths;
            Bitmap: TBitmap;
//dengland  We're going to keep the data so that we can rebuild the bitmaps as
//              the Pen and Brush colours change.  This is a speed optimisation;
//              I don't want to muck around with accessing the pixels via the
//              canvas interface (would be dreadfully slow) when displaying
//              text.  It does use a "lot" more memory, however.
            Data: TMemoryStream;
        end;

    private
        FName: string;
        FVersion: TGEOSVersion;
        FID: TGEOSFontID;
        FDetails: array of TFontDetail;
        FSizes: array of TGEOSFontSize;
        FSizeIdx: TGEOSFontSize;
        FPenColor: TColor;
        FBrushColor: TColor;
        FDirty: Boolean;
        FLocked: Integer;

        function  GetSize: TGEOSFontSize;
        procedure SetSize(const AValue: TGEOSFontSize);
        function  GetBaseline: Integer;
        procedure SetPenColor(const AValue: TColor);
        procedure SetBrushColor(const AValue: TColor);
        function  GetGlyphCount: Integer;
        function  GetSizesCount: Integer;
        function  GetSizes(const AIndex: Integer): TGEOSFontSize;

        procedure SetDirty(const AValue: Boolean);

        procedure DoPrepareDetails(const AIndex: Integer);
        procedure DoBuildSizes;
        procedure DoBuildBitmaps;

    public
        constructor Create(const AD64Image: TD64Image;
                const AEntry: TD64DirEntry; const AReport: TStrings = nil;
                const APen: TColor = clBlack;
                const ABrush: TColor = clWhite); overload;
        constructor Create(const APen: TColor = clBlack;
                const ABrush: TColor = clWhite); overload;
        destructor  Destroy; override;

        procedure Lock;
        procedure Unlock;

        procedure TextOut(const ACanvas: TCanvas; AX,AY: Integer;
                const AText: string);
        function  TextFitInfo(const AText: string; AMaxWidth: Integer): Integer;

        property  Name: string read FName;
        property  Version: TGEOSVersion read FVersion;
        property  ID: TGEOSFontID read FID;
        property  Size: TGEOSFontSize read GetSize write SetSize;
        property  Baseline: Integer read GetBaseline;
        property  GlyphCount: Integer read GetGlyphCount;
        property  SizesCount: Integer read GetSizesCount;
        property  Sizes[const AIndex: Integer]: TGEOSFontSize read GetSizes;
        property  PenColor: TColor read FPenColor write SetPenColor;
        property  BrushColor: TColor read FBrushColor write SetBrushColor;
    end;

implementation

const
    ARR_VAL_DAT_GEOSBSWFONT9: array[0..$2E7] of Byte = (
        $06,$3C,$00,$09,$08,$00,$CC,$00,$00,$00,$05,$00,$07,$00,$0B,$00,
        $11,$00,$17,$00,$1D,$00,$23,$00,$25,$00,$29,$00,$2D,$00,$33,$00,
        $39,$00,$3C,$00,$41,$00,$43,$00,$4A,$00,$4F,$00,$52,$00,$56,$00,
        $5A,$00,$5F,$00,$63,$00,$68,$00,$6D,$00,$72,$00,$77,$00,$79,$00,
        $7C,$00,$80,$00,$84,$00,$88,$00,$8E,$00,$94,$00,$9A,$00,$9F,$00,
        $A4,$00,$A9,$00,$AD,$00,$B1,$00,$B6,$00,$BC,$00,$BE,$00,$C2,$00,
        $C8,$00,$CC,$00,$D4,$00,$DA,$00,$E0,$00,$E5,$00,$EB,$00,$F0,$00,
        $F5,$00,$F9,$00,$FE,$00,$04,$01,$0C,$01,$12,$01,$18,$01,$1E,$01,
        $21,$01,$29,$01,$2C,$01,$32,$01,$3A,$01,$3E,$01,$43,$01,$48,$01,
        $4D,$01,$52,$01,$57,$01,$5A,$01,$5F,$01,$64,$01,$66,$01,$68,$01,
        $6D,$01,$6F,$01,$77,$01,$7C,$01,$82,$01,$87,$01,$8C,$01,$8F,$01,
        $93,$01,$96,$01,$9B,$01,$A1,$01,$A9,$01,$AF,$01,$B4,$01,$BA,$01,
        $BE,$01,$C0,$01,$C4,$01,$CA,$01,$D2,$01,$DD,$01,$02,$A5,$1E,$C1,
        $88,$A0,$80,$00,$00,$0C,$59,$82,$E2,$79,$8C,$00,$00,$38,$E1,$1C,
        $67,$3B,$99,$14,$51,$44,$14,$4E,$71,$CE,$3B,$A5,$14,$14,$51,$7D,
        $A0,$30,$00,$10,$08,$00,$40,$40,$85,$42,$00,$00,$00,$00,$00,$00,
        $00,$00,$00,$05,$42,$40,$07,$80,$02,$A5,$28,$CA,$09,$12,$A0,$00,
        $00,$52,$C4,$46,$84,$0A,$52,$01,$04,$45,$12,$92,$94,$A2,$25,$14,
        $52,$46,$36,$51,$4A,$29,$41,$25,$14,$14,$51,$05,$10,$10,$00,$08,
        $08,$00,$40,$80,$80,$42,$00,$00,$00,$00,$08,$00,$00,$00,$00,$09,
        $25,$80,$0F,$80,$02,$0F,$98,$12,$42,$09,$C2,$00,$00,$96,$44,$4A,
        $C8,$12,$52,$02,$72,$09,$74,$52,$84,$A2,$21,$14,$54,$45,$55,$51,
        $4A,$29,$21,$25,$14,$12,$91,$09,$08,$11,$00,$04,$EE,$31,$CC,$CE,
        $E5,$4A,$EC,$E3,$9C,$72,$6D,$28,$A4,$A2,$97,$89,$20,$00,$1C,$70,
        $02,$05,$08,$21,$E2,$0B,$E2,$00,$01,$1A,$48,$92,$2E,$21,$8E,$94,
        $01,$11,$54,$5C,$84,$BB,$A1,$F4,$58,$44,$94,$D1,$4A,$29,$11,$25,
        $14,$91,$0A,$11,$04,$12,$80,$01,$29,$4A,$52,$92,$95,$52,$92,$94,
        $52,$94,$89,$28,$A4,$94,$91,$11,$10,$00,$1C,$00,$02,$0F,$8C,$42,
        $42,$09,$CF,$87,$82,$12,$50,$5E,$29,$22,$42,$02,$72,$11,$77,$D2,
        $84,$A2,$2D,$14,$54,$44,$14,$51,$72,$2E,$09,$25,$15,$52,$84,$21,
        $02,$14,$40,$01,$29,$42,$5E,$92,$95,$62,$92,$94,$52,$94,$49,$28,
        $A4,$88,$92,$11,$10,$00,$1C,$70,$00,$05,$0A,$9A,$42,$0A,$A2,$00,
        $04,$12,$50,$42,$29,$22,$44,$01,$04,$01,$04,$52,$84,$A2,$25,$14,
        $52,$44,$14,$51,$42,$2A,$09,$24,$A6,$34,$44,$41,$01,$10,$00,$01,
        $29,$42,$50,$92,$95,$52,$92,$94,$52,$94,$29,$25,$24,$94,$94,$09,
        $20,$00,$0F,$80,$02,$05,$3C,$19,$C1,$10,$82,$10,$28,$0C,$5D,$82,
        $C6,$21,$88,$90,$00,$10,$E4,$5C,$77,$3A,$1D,$15,$91,$74,$14,$4E,
        $41,$C9,$71,$1C,$44,$14,$44,$7D,$00,$90,$00,$00,$AE,$39,$CE,$8E,
        $95,$4A,$92,$93,$9C,$74,$C4,$E2,$1B,$22,$77,$89,$20,$00,$07,$80,
        $00,$00,$08,$00,$00,$A0,$00,$20,$00,$00,$00,$00,$00,$00,$00,$20,
        $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$40,$00,$00,
        $00,$00,$00,$01,$80,$30,$00,$00,$00,$00,$00,$02,$01,$00,$00,$00,
        $10,$10,$00,$00,$00,$00,$10,$05,$40,$00,$00,$00,$00,$00,$00,$00,
        $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
        $00,$00,$00,$00,$00,$00,$00,$00,$00,$20,$00,$00,$00,$00,$00,$00,
        $00,$00,$3F,$C0,$00,$00,$00,$04,$02,$00,$00,$00,$10,$10,$00,$00,
        $00,$00,$20,$01,$00,$00,$00,$00);


{ TGEOSFont }

function TGEOSFont.GetSize: TGEOSFontSize;
    begin
    Result:= FDetails[FSizeIdx].Size;
    end;

procedure TGEOSFont.SetSize(const AValue: TGEOSFontSize);
    var
    i: Integer;

    begin
    i:= 0;
    while i < Length(FSizes) do
        begin
        if  FSizes[i] >= AValue then
            Break;
        Inc(i);
        end;

    if  (i = Length(FSizes))
    or  (FSizes[i] > AValue) then
        Exit;

    FSizeIdx:= i;
    end;

function TGEOSFont.GetBaseline: Integer;
    begin
    Result:= FDetails[FSizeIdx].Baseline;
    end;

procedure TGEOSFont.SetPenColor(const AValue: TColor);
    begin
    if  AValue <> FPenColor then
        begin
        FPenColor:= AValue;
        SetDirty(True);
        end;
    end;

procedure TGEOSFont.SetBrushColor(const AValue: TColor);
    begin
    if  AValue <> FBrushColor then
        begin
        FBrushColor:= AValue;
        SetDirty(True);
        end;
    end;

function TGEOSFont.GetGlyphCount: Integer;
    begin
    Result:= Length(FDetails[FSizeIdx].Widths);
    end;

function TGEOSFont.GetSizesCount: Integer;
    begin
    Result:= Length(FSizes)
    end;

function TGEOSFont.GetSizes(const AIndex: Integer): TGEOSFontSize;
    begin
    Result:= FSizes[AIndex];
    end;

procedure TGEOSFont.SetDirty(const AValue: Boolean);
    begin
    FDirty:= AValue;

    if  FDirty
    and (FLocked = 0) then
        DoBuildBitmaps;
    end;

procedure TGEOSFont.DoBuildSizes;
    var
    i: Integer;

    begin
    SetLength(FSizes, Length(FDetails));

    for i:= 0 to High(FDetails) do
        FSizes[i]:= FDetails[i].Size;
    end;

procedure TGEOSFont.DoPrepareDetails(const AIndex: Integer);
    var
    j: Integer;
    o: Word;
    g: Word;

    begin
    FDetails[AIndex].Data.Position:= 0;

    FDetails[AIndex].Baseline:= FDetails[AIndex].Data.ReadByte;
    FDetails[AIndex].BytesPerLine:= FDetails[AIndex].Data.ReadByte or
            (FDetails[AIndex].Data.ReadByte shl 8);
    FDetails[AIndex].Size:= FDetails[AIndex].Data.ReadByte;

    o:= FDetails[AIndex].Data.ReadByte or
            (FDetails[AIndex].Data.ReadByte shl 8);

    FDetails[AIndex].PixelsOffset:= FDetails[AIndex].Data.ReadByte or
            (FDetails[AIndex].Data.ReadByte shl 8);

    g:= (FDetails[AIndex].PixelsOffset - o) div 2;

    if  g = 0 then
        raise Exception.Create('Invalid font file.');

    SetLength(FDetails[AIndex].Offsets, g);
    SetLength(FDetails[AIndex].Widths, g - 1);

    for j:= 0 to High(FDetails[AIndex].Offsets) do
        FDetails[AIndex].Offsets[j]:= FDetails[AIndex].Data.ReadByte or
            (FDetails[AIndex].Data.ReadByte shl 8);

    for j:= 0 to High(FDetails[AIndex].Widths) do
        FDetails[AIndex].Widths[j]:= FDetails[AIndex].Offsets[j + 1] -
                FDetails[AIndex].Offsets[j];
    end;

procedure TGEOSFont.DoBuildBitmaps;
    var
    i,
    j,
    k: Integer;
    b: Byte;
    d: Byte;
    x,
    y: Integer;

    begin
    for i:= 0 to High(FDetails) do
        begin
        if  not Assigned(FDetails[i].Bitmap) then
            FDetails[i].Bitmap:= TBitmap.Create;

        FDetails[i].Bitmap.SetSize(FDetails[i].BytesPerLine * 8,
                FDetails[i].Size);

        FDetails[i].Data.Position:= FDetails[i].PixelsOffset;

        for y:= 0 to FDetails[i].Size - 1 do
            begin
            x:= 0;
            for j:= 0 to FDetails[i].BytesPerLine - 1 do
                begin
                d:= 128;
                b:= FDetails[i].Data.ReadByte;
                for k:= 7 downto 0 do
                    begin
                    if  (b and d) <> 0 then
                        FDetails[i].Bitmap.Canvas.Pixels[x, y]:= FPenColor
                    else
                        FDetails[i].Bitmap.Canvas.Pixels[x, y]:= FBrushColor;

                    d:= d shr 1;
                    Inc(x);
                    end;
                end;
            end;
        end;
    end;

constructor TGEOSFont.Create(const AD64Image: TD64Image;
        const AEntry: TD64DirEntry; const AReport: TStrings; const APen: TColor;
        const ABrush: TColor);
    var
    infoFork: array[$00..$FF] of Byte;
    VLIRFork: array[$00..$7F] of TD64TrackSectorRef;
    sz: array of TGEOSFontSizeID;

    procedure DoReportString(const AString: string = '');
        begin
        if  Assigned(AReport) then
            AReport.Add(AString);
        end;

    procedure DoGetInitialData;
        var
        m: TMemoryStream;

        begin
        m:= TMemoryStream.Create;
        try
            AD64Image.GetRawSector(AEntry.EntryData[$15], AEntry.EntryData[$16], m);
            m.Position:= 0;
            m.ReadBuffer(infoFork[0], $100);

            m.Clear;
            AD64Image.GetRawSector(AEntry.EntryData[$03], AEntry.EntryData[$04], m);
            m.Position:= 2;
            m.ReadBuffer(VLIRFork[0], $FE);

            finally
            m.Free;
            end;
        end;

    procedure DoGetHeaderID;
        var
        i: Integer;

        begin
        FName:= '';
        for i:= $4D to $58 do
            if  infoFork[i] in [$20..$7E] then
                FName:= FName + string(AnsiChar(infoFork[i]))
            else
                FName:= FName + ' ';

        FVersion.Minor:= infoFork[$5C] - Ord('0');
        FVersion.Major:= infoFork[$5A] - Ord('0');

        FID:= infoFork[$80] or (infoFork[$81] shl 8);

        DoReportString(Format('Font Name:  %s (Version %d.%d); ID:  $%4.4x', [FName,
                FVersion.Major, FVersion.Minor, FID]));
        DoReportString;
        end;

    procedure DoGetHeaderSizes;
        var
        i: Integer;
        w: Word;
        o: Byte;

        begin
        SetLength(sz, 32);
        o:= $82;
        for i:= 0 to 31 do
            begin
            w:= infoFork[o] or (infoFork[o + 1] shl 8);
            Inc(o, 2);
            sz[i].SetFromWord(w);
            end;
        end;

    procedure DoCheckHeaderInfo;
        var
        i,
        j: Integer;
        f: Boolean;

        begin
        DoReportString('Header font sizes:');

        i:= 0;
        while (i < Length(sz)) and (sz[i].ID > 0) do
            begin
            if  sz[i].ID <> FID then
                DoReportString(Format('%3.3d - ID does not match', [sz[i].Size]))
            else
                DoReportString(Format('%3.3d - ID matched', [sz[i].Size]));
            Inc(i);
            end;

        DoReportString;

        DoReportString('Record font sizes:');
        f:= False;
        i:= 0;
        while (i < Length(sz)) and (sz[i].ID > 0) do
            begin
            if  VLIRFork[sz[i].Size].Track = 0 then
                begin
                DoReportString(Format('%3.3d - Not found', [sz[i].Size]));
                f:= True;
                end
            else
                DoReportString(Format('%3.3d - Found', [sz[i].Size]));
            Inc(i);
            end;

        for i:= Low(TGEOSFontSize) to High(TGEOSFontSize) do
            if  VLIRFork[i].Track <> 0 then
                begin
//dengland      Was going to check incrementally as found but its only 32
//              entries and this will be more reliable.
                j:= 0;
                while j < Length(sz) do
                    begin
                    if  sz[j].Size = i then
                        Break;
                    Inc(j);
                    end;
                if  j = Length(sz) then
                    begin
                    f:= True;
                    DoReportString(Format(
                            '%3.3d - Record found but not in header.', [i]));
                    end;
                end;

        if  f then
            DoReportString('Record inconsistencies found.  Please check results.');

        DoReportString;
        end;

    procedure DoLoadRecordData;
        var
        i: Integer;
        cnt: Integer;
        sz: Cardinal;

        begin
        cnt:= 0;
        for i:= 0 to High(VLIRFork) do
            if  VLIRFork[i].Track <> 0 then
                Inc(cnt);

        SetLength(FDetails, cnt);

        cnt:= 0;
        for i:= 0 to High(VLIRFork) do
            if  VLIRFork[i].Track <> 0 then
                begin
                FDetails[cnt].Data:= TMemoryStream.Create;
                AD64Image.GetDataChain(VLIRFork[i].Track, VLIRFork[i].Sector,
                        FDetails[cnt].Data, sz);
                FDetails[cnt].Data.Size:= sz;

                DoPrepareDetails(cnt);

                Inc(cnt);
                end;
        end;

    begin
    FPenColor:= APen;
    FBrushColor:= ABrush;

//dengland Keep compiler happy
    infoFork[0]:= 0;
    VLIRfork[0].Track:= 0;

    DoGetInitialData;

    DoGetHeaderID;

    DoGetHeaderSizes;

    if  Assigned(AReport) then
        DoCheckHeaderInfo;

    DoLoadRecordData;

    DoBuildBitmaps;

    DoBuildSizes;

    FSizeIdx:= 0;
    end;

constructor TGEOSFont.Create(const APen: TColor; const ABrush: TColor);
    procedure DoLoadSystemFontData;
        begin
        SetLength(FDetails, 1);

        FDetails[0].Data:= TMemoryStream.Create;
        FDetails[0].Data.Write(ARR_VAL_DAT_GEOSBSWFONT9,
                SizeOf(ARR_VAL_DAT_GEOSBSWFONT9));
        FDetails[0].Data.Position:= 0;

        DoPrepareDetails(0);
        end;

    begin
    FPenColor:= APen;
    FBrushColor:= ABrush;

    FName:= 'BSW';
    FVersion.Minor:= 0;
    FVersion.Major:= 1;
    FID:= 1;

    DoLoadSystemFontData;

    DoBuildBitmaps;

    DoBuildSizes;

    FSizeIdx:= 0;
    end;

destructor TGEOSFont.Destroy;
    var
    i: Integer;

    begin
    for i:= High(FDetails) downto 0 do
        begin
        if  Assigned(FDetails[i].Bitmap) then
            FDetails[i].Bitmap.Free;

        if  Assigned(FDetails[i].Data) then
            FDetails[i].Data.Free;
        end;

    SetLength(FDetails, 0);

    inherited Destroy;
    end;

procedure TGEOSFont.Lock;
    begin
    Inc(FLocked);
    end;

procedure TGEOSFont.Unlock;
    begin
    if  FLocked > 0 then
        Dec(FLocked);

    if  FDirty
    and (FLocked = 0) then
        DoBuildBitmaps;
    end;

procedure TGEOSFont.TextOut(const ACanvas: TCanvas; AX, AY: Integer;
        const AText: string);
    var
    i: Integer;
    c: AnsiChar;
    b: Byte;
    w: Byte;

    begin
    for i:= 1 to Length(AText) do
        begin
        c:= AnsiChar(AText[i]);
        if  c > #$1F then
            b:= Ord(c) - $20
        else
            b:= $00;

        if  Length(FDetails[FSizeIdx].Widths) > b then
            begin
            w:= FDetails[FSizeIdx].Widths[b];

            ACanvas.CopyRect(Rect(AX, AY, AX + w, AY + FSizes[FSizeIdx]),
                    FDetails[FSizeIdx].Bitmap.Canvas,
                    Rect(FDetails[FSizeIdx].Offsets[b], 0,
                    FDetails[FSizeIdx].Offsets[b] + w, FSizes[FSizeIdx]));

            Inc(AX, w);
            end;
        end;
    end;

function TGEOSFont.TextFitInfo(const AText: string;
        AMaxWidth: Integer): Integer;
    var
    i: Integer;
    c: AnsiChar;
    b: Byte;
    w: Byte;
    m: Integer;

    begin
    m:= 0;
    Result:= 0;
    for i:= 1 to Length(AText) do
        begin
        c:= AnsiChar(AText[i]);
        if  c > #$1F then
            b:= Ord(c) - $20
        else
            b:= $00;

        if  Length(FDetails[FSizeIdx].Widths) > b then
            w:= FDetails[FSizeIdx].Widths[b]
        else
            w:= 0;

        if  (m + w) > AMaxWidth then
            Break;

        Inc(Result);
        Inc(m, w);
        end;
    end;

end.

