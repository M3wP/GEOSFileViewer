//------------------------------------------------------------------------------
//GEOSTypes
//=========
//Standard type and constant defines for GEOS.
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
unit GEOSTypes;

{$MODE DELPHI}
{$H+}

interface

uses
    Graphics, Classes, SysUtils;

const
    clC64Black = TColor($000000);
    clC64White = TColor($FFFFFF);
    clC64Red = TColor($3A4592);
    clC64Cyan = TColor($D3CA83);
    clC64Purple = TColor($B94C93);
    clC64Green = TColor($46B46F);
    clC64Blue = TColor($AD3543);
    clC64Yellow = TColor($79E9DD);
    clC64Orange = TColor($28669A);
    clC64Brown = TColor($004D63);
    clC64LtRed = TColor($757FC7);
    clC64DarkGrey = TColor($5C5C5C);
    clC64Grey = TColor($898989);
    clC64LtGreen = TColor($91F8B7);
    clC64LtBlue = TColor($E77884);
    clC64LightGrey = TColor($B7B7B7);

    ARR_COL_C64MEWPALETTE: array[0..15] of TColor = (
        clC64Black, clC64White, clC64Red, clC64Cyan, clC64Purple, clC64Green,
        clC64Blue, clC64Yellow, clC64Orange, clC64Brown, clC64LtRed,
        clC64DarkGrey, clC64Grey, clC64LtGreen, clC64LtBlue, clC64LightGrey);

type
    TD64TrackSectorRef = packed record
        Track,
        Sector: Byte;
    end;

    TGEOSVersion = packed record
        Major,
        Minor: Byte;
    end;

    TGEOSFontID = 0..$3FF;
    TGEOSFontSize = 0..$3F;

{ TC64GEOSFontSizeID }
    TGEOSFontSizeID = packed record
        ID: TGEOSFontID;
        Size: TGEOSFontSize;

        procedure SetFromWord(AValue: Word);
        function  GetAsWord: Word;
    end;

implementation

{ TC64GEOSFontSizeID }

procedure TGEOSFontSizeID.SetFromWord(AValue: Word);
    begin
    ID:= (AValue and $FFC0) shr 6;
    Size:= AValue and $3F;
    end;

function TGEOSFontSizeID.GetAsWord: Word;
    begin
    Result:= (ID shl 6) or Size;
    end;

end.

