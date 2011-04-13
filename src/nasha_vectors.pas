{*******************************************************************************
*                              NashaENGINE                                     *
*                      Copyright © 2010 Nathan Schagen                         *
*                            www.nashasoft.com                                 *
*                         chronozphere@gmail.com                               *
********************************************************************************
*                                                                              *
*  This file is part of NashaENGINE                                            *
*                                                                              *
*  The NashaENGINE library is free software: you can redistribute              *
*  it and/or modify it under the terms of the GNU Lesser General Public        *
*  License as published by the Free Software Foundation, either version 3      *
*  of the License, or any later version.                                       *
*                                                                              *
*  NashaENGINE is distributed in the hope that                                 *
*  it will be useful, but WITHOUT ANY WARRANTY; without even the               *
*  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.    *
*  See the GNU Lesser General Public License for more details.                 *
*                                                                              *
*  You should have received a copy of the GNU General Public License           *
*  along with the NashaENGINE. If not, see                                     *
*  http://www.gnu.org/licenses/                                                *
*                                                                              *
*******************************************************************************}

unit nasha_vectors;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  math,
  nasha_math;

type
  PVector2i = ^TVector2i;
  TVector2i = record
   case Boolean of
     true: ( x, y: Integer; );
     false: ( xy: array [0..1] of Integer; );
  end;

  PVector3i = ^TVector3i;
  TVector3i = record
   case Boolean of
     true: ( x, y, z: Integer; );
     false: ( xyz: array [0..2] of Integer; );
  end;

  PVector4i = ^TVector4i;
  TVector4i = record
   case Boolean of
     true: ( x, y, z, w: Integer; );
     false: ( xyzw: array [0..3] of Integer; );
  end;

  PVector2f = ^TVector2f;
  TVector2f = record
   case Boolean of
     true: ( x, y: Single; );
     false: ( xy: array [0..1] of Single; );
  end;

  PVector3f = ^TVector3f;
  TVector3f = record
   case Boolean of
     true: ( x, y, z: Single; );
     false: ( xyz: array [0..2] of Single; );
  end;

  PVector4f = ^TVector4f;
  TVector4f = record
   case Boolean of
     true: ( x, y, z, w: Single; );
     false: ( xyzw: array [0..3] of Single; );
  end;

  //2D Integer vectors

  function Vec2i(x, y: Integer): TVector2i; overload;
  function Vec2i(aVec: TVector2f): TVector2i; overload;
  function Vec2iAdd(const aV1, aV2: TVector2i): TVector2i;
  function Vec2iSub(const aV1, aV2: TVector2i): TVector2i;
  function vec2iLength(const aVec: TVector2i): Single;

  //2D float vectors

  function Vec2f(x, y: Single): TVector2f; overload;
  function Vec2f(aVec: TVector2i): TVector2f; overload;
  function vec2fAdd(const aV1, aV2: TVector2f): TVector2f;
  function vec2fSub(const aV1, aV2: TVector2f): TVector2f;
  function vec2fScale(const aVec: TVector2f; const aXScale, aYScale: Single): TVector2f;
  function vec2fScaleFactor(const aVec: TVector2f; const aFactor: Single): TVector2f;
  function vec2fLength(const aVec: TVector2f): Single;
  function vec2fLengthSqr(const aVec: TVector2f): Single;
  function vec2fNorm(const aVec: TVector2f): TVector2f;
  function vec2fDot(const aV1, aV2: TVector2f): Single;
  function Vec2fNeg(const aVec: TVector2f): TVector2f;
  function vec2fAngle(const aVec: TVector2f): Single;
  function Vec2fEquals(const aV1, aV2: TVector2f): Boolean;

  //3D Integer vectors

  function Vec3i(x, y, z: Integer): TVector3i;

  //3D float vectors

  function Vec3f(x, y, z: Single): TVector3f;
  function VecAdd(const aV1, aV2 : TVector3f): TVector3f;
  function VecSub(const aV1, aV2 : TVector3f): TVector3f;
  function VecScaleFactor(const aV : TVector3f; const aF : Single ): TVector3f;
  function VecScale(const aV1, aV2 : TVector3f): TVector3f;
  function VecCompare(const aV1, aV2 : TVector3f): boolean;
  function VecNegate(const aV: TVector3f): TVector3f;
  function VecAbs(const aV: TVector3f): TVector3f;
  function VecDistance(const aV1, aV2 : TVector3f): single;
  function VecLength(const aV: TVector3f): single;
  function VecLengthSqr(const aV: TVector3f): single;
  function VecNorm(const aV: TVector3f): TVector3f;
  function VecDot(const aV1, aV2 : TVector3f): single;
  function VecCross(const aV1, aV2 : TVector3f): TVector3f;
  function VecAngle(const aV1, aV2 : TVector3f) : single;
  function VecInterpolate(const aV1, aV2 : TVector3f; const aK: single): TVector3f;
  function VecEquals(const aV1, aV2: TVector3f): Boolean;
  function VecSnap(const aV: TVector3f; gridsize: Single): TVector3f;
  //4D float vectors

  function Vec4f(x, y, z, w: Single): TVector4f;
  function Vec4fEquals(const aV1, aV2: TVector4f): Boolean;

const
  //Some constants
  Vec3f_Zero: TVector3f = (x: 0; y: 0; z: 0);
  Vec3f_One:  TVector3f = (x: 1; y: 1; z: 1);

implementation

//2D integer vectors

function Vec2i(x, y: Integer): TVector2i;
begin
  Result.x := x;
  Result.y := y;
end;

function Vec2i(aVec: TVector2f): TVector2i;
begin
  Result.x := Round(aVec.x);
  Result.y := Round(aVec.y);
end;

function Vec2iAdd(const aV1, aV2: TVector2i): TVector2i;
begin
  Result.x := aV1.x + aV2.x;
  Result.y := aV1.y + aV2.y;
end;

function Vec2iSub(const aV1, aV2: TVector2i): TVector2i;
begin
  Result.x := aV1.x - aV2.x;
  Result.y := aV1.y - aV2.y;
end;

function vec2iLength(const aVec: TVector2i): Single;
begin
  Result := Sqrt(aVec.X*aVec.X + aVec.Y*aVec.Y);
end;

//2D Float vectors

function Vec2f(x, y: Single): TVector2f;
begin
  Result.x := x;
  Result.y := y;
end;

function Vec2f(aVec: TVector2i): TVector2f;
begin
  Result.x := aVec.x;
  Result.y := aVec.y;
end;

function vec2fAdd(const aV1, aV2: TVector2f): TVector2f;
begin
  Result.x := aV1.x + aV2.x;
  Result.y := aV1.y + aV2.y;
end;

function vec2fSub(const aV1, aV2: TVector2f): TVector2f;
begin
  Result.x := aV1.x - aV2.x;
  Result.y := aV1.y - aV2.y;
end;

function vec2fScale(const aVec: TVector2f; const aXScale, aYScale: Single): TVector2f;
begin
  Result.X := aVec.X * aXScale;
  Result.Y := aVec.Y * aYScale;
end;

function vec2fScaleFactor(const aVec: TVector2f; const aFactor: Single): TVector2f;
begin
  Result.X := aVec.X * aFactor;
  Result.Y := aVec.Y * aFactor;
end;

function vec2fLength(const aVec: TVector2f): Single;
begin
  Result := Sqrt(aVec.X*aVec.X + aVec.Y*aVec.Y);
end;

function vec2fLengthSqr(const aVec: TVector2f): Single;
begin
  Result := aVec.X*aVec.X + aVec.Y*aVec.Y;
end;

function vec2fNorm(const aVec: TVector2f): TVector2f;
var
  l: Single;
begin
  l := vec2fLength(aVec);
  if Abs(l) > EPSILON then
    Result := vec2fScaleFactor(aVec, 1/l)
  else
    Result := aVec;
end;

function vec2fDot(const aV1, aV2: TVector2f): Single;
begin
  Result := aV1.x * aV2.x + aV1.y * aV2.y;
end;

function Vec2fNeg(const aVec: TVector2f): TVector2f;
begin
  Result := Vec2f(-aVec.x, -aVec.y);
end;

function vec2fAngle(const aVec: TVector2f): Single;
begin
  //Compute angle
  if aVec.y <> 0 then
    Result := RadToDeg(ArcTan2(aVec.x, aVec.y))
  else if aVec.x > 0 then
    Result := 90
  else if aVec.x < 0 then
    Result := -90
  else
    Result := 0;
  if Result < 0 then
    Result := 360 + Result;
end;

function Vec2fEquals(const aV1, aV2: TVector2f): Boolean;
begin
  Result := (Equals(aV1.x, aV2.x) and
             Equals(aV1.y, aV2.y));
end;


//3D integer vectors

function Vec3i(x, y, z: Integer): TVector3i;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

//3D float vectors

function Vec3f(x, y, z: Single): TVector3f;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;


function  VecAdd(const aV1, aV2: TVector3f): TVector3f;
begin
  result.X := aV1.X + aV2.X;
  result.Y := aV1.Y + aV2.Y;
  result.Z := aV1.Z + aV2.Z;
end;

function  VecSub(const aV1, aV2 : TVector3f): TVector3f;
begin
  result.X := aV1.X - aV2.X;
  result.Y := aV1.Y - aV2.Y;
  result.Z := aV1.Z - aV2.Z;
end;

function  VecScaleFactor(const aV : TVector3f; const aF : Single ): TVector3f;
begin
  result.X := aV.X * aF;
  result.Y := aV.Y * aF;
  result.Z := aV.Z * aF;
end;

function  VecScale(const aV1, aV2 : TVector3f): TVector3f;
begin
  result.X := aV1.X * aV2.X;
  result.Y := aV1.Y * aV2.Y;
  result.Z := aV1.Z * aV2.Z;
end;

function  VecCompare(const aV1, aV2 : TVector3f): boolean;
begin
  Result := (Abs(aV1.x - aV2.x) < EPSILON) and
            (Abs(aV1.y - aV2.y) < EPSILON) and
            (Abs(aV1.z - aV2.z) < EPSILON);
end;

function VecNegate(const aV: TVector3f): TVector3f;
begin
  Result.X := -aV.X;
  Result.Y := -aV.Y;
  Result.Z := -aV.Z;
end;

function VecAbs(const aV: TVector3f): TVector3f;
begin
  Result.X := Abs(aV.X);
  Result.Y := Abs(aV.Y);
  Result.Z := Abs(aV.Z);
end;

function VecDistance(const aV1, aV2 : TVector3f): single;
begin
  Result := sqrt(sqr(aV2.X - aV1.X) + sqr(aV2.Y - aV1.Y) +sqr(aV2.Z - aV1.Z));
end;

function VecLength(const aV : TVector3f): single;
begin
  Result := sqrt((aV.X * aV.X) + (aV.Y * aV.Y) + (aV.Z * aV.Z));
end;

function VecLengthSqr(const aV: TVector3f): single;
begin
  Result := (aV.X * aV.X) + (aV.Y * aV.Y) + (aV.Z * aV.Z);
end;

function VecNorm(const aV: TVector3f): TVector3f;
var
  iMag : Single;
begin
  iMag := VecLength(aV);
  if iMag = 0 then
    iMag := 1;
  Result := VecScaleFactor(aV, 1/iMag);
end;

function  VecDot(const aV1, aV2 : TVector3f): single;
begin
  Result := ( (aV1.X * aV2.X) + (aV1.Y * aV2.Y) + (aV1.Z * aV2.Z) );
end;

function  VecCross(const aV1, aV2 : TVector3f): TVector3f;
begin
	Result.X := ((aV1.Y * aV2.Z) - (aV1.Z * aV2.Y));
	Result.Y := ((aV1.Z * aV2.X) - (aV1.X * aV2.Z));
	Result.Z := ((aV1.X * aV2.Y) - (aV1.Y * aV2.X));
end;

function VecAngle(const aV1, aV2 : TVector3f) : single;
var
  iDotProduct : Single;
  iVectorsMagnitude : Single;
  iAngle : real;
begin
  iDotProduct       := VecDot(aV1, aV2);
  iVectorsMagnitude := VecLength(aV1) * VecLength(aV2);
	iAngle            := arccos( iDotProduct / iVectorsMagnitude );
	if(isnan(iAngle)) then
    result := 0
  else
    result :=  iAngle;
end;

function VecInterpolate(const aV1, aV2 : TVector3f; const aK: single): TVector3f;
var iX : single;
begin
  iX := 1/aK;
  Result := VecAdd(aV1 , VecScaleFactor( VecSub(aV2,aV1),iX)  );
end;

function VecEquals(const aV1, aV2: TVector3f): Boolean;
begin
  Result := (Equals(aV1.x, aV2.x) and
             Equals(aV1.y, aV2.y) and
             Equals(aV1.z, aV2.z));
end;

function VecSnap(const aV: TVector3f; gridsize: Single): TVector3f;
begin
  Result.x := round(aV.x / gridsize) * gridsize;
  Result.y := round(aV.y / gridsize) * gridsize;
  Result.z := round(aV.z / gridsize) * gridsize;
end;



//4D Float vectors

function Vec4f(x, y, z, w: Single): TVector4f;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
  Result.w := w;
end;

function Vec4fEquals(const aV1, aV2: TVector4f): Boolean;
begin
  Result := (Equals(aV1.x, aV2.x) and
             Equals(aV1.y, aV2.y) and
             Equals(aV1.z, aV2.z) and
             Equals(aV1.w, aV2.w));
end;


end.
