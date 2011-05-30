{*******************************************************************************
                          New BSD-license

Copyright (c) 2011, Nathan Schagen
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * The name "Nathan Schagen" may not be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL NATHAN SCHAGEN BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*******************************************************************************}

unit nasha_matrices;

{$IFDEF FPC}
  {$MODE delphi}
{$ENDIF}

interface

uses
  nasha_math,
  nasha_vectors;

type
  PMatrix3  = ^TMatrix3;
  TMatrix3  = array[0..2,0..2] of single;
  PMatrix4  = ^TMatrix4;
  TMatrix4  = array[0..3,0..3] of single;

  //4x4 matrices
  function matEmpty(): TMatrix4;
  function matIdentity(): TMatrix4;
  function matMake(const BaseX, BaseY, BaseZ, Translate: TVector3f): TMatrix4;
  function matTranslate(const x, y, z: Single): TMatrix4;
  function matRotateX(const angle: Single): TMatrix4;
  function matRotateY(const angle: Single): TMatrix4;
  function matRotateZ(const angle: Single): TMatrix4;
  function matRotateAxis(const aAxis: TVector3f; const angle: Single): TMatrix4;
  function matScale(const aFactor: Single): TMatrix4;
  function matScaleNonUniform(const aX, aY, aZ: Single): TMatrix4;
  function matMultiply(const aM1, aM2: TMatrix4): TMatrix4;
  function matInv(const aM: TMatrix4): TMatrix4;
  function matScaleAll(const aM : TMatrix4; aSc: Single): TMatrix4;
  function matDeterminant(const aM: TMatrix4): Single;
  procedure matAdjoint(var aM: TMatrix4);
  function matPerspective(const aFov, aAspect, aNearPlane, aFarPlane : Single): TMatrix4;
  function matOrtho(const aLeft, aRight, aBottom, aTop, aZNear, aZFar: Single): TMatrix4;
  function matLookAt(const aPos, aLookAt, aUp: TVector3f): TMatrix4;
  //function matMakeNormalTransform(const M: TMatrix4): TMatrix4;
  function matTranspose(const M: TMatrix4): TMatrix4;
  function matTransform3f(const aM: TMatrix4; const aV: TVector3f): TVector3f;
  function matTransform4f(const aM: TMatrix4; const aV: TVector4f): TVector4f;
  function matNorm(const aM: TMatrix4): Single;

const
  Vec3Null : TVector3f = (x: 0; y: 0; z: 0);

  EmptyMatrix4 : TMatrix4 =    ((0, 0, 0, 0),
                                (0, 0, 0, 0),
                                (0, 0, 0, 0),
                                (0, 0, 0, 0));
  IdentityMatrix4 : TMatrix4 = ((1, 0, 0, 0),
                                (0, 1, 0, 0),
                                (0, 0, 1, 0),
                                (0, 0, 0, 1));

implementation

uses Math;

//4x4 matrices

function matEmpty(): TMatrix4;
begin
  result := EmptyMatrix4;
end;

function matIdentity(): TMatrix4;
begin
  result := IdentityMatrix4;
end;

function matMake(const BaseX, BaseY, BaseZ, Translate: TVector3f): TMatrix4;
begin
  Result := matEmpty();
  Result[0, 0] := BaseX.x;
  Result[0, 1] := BaseX.y;
  Result[0, 2] := BaseX.z;
  Result[1, 0] := BaseY.x;
  Result[1, 1] := BaseY.y;
  Result[1, 2] := BaseY.z;
  Result[2, 0] := BaseZ.x;
  Result[2, 1] := BaseZ.y;
  Result[2, 2] := BaseZ.z;
  Result[3, 0] := Translate.x;
  Result[3, 1] := Translate.y;
  Result[3, 2] := Translate.z;
  Result[3, 3] := 1.0;
end;

function matTranslate(const x, y, z: Single): TMatrix4;
begin
  Result := matIdentity();
  Result[3, 0] := x;
  Result[3, 1] := y;
  Result[3, 2] := z;
end;

function matRotateX(const angle: Single): TMatrix4;
var
  iRX : Single;
begin
  iRX := DegToRad(angle);
  result := matIdentity();
  result[1,1] := cos(iRX);
  result[1,2] := sin(iRX);
  result[2,1] := -sin(iRX);
  result[2,2] := cos(iRX);
end;

function matRotateY(const angle: Single): TMatrix4;
var
  iRY : Single;
begin
  iRY := DegToRad(angle);
  result := matIdentity();
  result[0,0] := cos(iRY);
  result[0,2] := -sin(iRY);
  result[2,0] := sin(iRY);
  result[2,2] := cos(iRY);
end;

function matRotateZ(const angle: Single): TMatrix4;
var
  iRZ : Single;
begin
  iRZ := DegToRad(angle);
  result := matIdentity();
  result[0,0] := cos(iRZ);
  result[0,1] := sin(iRZ);
  result[1,0] := -sin(iRZ);
  result[1,1] := cos(iRZ);
end;

function matRotateAxis(const aAxis: TVector3f; const angle: Single): TMatrix4;
var
  iCosA, iSinA, iAngle : Single;
begin
  iAngle := DegToRad(angle);
  Result := matIdentity();
  iCosA := cos(iAngle);
  iSinA := sin(iAngle);
  Result[0,0] := iCosA + (1 - iCosA)*aAxis.x*aAxis.x;
  Result[0,1] := (1 - iCosA)*aAxis.x*aAxis.z + aAxis.z*iSinA;
  Result[0,2] := (1 - iCosA)*aAxis.x*aAxis.z - aAxis.y*iSinA;

  Result[1,0] := (1 - iCosA)*aAxis.x*aAxis.y - aAxis.z*iSinA;
  Result[1,1] := iCosA + (1 - iCosA)*aAxis.y*aAxis.y;
  Result[1,2] := (1 - iCosA)*aAxis.y*aAxis.z + aAxis.x*iSinA;

  Result[2,0] := (1 - iCosA)*aAxis.x*aAxis.z + aAxis.y*iSinA;
  Result[2,1] := (1 - iCosA)*aAxis.y*aAxis.z - aAxis.x*iSinA;
  Result[2,2] := iCosA + (1 - iCosA)*aAxis.z*aAxis.z;
end;

function matScale(const aFactor: Single): TMatrix4;
begin
  Result := matIdentity();
  Result[0,0] := aFactor;
  Result[1,1] := aFactor;
  Result[2,2] := aFactor;
end;

function matScaleNonUniform(const aX, aY, aZ: Single): TMatrix4;
begin
  Result := matIdentity();
  Result[0,0] := aX;
  Result[1,1] := aY;
  Result[2,2] := aZ;
end;

function matMultiply(const aM1, aM2: TMatrix4): TMatrix4;
begin
  result[0,0]:=aM1[0,0]*aM2[0,0]+aM1[1,0]*aM2[0,1]+aM1[2,0]*aM2[0,2]+aM1[3,0]*aM2[0,3];
  result[0,1]:=aM1[0,1]*aM2[0,0]+aM1[1,1]*aM2[0,1]+aM1[2,1]*aM2[0,2]+aM1[3,1]*aM2[0,3];
  result[0,2]:=aM1[0,2]*aM2[0,0]+aM1[1,2]*aM2[0,1]+aM1[2,2]*aM2[0,2]+aM1[3,2]*aM2[0,3];
  result[0,3]:=aM1[0,3]*aM2[0,0]+aM1[1,3]*aM2[0,1]+aM1[2,3]*aM2[0,2]+aM1[3,3]*aM2[0,3];
  result[1,0]:=aM1[0,0]*aM2[1,0]+aM1[1,0]*aM2[1,1]+aM1[2,0]*aM2[1,2]+aM1[3,0]*aM2[1,3];
  result[1,1]:=aM1[0,1]*aM2[1,0]+aM1[1,1]*aM2[1,1]+aM1[2,1]*aM2[1,2]+aM1[3,1]*aM2[1,3];
  result[1,2]:=aM1[0,2]*aM2[1,0]+aM1[1,2]*aM2[1,1]+aM1[2,2]*aM2[1,2]+aM1[3,2]*aM2[1,3];
  result[1,3]:=aM1[0,3]*aM2[1,0]+aM1[1,3]*aM2[1,1]+aM1[2,3]*aM2[1,2]+aM1[3,3]*aM2[1,3];
  result[2,0]:=aM1[0,0]*aM2[2,0]+aM1[1,0]*aM2[2,1]+aM1[2,0]*aM2[2,2]+aM1[3,0]*aM2[2,3];
  result[2,1]:=aM1[0,1]*aM2[2,0]+aM1[1,1]*aM2[2,1]+aM1[2,1]*aM2[2,2]+aM1[3,1]*aM2[2,3];
  result[2,2]:=aM1[0,2]*aM2[2,0]+aM1[1,2]*aM2[2,1]+aM1[2,2]*aM2[2,2]+aM1[3,2]*aM2[2,3];
  result[2,3]:=aM1[0,3]*aM2[2,0]+aM1[1,3]*aM2[2,1]+aM1[2,3]*aM2[2,2]+aM1[3,3]*aM2[2,3];
  result[3,0]:=aM1[0,0]*aM2[3,0]+aM1[1,0]*aM2[3,1]+aM1[2,0]*aM2[3,2]+aM1[3,0]*aM2[3,3];
  result[3,1]:=aM1[0,1]*aM2[3,0]+aM1[1,1]*aM2[3,1]+aM1[2,1]*aM2[3,2]+aM1[3,1]*aM2[3,3];
  result[3,2]:=aM1[0,2]*aM2[3,0]+aM1[1,2]*aM2[3,1]+aM1[2,2]*aM2[3,2]+aM1[3,2]*aM2[3,3];
  result[3,3]:=aM1[0,3]*aM2[3,0]+aM1[1,3]*aM2[3,1]+aM1[2,3]*aM2[3,2]+aM1[3,3]*aM2[3,3];
end;

function matTranspose(const M: TMatrix4): TMatrix4;
var I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do Result[J, I] := M[I, J];
end;

function matInv(const aM: TMatrix4): TMatrix4;
var
  iDet : Single;
begin
  iDet := matDeterminant(aM);
  Result := matIdentity();
  if Abs(iDet) >= EPSILON then
  begin
    Result := aM;
    matAdjoint(Result);
    matScaleAll(Result, 1 / iDet);
  end;
end;

function matScaleAll(const aM : TMatrix4; aSc: Single): TMatrix4;
var
 iI, iJ: Integer;
begin
  for iI := 0 to 3 do
    for iJ := 0 to 3 do Result[iI, iJ] := aM[iI, iJ] * aSc;
end;

function matDeterminant(const aM: TMatrix4): Single;

  function Matrix_DetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single;
  begin
    Result := a1 * (b2 * c3 - b3 * c2) -
              b1 * (a2 * c3 - a3 * c2) +
              c1 * (a2 * b3 - a3 * b2);
  end;

var a1, a2, a3, a4,
    b1, b2, b3, b4,
    c1, c2, c3, c4,
    d1, d2, d3, d4  : Single;
begin
  a1 := aM[0, 0];  b1 := aM[0, 1];  c1 := aM[0, 2];  d1 := aM[0, 3];
  a2 := aM[1, 0];  b2 := aM[1, 1];  c2 := aM[1, 2];  d2 := aM[1, 3];
  a3 := aM[2, 0];  b3 := aM[2, 1];  c3 := aM[2, 2];  d3 := aM[2, 3];
  a4 := aM[3, 0];  b4 := aM[3, 1];  c4 := aM[3, 2];  d4 := aM[3, 3];
  Result := a1 * Matrix_DetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4) -
            b1 * Matrix_DetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4) +
            c1 * Matrix_DetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4) -
            d1 * Matrix_DetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);
end;

procedure matAdjoint(var aM: TMatrix4);
var a1, a2, a3, a4,
    b1, b2, b3, b4,
    c1, c2, c3, c4,
    d1, d2, d3, d4: Single;

  function Matrix_DetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single;
  begin
    Result := a1 * (b2 * c3 - b3 * c2) -
              b1 * (a2 * c3 - a3 * c2) +
              c1 * (a2 * b3 - a3 * b2);
  end;

begin
    a1 :=  aM[0, 0]; b1 :=  aM[0, 1];
    c1 :=  aM[0, 2]; d1 :=  aM[0, 3];
    a2 :=  aM[1, 0]; b2 :=  aM[1, 1];
    c2 :=  aM[1, 2]; d2 :=  aM[1, 3];
    a3 :=  aM[2, 0]; b3 :=  aM[2, 1];
    c3 :=  aM[2, 2]; d3 :=  aM[2, 3];
    a4 :=  aM[3, 0]; b4 :=  aM[3, 1];
    c4 :=  aM[3, 2]; d4 :=  aM[3, 3];
    aM[0, 0] :=  Matrix_DetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4);
    aM[1, 0] := -Matrix_DetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4);
    aM[2, 0] :=  Matrix_DetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4);
    aM[3, 0] := -Matrix_DetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);
    aM[0, 1] := -Matrix_DetInternal(b1, b3, b4, c1, c3, c4, d1, d3, d4);
    aM[1, 1] :=  Matrix_DetInternal(a1, a3, a4, c1, c3, c4, d1, d3, d4);
    aM[2, 1] := -Matrix_DetInternal(a1, a3, a4, b1, b3, b4, d1, d3, d4);
    aM[3, 1] :=  Matrix_DetInternal(a1, a3, a4, b1, b3, b4, c1, c3, c4);
    aM[0, 2] :=  Matrix_DetInternal(b1, b2, b4, c1, c2, c4, d1, d2, d4);
    aM[1, 2] := -Matrix_DetInternal(a1, a2, a4, c1, c2, c4, d1, d2, d4);
    aM[2, 2] :=  Matrix_DetInternal(a1, a2, a4, b1, b2, b4, d1, d2, d4);
    aM[3, 2] := -Matrix_DetInternal(a1, a2, a4, b1, b2, b4, c1, c2, c4);
    aM[0, 3] := -Matrix_DetInternal(b1, b2, b3, c1, c2, c3, d1, d2, d3);
    aM[1, 3] :=  Matrix_DetInternal(a1, a2, a3, c1, c2, c3, d1, d2, d3);
    aM[2, 3] := -Matrix_DetInternal(a1, a2, a3, b1, b2, b3, d1, d2, d3);
    aM[3, 3] :=  Matrix_DetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3);
end;

function matPerspective(const aFov, aAspect, aNearPlane, aFarPlane : Single): TMatrix4;
var
  n, f, r, l, t, b : Single;
  iM : TMatrix4;
begin
  iM := matIdentity();

	n := aNearPlane;
	f := aFarPlane;
	r := tan(DegToRad(aFov * 0.5)) * n;
	l := -r;
	t := r / aAspect;
  b := -t;

	iM[0, 0] := 2*n/(r-l);
  iM[0, 1] := 0;
  iM[0, 2] := 0;
  iM[0, 3] := 0;

	iM[1, 0] := 0;
  iM[1, 1] := 2*n/(t-b);
  iM[1, 2] := 0;
  iM[1, 3] := 0;

	iM[2, 0] := (r+l)/(r-l);
  iM[2, 1] := (t+b)/(t-b);
  iM[2, 2] := -(f+n)/(f-n);
  iM[2, 3] := -1;

	iM[3, 0] := 0;
  iM[3, 1] := 0;
  iM[3, 2] := -2*n*f/(f-n);
  iM[3, 3] := 0;

  result := iM;
end;

//Makes an orthogonal projection matrix with the following properties
// X points to left
// Y points to bottom
// Z points away
// The size of the screen will be X 0..1 and Y 0..1 and Z 0..1
// (0, 0, 0) being the topleft corner
function matOrtho(const aLeft, aRight, aBottom, aTop, aZNear, aZFar: Single): TMatrix4;
var
  iM : TMatrix4;
begin
  iM := matIdentity();

  iM[0, 0] := 2 / (aRight - aLeft);
  iM[1, 1] := 0;
  iM[2, 2] := 0;
  iM[3, 3] := 0;

  iM[1, 0] := 0;
  iM[1, 1] := 2 / (aTop - aBottom);
  iM[1, 2] := 0;
  iM[1, 3] := 0;

  iM[2, 0] := 0;
  iM[2, 1] := 0;
  iM[2, 2] := -2 / (aZFar - aZNear);
  iM[2, 3] := 0;

  iM[3, 0] := -((aRight + aLeft) / (aRight - aLeft));
  iM[3, 1] := -((aTop + aBottom) / (aTop - aBottom));
  iM[3, 2] := -((aZFar + aZNear) / (aZFar - aZNear));
  iM[3, 3] := 1;

  result := iM;
end;

//Creates a matrix for a LookAt camera
// @aPos = camera position
// @aLookAt = the position where the camera looks at
// @aUp = Vector that denotes the upper side of the view
function matLookAt(const aPos, aLookAt, aUp: TVector3f): TMatrix4;
var
  X, Y, Z: TVector3f;
begin
  //Compute a matrix that describes the camera in world space
  Z := VecNegate(VecNorm(VecSub(aLookAt, aPos)));
  X := VecNorm(VecCross(VecNorm(aUp), Z));
  Y := VecNorm(VecCross(Z, X));
  Result := matMake(X, Y, Z, Vec3f(0,0,0));

  //We transpose because we want the inverse of our orthogonal matrix
  Result := matTranspose( Result );

  //Now subtract the camera position
  Result := matMultiply(Result, matTranslate(-aPos.x, -aPos.y, -aPos.z));
end;

function matTransform3f(const aM: TMatrix4; const aV: TVector3f): TVector3f;
begin
  result.X := aV.X * aM[0,0] + aV.Y * aM[1,0] + aV.Z * aM[2,0] + aM[3,0];
  result.Y := aV.X * aM[0,1] + aV.Y * aM[1,1] + aV.Z * aM[2,1] + aM[3,1];
  result.Z := aV.X * aM[0,2] + aV.Y * aM[1,2] + aV.Z * aM[2,2] + aM[3,2];
end;

function matTransform4f(const aM: TMatrix4; const aV: TVector4f): TVector4f;
begin
  result.X := aV.X * aM[0,0] + aV.Y * aM[1,0] + aV.Z * aM[2,0] + aM[3,0];
  result.Y := aV.X * aM[0,1] + aV.Y * aM[1,1] + aV.Z * aM[2,1] + aM[3,1];
  result.Z := aV.X * aM[0,2] + aV.Y * aM[1,2] + aV.Z * aM[2,2] + aM[3,2];
  result.W := 1;
end;

function matNorm(const aM: TMatrix4): Single;
var
  XSum, YSum, ZSum: Single;
begin
  XSum := abs(aM[0, 0]) + abs(aM[0, 1]) + abs(aM[0, 2]);
  YSum := abs(aM[1, 0]) + abs(aM[1, 1]) + abs(aM[1, 2]);
  ZSum := abs(aM[2, 0]) + abs(aM[2, 1]) + abs(aM[2, 2]);
  Result := Max3f(XSum, YSum, ZSum);
end;

end.
