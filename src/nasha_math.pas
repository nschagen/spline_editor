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

unit nasha_math;

interface

  function Max2i(aA, aB: Integer): Integer;
  function Max3i(aA, aB, aC: Integer): Integer;
  function Max4i(aA, aB, aC, aD: Integer): Integer;
  function Max2f(aA, aB: Single): Single;
  function Max3f(aA, aB, aC: Single): Single;
  function Max4f(aA, aB, aC, aD: Single): Single;
  function Min2i(aA, aB: Integer): Integer;
  function Min3i(aA, aB, aC: Integer): Integer;
  function Min4i(aA, aB, aC, aD: Integer): Integer;
  function Min2f(aA, aB: Single): Single;
  function Min3f(aA, aB, aC: Single): Single;
  function Min4f(aA, aB, aC, aD: Single): Single;

  function Equals(const a, b: Single): Boolean;
  function Clamp(const Value: Single; UpperBound: Single = 1; LowerBound: Single = 0): Single;
  function ClampAngle(const Angle: Single): Single;

const
  //A treshold for float comparison
  EPSILON = 1e-5;

implementation

function Max2i(aA, aB: Integer): Integer;
begin
  if aA > aB then Result := aA
  else Result := aB;
end;

function Max3i(aA, aB, aC: Integer): Integer;
begin
  Result := Max2i(Max2i(aA, aB), aC);
end;

function Max4i(aA, aB, aC, aD: Integer): Integer;
begin
  Result := Max2i(Max2i(aA, aB), Max2i(aC, aD));
end;

function Max2f(aA, aB: Single): Single;
begin
  if aA > aB then Result := aA
  else Result := aB;
end;

function Max3f(aA, aB, aC: Single): Single;
begin
  Result := Max2f(Max2f(aA, aB), aC);
end;

function Max4f(aA, aB, aC, aD: Single): Single;
begin
  Result := Max2f(Max2f(aA, aB), Max2f(aC, aD));
end;

function Min2i(aA, aB: Integer): Integer;
begin
  if aA < aB then Result := aA
  else Result := aB;
end;

function Min3i(aA, aB, aC: Integer): Integer;
begin
  Result := Min2i(Min2i(aA, aB), aC);
end;

function Min4i(aA, aB, aC, aD: Integer): Integer;
begin
  Result := Min2i(Min2i(aA, aB), Min2i(aC, aD));
end;

function Min2f(aA, aB: Single): Single;
begin
  if aA < aB then Result := aA
  else Result := aB;
end;

function Min3f(aA, aB, aC: Single): Single;
begin
  Result := Min2f(Min2f(aA, aB), aC);
end;

function Min4f(aA, aB, aC, aD: Single): Single;
begin
  Result := Min2f(Min2f(aA, aB),Min2f(aC, aD));
end;


function Clamp(const Value: Single; UpperBound: Single = 1; LowerBound: Single = 0): Single;
begin
  Result := Value;
  if (Result < LowerBound) then
    Result := LowerBound
  else
    if (Result >= Upperbound) then
      Result := Upperbound;
end;

function ClampAngle(const Angle: Single): Single;
begin
  Result := Angle;
  if Result < 0    then Result := (1 - Frac(Result / 360)) * 360;
  if Result >= 360 then Result := Frac(Result / 360)       * 360;
end;

function Equals(const a, b: Single): Boolean;
begin
  Result := (Abs(a - b) < EPSILON);
end;

end.