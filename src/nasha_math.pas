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
