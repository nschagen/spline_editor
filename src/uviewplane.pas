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
unit uviewplane;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nasha_vectors;

type
  TViewPlane = (vpXY, vpYZ, vpXZ);

  function ProjectOnViewPlane(aVector: TVector3f; aPlane: TViewPlane): TVector2f;
  function UnProjectFromViewPlane(aVector: TVector2f; aPlane: TViewPlane): TVector3f;

implementation

function ProjectOnViewPlane(aVector: TVector3f; aPlane: TViewPlane): TVector2f;
begin
  case aPlane of
  vpXY: begin
          Result.x := aVector.x;
          Result.y := -aVector.y;
        end;
  vpYZ: begin
          Result.x := aVector.z;
          Result.y := -aVector.y;
        end;
  vpXZ: begin
          Result.x := aVector.x;
          Result.y := aVector.z;
        end;
  end;
end;

function UnProjectFromViewPlane(aVector: TVector2f; aPlane: TViewPlane): TVector3f;
begin
  case aPlane of
  vpXY: begin
          Result.x := aVector.x;
          Result.y := -aVector.y;
          Result.z := 0;
        end;
  vpYZ: begin
          Result.x := 0;
          Result.y := -aVector.y;
          Result.z := aVector.x;
        end;
  vpXZ: begin
          Result.x := aVector.x;
          Result.y := 0;
          Result.z := aVector.y;
        end;
  end;
end;

end.

