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

