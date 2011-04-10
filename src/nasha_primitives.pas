unit nasha_primitives;

interface

uses
  nasha_math,
  nasha_vectors,
  nasha_matrices;

type
  {:: TnaRectf ::}
  TnaRecti = record
    x, y, Width, Height: Integer;
  end;

  {:: TnaRectf ::}
  TnaRectf = record
    x, y, Width, Height: Single;
  end;

  {:: TnaAABBf ::}
  TnaAABBf = record
    case Boolean of
    False: (x, y, z, Width, Height, Length: Single);
    True:  (Min, Size: TVector3f);
  end;

  {:: TnaAABBf ::}
  TnaAABBi = record
    case Boolean of
    False: (x, y, z, Width, Height, Length: Integer);
    True:  (Min, Size: TVector3i);
  end;

  {:: TnaCircle ::}
  TnaCircle = record
    center: TVector2f;
    radius: Single;
  end;

  {:: TnaSphere ::}
  TnaSphere = record
    center: TVector3f;
    radius: Single;
  end;

  {:: TnaPlane ::}
  TnaPlane = record
    case Boolean of
      True: ( a, b, c, d: Single; );
      False: ( normal : TVector3f; dist: Single; );
  end;

  //Rectangles
  function Recti(const x, y, w, h: Integer): TnaRecti;


  function Rectf(const x, y, w, h: Single): TnaRectf;
  function RectfHasPoint(const aR: TnaRectf; const aPt: TVector2f): Boolean;

  //AABB
  function AABBi(const x, y, z, w, h, l: Integer): TnaAABBi;


  function AABBf(const x, y, z, w, h, l: Single): TnaAABBf;

  //Circles
  function Circle(const aCenter: TVector2f; const aRadius: Single): TnaCircle;
  function CircleHasPt(const aCircle: TnaCircle; const Pt: TVector2f): Boolean;
  function CircleCollision(const aC1, aC2: TnaCircle): Boolean;
  function CircleIsIdentity(const aCircle: TnaCircle): Boolean;
  function CircleAddPt(const aCircle: TnaCircle; const Pt: TVector2f): TnaCircle;
  function CircleEquals(const aC1, aC2: TnaCircle): Boolean;

  //Spheres
  function Sphere(const aCenter: TVector3f; const aRadius : single ): TnaSphere;
  function SphereTransform(const aSphere: TnaSphere; const mat: TMatrix4): TnaSphere;
  function SphereIntresect(const aS1, aS2: TnaSphere): Boolean;

  //Planes
  function  Plane(const aA, aB, aC, aD: Single): TnaPlane;
  function  PlaneFromNormal(const aNormal: TVector3f; const aD: Single): TnaPlane;
  function  PlaneCompare(const aP1, aP2 : TnaPlane): boolean;
  procedure PlaneNormalize(var aPlane : TnaPlane);
  function  PlaneFromPoints3f(const aP1, aP2, aP3 : TVector3f): TnaPlane;
  function  PlaneDistTo3f(const aPlane : TnaPlane; const aP : TVector3f): Single;


const
  Sphere_Null:      TnaSphere = (center: (x: 0; y: 0; z: 0); radius: 0);
  Sphere_Identity:  TnaSphere = (center: (x: 0; y: 0; z: 0); radius: 1);

  Circle_Null:      TnaCircle = (center: (x: 0; y: 0); radius: 0);
  Circle_Identity:  TnaCircle = (center: (x: 0; y: 0); radius: 1);
implementation

//Rectangles

function Recti(const x, y, w, h: Integer): TnaRecti;
begin
  Result.x := x;
  Result.y := y;
  Result.Width := w;
  Result.Height := h;
end;

function Rectf(const x, y, w, h: Single): TnaRectf;
begin
  Result.x := x;
  Result.y := y;
  Result.Width := w;
  Result.Height := h;
end;

function RectfHasPoint(const aR: TnaRectf; const aPt: TVector2f): Boolean;
begin
  Result := (aPt.x < Max2f(aR.x, aR.x + aR.Width)) and
            (aPt.x > Min2f(aR.x, aR.x + aR.Width)) and
            (aPt.y < Max2f(aR.y, aR.y + aR.Height)) and
            (aPt.y > Min2f(aR.y, aR.y + aR.Height));
end;

function AABBi(const x, y, z, w, h, l: Integer): TnaAABBi;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
  Result.Width := w;
  Result.Height := h;
  Result.Length := l;
end;

function AABBf(const x, y, z, w, h, l: Single): TnaAABBf;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
  Result.Width := w;
  Result.Height := h;
  Result.Length := l;
end;

//Circles

function Circle(const aCenter: TVector2f; const aRadius: Single): TnaCircle;
begin
  Result.center := aCenter;
  Result.radius := aRadius;
end;

function CircleAddPt(const aCircle: TnaCircle; const Pt: TVector2f): TnaCircle;
var
  d: Single;
begin
  if CircleEquals( aCircle, Circle_Null ) then
  begin
    Result.center := Pt;
    Result.radius := 0;
  end
  else
  begin
    d := Vec2fLength(Vec2fSub( Pt, aCircle.center ) );
    if d > aCircle.radius then
    begin
      Result.radius := (aCircle.radius + d) / 2;
      Result.center := Vec2fAdd(aCircle.center, Vec2f(
                          (Pt.x - aCircle.center.x)*(1/d)*(Result.radius - aCircle.radius),
                          (Pt.y - aCircle.center.y)*(1/d)*(Result.radius - aCircle.radius) ));
    end
    else
      Result := aCircle;
  end;
end;

function CircleIsIdentity(const aCircle: TnaCircle): Boolean;
begin
  Result := (Abs(aCircle.center.x)   < EPSILON) and
            (Abs(aCircle.center.y)   < EPSILON) and
            (Abs(aCircle.radius-1) < EPSILON);
end;

function CircleHasPt(const aCircle: TnaCircle; const Pt: TVector2f): Boolean;
begin
  Result := (vec2fLength(Vec2fSub( Pt, aCircle.center )) < aCircle.radius);
end;

function CircleCollision(const aC1, aC2: TnaCircle): Boolean;
begin
  Result := (vec2fLength( Vec2fSub(aC1.center, aC2.center) ) < (aC1.Radius + aC2.Radius));
end;

function CircleEquals(const aC1, aC2: TnaCircle): Boolean;
begin
  Result := (vec2fEquals(aC1.center, aC2.center) and
             Equals(aC1.radius, aC2.radius));
end;


//Spheres


function Sphere(const aCenter: TVector3f;
                const aRadius : single ): TnaSphere;
begin
  Result.center := aCenter;
  Result.radius := aRadius;
end;

function SphereTransform(const aSphere: TnaSphere; const mat: TMatrix4): TnaSphere;
begin
  Result.center := matTransform3f(mat, aSphere.center);
  Result.radius := matNorm(mat) * aSphere.radius;
end;

function SphereIntresect(const aS1, aS2: TnaSphere): Boolean;
var
  v: TVector3f;
  radiusSqr: Single;
begin
  v := vecSub(aS1.center, aS2.center);
  radiusSqr := (aS1.radius + aS2.radius) * (aS1.radius + aS2.radius);
  result := (vecLengthSqr(v) < radiusSqr);
end;

function SphereGrowToContain(const aContainer, aSphere: TnaSphere): TnaSphere;
begin

end;

// Planes

function  Plane(const aA, aB, aC, aD: Single): TnaPlane;
begin
  Result.a := aA;
  Result.b := aB;
  Result.c := aC;
  Result.d := aD;
end;

function  PlaneFromNormal(const aNormal: TVector3f; const aD: Single): TnaPlane;
begin
  Result.normal := aNormal;
  Result.dist   := aD;
end;

function  PlaneCompare(const aP1, aP2 : TnaPlane): boolean;
begin
  Result := (Abs(aP1.a - aP2.a) < EPSILON) and
            (Abs(aP1.b - aP2.b) < EPSILON) and
            (Abs(aP1.c - aP2.c) < EPSILON) and
            (Abs(aP1.d - aP2.d) < EPSILON);
end;

procedure PlaneNormalize(var aPlane : TnaPlane);
var
  iMag : Single;
begin
  iMag := VecLength(aPlane.normal);
  aPlane.a := aPlane.a / iMag;
  aPlane.b := aPlane.b / iMag;
  aPlane.c := aPlane.c / iMag;
  aPlane.d := aPlane.d / iMag;
end;

function  PlaneFromPoints3f(const aP1, aP2, aP3 : TVector3f): TnaPlane;
var
  iNormal : TVector3f;
begin
  iNormal := VecCross( VecSub(aP2, aP1), VecSub(aP3, aP1) );
  VecNorm(iNormal);
  with Result do
  begin
    a := iNormal.x;
    b := iNormal.y;
    c := iNormal.z;
    d := -(a * iNormal.x + b * iNormal.y + c * iNormal.z);
  end;
end;

function  PlaneDistTo3f(const aPlane : TnaPlane; const aP : TVector3f): Single;
begin
  Result := aPlane.normal.x * aP.x +
            aPlane.normal.y * aP.y +
            aPlane.normal.z * aP.z +
            aPlane.dist;
end;



end.
