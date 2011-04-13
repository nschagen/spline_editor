unit uspline;

 {$mode objfpc}{$H+}

interface

uses
  math,
  classes,
  sysutils,
  nasha_math,
  nasha_vectors;

type
  TSpline = class;
  TSplineAnchor = class;

  PSplineSegment = ^TSplineSegment;
  TSplineSegment = record
    ID:        Integer;
    t:         Single;        //our spline parameter
    s:         Single;        //The distance from the beginning of the spline
    v_pos:     TVector3f;     //Position in world space
    v_center:  TVector3f;     //The center of the segment
    v_forward: TVector3f;     //Points forward in the spline direction
    v_up:      TVector3f;     //Points upwards
    Angle:     Single;        //Angle between vectors towards previous and next SplineNode
    NextAnchor:TSplineAnchor; //The next anchor on the spline
  end;

  TSplineAnchor = class
  private
    FSpline              : TSpline;
    FPosition            : TVector3f;
    FUpVector            : TVector3f;
    FTangentVector       : TVector3f;
    function GetPreviousAnchor : TSplineAnchor;
    function GetNextAnchor : TSplineAnchor;
  public
    constructor Create(aSpline: TSpline);
    procedure CalculateTangentVectors;
    function Clone: TSplineAnchor;
    procedure Assign(cp: TSplineAnchor);

    property Spline: TSpline read FSpline;
    property Position: TVector3f read FPosition write FPosition;
    property UpVector: TVector3f read FUpVector write FUpVector;
    property TangentVector: TVector3f read FTangentVector write FTangentVector;
    property Previous: TSplineAnchor read GetPreviousAnchor;
    property Next: TSplineAnchor read GetNextAnchor;
  end;

  { TSpline }

  TOnAddAnchorEvent = procedure (aAnchor: TSplineAnchor) of object;
  TOnDeleteAnchorEvent = procedure (aAnchor: TSplineAnchor) of object;

  TSpline = class
  private
    FIsClosed       : Boolean;
    FAnchors        : TList;
    FSegments       : TList;
    FOnAddAnchor    : TOnAddAnchorEvent;
    FOnDeleteAnchor : TOnDeleteAnchorEvent;
    procedure CubicBezierInterpolation(CP1, CP2: TSplineAnchor; t: Single; var v: TVector3f);
    procedure SetIsClosed(const aValue: Boolean);
    function CalculateSplineSegmentLength(t0,t1: Single): Single;
    function GetAnchorsAt(const t: single; out cp1,cp2: TSplineAnchor; out localt: single): Boolean;
    function DY(t: single; interval: single = 1e-3): TVector3f;
    function GetSpeedAt(t: single): single;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddAnchor(aAnchor: TSplineAnchor);
    procedure InsertAnchor(I: Integer; aAnchor: TSplineAnchor);
    procedure DeleteAnchor(aAnchor: TSplineAnchor); overload;
    procedure DeleteAnchor(aIndex: Integer); overload;
    function IndexOf(aAnchor: TSplineAnchor): Integer;
    procedure Clear;
    procedure CalculateTangentVectors;

    function GetPosition(t: single): TVector3f;
    function GetDirection(t: single): TVector3f;
    function GetUp(t: single): TVector3f;
    function GetCurveParameter(s: single): single;
    function CalculateLength: Single;

    procedure ComputeSegments(aNodeCount: Integer; aSplineLength: Single = 0);
    function GetSegment(aIndex: Integer): PSplineSegment;
    function GetSegmentCount(): Integer;
    procedure ClearSegments();
    function GetClosestSegment(aVec: TVector3f): PSplineSegment;

    function GetSplineAnchorCount : Integer;
    function GetSplineAnchor(aIndex: Integer): TSplineAnchor;
    function GetSplineAnchorIndex(aAnchor: TSplineAnchor): Integer;

    property Anchors[I: Integer]: TSplineAnchor read GetSplineAnchor;
    property AnchorCount: Integer read GetSplineAnchorCount;
    property Segments[I: Integer]: PSplineSegment read GetSegment;
    property SegmentCount: Integer read GetSegmentCount;
    property IsClosed: Boolean   read FIsClosed write SetIsClosed;

    property OnAddAnchor: TOnAddAnchorEvent read FOnAddAnchor write FOnAddAnchor;
    property OnDeleteAnchor: TOnDeleteAnchorEvent read FOnDeleteAnchor write FOnDeleteAnchor;
  end;

  ESplineException = class(Exception);

implementation

{ TSplineAnchor }

constructor TSplineAnchor.Create(aSpline: TSpline);
begin
  inherited Create;
  FSpline := aSpline;
end;

function TSplineAnchor.GetPreviousAnchor : TSplineAnchor;
begin
  Result := FSpline.GetSplineAnchor(FSpline.GetSplineAnchorIndex(Self) - 1);
  if Result = nil then
    if FSpline.IsClosed then
    begin
      // wrap-around to end of Spline
      Result := FSpline.GetSplineAnchor(FSpline.GetSplineAnchorCount - 1);
    end;
end;

function TSplineAnchor.GetNextAnchor : TSplineAnchor;
begin
  Result := FSpline.GetSplineAnchor(FSpline.GetSplineAnchorIndex(Self) + 1);
  if Result = nil then
    if FSpline.IsClosed then
    begin
      // wrap-around to beginning of Spline
      Result := FSpline.GetSplineAnchor(0);
    end;
end;

function  TSplineAnchor.Clone: TSplineAnchor;
begin
  Result := TSplineAnchor.Create(FSpline);
  Result.Position      := FPosition;
  Result.UpVector      := FUpVector;
  Result.TangentVector := FTangentVector;
end;

procedure TSplineAnchor.Assign(cp: TSplineAnchor);
begin
  FPosition      := cp.Position;
  FUpVector      := cp.UpVector;
  FTangentVector := cp.TangentVector;
end;

procedure TSplineAnchor.CalculateTangentVectors;
{
  cp  = this anchor
  cp1 = previous anchor
  cp2 = next anchor
  m1  = magnitude of a;
  m2  = magnitude of b;
  d   = maximum tangent length (1/2 of minimum of both neighbouring segment lengths [m1,m2])

  a  = vector(cp1 - cp)
  b  = vector(cp2 - cp)
  c  = a X b (cross product)
  n  = -((a + b)/2)

  t1 = c X n (cross product)
  t2 = -t1

      cp
      /\
     /  \
 cp1/    \cp2
}
{var
  cp,cp1,cp2: TSplineAnchor;
  a,b,c,n,t1,t2: TVector3f;
  d,m1,m2: single; }
begin
  {cp  := Self;
  cp1 := GetPreviousAnchor;
  cp2 := GetNextAnchor;

  //Initialize the tangentvectors (quick & dirty)
  if Assigned(cp2) then
    cp.FTangentVector := VecScaleFactor(VecSub(cp.Position,cp2.Position),1/3);

  //If both neighbouring anchor exist, do a more sophisticated computation
  if Assigned(cp1) and Assigned(cp2) then
  begin
    a := VecSub(cp1.Position,cp.Position);
    b := VecSub(cp2.Position,cp.Position);
    c := VecCross(a,b);

    //We are actually checking the angle here.
    //If the angle is too close to 0 or 180, we skip the following calculation
    //Explanation: The length of A x B = cos(angle)
    if VecLength(c) > 0.1 then
    begin
      //normalize c for future calculations
      c := VecNorm(c);

      //Compute normal vector
      //This vector is perpendicular to the tangents
      //We compute it by averaging the a and b vectors
      //The result is then scaled to the average length of a and b
      n := VecNegate(VecScaleFactor(VecAdd(VecNorm(a),VecNorm(b)),0.5));

      //Compute tangents (will have unit length)
      t1 := VecNorm(VecCross(c,n));
      t2 := VecNegate(t1);

      //Compute tangent length
      //We will use the minimum length of a and b, divided by 2
      m1 := VecLength(a);
      m2 := VecLength(b);
      d  := Min2f(m1,m2)/2;

      //Scale tangents
      //don't need to normalize because they are unit length allready
      t1 := VecScaleFactor(t1,d);
      //t2 := VecScale(t2,d);

      cp.FTangentVector  := t1;
    end;
  end;   }
end;

//
// TSpline
//
constructor TSpline.Create;
begin
  inherited Create;

  FAnchors  := TList.Create;
  FSegments := TList.Create;
  FOnAddAnchor    := nil;
  FOnDeleteAnchor := nil;

  SetIsClosed(False);
end;

destructor  TSpline.Destroy;
begin
  Clear;
  FAnchors.Free;
  FSegments.Free;
  inherited Destroy;
end;

procedure TSpline.AddAnchor(aAnchor: TSplineAnchor);
begin
  FAnchors.Add(aAnchor);
  if Assigned(FOnAddAnchor) then FOnAddAnchor(aAnchor);
end;

procedure TSpline.InsertAnchor(I: Integer; aAnchor: TSplineAnchor);
begin
  FAnchors.Insert(I, aAnchor);
  if Assigned(FOnAddAnchor) then FOnAddAnchor(aAnchor);
end;

procedure TSpline.DeleteAnchor(aAnchor: TSplineAnchor);
begin
  if Assigned(FOnDeleteAnchor) then FOnDeleteAnchor(aAnchor);
  FAnchors.Remove(aAnchor);
  aAnchor.Free();
end;

procedure TSpline.DeleteAnchor(aIndex: Integer);
var
  Anchor: TSplineAnchor;
begin
  Anchor := Anchors[aIndex];
  if assigned(Anchor) then
    DeleteAnchor(Anchor);
end;

function TSpline.IndexOf(aAnchor: TSplineAnchor): Integer;
begin
  Result := FAnchors.IndexOf(aAnchor);
end;

procedure TSpline.CalculateTangentVectors;
var
  i: Integer;
begin
  //Calculate tangents for every anchor
  for i := 0 to GetSplineAnchorCount - 1 do
    GetSplineAnchor(i).CalculateTangentVectors();
end;

procedure TSpline.SetIsClosed(const aValue: Boolean);
begin
  if aValue <> FIsClosed then begin
    FIsClosed := aValue;
    CalculateTangentVectors;
  end;
end;

function  TSpline.GetSplineAnchorCount : Integer;
begin
  Result := FAnchors.Count;
end;

function  TSpline.GetSplineAnchor(aIndex: Integer): TSplineAnchor;
begin
  Result := nil;
  if (aIndex < 0) Or (aIndex >= FAnchors.Count) then exit;
  Result := TSplineAnchor(FAnchors.Items[aIndex]);
end;

function  TSpline.GetSplineAnchorIndex(aAnchor: TSplineAnchor): Integer;
begin
  Result := FAnchors.IndexOf(aAnchor);
end;

function  TSpline.CalculateSplineSegmentLength(t0,t1: Single): Single;
const
  cMinSegmentLength    = 1; // 1 units in length
  cMinSegmentLengthSqr = Sqr(cMinSegmentLength);

var
  m0,m1: TVector3f;
  t,d: Single;
begin
  m0 := GetPosition(t0);
  m1 := GetPosition(t1);
  d := Sqr(m0.x - m1.x) + Sqr(m0.y - m1.y) + Sqr(m0.z - m1.z);
  if d > cMinSegmentLengthSqr then begin
    // segment length is larger than the minimum segment length so split again
    t := (t0 + t1) / 2;
    Result := CalculateSplineSegmentLength(t0,t) +
              CalculateSplineSegmentLength(t,t1);
  end else begin
    Result := Sqrt(d);
  end;
end;

function  TSpline.CalculateLength: Single;
begin
  Result := CalculateSplineSegmentLength(0,0.5) +
            CalculateSplineSegmentLength(0.5,1);
end;

function  TSpline.GetAnchorsAt(const t: single; out cp1,cp2: TSplineAnchor; out localt: single): Boolean;
var
  fIndex: Single;
  iIndex1: Integer;
  iIndex2: Integer;
begin
  Result := False;

  localt  := t;
  CP1 := nil;
  CP2 := nil;

  if GetSplineAnchorCount <= 1 then Exit;

  if localt <= 0.0 then begin
    CP1 := GetSplineAnchor(0);
    CP2 := GetSplineAnchor(1);
    localt   := 0.0;
  end else begin
    if FIsClosed then
      fIndex := localt * (GetSplineAnchorCount)
    else
      fIndex := localt * (GetSplineAnchorCount - 1);

    iIndex1 := Trunc(fIndex);
    iIndex2 := iIndex1 + 1;
    fIndex  := fIndex - iIndex1;

    iIndex1 := iIndex1 mod GetSplineAnchorCount;
    iIndex2 := iIndex2 mod GetSplineAnchorCount;

    CP1 := GetSplineAnchor(iIndex1);
    CP2 := GetSplineAnchor(iIndex2);
    if fIndex >= 1 then fIndex := 1;
    localt := fIndex;
  end;

  Result := True;
end;

function  TSpline.GetPosition(t: single): TVector3f;
var
  cp1 : TSplineAnchor;
  cp2 : TSplineAnchor;
  localt : Single;
begin
  Result := Vec3f(0,0,0);

  if not GetAnchorsAt(t,cp1,cp2,localt) then Exit;

  CubicBezierInterpolation(cp1,cp2,localt,Result);
end;

function  TSpline.GetDirection(t: single): TVector3f;
const
  cDirThreshold = 0.0001;
var
  pa,pb: TVector3f;
  cp1    : TSplineAnchor;
  cp2    : TSplineAnchor;
  t1     : single;
  t2     : single;
  localt : single;
begin
  Result := Vec3f(0,0,0);

  if not GetAnchorsAt(t,cp1,cp2, localt) then Exit;

  t1 := localt - cDirThreshold;
  t2 := localt + cDirThreshold;

  CubicBezierInterpolation(cp1,cp2,t1,pa);
  CubicBezierInterpolation(cp1,cp2,t2,pb);
  Result := VecNorm(VecSub(pb,pa));
end;

function  TSpline.GetUp(t: single): TVector3f;
var
  cp1    : TSplineAnchor;
  cp2    : TSplineAnchor;
  localt : Single;
begin
  Result := Vec3f(0,0,0);

  if not GetAnchorsAt(t,cp1,cp2, localt) then Exit;

  Result.x := cp1.Position.x + (cp2.Position.x - cp1.Position.x) * localt;
  Result.y := cp1.Position.y + (cp2.Position.y - cp1.Position.y) * localt;
  Result.z := cp1.Position.z + (cp2.Position.z - cp1.Position.z) * localt;

  Result := VecNorm(Result);
end;

//Computes the tangent vector at a position along the curve denoted by
//the curve parameter t (in range 0...1)
function TSpline.DY(t: single; interval: single = 1e-3): TVector3f;
begin
  Result := VecScaleFactor(VecSub(GetPosition(t + interval), GetPosition(t)), 1/interval);
end;

//Returns the "speed" at a position t on the curve.
//The speed is "how fast the position changes, over a change of parameter t".
function TSpline.GetSpeedAt(t: single): single;
begin
  Result := VecLength(DY(t));
end;

//This is the most important function here. It offers a better
//so called "parameterization" for our curve.
//s will be a value between 0 and the length of the curve.
//The function will return a curve parameter (0 <= t <= 1).
function TSpline.GetCurveParameter(s: single): single;
var
  h              : Single;
  i              : Integer;
  k1, k2, k3, k4 : Single;
const
  n:  Integer = 100;
begin
  Result := 0;
  h := s / n;

  //We use the Runge-Kutta method for solving ordinary differential
  //equations here. I just copied this part and it worked YAY. :)
  for i := 1 to n do
  begin
    k1 := h/GetSpeedAt(Result);
    k2 := h/GetSpeedAt(Result + k1/2);
    k3 := h/GetSpeedAt(Result + k2/2);
    k4 := h/GetSpeedAt(Result + k3);
    Result := Result + (k1 + 2*(k2 + k3) + k4)/6;
  end;
end;

procedure TSpline.ComputeSegments(aNodeCount: Integer; aSplineLength: Single = 0);
var
  I: Integer;
  len, lt:  Single;
  Node: PSplineSegment;
  a, b, c: TVector3f;
  anchor: TSplineAnchor;
begin
  if AnchorCount < 2 then raise ESplineException.Create('Spline needs at least 2 anchors!');

  //Compute spline length once
  if nasha_math.equals(aSplineLength, 0) then
    len := CalculateLength()
  else
    len := aSplineLength;

  ClearSegments();

  for I := 0 to aNodeCount - 1 do
  begin
    GetMem(Node, sizeof(TSplineSegment));
    with Node^ do
    begin
      ID        := I;
      s         := (I/aNodeCount)*Len;
      t         := GetCurveParameter(s);
      v_pos     := GetPosition(t);
      v_forward := GetDirection(t);
      v_up      := GetUp(t);
      angle     := 0; //It's calculated later

      //Sets NextAnchor (anchor and lt are dummy variables that we won't use)
      GetAnchorsAt(t, anchor, NextAnchor, lt);
    end;
    FSegments.Add(Node);
  end;

  //Compute angles and centers
  for I := 0 to aNodeCount - 1 do
  begin
    if I = 0 then
    begin
      a := PSplineSegment(Segments[aNodeCount-1])^.v_pos;
      b := PSplineSegment(Segments[I+1])^.v_pos;
    end
    else
    if I = aNodeCount - 1 then
    begin
      a := PSplineSegment(Segments[I-1])^.v_pos;
      b := PSplineSegment(Segments[0])^.v_pos;
    end
    else
    begin
      a := PSplineSegment(Segments[I-1])^.v_pos;
      b := PSplineSegment(Segments[I+1])^.v_pos;
    end;
    c := PSplineSegment(Segments[I])^.v_pos;

    PSplineSegment(Segments[I])^.angle := RadToDeg(ArcCos(
        VecDot( vecNorm(VecSub(c, a)),
                vecNorm(VecSub(b, c)) )));

    PSplineSegment(Segments[I])^.v_center := VecScaleFactor(VecAdd(c, b), 0.5);
  end;
end;

function TSpline.GetClosestSegment(aVec: TVector3f): PSplineSegment;
var
  I: Integer;
  SqrDist,
  ClosestSqrDist: Single;
begin
  Result := nil;
  ClosestSqrDist := MaxInt;
  for I := 0 to SegmentCount - 1 do
  begin
    //compute squared distance (saves us one square root)
    SqrDist := vecLengthSqr(vecSub(Segments[I]^.v_center, aVec));
    if SqrDist < ClosestSqrDist then
    begin
      ClosestSqrDist := SqrDist;
      Result         := Segments[I];
    end;
  end;
end;

function TSpline.GetSegment(aIndex: Integer): PSplineSegment;
begin
  if (aIndex >= 0) and (aIndex < FSegments.Count) then
    Result := PSplineSegment(FSegments[aIndex])
  else
    Result := nil;
end;

function TSpline.GetSegmentCount(): Integer;
begin
  Result := FSegments.Count;
end;

procedure TSpline.ClearSegments();
var
  I: Integer;
begin
  for I := 0 to FSegments.Count - 1 do
    FreeMem(FSegments[I], sizeof(TSplineSegment));
  FSegments.Clear();
end;

procedure TSpline.Clear;
var
  i: Integer;
begin
  ClearSegments;
  for i := FAnchors.Count - 1 downto 0 do
    TSplineAnchor(FAnchors.Items[i]).Free;
  FAnchors.Clear;
end;

procedure TSpline.CubicBezierInterpolation(CP1, CP2: TSplineAnchor; t: Single; var v: TVector3f);
var
  tt : single;
  ttt: single;
  p0 : TVector3f;
  p1 : TVector3f;
  p2 : TVector3f;
  p3 : TVector3f;
  pa : TVector3f;
  pb : TVector3f;
  pc : TVector3f;
begin
  // curve anchor (or tangent points)
  p1 := vecAdd(CP1.Position, CP1.TangentVector);
  p2 := vecAdd(CP2.Position, vecNegate(CP2.TangentVector));

  // curve end points
  p0 := CP1.Position;
  p3 := CP2.Position;

  //Compute spline now
  tt   := t * t;
  ttt  := tt * t;

  pc.x := 3 * (p1.x - p0.x);
  pc.y := 3 * (p1.y - p0.y);
  pc.z := 3 * (p1.z - p0.z);
  pb.x := 3 * (p2.x - p1.x) - pc.x;
  pb.y := 3 * (p2.y - p1.y) - pc.y;
  pb.z := 3 * (p2.z - p1.z) - pc.z;
  pa.x := p3.x - p0.x - pc.x - pb.x;
  pa.y := p3.y - p0.y - pc.y - pb.y;
  pa.z := p3.z - p0.z - pc.z - pb.z;

  v.x := pa.x * ttt + pb.x * tt + pc.x * t + p0.x;
  v.y := pa.y * ttt + pb.y * tt + pc.y * t + p0.y;
  v.z := pa.z * ttt + pb.z * tt + pc.z * t + p0.z;
end;

end.

