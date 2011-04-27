unit usplinemodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uspline, uviewplane, nasha_vectors, nasha_primitives;

type
  TSplineModel = class;
  TAbstractHandle = class;

  //Used when the user selects a new anchor
  TOnSelectAnchorEvent = procedure (aModel: TSplineModel; aAnchor: TSplineAnchor) of object;
  //Used when an anchor is added/removed from the spline
  TOnSplineChangeEvent = procedure (aModel: TSplineModel) of object;
  //Used when an anchor is changed (it's position/tangent or upvector)
  TOnAnchorChangeEvent = procedure (aModel: TSplineModel; aAnchor: TSplineAnchor) of object;

  { TSplineModel }

  TSplineModel = class
  private
    FFilename:       String;
    FSpline:         TSpline;
    FSelectedAnchor: TSplineAnchor;
    FOnSelectAnchor: TOnSelectAnchorEvent;
    FOnSplineChange: TOnSplineChangeEvent;
    FOnAnchorChange: TOnAnchorChangeEvent;
    FUpdating:       Boolean;  //Ignores SplineChange Events

    procedure SetSelectedAnchor(const AValue: TSplineAnchor);
    procedure SetSpline(const AValue: TSpline);
    procedure OnAddAnchor(aAnchor: TSplineAnchor);
    procedure OnDeleteAnchor(aAnchor: TSplineAnchor);
    procedure SetUpdating(const AValue: Boolean);
    procedure SplineChanged();
  public
    constructor Create();

    //Allows you to access a "virtual" list of draggable handles
    function getHandle(I: Integer): TAbstractHandle;
    function getHandleCount: Integer;

    procedure Scale(aFactors: TVector3f);
    procedure Move(aDisplacement: TVector3f);
    function getAABB(): TnaAABBf;
    procedure ComputeSplineSegments();
    function GetClosestSegmentInViewPlane(aViewVec: TVector2f; aViewPlane: TViewPlane): PSplineSegment;

    property Spline: TSpline read FSpline write SetSpline;
    property FileName: String read FFilename write FFilename;
    property Updating: Boolean read FUpdating write SetUpdating;
    property SelectedAnchor: TSplineAnchor read FSelectedAnchor write SetSelectedAnchor;
    property OnSelectAnchor: TOnSelectAnchorEvent read FOnSelectAnchor write FOnSelectAnchor;
    property OnSplineChange: TOnSplineChangeEvent read FOnSplineChange write FOnSplineChange;
    property OnAnchorChange: TOnAnchorChangeEvent read FOnAnchorChange write FOnAnchorChange;
  end;

  { TAbstractHandle }

  TAbstractHandle = class
  public
    function GetPosition: TVector3f; virtual; abstract;
    procedure SetPosition(const AValue: TVector3f); virtual; abstract;
    property Position: TVector3f read GetPosition write SetPosition;
  end;

  TSplineAnchorHandleType = (htAnchor, htTangent1, htTangent2);

  { TSplineAnchorHandle }

  TSplineAnchorHandle = class(TAbstractHandle)
  private
    FSplineModel: TSplineModel;
    FAnchor:      TSplineAnchor;
    FType:        TSplineAnchorHandleType;
  public
    constructor Create(aModel: TSplineModel; aAnchor: TSplineAnchor; aType: TSplineAnchorHandleType);
    function GetPosition: TVector3f;  override;
    procedure SetPosition(const AValue: TVector3f); override;

    property SplineModel: TSplineModel read FSplineModel;
    property Anchor: TSplineAnchor read FAnchor;
    property HandleType: TSplineAnchorHandleType read FType;
  end;

implementation

{ TSplineModel }

constructor TSplineModel.Create();
begin
  FSpline         := nil;
  FSelectedAnchor := nil;
  FOnSelectAnchor := nil;
  FOnSplineChange := nil;
  FUpdating       := False;
end;

procedure TSplineModel.SetSpline(const AValue: TSpline);
begin
  if FSpline = AValue then exit;

  FSpline                := AValue;
  FSpline.OnAddAnchor    := @OnAddAnchor;
  FSpline.OnDeleteAnchor := @OnDeleteAnchor;

  SplineChanged();
end;

procedure TSplineModel.OnAddAnchor(aAnchor: TSplineAnchor);
begin
  if FUpdating then Exit;
  FSelectedAnchor := aAnchor;

  SplineChanged();
end;

procedure TSplineModel.OnDeleteAnchor(aAnchor: TSplineAnchor);
begin
  if FUpdating then Exit;

  //Select next Anchor if this one is to be deleted
  if FSelectedAnchor = aAnchor then
    FSelectedAnchor := aAnchor.Next;

  SplineChanged();
end;

procedure TSplineModel.SetUpdating(const AValue: Boolean);
begin
  //When we are done with updating... update the spline
  if not AValue and FUpdating then
    SplineChanged();

  FUpdating := AValue;
end;

procedure TSplineModel.SplineChanged();
begin
  ComputeSplineSegments();
  if Assigned(FOnSplineChange) then FOnSplineChange(Self);
end;

procedure TSplineModel.ComputeSplineSegments();
var
  SplineLength: Single;
begin
  //Recomputes spline segments
  //Each segment has a fixed length
  //SplineLength := Spline.CalculateLength();
  //Spline.ComputeSegments(Spline.AnchorCount * 2, SplineLength);
end;

procedure TSplineModel.SetSelectedAnchor(const AValue: TSplineAnchor);
begin
  if FSelectedAnchor <> AValue then
  begin
    FSelectedAnchor := AValue;
    if Assigned(FOnSelectAnchor) then
      FOnSelectAnchor(Self, AValue);
  end;
end;

function TSplineModel.GetHandle(I: Integer): TAbstractHandle;
var
  Anchor: TSplineAnchor;
begin
  if not assigned(Spline) then raise Exception.Create('No spline assigned!');

  Anchor := Spline.Anchors[I div 3];
  case (I mod 3) of
    0: Result := TSplineAnchorHandle.Create(Self, Anchor, htAnchor);
    1: Result := TSplineAnchorHandle.Create(Self, Anchor, htTangent1);
    2: Result := TSplineAnchorHandle.Create(Self, Anchor, htTangent2);
  end;
end;

function TSplineModel.getHandleCount: Integer;
begin
  Result := Spline.AnchorCount * 3;
end;

function TSplineModel.getAABB(): TnaAABBf;
var
  v: TVector3f;
  I: Integer;
begin
  v := Spline.Anchors[0].Position;
  Result.Min := v;
  Result.Size := Vec3f(0,0,0);

  //Sample points along spline and add them to the AABB
  for I:=1 to 99 do
  begin
    v := Spline.GetPosition(I/100);
    AABBfAddPoint(Result, v);
  end;
end;

function TSplineModel.GetClosestSegmentInViewPlane(aViewVec: TVector2f; aViewPlane: TViewPlane): PSplineSegment;
var
  I: Integer;
  SqrDist,
  ClosestSqrDist: Single;
begin
  Result := nil;
  ClosestSqrDist := MaxInt;
  for I := 0 to Spline.GetSegmentCount() - 1 do
  begin
    //compute squared distance (saves us one square root)
    SqrDist := vec2fLengthSqr(vec2fSub(ProjectOnViewPlane(Spline.Segments[I]^.v_center, aViewPlane), aViewVec));
    if SqrDist < ClosestSqrDist then
    begin
      ClosestSqrDist := SqrDist;
      Result         := Spline.Segments[I];
    end;
  end;
end;

procedure TSplineModel.Scale(aFactors: TVector3f);
var
  I: Integer;
begin
  for I:=0 to Spline.AnchorCount-1 do
  begin
    Spline.Anchors[I].Position      := vecScale(Spline.Anchors[I].Position, aFactors);
    Spline.Anchors[I].TangentVector := vecScale(Spline.Anchors[I].TangentVector, aFactors);
  end;

  ComputeSplineSegments();
end;

procedure TSplineModel.Move(aDisplacement: TVector3f);
var
  I: Integer;
  Anchor: TSplineAnchor;
begin
  for I:=0 to Spline.AnchorCount-1 do
  begin
    Spline.Anchors[I].Position := vecAdd(Spline.Anchors[I].Position, aDisplacement);
  end;

  ComputeSplineSegments();
end;

{ TSplineAnchorHandle }

constructor TSplineAnchorHandle.Create(aModel: TSplineModel; aAnchor: TSplineAnchor; aType: TSplineAnchorHandleType);
begin
  FSplineModel  := aModel;
  FAnchor := aAnchor;
  FType   := aType;
end;

function TSplineAnchorHandle.GetPosition: TVector3f;
begin
  case FType of
    htAnchor:     Result := FAnchor.Position;
    htTangent1:   Result := vecAdd(FAnchor.Position, FAnchor.TangentVector );
    htTangent2:   Result := vecSub(FAnchor.Position, FAnchor.TangentVector );
  end;
end;

procedure TSplineAnchorHandle.SetPosition(const AValue: TVector3f);
begin
  case FType of
    htAnchor:     FAnchor.Position      := AValue;
    htTangent1:   FAnchor.TangentVector := vecSub(AValue, FAnchor.Position);
    htTangent2:   FAnchor.TangentVector := vecNegate(vecSub(AValue, FAnchor.Position));
  end;

  FSplineModel.ComputeSplineSegments();
  if Assigned(FSplineModel.OnAnchorChange) then
    FSplineModel.OnAnchorChange(FSplineModel, FAnchor);
end;

end.

