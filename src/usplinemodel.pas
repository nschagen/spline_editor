unit usplinemodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uspline, uviewplane, nasha_vectors, nasha_primitives,
  math, nasha_math;

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
    FChangesSaved:   Boolean;
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

    procedure UpdateUpVector(aAnchor: TSplineAnchor);
    function UpVectorToAngle(aAnchor: TSplineAnchor): Single;
    procedure SetUpvectorAsAngle(aAnchor: TSplineAnchor; aAngle: Single);

    property Spline: TSpline read FSpline write SetSpline;
    property FileName: String read FFilename write FFilename;
    property ChangesSaved: Boolean read FChangesSaved write FChangesSaved;
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
  if (not AValue) and FUpdating then
  begin
    FUpdating := False;
    SplineChanged();
  end;

  FUpdating := AValue;
end;

procedure TSplineModel.SplineChanged();
begin
  //Do not process changes while we are updating
  if FUpdating then Exit;

  FChangesSaved := False;
  if Assigned(FOnSplineChange) then FOnSplineChange(Self);
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

procedure TSplineModel.Scale(aFactors: TVector3f);
var
  I: Integer;
begin
  Updating := True;

  for I:=0 to Spline.AnchorCount-1 do
  begin
    Spline.Anchors[I].Position      := vecScale(Spline.Anchors[I].Position, aFactors);
    Spline.Anchors[I].TangentVector := vecScale(Spline.Anchors[I].TangentVector, aFactors);
  end;

  Updating := False;
end;

procedure TSplineModel.Move(aDisplacement: TVector3f);
var
  I: Integer;
  Anchor: TSplineAnchor;
begin
  Updating := True;

  for I:=0 to Spline.AnchorCount-1 do
  begin
    Spline.Anchors[I].Position := vecAdd(Spline.Anchors[I].Position, aDisplacement);
  end;

  Updating := False;
end;

//Converts the upvector of the given anchor to an angle (in degrees)
//Angle rotates upvector around spline in clockwise arder when looking along the spline
//in positive direction
function TSplineModel.UpVectorToAngle(aAnchor: TSplineAnchor): Single;
const
  UNIVERSAL_UP: TVector3f = (x: 0; y: 1; z: 0);
  FALLBACK_UP:  TVector3f = (x: 0; y: 0; z: 1);
var
  T, Up, C: TVector3f;
begin
  T := vecNorm(aAnchor.TangentVector);

  c := Veccross(T,  UNIVERSAL_UP);
  if vecEquals(c, Vec3f(0,0,0)) then
  begin
  	//Use angle between upvector and Z
    Up     := FALLBACK_UP;
  end else begin
  	//Use angle between upvector and the up-pointing vector in the plane defined by tangent
  	Up     := vecNorm(vecCross(vecNorm(C), T));
  end;
	Result := RadToDeg(arccos(Clamp(vecDot(Up, vecNorm(aAnchor.UpVector)))));

  //Special case for 3rd and 4rth quadrant (we want 0-360 angles!!!)
  if not vecEquals(vecNorm(vecCross(Up, aAnchor.UpVector)), T) then
    Result := 360 - Result;

  //Make sure our output is OK
  Result := ClampAngle(Result);
end;

//Sets the upvector of the given anchor by giving an angle
//Angle rotates upvector around spline in clockwise arder when looking along the spline
//in positive direction
procedure TSplineModel.SetUpvectorAsAngle(aAnchor: TSplineAnchor; aAngle: Single);
const
  UNIVERSAL_UP: TVector3f = (x: 0; y: 1; z: 0);
  FALLBACK_UP:  TVector3f = (x: 0; y: 0; z: 1);
var
  T, Up, Left: TVector3f;
begin
  T := vecNorm(aAnchor.TangentVector);
  Left := Veccross(T,  UNIVERSAL_UP);
  if vecEquals(Left, Vec3f(0,0,0)) then
  begin
    //Spline runs vertical (Special case!!)
    //Rotate clockwise around Y axis (Angle = 0 results in Z)
    aAnchor.Upvector := Vec3f(sin(DegToRad(aAngle)),
                              0,
                              cos(DegToRad(aAngle)));
  end else begin
    //Spline runs vertical (Special case!!)
    //Rotate clockwise around Y axis (Angle = 0 results in Z)
    aAnchor.Upvector := vecAdd(vecScaleFactor(Left,         sin(DegToRad(aAngle)) ),
                               vecScaleFactor(UNIVERSAL_UP, cos(DegToRad(aAngle)) ));
  end;
end;

//Must be called after changing the tangent of aAnchor.
//This will make sure that the upvector will stay perpendicular to the spline
procedure TSplineModel.UpdateUpVector(aAnchor: TSplineAnchor);
const
  UNIVERSAL_UP: TVector3f = (x: 0; y: 1; z: 0);
  FALLBACK_UP:  TVector3f = (x: 0; y: 0; z: 1);
var
  T,C: TVector3f;
begin
  T := VecNorm(aAnchor.TangentVector);
  c := VecCross(T,  UNIVERSAL_UP);
  if vecEquals(c, Vec3f(0,0,0)) then
    //Project Vector onto XZ plane
    aAnchor.UpVector := VecNorm(Vec3f(aAnchor.Upvector.x,
                                      0,
                                      aAnchor.Upvector.z))
  else
    //Project upvector onto plane (who's normal is the Tangent)
    aAnchor.UpVector := VecNorm(VecCross(vecNorm(c), T))
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

  //Fire OnSplineChange and OnAnchorChange events
  FSplineModel.SplineChanged();
  if Assigned(FSplineModel.OnAnchorChange) then
    FSplineModel.OnAnchorChange(FSplineModel, FAnchor);
end;

end.
