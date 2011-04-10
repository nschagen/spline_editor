unit usplinemodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uspline, nasha_vectors;

type
  TAbstractHandle = class;

  { TSplineModel }

  TSplineModel = class
  private
    FSpline: TSpline;

    procedure SetSpline(const AValue: TSpline);
  public
    constructor Create();

    function GetHandle(I: Integer): TAbstractHandle;
    function getHandleCount: Integer;

    property Spline: TSpline read FSpline write SetSpline;
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
    FAnchor: TSplineAnchor;
    FType:   TSplineAnchorHandleType;
  public
    constructor Create(aAnchor: TSplineAnchor; aType: TSplineAnchorHandleType);
    function GetPosition: TVector3f;  override;
    procedure SetPosition(const AValue: TVector3f); override;
  end;

implementation

{ TSplineModel }

constructor TSplineModel.Create();
begin

end;

procedure TSplineModel.SetSpline(const AValue: TSpline);
begin
  if FSpline = AValue then exit;
  FSpline := AValue;
end;

function TSplineModel.GetHandle(I: Integer): TAbstractHandle;
var
  Anchor: TSplineAnchor;
begin
  if not assigned(Spline) then raise Exception.Create('No spline assigned!');

  Anchor := Spline.Anchors[I div 3];
  case (I mod 3) of
    0: Result := TSplineAnchorHandle.Create(Anchor, htAnchor);
    1: Result := TSplineAnchorHandle.Create(Anchor, htTangent1);
    2: Result := TSplineAnchorHandle.Create(Anchor, htTangent2);
  end;
end;

function TSplineModel.getHandleCount: Integer;
begin
  Result := Spline.AnchorCount * 3;
end;

{ TSplineAnchorHandle }

constructor TSplineAnchorHandle.Create(aAnchor: TSplineAnchor; aType: TSplineAnchorHandleType);
begin
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
end;

end.

