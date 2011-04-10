unit uview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, uspline,
  nasha_vectors, nasha_primitives, BGRABitmap, BGRABitmapTypes, math, usplinemodel;

type
  TViewPlane = (vpXY, vpYZ, vpXZ);

  { Responsible for drawing the output onto the canvas }

  { TSplineEditorView }

  TSplineEditorView = class
  private
    FPaintBox:    TPaintBox;
    FSplineModel: TSplineModel;
    FViewPlane:   TViewPlane;
    FViewCenter:  TVector3f;
    FViewHeight:  Single;
    FScreenWidth: Integer;
    FScreenHeight:Integer;
    FScreenBitmap:TBGRABitmap;
    FOnDraw:      TNotifyEvent;
    function GetViewRect(): TnaRectf;
    procedure SetViewCenter(const AValue: TVector3f);
    procedure SetViewHeight(const AValue: Single);
    procedure SetViewPlane(aViewPlane: TViewPlane);
    procedure DrawBackground();
    procedure DrawSpline();
    procedure DrawAnchor(aAnchor: TSplineAnchor);
    procedure Draw(Sender: TObject);
    procedure Resize(Sender: TObject);
  public
    constructor Create(aPaintBox: TPaintBox);
    destructor Destroy(); override;

    function ScreenToWorld(aScreenPos: TVector2i): TVector3f;
    function WorldToScreen(aWorldPos: TVector3f): TVector2i;
    function ScreenToView(aScreenPos: TVector2i): TVector2f;
    function ViewToScreen(aViewPos: TVector2f): TVector2i;
    function WorldToView(aVector: TVector3f): TVector2f;
    function ViewToWorld(aVector: TVector2f): TVector3f;
    function ScreenDeltaToView(aDelta: TVector2i): TVector2f;

    procedure ReDraw();
    procedure Zoom(const aFactor: Single);
    procedure Move(const aMouseDelta: TVector2i);

    property PaintBox: TPaintBox read FPaintBox;
    property SplineModel: TSplineModel read FSplineModel write FSplineModel;
    property ViewPlane: TViewPlane read FViewPlane write SetViewPlane;
    property ViewCenter: TVector3f read FViewCenter write SetViewCenter;
    property ViewHeight: Single read FViewHeight write SetViewHeight;
    property ScreenWidth: Integer read FScreenWidth;
    property ScreenHeight: Integer read FScreenHeight;
    property ScreenBitmap: TBGRABitmap read FScreenBitmap;
  end;



const
  //Define colors as constants here!!
  CLR_BACKGROUND = TColor($FF0000);
  CLR_GRID       = TColor($A0A0A0);
  CLR_SPLINE     = TColor($00FF00);
  CLR_ANCHOR     = TColor($0000FF);
  CLR_TANGENT    = TColor($FF00FF);

  GRID_SIZE      = 10.0;

implementation

{ TSplineEditorView }

constructor TSplineEditorView.Create(aPaintBox: TPaintBox);
begin
  inherited Create();
  FSplineModel        := nil;
  FViewPlane          := vpXY;
  FViewCenter         := vec3f(0,0,0);
  FViewHeight         := 100.0;
  FScreenBitmap       := TBGRABitmap.Create();
  FOnDraw             := nil;

  FPaintBox           := aPaintBox;
  FPaintBox.OnPaint   := @Draw;
  FPaintBox.OnResize  := @Resize;
  Resize(nil);
end;

destructor TSplineEditorView.Destroy();
begin
  FScreenBitmap.Free;
  inherited;
end;

procedure TSplineEditorView.ReDraw();
begin
  Draw(nil); //Call the Draw event hander to force a repaint!
end;

procedure TSplineEditorView.Draw(Sender: TObject);
begin
  DrawBackground();

  if Assigned(FSplineModel) and Assigned(FSplineModel.Spline) then
    DrawSpline();

  FScreenBitmap.Draw(FPaintBox.Canvas, 0, 0, true);
end;

procedure TSplineEditorView.DrawBackground();

  procedure DrawGridLine(x1,y1,x2,y2: Single; c: TBGRAPixel; Thickness: Single);
  var
    v1,v2: TVector2i;
  begin
    v1 := ViewToScreen(vec2fScaleFactor(vec2f(x1,y1), GRID_SIZE));
    v2 := ViewToScreen(vec2fScaleFactor(vec2f(x2,y2), GRID_SIZE));
    FScreenBitmap.DrawLineAntiAlias(v1.x, v1.y, v2.x, v2.y, c, Thickness );
  end;

var
  X, Y:     Integer;
  c:        TBGRAPixel;
  ViewRect: TnaRectf;
  GridRect: TnaRecti;
  t:        Single;
begin
  FScreenBitmap.Rectangle(0,0,FScreenWidth,FScreenHeight, BGRA(30,30,30,255), BGRA(30,30,30,255), dmSet);
  c := BGRA(192,192,192,255);

  ViewRect        := GetViewRect();
  GridRect.x      := Floor(viewRect.x/GRID_SIZE);
  GridRect.y      := Floor(viewRect.y/GRID_SIZE);
  GridRect.width  := Ceil(viewRect.Width/GRID_SIZE)+1;
  GridRect.height := Ceil(viewRect.Height/GRID_SIZE)+1;

  for X:=GridRect.x to GridRect.x + GridRect.Width do
  begin
    if X=0 then
    begin
      c := BGRA(0,192,0,255);
      t := 2;
    end else begin
      c := BGRA(60,60,60,255);
      t := 1;
    end;
    DrawGridLine(X, GridRect.y,
                 X, GridRect.y+GridRect.Height, c, t );
  end;
  for Y:=GridRect.y to GridRect.y + GridRect.Height do
  begin
    if Y=0 then
    begin
      c := BGRA(192,0,0,255);
      t := 2;
    end else begin
      c := BGRA(60,60,60,255);
      t := 1;
    end;

    DrawGridLine(GridRect.x,                Y,
                 GridRect.x+GridRect.Width, Y, c, t );
  end;
end;

procedure TSplineEditorView.DrawSpline();
var
  c: TBGRAPixel;
  SegmentCount, I: Integer;
  storedSpline: array of TPointF;
  v: TVector3f;
  pos: TVector2i;
  Handle: TAbstractHandle;
begin
  c := BGRA(200,200,200,255);

  //Draw spline
  SegmentCount := Round(FSplineModel.Spline.CalculateLength()/3);
  SetLength(storedSpline, SegmentCount);
  for I:=0 to SegmentCount -1 do
  begin
    pos := WorldToScreen(FSplineModel.Spline.GetPosition(I/SegmentCount));
    storedSpline[I].x := pos.x;
    storedSpline[I].y := pos.y;
  end;
  FScreenBitmap.DrawPolygonAntialias(storedSpline,c,2);

  for I:=0 to FSplineModel.GetHandleCount()-1 do
  begin
    Handle := FSplineModel.GetHandle(I);
    pos := WorldToScreen(Handle.GetPosition());
    FScreenBitmap.Rectangle(Round(pos.x-5), Round(pos.y-5),
                            Round(pos.x+5), Round(pos.y+5),BGRA(255,255,255,255), BGRA(0,0,255,192), dmLinearBlend);
    Handle.Free();
  end;

  //Draw anchors
  for I:=0 to FSplineModel.Spline.AnchorCount -1 do
    DrawAnchor(FSplineModel.Spline.Anchors[I]);
end;

procedure TSplineEditorView.DrawAnchor(aAnchor: TSplineAnchor);
var
  v1,v2: TVector2i;
begin
  v1 := WorldToScreen(VecAdd(aAnchor.Position, aAnchor.TangentVector));
  v2 := WorldToScreen(VecSub(aAnchor.Position, aAnchor.TangentVector));
  FScreenBitmap.DrawLineAntialias(v1.x,v1.y,v2.x,v2.y,BGRA(0,0,255,192),2);
end;

procedure TSplineEditorView.Zoom(const aFactor: Single);
begin
  ViewHeight := ViewHeight * aFactor;
end;

procedure TSplineEditorView.Move(const aMouseDelta: TVector2i);
begin
  ViewCenter := VecAdd(ViewCenter,
                       ViewToWorld( ScreenDeltaToView( Vec2i(-aMouseDelta.x,-aMouseDelta.y)) ));
end;

function TSplineEditorView.ScreenDeltaToView(aDelta: TVector2i): TVector2f;
var
  XScale, YScale: Single;
  viewRect:       TnaRectf;
begin
  viewRect := getViewRect();
  XScale := viewRect.width / FScreenWidth;
  YScale := viewRect.height / FScreenHeight;
  Result := Vec2fScale( Vec2f(aDelta.x,aDelta.y), XScale, YScale);
end;

function TSplineEditorView.GetViewRect(): TnaRectf;
var
  center:     TVector2f;
  AspectRatio: Single;
begin
  AspectRatio   := (FScreenWidth / FScreenHeight);
  center        := WorldToView( FViewCenter );
  Result.x      := center.x - AspectRatio * (FViewHeight / 2);
  Result.y      := center.y - (FViewHeight / 2);
  Result.Width  := AspectRatio * FViewHeight;
  Result.Height := ViewHeight;
end;

function TSplineEditorView.ScreenToWorld(aScreenPos: TVector2i): TVector3f;
var
  viewRect: TnaRectf;
  Pos, Size, A: TVector2f;
begin
  viewRect := getViewRect();
  A.x := ((aScreenPos.x / FScreenWidth) * viewRect.width) + viewRect.x;
  A.y := ((aScreenPos.y / FScreenHeight) * viewRect.height) + viewRect.y;
  Result := ViewToWorld(A);
end;

function TSplineEditorView.WorldToScreen(aWorldPos: TVector3f): TVector2i;
var
  viewRect: TnaRectf;
  Pos, Size, A: TVector2f;
begin
  viewRect := getViewRect();
  A    := WorldToView(aWorldPos);
  Result.x := Round(((A.x - viewRect.x) / viewRect.width) * FScreenWidth);
  Result.y := Round(((A.y - viewRect.y) / viewRect.height) * FScreenHeight);
end;

function TSplineEditorView.ScreenToView(aScreenPos: TVector2i): TVector2f;
var
  viewRect: TnaRectf;
begin
  viewRect := getViewRect();
  Result.x := ((aScreenPos.x / FScreenWidth) * viewRect.width) + viewRect.x;
  Result.y := ((aScreenPos.y / FScreenHeight) * viewRect.height) + viewRect.y;
end;

function TSplineEditorView.ViewToScreen(aViewPos: TVector2f): TVector2i;
var
  viewRect: TnaRectf;
  Pos, Size: TVector2f;
begin
  viewRect := getViewRect();
  Result.x := Round(((aViewPos.x - viewRect.x) / viewRect.width) * FScreenWidth);
  Result.y := Round(((aViewPos.y - viewRect.y) / viewRect.height) * FScreenHeight);
end;

function TSplineEditorView.WorldToView(aVector: TVector3f): TVector2f;
begin
  case FViewPlane of
  vpXY: begin
          Result.x := aVector.x;
          Result.y := aVector.y;
        end;
  vpYZ: begin
          Result.x := aVector.y;
          Result.y := aVector.z;
        end;
  vpXZ: begin
          Result.x := aVector.x;
          Result.y := aVector.z;
        end;
  end;
end;

function TSplineEditorView.ViewToWorld(aVector: TVector2f): TVector3f;
begin
  case FViewPlane of
  vpXY: begin
          Result.x := aVector.x;
          Result.y := aVector.y;
          Result.z := 0;
        end;
  vpYZ: begin
          Result.x := 0;
          Result.y := aVector.x;
          Result.z := aVector.y;
        end;
  vpXZ: begin
          Result.x := aVector.x;
          Result.y := 0;
          Result.z := aVector.y;
        end;
  end;
end;

procedure TSplineEditorView.Resize(Sender: TObject);
begin
  FScreenWidth := FPaintBox.Width;
  FScreenHeight := FPaintBox.Height;
  FScreenBitmap.SetSize(FScreenWidth, FScreenHeight);
  ReDraw();
end;

procedure TSplineEditorView.SetViewCenter(const AValue: TVector3f);
begin
  FViewCenter := AValue;
  Redraw();
end;

procedure TSplineEditorView.SetViewHeight(const AValue: Single);
begin
  FViewHeight := AValue;
  Redraw();
end;

procedure TSplineEditorView.SetViewPlane(aViewPlane: TViewPlane);
begin
  FViewPlane := aViewPlane;
  Redraw();
end;

end.

