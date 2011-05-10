unit uview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, uspline,
  nasha_vectors, nasha_primitives, BGRABitmap, BGRABitmapTypes,
  math, usplinemodel, uviewplane;

type


  { Responsible for drawing the output onto the canvas }

  { TSplineEditorView }

  TSplineEditorView = class
  private
    FPaintBox:    TPaintBox;
    FShowTangents: Boolean;
    FShowUpVectors: Boolean;
    FSplineModel: TSplineModel;
    FViewPlane:   TViewPlane;
    FViewCenter:  TVector3f;
    FViewHeight:  Single;
    FScreenWidth: Integer;
    FScreenHeight:Integer;
    FScreenBitmap:TBGRABitmap;
    FOnDraw:      TNotifyEvent;
    function GetViewRect(): TnaRectf;
    procedure GetAxisColors(out aHorizontal, aVertical: TBGRAPixel);
    procedure SetShowTangents(const AValue: Boolean);
    procedure SetShowUpVectors(const AValue: Boolean);
    procedure SetViewCenter(const AValue: TVector3f);
    procedure SetViewHeight(const AValue: Single);
    procedure SetViewPlane(aViewPlane: TViewPlane);
    procedure DrawBackground();
    procedure DrawSpline();
    procedure DrawAxes();
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
    function ScreenDeltaToWorld(aDelta: TVector2i): TVector3f;

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
    property ShowTangents: Boolean read FShowTangents write SetShowTangents;
    property ShowUpVectors: Boolean read FShowUpVectors write SetShowUpVectors;
  end;

const
  //Define colors as constants here!!
  CLR_BACKGROUND = TColor($FF0000);
  CLR_GRID       = TColor($A0A0A0);
  CLR_SPLINE     = TColor($00FF00);
  CLR_ANCHOR     = TColor($0000FF);
  CLR_TANGENT    = TColor($FF00FF);

  AXIS_COLOR_X   = TColor($0000FF);
  AXIS_COLOR_Y   = TColor($00FF00);
  AXIS_COLOR_Z   = TColor($FF0000);

  GRID_SIZE      = 10.0;

  //Minimal distance between the head of the upvector and the anchor
  //which is required to show it.
  UPVECTOR_SHOW_TOLERANCE = 5;

  //Length of upvector (after multiplying with view Height)
  UPVECTOR_LENGTH = 0.07;

  MIN_VIEW_HEIGHT = 1;
  MAX_VIEW_HEIGHT = 2000;

implementation

{ TSplineEditorView }

constructor TSplineEditorView.Create(aPaintBox: TPaintBox);
begin
  inherited Create();
  FSplineModel        := nil;
  FViewPlane          := vpXZ;
  FViewCenter         := vec3f(0,0,0);
  FViewHeight         := 100.0;
  FScreenBitmap       := TBGRABitmap.Create();
  FOnDraw             := nil;
  FShowTangents       := True;
  FShowUpVectors      := True;

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
var
  GridSize: Single;

  procedure DrawGridLine(x1,y1,x2,y2: Single; c: TBGRAPixel; Thickness: Single);
  var
    v1,v2: TVector2i;
  begin
    v1 := ViewToScreen(vec2fScaleFactor(vec2f(x1,y1), GridSize));
    v2 := ViewToScreen(vec2fScaleFactor(vec2f(x2,y2), GridSize));
    FScreenBitmap.DrawLineAntiAlias(v1.x, v1.y, v2.x, v2.y, c, Thickness );
  end;

var
  X, Y:     Integer;
  c:        TBGRAPixel;
  ViewRect: TnaRectf;
  GridRect: TnaRecti;
  t:        Single;
  HorColor,
  VerColor: TBGRAPixel;
begin
  FScreenBitmap.Rectangle(0,0,FScreenWidth,FScreenHeight, BGRA(30,30,30,255), BGRA(30,30,30,255), dmSet);
  c := BGRA(192,192,192,255);

  //Determine Axis colors
  GetAxisColors(HorColor, VerColor);

  GridSize := GRID_SIZE;
  if viewHeight > 300 then
    GridSize := GRID_SIZE * 5;

  ViewRect        := GetViewRect();
  GridRect.x      := Floor(viewRect.x/GridSize);
  GridRect.y      := Floor(viewRect.y/GridSize);
  GridRect.width  := Ceil(viewRect.Width/GridSize)+1;
  GridRect.height := Ceil(viewRect.Height/GridSize)+1;

  for X:=GridRect.x to GridRect.x + GridRect.Width do
  begin
    if X=0 then
    begin
      c := VerColor;
      c.alpha := 128;
      t := 2;
    end else begin
      c := BGRA(60,60,60,255);
      t := 1;
    end;

    DrawGridLine(X, GridRect.y, X, GridRect.y+GridRect.Height, c, t );
  end;
  for Y:=GridRect.y to GridRect.y + GridRect.Height do
  begin
    if Y=0 then
    begin
      c := HorColor;
      c.alpha := 128;
      t := 2;
    end else begin
      c := BGRA(60,60,60,255);
      t := 1;
    end;

    DrawGridLine(GridRect.x, Y, GridRect.x+GridRect.Width, Y, c, t );
  end;
end;

procedure TSplineEditorView.DrawSpline();
var
  c: TBGRAPixel;
  SegmentCount, I: Integer;
  Segment:  PSplineSegment;
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

  //Draw anchors
  for I:=0 to FSplineModel.Spline.AnchorCount -1 do
    if not (FSplineModel.SelectedAnchor = FSplineModel.Spline.Anchors[I]) then
      DrawAnchor(FSplineModel.Spline.Anchors[I]);

  //Draw selected anchor last
  if Assigned(FSplineModel.SelectedAnchor) then
    DrawAnchor(FSplineModel.SelectedAnchor);

  //Draw "insert anchor" points
  for I:=0 to FSplineModel.Spline.AnchorCount -1 do
  begin
    v := FSplineModel.Spline.GetPosition( (I+0.5)/SplineModel.Spline.AnchorCount );
    if Assigned(Segment) then
    begin
      pos := WorldToScreen(v);
      FScreenBitmap.Rectangle(pos.x-2, pos.y-8, pos.x+2, pos.y+8, BGRA(255,0,0,255), BGRA(255,0,0,128), dmLinearBlend);
      FScreenBitmap.Rectangle(pos.x-8, pos.y-2, pos.x+8, pos.y+2, BGRA(255,0,0,255), BGRA(255,0,0,128), dmLinearBlend);
    end;
  end;

  //Also draw axes in the bottom left corner
  DrawAxes();
end;

procedure TSplineEditorView.DrawAxes();

  //This just draws a line, for sake of simplicity ;)
  procedure DrawArrow(origin, direction: TVector2i; c: TBGRAPixel);
  var
    v2: TVector2i;
  begin
    v2 := Vec2iAdd(origin, direction);
    FScreenBitmap.DrawLineAntialias(origin.x,origin.y,v2.x,v2.y,c,3);
  end;

var
  HorColor, VerColor: TBGRAPixel;
  origin: TVector2i;
begin
  Origin := Vec2i(30, ScreenHeight - 30);

  case FViewPlane of
    vpXY: begin
            HorColor := BGRA(255,0,0,255);
            VerColor := BGRA(0,255,0,255);

            DrawArrow(Origin, Vec2i(Vec2fScaleFactor(WorldToView(Vec3f(1,0,0)),20)), ColorToBGRA(AXIS_COLOR_X));
            DrawArrow(Origin, Vec2i(Vec2fScaleFactor(WorldToView(Vec3f(0,1,0)),20)), ColorToBGRA(AXIS_COLOR_Y));
          end;
    vpYZ: begin
            HorColor := BGRA(0,255,0,255);
            VerColor := BGRA(0,0,255,255);

            DrawArrow(Origin, Vec2i(Vec2fScaleFactor(WorldToView(Vec3f(0,1,0)),20)), ColorToBGRA(AXIS_COLOR_Y));
            DrawArrow(Origin, Vec2i(Vec2fScaleFactor(WorldToView(Vec3f(0,0,1)),20)), ColorToBGRA(AXIS_COLOR_Z));
          end;
    vpXZ: begin
            HorColor := BGRA(255,0,0,255);
            VerColor := BGRA(0,0,255,255);

            DrawArrow(Origin, Vec2i(Vec2fScaleFactor(WorldToView(Vec3f(1,0,0)),20)), ColorToBGRA(AXIS_COLOR_X));
            DrawArrow(Origin, Vec2i(Vec2fScaleFactor(WorldToView(Vec3f(0,0,1)),20)), ColorToBGRA(AXIS_COLOR_Z));
          end;
  end;
end;

procedure TSplineEditorView.DrawAnchor(aAnchor: TSplineAnchor);
var
  t1,t2,a,u: TVector2i;
  FillColor, BorderColor, UpColor: TBGRAPixel;
begin
  if aAnchor = SplineModel.SelectedAnchor then
  begin
    BorderColor := BGRA(255,255,255,255);
    FillColor   := BGRA(255,192,0,192);
    UpColor     := BGRA(126, 192, 126, 192);
  end else begin
    BorderColor := BGRA(255,255,255,255);
    FillColor   := BGRA(0,0,255,192);
    UpColor     := BGRA(0, 192, 0, 192);
  end;

  a  := WorldToScreen(aAnchor.Position);
  t1 := WorldToScreen(VecAdd(aAnchor.Position, aAnchor.TangentVector));
  t2 := WorldToScreen(VecSub(aAnchor.Position, aAnchor.TangentVector));
  u  := WorldToScreen(VecAdd(aAnchor.Position, VecScaleFactor(aAnchor.UpVector,UPVECTOR_LENGTH*ViewHeight)));

  FScreenBitmap.Rectangle(a.x-5,  a.y-5,  a.x+5,  a.y+5, BorderColor, FillColor, dmLinearBlend);

  if FShowTangents then
  begin
    FScreenBitmap.Rectangle(t1.x-5, t1.y-5, t1.x+5, t1.y+5,BorderColor, FillColor, dmLinearBlend);
    FScreenBitmap.Rectangle(t2.x-5, t2.y-5, t2.x+5, t2.y+5,BorderColor, FillColor, dmLinearBlend);

    FScreenBitmap.DrawLineAntialias(t1.x,t1.y,t2.x,t2.y,FillColor,2);
  end;
  if FShowUpVectors and (Vec2iLength(Vec2iSub(a,u)) > UPVECTOR_SHOW_TOLERANCE) then
  begin
    FScreenBitmap.DrawLineAntialias(a.x,a.y,u.x,u.y,UpColor,2);
    FScreenBitmap.Rectangle(u.x-5, u.y-5, u.x+5, u.y+5,BGRA(0,0,0,0), UpColor, dmLinearBlend);
  end;
end;

procedure TSplineEditorView.Zoom(const aFactor: Single);
begin
  ViewHeight := ViewHeight * aFactor;
  if ViewHeight > MAX_VIEW_HEIGHT then ViewHeight := MAX_VIEW_HEIGHT;
  if ViewHeight < MIN_VIEW_HEIGHT then ViewHeight := MIN_VIEW_HEIGHT;
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
  Result := Vec2fScale( Vec2f(aDelta), XScale, YScale);
end;

function TSplineEditorView.ScreenDeltaToWorld(aDelta: TVector2i): TVector3f;
begin
  Result := ViewToWorld(ScreenDeltaToView(aDelta));
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
  Result := ProjectOnViewPlane(aVector, FViewPlane);
end;

function TSplineEditorView.ViewToWorld(aVector: TVector2f): TVector3f;
begin
  Result := UnProjectFromViewPlane(aVector, FViewPlane);
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

procedure TSplineEditorView.SetShowTangents(const AValue: Boolean);
begin
  FShowTangents := AValue;
  Redraw();
end;

procedure TSplineEditorView.SetShowUpVectors(const AValue: Boolean);
begin
  FShowUpVectors := AValue;
  Redraw();
end;

procedure TSplineEditorView.GetAxisColors(out aHorizontal, aVertical: TBGRAPixel);
begin
  case FViewPlane of
  vpXY: begin
          aHorizontal := ColorToBGRA(AXIS_COLOR_X);
          aVertical   := ColorToBGRA(AXIS_COLOR_Y);
        end;
  vpYZ: begin
          aHorizontal := ColorToBGRA(AXIS_COLOR_Y);
          aVertical   := ColorToBGRA(AXIS_COLOR_Z);
        end;
  vpXZ: begin
          aHorizontal := ColorToBGRA(AXIS_COLOR_X);
          aVertical   := ColorToBGRA(AXIS_COLOR_Z);
        end;
  end;
end;

end.

