unit umouse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls, uview, usplinemodel,
  nasha_primitives, nasha_vectors;

type
  TDragOperation = (doNone, doAnchor, doMoveView);

  { TMouseController }

  TMouseController = class
  private
    FView:        TSplineEditorView;
    FSplineModel: TSplineModel;
    FOperation:   TDragOperation;
    FStart:       TVector2i;
    FDragHandle:  TAbstractHandle;
    function IsHandleClicked(aMousePos: TVector2i): TAbstractHandle;
    function IsOnViewBorder(aMouesPos: TVector2i): TVector2f;
  public
    constructor Create();

    //Mouse Operations
    procedure EditorMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
    procedure EditorMouseEnter(Sender: TObject);
    procedure EditorMouseLeave(Sender: TObject);
    procedure EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure EditorMouseUp(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
    procedure EditorMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

    property View: TSplineEditorView read FView write FView;
    property SplineModel: TSplineModel read FSplineModel write FSplineModel;
  end;

implementation

{ TMouseController }

constructor TMouseController.Create();
begin
  FView         := nil;
  FSplineModel  := nil;
  FOperation    := doNone;
  FStart        := vec2i(0,0);
  FDragHandle   := nil;
end;

procedure TMouseController.EditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    //Left click...
    if View.ShowTangents then
    begin
      //did we click a handle? If so, move it!
      FDragHandle := IsHandleClicked(Vec2i(X,Y));
      if Assigned(FDragHandle) then
        FOperation := doAnchor;
    end;
  end
  else if Button = mbRight then
  begin
    //Right click, move view
    FOperation := doMoveView;
  end;

  FStart := vec2i(X,Y);
end;

procedure TMouseController.EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  handlePos: TVector3f;
  viewMove: TVector2f;
  handleMove: TVector2i;
begin
  case FOperation of
    doMoveView: begin
                //Move view
                View.Move( Vec2i( X-FStart.X, Y-FStart.Y ));
                FStart := vec2i(X,Y);
              end;
    doAnchor: begin
                //Move handle proportional to movement of mouse in screen space
                handlePos := FDragHandle.GetPosition();

                //Do we need to shift the view while moving an anchor??
                //Ammount of movement is proportional to viewHeight
                viewMove := Vec2fScaleFactor( IsOnViewBorder(Vec2i(X,Y)), (View.ViewHeight/10) );
                View.ViewCenter := VecAdd(View.ViewCenter, View.ScreenDeltaToWorld(Vec2i(viewMove)));

                //Compute handle displacement vector
                handleMove := Vec2i( X - FStart.x + Round(viewMove.x),
                                     Y - FStart.y + Round(viewMove.y));

                //Move handle and redraw
                handlePos := VecAdd(handlePos, View.ScreenDeltaToWorld(handleMove));
                FDragHandle.SetPosition(handlePos);
                View.ReDraw();

                FStart := vec2i(X,Y);
              end;
  end;
end;

procedure TMouseController.EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case FOperation of
    doMoveView: begin

              end;
  end;
  FOperation := doNone;
end;

procedure TMouseController.EditorMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
    View.Zoom(0.90)
  else
    View.Zoom(1.1);
end;

procedure TMouseController.EditorMouseEnter(Sender: TObject);
begin

end;

procedure TMouseController.EditorMouseLeave(Sender: TObject);
begin

end;

function TMouseController.IsHandleClicked(aMousePos: TVector2i): TAbstractHandle;
const
  HANDLE_SIZE = 10;

  function GetClickableHandleRect(aHandlePos: TVector3f): TnaRectf;
  var
    v: TVector2i;
  begin
    v := View.WorldToScreen(aHandlePos);
    Result.x := v.x - (HANDLE_SIZE div 2);
    Result.y := v.y - (HANDLE_SIZE div 2);
    Result.width  := HANDLE_SIZE;
    Result.height :=  HANDLE_SIZE;
  end;

var
  //ViewPos: TVector2f;
  HandleRect: TnaRectf;
  Handle: TAbstractHandle;
  I: Integer;
begin
  Result := nil;
  for I:=0 to FSplineModel.getHandleCount()-1 do
  begin
    Handle := FSplineModel.GetHandle(I);
    if RectfHasPoint( GetClickableHandleRect(Handle.Position), Vec2f(aMousePos.x, aMousePos.y)) then
    begin
      Result := Handle;
      Exit;
    end;
    Handle.Free();
  end;
end;

//Checks whether the mouse is on the border of the view
//If so, return a vector that indicates in which direction the view should be moved
function TMouseController.IsOnViewBorder(aMouesPos: TVector2i): TVector2f;
var
  mousepos: TVector2f;
begin
  mousepos.x := aMouesPos.x / View.ScreenWidth;
  mousepos.y := aMouesPos.y / View.ScreenHeight;
  Result := Vec2f(0,0);
  if (mousepos.x < 0.1) then Result.x := -1;
  if (mousepos.x > 0.9) then Result.x :=  1;
  if (mousepos.y < 0.1) then Result.y := -1;
  if (mousepos.y > 0.9) then Result.y :=  1;
end;

end.

