unit umouse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls, uview, usplinemodel,
  nasha_primitives, nasha_vectors;

type
  TDragOperation = (doNone, doAnchor, doCamera);

  { TMouseController }

  TMouseController = class
  private
    FView:        TSplineEditorView;
    FSplineModel: TSplineModel;
    FOperation:   TDragOperation;
    FStart:       TVector2i;
    FDragHandle:  TAbstractHandle;
    function IsHandleClicked(aMousePos: TVector2i): TAbstractHandle;
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
    FDragHandle := IsHandleClicked(Vec2i(X,Y));
    if Assigned(FDragHandle) then
      FOperation := doAnchor;
    FStart := vec2i(X,Y);
  end
  else if Button = mbRight then
  begin
    FOperation := doCamera;
    FStart := vec2i(X,Y);
  end;
end;

procedure TMouseController.EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  v: TVector3f;
begin
  case FOperation of
    doCamera: begin
                View.Move( Vec2i( X-FStart.X, Y-FStart.Y ));
                FStart := vec2i(X,Y);
              end;
    doAnchor: begin
                //Move handle proportional to movement of mouse in screen space
                v := FDragHandle.GetPosition();
                v := VecAdd(v, View.ViewToWorld( View.ScreenDeltaToView( Vec2i( X-FStart.X, Y-FStart.Y ) ) ) );
                FDragHandle.SetPosition(v);
                View.ReDraw();

                FStart := vec2i(X,Y);
              end;
  end;
end;

procedure TMouseController.EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case FOperation of
    doCamera: begin

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

end.

