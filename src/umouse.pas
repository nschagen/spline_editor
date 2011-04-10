unit umouse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls, uview, usplinemodel, nasha_vectors;

type
  TDragOperation = (doNone, doAnchor, doTangent, doUpVector, doCamera);

  { TMouseController }

  TMouseController = class
  private
    FView:        TSplineEditorView;
    FSplineModel: TSplineModel;
    FOperation:   TDragOperation;
    FStart:       TVector2i;
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
  FView := nil;
  FSplineModel := nil;
  FOperation := doNone;
end;

procedure TMouseController.EditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin

  end
  else if Button = mbRight then
  begin
    FOperation := doCamera;
    FStart.x := X;
    FStart.y := Y;
  end;
end;

procedure TMouseController.EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  case FOperation of
    doCamera: begin
                View.Move( Vec2i( X-FStart.X, Y-FStart.Y ));
                FStart.x := X;
                FStart.y := Y;
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

  else

end;

procedure TMouseController.EditorMouseEnter(Sender: TObject);
begin

end;

procedure TMouseController.EditorMouseLeave(Sender: TObject);
begin

end;

end.

