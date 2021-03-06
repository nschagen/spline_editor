{*******************************************************************************
                          New BSD-license

Copyright (c) 2011, Nathan Schagen
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * The name "Nathan Schagen" may not be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL NATHAN SCHAGEN BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*******************************************************************************}
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
    procedure EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure EditorMouseUp(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
    procedure EditorMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

    property View: TSplineEditorView read FView write FView;
    property SplineModel: TSplineModel read FSplineModel write FSplineModel;
  end;

const
  //The maximal distance between the mouse position in view space and the spline segment center
  //that will cause a new anchor to be inserted

  //This number must first be multiplied by the view-height, to get this distance
  //So it's the tolerance distance per pixel in height
  //INSERT_ANCHOR_TOLERANCE = 0.1;
  INSERT_ANCHOR_TOLERANCE = 8;
implementation

uses uspline;

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
var
  Segment:      PSplineSegment;
  distToSpline: Single;
  pos:          TVector2i;
  I:            Integer;
  NewAnchor:    TSplineAnchor;
  v:            TVector3f;
begin
  if Button = mbLeft then
  begin
    //Left click...  deselect first
    SplineModel.SelectedAnchor := nil;

    //did we click a handle? If so, move it!
    FDragHandle := IsHandleClicked(Vec2i(X,Y));
    if Assigned(FDragHandle) then
    begin
      if FDragHandle is TSplineAnchorHandle then
      begin

        //Only select tangents when they are shown too!!
        if (TSplineAnchorHandle(FDragHandle).HandleType = htAnchor) or View.ShowTangents then
        begin
          FOperation := doAnchor;

          //Select the anchor that corresponds to this handle
          SplineModel.SelectedAnchor := TSplineAnchorHandle(FDragHandle).Anchor;

          //We are updating the spline... ignore events until done
          SplineModel.Updating := True;
        end;
      end;
    end else begin

      //Check if we clicked right next to the spline (will insert an anchor)
      for I:=0 to SplineModel.Spline.AnchorCount -1 do
      begin
        v := FSplineModel.Spline.GetPosition( (I+0.5)/SplineModel.Spline.AnchorCount );
        if Assigned(Segment) then
        begin
          if vec2fLength( Vec2fSub(Vec2f(View.WorldToScreen(v)), Vec2f(X,Y)) ) < INSERT_ANCHOR_TOLERANCE then
          begin
            NewAnchor := TSplineAnchor.Create(FSplineModel.Spline);
            NewAnchor.Position := v;
            NewAnchor.TangentVector := VecScaleFactor( FSplineModel.Spline.GetDirection((I+0.5)/SplineModel.Spline.AnchorCount), 10 );
            NewAnchor.UpVector := vec3f(0,1,0);
            FSplineModel.Spline.InsertAnchor( I+1 mod SplineModel.Spline.AnchorCount,
                                              NewAnchor);
          end;
        end;
      end;

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
  //We are done updating... now process OnSplineChanged event
  SplineModel.Updating := False;

  FOperation := doNone;
end;

procedure TMouseController.EditorMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
    View.Zoom(0.90)
  else
    View.Zoom(1.1);
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
