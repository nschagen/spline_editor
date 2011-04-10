unit usplineeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uview, uspline, Controls;

//type

  { TSplineEditor }

  {TSplineEditor = class
  private
    FPaintBox:      TPaintBox;
    FView:          TSplineEditorView;
    FMouse:         TMouseController;
    FSpline:        TSpline;
    FDragOperation: TDragOperation;
    FSavedToFile:   String;
  public
    constructor Create(aPaintBox: TPaintBox);
    destructor Destroy();  override;

    property Spline: TSpline read FSpline;
    property View: TSplineEditorView read FView;
    property SavedToFile: String read FSavedToFile write FSavedToFile;
  end;}

implementation

uses umain;

{ TSplineEditor

constructor TSplineEditor.Create();
begin
  inherited;
  FView := TSplineEditorView.Create(MainForm.ViewPaintBox);
  FDragOperation := doNone;
end;

destructor TSplineEditor.Destroy();
begin
  FView.Free();
  inherited;
end;

procedure TSplineEditor.EditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
  begin

  end
  else if (Button = mbRight) then
  begin
    FDragOperation := doCamera;

  end;
end;

procedure TSplineEditor.EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  case FDragOperation of

  end;
end;

procedure TSplineEditor.EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case FDragOperation of

  end;
  FDragOperation := doNone;
end;

procedure TSplineEditor.EditorMouseEnter(Sender: TObject);
begin

end;

procedure TSplineEditor.EditorMouseLeave(Sender: TObject);
begin

end;

procedure TSplineEditor.EditorMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin

end;           }


end.

