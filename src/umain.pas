unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, DbCtrls, Spin, ComCtrls, LCLType, types, usplineeditor,
  BGRABitmap, BGRABitmapTypes, uspline, uview, usplinemodel, nasha_vectors, umouse;

type

  { TMainForm }

  TMainForm = class(TForm)
    FloatSpinEdit1: TFloatSpinEdit;
    FloatSpinEdit2: TFloatSpinEdit;
    FloatSpinEdit3: TFloatSpinEdit;
    FloatSpinEdit4: TFloatSpinEdit;
    FloatSpinEdit5: TFloatSpinEdit;
    FloatSpinEdit6: TFloatSpinEdit;
    FloatSpinEdit7: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    AnchorList: TListBox;
    MainMenu1: TMainMenu;
    ContentsMemo: TMemo;
    mnAbout: TMenuItem;
    mnHelp: TMenuItem;
    mnResetCamera: TMenuItem;
    mnZoomOut: TMenuItem;
    mnZoomIn: TMenuItem;
    mnUpVectors: TMenuItem;
    mnTangents: TMenuItem;
    mnXZ: TMenuItem;
    mnXY: TMenuItem;
    mnYZ: TMenuItem;
    mnView: TMenuItem;
    mnNew: TMenuItem;
    mnOpen: TMenuItem;
    mnClose: TMenuItem;
    mnSave: TMenuItem;
    mnFile: TMenuItem;
    OpenSplineDlg: TOpenDialog;
    ViewPaintBox: TPaintBox;
    LeftPanel: TPanel;
    AnchorPanel: TPanel;
    SaveSplineDlg: TSaveDialog;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnCloseClick(Sender: TObject);
    procedure mnNewClick(Sender: TObject);
    procedure mnOpenClick(Sender: TObject);
    procedure mnSaveClick(Sender: TObject);
    procedure ViewPaintBoxPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm:     TMainForm;
  View:         TSplineEditorView;
  MouseController: TMouseController;
  SplineModel:  TSplineModel;
  Spline:       TSpline;

implementation

{$R *.lfm}

{ TMainForm }

procedure AddAnchor(Pos, Tangent: TVector3f);
var
  Anchor: TSplineAnchor;
begin
  Anchor := TSplineAnchor.Create(Spline);
  Anchor.Position      := Pos;
  Anchor.TangentVector := Tangent;
  Anchor.UpVector      := Vec3f(0,0,1);
  Spline.AddAnchor(Anchor);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  //Create editor
  View := TSplineEditorView.Create(ViewPaintBox);

  MouseController           := TMouseController.Create();
  ViewPaintBox.OnMouseDown  := @MouseController.EditorMouseDown;
  ViewPaintBox.OnMouseMove  := @MouseController.EditorMouseMove;
  ViewPaintBox.OnMouseUp    := @MouseController.EditorMouseUp;
  ViewPaintBox.OnMouseEnter := @MouseController.EditorMouseEnter;
  ViewPaintBox.OnMouseLeave := @MouseController.EditorMouseLeave;
  ViewPaintBox.OnMouseWheel := @MouseController.EditorMouseWheel;

  Spline := TSpline.Create();
  Spline.IsClosed := True;

  SplineModel := TSplineModel.Create();
  SplineModel.Spline := Spline;

  View.SplineModel := SplineModel;
  MouseController.SplineModel := SplineModel;
  MouseController.View := View;

  AddAnchor( Vec3f(-20, -20, 0), Vec3f(15, -15,0) );
  AddAnchor( Vec3f(30, -25, -3), Vec3f(15, 5,0) );
  AddAnchor( Vec3f(20, 20,-5), Vec3f(-15, 15,0) );
  AddAnchor( Vec3f(-20, 20,2), Vec3f(-15, -15,0) );

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  //Destroy editor
  View.Free();
  Spline.Free();
  SplineModel.Free();
end;

procedure TMainForm.mnNewClick(Sender: TObject);
begin
  //Create new spline
  if (Application.MessageBox('Save changes?','New Spline',MB_YESNO) = IDYES) then
  begin

  end else begin

  end;
end;

procedure TMainForm.mnCloseClick(Sender: TObject);
begin
  //Close this spline

end;

procedure TMainForm.mnOpenClick(Sender: TObject);
begin
  //Open spline from file

end;

procedure TMainForm.mnSaveClick(Sender: TObject);
begin
  //Save spline to file

end;

procedure TMainForm.ViewPaintBoxPaint(Sender: TObject);
var
  image: TBGRABitmap;
  pts: array of TPointF;
  storedSpline: array of TPointF;
  c: TBGRAPixel;
  I: integer;
  SegmentCount: Integer;
  v: TVector3f;
begin
  //Redraw the view to internal bitmap
  //Editor.View.ReDraw();

  //Draw internal bitmap into paintbox
  //Editor.View.ScreenBitmap.Draw(ViewPaintBox.Canvas,0,0,True);

  image := TBGRABitmap.Create(ClientWidth,ClientHeight,ColorToBGRA(ColorToRGB(clBtnFace)));
  image.Rectangle(0,0,ClientWidth,ClientHeight, BGRA(0,0,255,255), BGRA(0,0,255,255), dmSet);
  c := ColorToBGRA(ColorToRGB(clWindowText));

  SegmentCount := Round(Spline.CalculateLength()/5);
  SetLength(storedSpline, SegmentCount);
  for I:=0 to SegmentCount -1 do
  begin
    v := Spline.GetPosition(I/SegmentCount);
    storedSpline[I].x := v.x;
    storedSpline[I].y := v.y;
    image.Rectangle(Round(v.x-2), Round(v.y-2),
                    Round(v.x+2), Round(v.y+2),c,c, dmSet);
  end;

  image.DrawPolygonAntialias(storedSpline,c,2);

  setLength(storedSpline, 3);
  storedSpline[0].x := 50;
  storedSpline[0].y := 50;
  storedSpline[1].x := 100;
  storedSpline[1].y := 100;
  storedSpline[2].x := 0;
  storedSpline[2].y := 100;

  image.DrawPolygonAntialias(storedSpline,c,1);

  image.Draw(ViewPaintBox.Canvas,0,0,True);
  image.free;
end;

end.

