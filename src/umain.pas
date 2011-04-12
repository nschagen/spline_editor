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
    mnScaleSpline: TMenuItem;
    mnMoveSpline: TMenuItem;
    mnEdit: TMenuItem;
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
    procedure mnAboutClick(Sender: TObject);
    procedure mnCloseClick(Sender: TObject);
    procedure mnEditClick(Sender: TObject);
    procedure mnMoveSplineClick(Sender: TObject);
    procedure mnNewClick(Sender: TObject);
    procedure mnOpenClick(Sender: TObject);
    procedure mnResetCameraClick(Sender: TObject);
    procedure mnSaveClick(Sender: TObject);
    procedure mnScaleSplineClick(Sender: TObject);
    procedure mnTangentsClick(Sender: TObject);
    procedure mnUpVectorsClick(Sender: TObject);
    procedure mnZoomInClick(Sender: TObject);
    procedure mnZoomOutClick(Sender: TObject);
    procedure SelectViewPlane(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm:         TMainForm;
  View:             TSplineEditorView;
  MouseController:  TMouseController;
  SplineModel:      TSplineModel;
  Spline:           TSpline;

implementation

uses umoveform, uscaleform;

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

  AddAnchor( Vec3f(-20, 0, -20), Vec3f(15, 0, -15) );
  AddAnchor( Vec3f(30, -3, -25), Vec3f(15, 0, 5) );
  AddAnchor( Vec3f(20, -5, 20), Vec3f(-15, 0, 15) );
  AddAnchor( Vec3f(-20, 2, 20), Vec3f(-15, 0, -15) );
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  //Destroy editor
  View.Free();
  Spline.Free();
  SplineModel.Free();
end;

procedure TMainForm.mnAboutClick(Sender: TObject);
begin
  ShowMessage('Spline Editor by Nathan Schagen a.k.a Chronozphere (April 2011)');
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

procedure TMainForm.mnEditClick(Sender: TObject);
begin

end;

procedure TMainForm.mnOpenClick(Sender: TObject);
begin
  //Open spline from file

end;

procedure TMainForm.mnSaveClick(Sender: TObject);
begin
  //Save spline to file

end;

procedure TMainForm.mnMoveSplineClick(Sender: TObject);
var
  displacement: TVector3f;
begin
  displacement := ShowMoveDialog();
  if not VecEquals(displacement, Vec3f(0,0,0)) then
  begin
    SplineModel.Move(displacement);
    View.ReDraw();
  end;
end;

procedure TMainForm.mnScaleSplineClick(Sender: TObject);
var
  factors: TVector3f;
begin
  factors := ShowScaleDialog( SplineModel.getAABB() );
  if not VecEquals(factors, Vec3f(0,0,0)) then
  begin
    SplineModel.Scale(factors);
    View.ReDraw();
  end;
end;

procedure TMainForm.mnTangentsClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  View.ShowTangents := TMenuItem(Sender).Checked;
end;

procedure TMainForm.mnUpVectorsClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  View.ShowUpVectors := TMenuItem(Sender).Checked;
end;

procedure TMainForm.mnZoomInClick(Sender: TObject);
begin
  View.Zoom(0.80);
  View.ReDraw();
end;

procedure TMainForm.mnZoomOutClick(Sender: TObject);
begin
  View.Zoom(1.25);
  View.ReDraw();
end;

procedure TMainForm.mnResetCameraClick(Sender: TObject);
begin
  View.ViewCenter := Vec3f(0,0,0);
  View.ViewHeight := 100;
  View.ReDraw();
end;

procedure TMainForm.SelectViewPlane(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
  case TComponent(Sender).Tag of
  0:  View.ViewPlane := vpXY;
  1:  View.ViewPlane := vpYZ;
  2:  View.ViewPlane := vpXZ;
  end;
  View.ReDraw();
end;

end.

