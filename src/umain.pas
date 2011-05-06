unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, DbCtrls, Spin, ComCtrls, LCLType, types, usplineeditor,
  BGRABitmap, BGRABitmapTypes, uspline, uview, usplinemodel, nasha_vectors, umouse,
  uviewplane, usplinexml;

type

  { TMainForm }

  TMainForm = class(TForm)
    edtPosX: TFloatSpinEdit;
    edtPosZ: TFloatSpinEdit;
    edtPosY: TFloatSpinEdit;
    edtDirX: TFloatSpinEdit;
    edtDirY: TFloatSpinEdit;
    edtDirZ: TFloatSpinEdit;
    edtUpVecAngle: TFloatSpinEdit;
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
    procedure AnchorListClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mnAboutClick(Sender: TObject);
    procedure mnCloseClick(Sender: TObject);
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
    procedure SelectAnchor(aModel: TSplineModel; aAnchor: TSplineAnchor);
    procedure SplineChange(aModel: TSplineModel);
    procedure AnchorChange(aModel: TSplineModel; aAnchor: TSplineAnchor);
    procedure ChangeAnchorProperty(Sender: TObject);
  private
    { private declarations }
  public
    function SaveSpline(): Boolean;
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

procedure InitSpline(aSpline: TSpline);

  procedure AddAnchor(Pos, Tangent: TVector3f);
  var
    Anchor: TSplineAnchor;
  begin
    Anchor := TSplineAnchor.Create(Spline);
    Anchor.Position      := Pos;
    Anchor.TangentVector := Tangent;
    Anchor.UpVector      := Vec3f(0,1,0);
    aSpline.AddAnchor(Anchor);
  end;

begin
  //Create spline
  aSpline.Clear();

  AddAnchor( Vec3f(-20, 0, -20), Vec3f(12, 0, -12) );
  AddAnchor( Vec3f(20,  0, -20), Vec3f(12, 0, 12) );
  AddAnchor( Vec3f(20,  0, 20), Vec3f(-12, 0, 12) );
  AddAnchor( Vec3f(-20, 0, 20), Vec3f(-12, 0, -12) );
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  //Create spline
  Spline := TSpline.Create();
  Spline.IsClosed := True;
  InitSpline(Spline);

  //Create spline model, that wraps up the spline
  SplineModel := TSplineModel.Create();
  SplineModel.Spline := Spline;
  SplineModel.OnSelectAnchor := @SelectAnchor;
  SplineModel.OnSplineChange := @SplineChange;
  SplineModel.OnAnchorChange := @AnchorChange;
  SplineModel.ChangesSaved := True;

  //This object will draw the viewport
  View := TSplineEditorView.Create(ViewPaintBox);
  View.SplineModel := SplineModel;

  //Let mouse controller handle any mouse input
  MouseController             := TMouseController.Create();
  MouseController.SplineModel := SplineModel;
  MouseController.View        := View;
  ViewPaintBox.OnMouseDown    := @MouseController.EditorMouseDown;
  ViewPaintBox.OnMouseMove    := @MouseController.EditorMouseMove;
  ViewPaintBox.OnMouseUp      := @MouseController.EditorMouseUp;
  ViewPaintBox.OnMouseEnter   := @MouseController.EditorMouseEnter;
  ViewPaintBox.OnMouseLeave   := @MouseController.EditorMouseLeave;
  ViewPaintBox.OnMouseWheel   := @MouseController.EditorMouseWheel;

  //Make sure the spline anchors are visible in anchorList
  SplineChange(SplineModel);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  //Destroy editor
  View.Free();
  Spline.Free();
  SplineModel.Free();
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    SplineModel.Spline.DeleteAnchor( SplineModel.SelectedAnchor );

    View.ReDraw();
  end;
end;

procedure TMainForm.mnAboutClick(Sender: TObject);
begin
  ShowMessage('Spline Editor by Nathan Schagen a.k.a Chronozphere (April 2011)');
end;

procedure TMainForm.mnNewClick(Sender: TObject);
begin
  //Create new spline
  if (not SplineModel.ChangesSaved) then
    if (Application.MessageBox('Save changes?','New Spline',MB_YESNO) = IDYES) then
      if not SaveSpline() then
        Exit; //Abort when the user cancelled the save dialog

  //Clear and initialize spline
  InitSpline(Spline);
  SplineModel.ChangesSaved := True;
end;

procedure TMainForm.mnCloseClick(Sender: TObject);
begin
  //Save spline before closing?
  if (not SplineModel.ChangesSaved) then
    if(Application.MessageBox('Save changes?','Closing Editor',MB_YESNO) = IDYES) then
      if not SaveSpline() then
        Exit;

  Close();
end;

procedure TMainForm.mnOpenClick(Sender: TObject);
var
  Loader: TSplineLoader;
begin
  //Save existing spline before opening?
  if (not SplineModel.ChangesSaved) then
    if(Application.MessageBox('Save changes?','Open Spline',MB_YESNO) = IDYES) then
      if not SaveSpline() then
        Exit;

  //Open spline from file
  if OpenSplineDlg.Execute() then
  begin
    SplineModel.FileName := OpenSplineDlg.FileName;

    Loader := TSplineLoader.Create();
    try
      Loader.LoadFromFile( Spline, SplineModel.Filename );
      SplineModel.ChangesSaved := True;
    finally
      Loader.Free();
    end;
  end;
end;

function TMainForm.SaveSpline(): Boolean;
var
  Saver: TSplineSaver;
begin
  Result := False;
  if SplineModel.Filename = '' then
    if SaveSplineDlg.Execute() then
      SplineModel.Filename := SaveSplineDlg.FileName
    else
      Exit; //no file name given.. abort!

  //File exists.. overwrite??
  if FileExists(SplineModel.Filename) then
    if (Application.MessageBox('File allready exists. Overwrite?','Save Spline',MB_YESNO) = IDNO) then
      Exit;

  Saver := TSplineSaver.Create();
  try
    Saver.SaveToFile( Spline, SplineModel.Filename );
    SplineModel.ChangesSaved := True;
    Result := True;
  finally
    Saver.Free();
  end;
end;

procedure TMainForm.mnSaveClick(Sender: TObject);
begin
  //Save spline to file
  SaveSpline();
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

procedure TMainForm.SelectAnchor(aModel: TSplineModel; aAnchor: TSplineAnchor);
begin
  //Called when an anchor was clicked

  //Select the right item corresponding to the Anchor index
  if assigned(aAnchor) then
  begin
    AnchorList.ItemIndex := aModel.Spline.IndexOf(aAnchor);

    edtPosX.Enabled := True;
    edtPosY.Enabled := True;
    edtPosZ.Enabled := True;
    edtDirX.Enabled := True;
    edtDirY.Enabled := True;
    edtDirZ.Enabled := True;

    edtPosX.Value := aAnchor.Position.x;
    edtPosY.Value := aAnchor.Position.y;
    edtPosZ.Value := aAnchor.Position.z;
    edtDirX.Value := aAnchor.TangentVector.x;
    edtDirY.Value := aAnchor.TangentVector.y;
    edtDirZ.Value := aAnchor.TangentVector.z;
    edtUpVecAngle.Value := aModel.UpVectorToAngle(aAnchor);
  end else begin
    AnchorList.ItemIndex := -1;

    edtPosX.Enabled := False;
    edtPosY.Enabled := False;
    edtPosZ.Enabled := False;
    edtDirX.Enabled := False;
    edtDirY.Enabled := False;
    edtDirZ.Enabled := False;

    edtPosX.Value := 0.0;
    edtPosY.Value := 0.0;
    edtPosZ.Value := 0.0;
    edtDirX.Value := 0.0;
    edtDirY.Value := 0.0;
    edtDirZ.Value := 0.0;
  end;

  View.ReDraw();
end;

procedure TMainForm.SplineChange(aModel: TSplineModel);
var
  I: Integer;
begin
  //Called when the spline has changed (added or removed an anchor)

  AnchorList.Clear;
  for I:=0 to aModel.Spline.AnchorCount -1 do
  begin
    AnchorList.Items.Add(Format('Anchor %d',[I]));
  end;

  View.ReDraw();
end;

procedure TMainForm.AnchorChange(aModel: TSplineModel; aAnchor: TSplineAnchor);
begin
  //Called when an anchor is manipulated
  edtPosX.Value := aAnchor.Position.x;
  edtPosY.Value := aAnchor.Position.y;
  edtPosZ.Value := aAnchor.Position.z;
  edtDirX.Value := aAnchor.TangentVector.x;
  edtDirY.Value := aAnchor.TangentVector.y;
  edtDirZ.Value := aAnchor.TangentVector.z;
  edtUpVecAngle.Value := aModel.UpVectorToAngle(aAnchor);
end;

procedure TMainForm.ChangeAnchorProperty(Sender: TObject);
var
  Anchor: TSplineAnchor;
  v:      TVector3f;
begin
  if (not TWinControl(Sender).Focused) then Exit;

  Anchor := SplineModel.SelectedAnchor;
  if assigned(Anchor) then
  begin
    Anchor.Position      := Vec3f(edtPosX.Value,
                                  edtPosY.Value,
                                  edtPosZ.Value);
    Anchor.TangentVector := Vec3f(edtDirX.Value,
                                  edtDirY.Value,
                                  edtDirZ.Value);
  end;

  //Update view to reflect new situation
  View.ReDraw();
end;

procedure TMainForm.AnchorListClick(Sender: TObject);
begin
  SplineModel.SelectedAnchor := SplineModel.Spline.Anchors[AnchorList.ItemIndex];
end;

end.
