unit uscaleform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, Buttons, nasha_vectors, nasha_primitives;

type

  TScaleMethod = (smFactors, smResize);

  { TScaleForm }

  TScaleForm = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    edtX: TFloatSpinEdit;
    edtY: TFloatSpinEdit;
    edtZ: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioGroup1: TRadioGroup;
    procedure RadioButton1Click(Sender: TObject);
  private
    FScaleMethod: TScaleMethod;
    FSplineAABB: TnaAABBf;
  public
    { public declarations }
  end;

  function ShowScaleDialog(aSplineAABB: TnaAABBf): TVector3f;

var
  ScaleForm: TScaleForm;

implementation

{$R *.lfm}

function ShowScaleDialog(aSplineAABB: TnaAABBf): TVector3f;
var
  input: TVector3f;
begin
    Result := Vec3f(0,0,0);
    ScaleForm.FSplineAABB := aSplineAABB;
    if ScaleForm.ShowModal() = mrOK then
    begin
      input := Vec3f(ScaleForm.edtX.Value, ScaleForm.edtY.Value, ScaleForm.edtZ.Value);
      case ScaleForm.FScaleMethod of
        smFactors: Result := input;
        smResize:  begin
                     Result := Vec3f(input.x / ScaleForm.FSplineAABB.width,
                                     input.y / ScaleForm.FSplineAABB.height,
                                     input.z / ScaleForm.FSplineAABB.length);
                   end;
      end;
    end;
end;

{ TScaleForm }

procedure TScaleForm.RadioButton1Click(Sender: TObject);
begin
  with ScaleForm do
  begin
    case TComponent(Sender).Tag of
    0:  begin
          edtX.Value := 1.0;
          edtY.Value := 1.0;
          edtZ.Value := 1.0;
          FScaleMethod := smFactors;
        end;
    1:  begin
          edtX.Value := FSplineAABB.width;
          edtY.Value := FSplineAABB.height;
          edtZ.Value := FSplineAABB.length;
          FScaleMethod := smResize;
        end;
    end;
  end;
end;

end.

