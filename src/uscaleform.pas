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

