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
unit umoveform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, Buttons, nasha_vectors;

type

  { TMoveForm }

  TMoveForm = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    edtX: TFloatSpinEdit;
    edtY: TFloatSpinEdit;
    edtZ: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  private
    { private declarations }
  public
  end;

  function ShowMoveDialog(): TVector3f;

var
  MoveForm: TMoveForm;
implementation

uses umain;

{$R *.lfm}

function ShowMoveDialog(): TVector3f;
begin
    Result := Vec3f(0,0,0);
    if MoveForm.ShowModal() = mrOK then
      Result := Vec3f(MoveForm.edtX.Value, MoveForm.edtY.Value, MoveForm.edtZ.Value);    ;
end;

end.

