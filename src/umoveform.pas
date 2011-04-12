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

