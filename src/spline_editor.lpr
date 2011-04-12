program spline_editor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umain, usplineeditor, uview, nasha_vectors, nasha_math, nasha_matrices,
  nasha_primitives, uspline, usplinexml, bgrabitmappack, umouse, usplinemodel,
  uscaleform, umoveform
  { you can add units after this };

{$R *.res}

begin
  Application.Title := 'Spline Editor';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TScaleForm, ScaleForm);
  Application.CreateForm(TMoveForm, MoveForm);
  Application.Run;
end.

