program spline_editor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umain, usplineeditor, uview, nasha_vectors, nasha_math, nasha_matrices,
  nasha_primitives, uspline, usplinexml, bgrabitmappack, umouse, usplinemodel
  { you can add units after this };

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

