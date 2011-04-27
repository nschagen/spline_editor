unit usplinexml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uspline, DOM, XMLWrite;

type

  { TSplineLoader }

  TSplineLoader = class
  public
    constructor Create();
    procedure LoadFromFile(aSpline: TSpline; aFileName: String);
  end;

  { TSplineSaver }

  TSplineSaver = class
  public
    constructor Create();
    procedure SaveToFile(aSpline: TSpline; aFileName: String);
  end;

implementation

{ TSplineSaver }

constructor TSplineSaver.Create();
begin

end;

procedure TSplineSaver.SaveToFile(aSpline: TSpline; aFileName: String);
var
  Doc: TXMLDocument;
  Anchor: TSplineAnchor;
  Root, AnchorNode, Position, Tangent, UpVector: TDOMNode;
  FS: TFileStream;
  I: Integer;
begin
  try
    Doc := TXMLDocument.Create();
    FS  := TFileStream.Create(aFileName, fmCreate);

    Root := Doc.CreateElement('Spline');
    TDOMElement(Root).SetAttribute('Closed','True');
    Doc.AppendChild(Root);

    for I:=0 to aSpline.AnchorCount-1 do
    begin
      Anchor := aSpline.Anchors[I];

      AnchorNode    := Doc.CreateElement('Anchor');
      TDOMElement(AnchorNode).SetAttribute('ID', IntToStr(I));

      Position  := Doc.CreateElement('Position');
      with TDOMElement(Position) do
      begin
        SetAttribute('x',Format('%.3n',[Anchor.Position.x]));
        SetAttribute('y',Format('%.3n',[Anchor.Position.y]));
        SetAttribute('z',Format('%.3n',[Anchor.Position.z]));
      end;

      Tangent   := Doc.CreateElement('Tangent');
      with TDOMElement(Tangent) do
      begin
        SetAttribute('x',Format('%.3n',[Anchor.TangentVector.x]));
        SetAttribute('y',Format('%.3n',[Anchor.TangentVector.y]));
        SetAttribute('z',Format('%.3n',[Anchor.TangentVector.z]));
      end;

      Upvector  := Doc.CreateElement('Upvector');
      with TDOMElement(Upvector) do
      begin
        SetAttribute('x',Format('%.3n',[Anchor.Upvector.x]));
        SetAttribute('y',Format('%.3n',[Anchor.Upvector.y]));
        SetAttribute('z',Format('%.3n',[Anchor.Upvector.z]));
      end;

      AnchorNode.AppendChild(Position);
      AnchorNode.AppendChild(Tangent);
      AnchorNode.AppendChild(Upvector);

      Root.AppendChild(AnchorNode);
    end;

    WriteXMLFile(Doc, FS);
  finally
    Doc.Free();
    FS.Free();
  end;
end;

{ TSplineLoader }

constructor TSplineLoader.Create();
begin

end;

procedure TSplineLoader.LoadFromFile(aSpline: TSpline; aFileName: String);
var
  Doc: TXMLDocument;
  Root: TDOMNode;
begin
  try
    Doc := TXMLDocument.Create();
  finally
    Doc.Free();
  end;
end;

end.

