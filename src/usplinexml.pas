unit usplinexml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uspline, DOM, XMLWrite, XMLRead, nasha_vectors;

type

  { TSplineLoader }

  TSplineLoader = class
  public
    procedure LoadFromFile(aSpline: TSpline; aFileName: String);
  end;

  { TSplineSaver }

  TSplineSaver = class
  public
    procedure SaveToFile(aSpline: TSpline; aFileName: String);
  end;

implementation

{ TSplineSaver }

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

      //Set text content
      AnchorNode.TextContent:= Anchor.Text;

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

procedure TSplineLoader.LoadFromFile(aSpline: TSpline; aFileName: String);
var
  Doc: TXMLDocument;
  Anchor: TSplineAnchor;
  Root, AnchorNode, Position, Tangent, UpVector: TDOMNode;
  v: TVector3f;
begin
  try
    //Remove all anchors
    aSpline.Clear();

    //Parse XML file
    Doc := TXMLDocument.Create();
    ReadXMLFile(Doc, aFileName);
    try
      AnchorNode := Doc.DocumentElement.FirstChild;
      while Assigned(AnchorNode) do
      begin
        Anchor := TSplineAnchor.Create(aSpline);
        Position := AnchorNode.FindNode('Position');
        v.x := StrToFloat(TDOMElement(Position).GetAttribute('x'));
        v.y := StrToFloat(TDOMElement(Position).GetAttribute('y'));
        v.z := StrToFloat(TDOMElement(Position).GetAttribute('z'));
        Anchor.Position := v;

        Tangent := AnchorNode.FindNode('Tangent');
        v.x := StrToFloat(TDOMElement(Tangent).GetAttribute('x'));
        v.y := StrToFloat(TDOMElement(Tangent).GetAttribute('y'));
        v.z := StrToFloat(TDOMElement(Tangent).GetAttribute('z'));
        Anchor.TangentVector := v;

        Upvector := AnchorNode.FindNode('Upvector');
        v.x := StrToFloat(TDOMElement(Upvector).GetAttribute('x'));
        v.y := StrToFloat(TDOMElement(Upvector).GetAttribute('y'));
        v.z := StrToFloat(TDOMElement(Upvector).GetAttribute('z'));
        Anchor.UpVector := v;

        Anchor.Text := AnchorNode.TextContent;

        aSpline.AddAnchor(Anchor);
        AnchorNode := AnchorNode.NextSibling;
      end;
    except
      on E: Exception do
        raise Exception.Create('Failed parsing Spline file: '+E.Message);
    end;

  finally
    Doc.Free();
  end;
end;

end.
