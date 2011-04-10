unit usplinexml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uspline;

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
begin

end;

{ TSplineLoader }

constructor TSplineLoader.Create();
begin

end;

procedure TSplineLoader.LoadFromFile(aSpline: TSpline; aFileName: String);
begin

end;

end.

