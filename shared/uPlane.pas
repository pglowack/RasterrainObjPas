unit uPlane;

interface

uses
  uVect, uRay, uColour, uSceneObject;

type
  Plane = class(SceneObject)
  private
    normal: Vect;
    distance: Double;
    color: Colour;
  public
    constructor Create; overload;
    constructor Create(normalValue: Vect; distanceValue: Double; colorValue: Colour); overload;
    function getPlaneNormal: Vect;
    function getPlaneDistance: Double;
    function getColor: Colour; override;
    function getNormalAt(point: Vect): Vect; override;
    function findIntersection(r: Ray): Double; override;
  end;

implementation

uses System.Math;

{ Plane }

constructor Plane.Create;
begin
  normal := Vect.Create(1,0,0);
  distance := 0;
  color := Colour.Create(0.5, 0.5, 0.5, 0);
end;

constructor Plane.Create(normalValue: Vect; distanceValue: Double;
  colorValue: Colour);
begin
  normal := normalValue;
  distance := distanceValue;
  color := colorValue;
end;

function Plane.findIntersection(r: Ray): Double;
var ray_direction: Vect; a, b: Double;
begin
  ray_direction := r.direction;

  a := ray_direction.dotProduct(normal);

  if SameValue(a,0) then
    Result := -1
  else
  begin
    b := normal.dotProduct(r.origin.vectAdd(normal.vectMult(distance).negative()));
    Result := -1*b/a;
  end;
end;

function Plane.getColor: Colour;
begin
  Result := color;
end;

function Plane.getNormalAt(point: Vect): Vect;
begin
  Result := normal;
end;

function Plane.getPlaneDistance: Double;
begin
  Result := distance;
end;

function Plane.getPlaneNormal: Vect;
begin
  Result := normal;
end;

end.
