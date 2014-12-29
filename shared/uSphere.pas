unit uSphere;

interface

uses
  uVect, uRay, uColour, uSceneObject;

type
  Sphere = class(SceneObject)
  private
    center: Vect;
    radius: Double;
    color: Colour;
  public
    constructor Create; overload;
    constructor Create(centerValue: Vect; radiusValue: Double; colorValue: Colour); overload;
    function getSphereCenter: Vect;
    function getSphereRadius: Double;
    function getColor: Colour; override;
    function getNormalAt(point: Vect): Vect; override;
    function findIntersection(r: Ray): Double; override;
  end;


implementation

{ Sphere }

constructor Sphere.Create;
begin
  center := Vect.Create(0,0,0);
  radius := 1;
  color := Colour.Create(0.5, 0.5, 0.5, 0);
end;

constructor Sphere.Create(centerValue: Vect; radiusValue: Double;
  colorValue: Colour);
begin
  center := centerValue;
  radius := radiusValue;
  color := colorValue;
end;

function Sphere.findIntersection(r: Ray): Double;
var ray_origin, ray_direction, sphere_center: Vect;
  ray_origin_x, ray_origin_y, ray_origin_z,
  ray_direction_x, ray_direction_y, ray_direction_z,
  sphere_center_x, sphere_center_y, sphere_center_z,
  b, c, discriminant, root_1, root_2: Double;
begin
  ray_origin := r.getRayOrigin();
  ray_origin_x := ray_origin.getVectX();
  ray_origin_y := ray_origin.getVectY();
  ray_origin_z := ray_origin.getVectZ();

  ray_direction := r.getRayDirection();
  ray_direction_x := ray_direction.getVectX();
  ray_direction_y := ray_direction.getVectY();
  ray_direction_z := ray_direction.getVectZ();

	sphere_center := center;
	sphere_center_x := sphere_center.getVectX();
	sphere_center_y := sphere_center.getVectY();
	sphere_center_z := sphere_center.getVectZ();

  b := (2*(ray_origin_x - sphere_center_x)*ray_direction_x) + (2*(ray_origin_y - sphere_center_y)*ray_direction_y) + (2*(ray_origin_z - sphere_center_z)*ray_direction_z);
	c := (ray_origin_x - sphere_center_x)*(ray_origin_x - sphere_center_x)
     + (ray_origin_y - sphere_center_y)*(ray_origin_y - sphere_center_y)
     + (ray_origin_z - sphere_center_z)*(ray_origin_z - sphere_center_z)
     - (radius*radius);

  discriminant := b*b - 4*c;

  if discriminant > 0 then
  begin
    // the ray intersects the sphere

    // the first root
		root_1 := ((-1*b - sqrt(discriminant))/2) - 0.000001;

    if root_1 > 0 then
      // the first root is the smallest positive root
      Result := root_1
    else
    begin
      // the second root is the smallest positive root
      root_2 := ((sqrt(discriminant) - b)/2) - 0.000001;
      Result := root_2;
    end;

  end
  else
    // the ray missed the sphere
    Result := -1;

end;

function Sphere.getColor: Colour;
begin
  Result := color;
end;

function Sphere.getNormalAt(point: Vect): Vect;
begin
  // normal always points away from the center of a sphere
  Result := point.vectAdd(center.negative()).normalize();
end;

function Sphere.getSphereCenter: Vect;
begin
  Result := center;
end;

function Sphere.getSphereRadius: Double;
begin
  Result := radius;
end;

end.
