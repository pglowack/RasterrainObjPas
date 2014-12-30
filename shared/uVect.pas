unit uVect;

interface

type
  Vect = record
    x, y, z: Double;
    class function Create: Vect; overload; static;
    class function Create(i,j,k: Double): Vect; overload; static;
    function magnitude: Double;
    function normalize: Vect;
    function negative: Vect;
    function dotProduct(v: Vect): double;
    function crossProduct(v: Vect): Vect;
    function vectAdd(v: Vect): Vect;
    function vectMult(scalar: Double): Vect;
  end;

implementation

{ Vect }

class function Vect.Create: Vect;
begin
  Result.x := 0;
  Result.y := 0;
  Result.z := 0;
end;

class function Vect.Create(i, j, k: Double): Vect;
begin
  Result.x := i;
  Result.y := j;
  Result.z := k;
end;

function Vect.crossProduct(v: Vect): Vect;
begin
	Result := Vect.Create(
    y * v.z - z * v.y,
    z * v.x - x * v.z,
    x * v.y - y * v.x
  );
end;

function Vect.dotProduct(v: Vect): double;
begin
	Result := x * v.x + y * v.y + z * v.z;
end;

function Vect.magnitude: Double;
begin
	Result := sqrt((x*x) + (y*y) + (z*z));
end;

function Vect.negative: Vect;
begin
  Result := Vect.Create(-x,-y,-z);
end;

function Vect.normalize: Vect;
var m: Double;
begin
  m := magnitude;
	Result := Vect.Create(x/m, y/m, z/m);
end;

function Vect.vectAdd(v: Vect): Vect;
begin
	Result := Vect.Create(x + v.x, y + v.y, z + v.z);
end;

function Vect.vectMult(scalar: Double): Vect;
begin
  Result := Vect.Create(x*scalar, y*scalar, z*scalar);
end;

end.
