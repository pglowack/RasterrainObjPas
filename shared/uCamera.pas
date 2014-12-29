unit uCamera;

interface

uses
  uVect;

type
  Camera = record
    campos, camdir, camright, camdown: Vect;
    class function Create: Camera; overload; static;
    class function Create(pos, dir, right, down: Vect): Camera; overload; static;
    function getCameraPosition: Vect;
    function getCameraDirection: Vect;
    function getCameraRight: Vect;
    function getCameraDown: Vect;
  end;

implementation

{ Camera }

class function Camera.Create: Camera;
begin
  Result.campos := Vect.Create(0,0,0);
  Result.camdir := Vect.Create(0,0,1);
  Result.camright := Vect.Create(0,0,0);
  Result.camdown := Vect.Create(0,0,0);
end;

class function Camera.Create(pos, dir, right, down: Vect): Camera;
begin
  Result.campos := pos;
  Result.camdir := dir;
  Result.camright := right;
  Result.camdown := down;
end;

function Camera.getCameraDirection: Vect;
begin
  Result := camdir;
end;

function Camera.getCameraDown: Vect;
begin
  Result := camdown;
end;

function Camera.getCameraPosition: Vect;
begin
  Result := campos;
end;

function Camera.getCameraRight: Vect;
begin
  Result := camright;
end;

end.
