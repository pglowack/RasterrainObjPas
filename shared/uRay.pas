unit uRay;

interface

uses
  uVect;

type
  Ray = record
    origin, direction: Vect;
    class function Create: Ray; overload; static;
    class function Create(o,d: Vect): Ray; overload; static;
  end;

implementation

{ Ray }

class function Ray.Create: Ray;
begin
  Result.origin := Vect.Create(0,0,0);
  Result.direction := Vect.Create(1,0,0);
end;

class function Ray.Create(o, d: Vect): Ray;
begin
  Result.origin := o;
  Result.direction := d;
end;

end.
