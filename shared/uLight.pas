unit uLight;

interface

uses
  uVect, uColour, uSource;

type
  Light = class(Source)
  private
    position: Vect;
    color: Colour;
  public
    constructor Create; overload;
    constructor Create(p: Vect; c: Colour); overload;
	  function getLightPosition: Vect; override;
	  function getLightColor: Colour; override;
  end;

implementation

{ Light }

constructor Light.Create;
begin
  position := Vect.Create(0,0,0);
  color := Colour.Create(1,1,1,0);
end;

constructor Light.Create(p: Vect; c: Colour);
begin
  position := p;
  color := c;
end;

function Light.getLightColor: Colour;
begin
  Result := color;
end;

function Light.getLightPosition: Vect;
begin
  Result := position;
end;

end.
