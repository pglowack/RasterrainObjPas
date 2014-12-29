unit uColour;

interface

type
  Colour = record
    red, green, blue, special: Double;
    class function Create: Colour; overload; static;
    class function Create(r,g,b,s: Double): Colour; overload; static;
	  function getColorRed: Double;
	  function getColorGreen: Double;
	  function getColorBlue: Double;
	  function getColorSpecial: Double;
    procedure setColorRed(redValue: Double);
    procedure setColorGreen(greenValue: Double);
    procedure setColorBlue(blueValue: Double);
    procedure setColorSpecial(specialValue: Double);
    function 	brightness: Double;
    function colorScalar(scalar: Double): Colour;
    function colorAdd(color: Colour): Colour;
    function colorMultiply(color: Colour): Colour;
    function colorAverage(color: Colour): Colour;
    function Clip: Colour;
  end;

implementation

{ Colour }

class function Colour.Create: Colour;
begin
  Result.red := 0.5;
  Result.green := 0.5;
  Result.blue := 0.5;
  Result.special := 0; // missing in Color.h
end;

function Colour.brightness: Double;
begin
  Result := (red + green + blue)/3;
end;

function Colour.Clip: Colour;
var alllight, excesslight: Double;
begin
  alllight := red + green + blue;
  excesslight := alllight - 3;

  if excesslight > 0 then
  begin
    red := red + excesslight*(red/alllight);
    green := green + excesslight*(green/alllight);
    blue := blue + excesslight*(blue/alllight);
  end;

  if red > 1 then red := 1;
  if green > 1 then green := 1;
  if blue > 1 then blue := 1;
  if red < 0 then red := 0;
  if green < 0 then green := 0;
  if blue < 0 then blue := 0;

  Result := Colour.Create(red, green, blue, special);
end;

function Colour.colorAdd(color: Colour): Colour;
begin
  Result := Colour.Create(red + color.getColorRed(), green + color.getColorGreen(), blue + color.getColorBlue(), special);
end;

function Colour.colorAverage(color: Colour): Colour;
begin
  Result := Colour.Create((red + color.getColorRed())/2, (green + color.getColorGreen())/2, (blue + color.getColorBlue())/2, special);
end;

function Colour.colorMultiply(color: Colour): Colour;
begin
  Result := Colour.Create(red*color.getColorRed(), green*color.getColorGreen(), blue*color.getColorBlue(), special);
end;

function Colour.colorScalar(scalar: Double): Colour;
begin
  Result := Colour.Create(red*scalar, green*scalar, blue*scalar, special);
end;

class function Colour.Create(r, g, b, s: Double): Colour;
begin
  Result.red := r;
  Result.green := g;
  Result.blue := b;
  Result.special := s;
end;

function Colour.getColorBlue: Double;
begin
  Result := blue;
end;

function Colour.getColorGreen: Double;
begin
  Result := green;
end;

function Colour.getColorRed: Double;
begin
  Result := red;
end;

function Colour.getColorSpecial: Double;
begin
  Result := special;
end;

procedure Colour.setColorBlue(blueValue: Double);
begin
  blue := blueValue;
end;

procedure Colour.setColorGreen(greenValue: Double);
begin
  green := greenValue;
end;

procedure Colour.setColorRed(redValue: Double);
begin
  red := redValue;
end;

procedure Colour.setColorSpecial(specialValue: Double);
begin
  special := specialValue;
end;

end.
