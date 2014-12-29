unit uSource;

interface

uses
  uVect, uColour;

type
  Source = class abstract
  public
	  function getLightPosition: Vect; virtual; abstract;
	  function getLightColor: Colour; virtual; abstract;
  end;

implementation

end.
