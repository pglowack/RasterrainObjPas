unit uSceneObject;

interface

uses
  uColour, uVect, uRay;

type
  SceneObject = class abstract
  public
    function getColor: Colour; virtual; abstract;
    function getNormalAt(point: Vect): Vect; virtual; abstract;
    function findIntersection(r: Ray): Double; virtual; abstract;
  end;

implementation

end.
