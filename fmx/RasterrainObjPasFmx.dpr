program RasterrainObjPasFmx;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFormRenderFMX in 'uFormRenderFMX.pas' {FormRenderFMX},
  uVect in '..\shared\uVect.pas',
  uRay in '..\shared\uRay.pas',
  uSceneObject in '..\shared\uSceneObject.pas',
  uColour in '..\shared\uColour.pas',
  uPlane in '..\shared\uPlane.pas',
  uSphere in '..\shared\uSphere.pas',
  uCamera in '..\shared\uCamera.pas',
  uLight in '..\shared\uLight.pas',
  uSource in '..\shared\uSource.pas',
  uSimpleRayTracer in '..\shared\uSimpleRayTracer.pas',
  uRendererFMX in 'uRendererFMX.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormRenderFMX, FormRenderFMX);
  Application.Run;
end.
