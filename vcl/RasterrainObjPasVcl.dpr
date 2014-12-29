program RasterrainObjPasVcl;

uses
  Vcl.Forms,
  uFormRenderVCL in 'uFormRenderVCL.pas' {Form17},
  uRendererVCL in 'uRendererVCL.pas',
  uVect in '..\shared\uVect.pas',
  uSphere in '..\shared\uSphere.pas',
  uSource in '..\shared\uSource.pas',
  uSimpleRayTracer in '..\shared\uSimpleRayTracer.pas',
  uSceneObject in '..\shared\uSceneObject.pas',
  uRay in '..\shared\uRay.pas',
  uPlane in '..\shared\uPlane.pas',
  uLight in '..\shared\uLight.pas',
  uColour in '..\shared\uColour.pas',
  uCamera in '..\shared\uCamera.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm17, Form17);
  Application.Run;
end.
