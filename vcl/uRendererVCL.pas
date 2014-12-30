unit uRendererVCL;

interface

uses
  VCL.Graphics, uSimpleRayTracer;

type
  TRendererVCL = class
  private
    FSimpleRayTracer: TSimpleRayTracer;
  public
    constructor Create(aWidth, aHeight: integer; aAntiAlias: integer = 1);
    destructor Destroy; override;
    function DoRender: TBitmap;
  end;

implementation

uses
  System.Types;

{ TRendererVCL }

constructor TRendererVCL.Create(aWidth, aHeight, aAntiAlias: integer);
begin
  FSimpleRayTracer := TSimpleRayTracer.Create(aWidth, aHeight, aAntiAlias);
end;

destructor TRendererVCL.Destroy;
begin
  FSimpleRayTracer.Free;
  inherited;
end;

function TRendererVCL.DoRender: TBitmap;
var bmp: TBitmap; CurrRow, OffSet: integer;
  x,y: integer; pRed, pGreen, pBlue, pAlpha: PByte;
  c: RGBType; h: integer;
begin
  FSimpleRayTracer.CalculatePixelColors;

  bmp := TBitmap.Create;
  bmp.PixelFormat := pf24bit;
  bmp.Width := FSimpleRayTracer.getWidth;
  bmp.Height := FSimpleRayTracer.getHeight;
  h := FSimpleRayTracer.getHeight;

  CurrRow := Integer(bmp.ScanLine[0]);
  OffSet := Integer(bmp.ScanLine[1]) - CurrRow;

  for y := 0 to bmp.Height - 1 do
  begin
    for x := 0 to bmp.Width - 1 do
    begin
      pBlue  := pByte(CurrRow + x*3);
      pGreen := pByte(CurrRow + x*3 + 1);
      pRed   := pByte(CurrRow + x*3 + 2);
      pAlpha := pByte(CurrRow + x*3 + 3);
      c := FSimpleRayTracer.getPixelData(x, h-y);
      pBlue^  := round(c.b * 255);
      pGreen^ := round(c.g * 255);
      pRed^   := round(c.r * 255);
      pAlpha^ := 255;
    end;
    inc(CurrRow, OffSet);
  end;

  Result := bmp;
end;

end.
