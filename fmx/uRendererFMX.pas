unit uRendererFMX;

interface

uses
  System.UITypes,
  FMX.Graphics,
  uSimpleRayTracer;

type
  TRendererFMX = class
  private
    FSimpleRayTracer: TSimpleRayTracer;
    function GetPixelColor(x,y: integer): TAlphaColor;
  public
    constructor Create(aWidth, aHeight: integer; aAntiAlias: integer = 1);
    destructor Destroy; override;
    function DoRender: TBitmap;
  end;

implementation

{ TRendererFMX }

constructor TRendererFMX.Create(aWidth, aHeight: integer; aAntiAlias: integer = 1);
begin
  FSimpleRayTracer := TSimpleRayTracer.Create(aWidth, aHeight, aAntiAlias);
end;

destructor TRendererFMX.Destroy;
begin
  FSimpleRayTracer.Free;
  inherited;
end;

function TRendererFMX.DoRender: TBitmap;
var bmp: TBitmap; data: TBitmapData;
  w,h,x,y: integer; c: TAlphaColor;
begin
  FSimpleRayTracer.CalculatePixelColors;

  w := FSimpleRayTracer.getWidth;
  h := FSimpleRayTracer.getHeight;

  bmp := TBitmap.Create;
  bmp.Width := w;
  bmp.Height := h;

  bmp.Map(TMapAccess.Write, data);
  try
    for x := 0 to w - 1 do
      for y := 0 to h - 1 do
      begin
        c := GetPixelColor(x, h-y);
        data.SetPixel(x, y, c);
      end;

  finally
    bmp.Unmap(data);
  end;

  Result := bmp;
end;

function TRendererFMX.GetPixelColor(x, y: integer): TAlphaColor;
var c: RGBType; colorF: TAlphaColorF;
begin
  c := FSimpleRayTracer.getPixelData(x, y);
  colorF := TAlphaColorF.Create(c.r, c.g, c.b);
  Result := colorF.ToAlphaColor;
end;

end.
