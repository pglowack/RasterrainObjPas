unit uFormRenderFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls;

type
  TFormRenderFMX = class(TForm)
    btnRender: TButton;
    LabelTime: TLabel;
    ToolBar1: TToolBar;
    Image1: TImage;
    TrackBarAntiAliasingLevel: TTrackBar;
    LabelAntiAliasing: TLabel;
    procedure btnRenderClick(Sender: TObject);
    procedure TrackBarAntiAliasingLevelChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormRenderFMX: TFormRenderFMX;

implementation

uses
  uRendererFMX, System.Diagnostics;

{$R *.fmx}

procedure TFormRenderFMX.btnRenderClick(Sender: TObject);
var sw: TStopwatch; r: TRendererFMX; bmp: TBitmap;
begin
  LabelTime.Text := 'Rendering... Please wait.';
  self.Invalidate;
  Application.ProcessMessages;

  sw := TStopwatch.StartNew;
  r := TRendererFMX.Create(round(Image1.Width), round(Image1.Height), round(TrackBarAntiAliasingLevel.Value));
  bmp := r.DoRender;
  Image1.Bitmap.Assign(bmp);
  sw.Stop;

  LabelTime.Text := IntToStr(sw.ElapsedMilliseconds) + ' msec ('
    + IntToStr(sw.ElapsedTicks) + ' ticks)';
end;

procedure TFormRenderFMX.FormCreate(Sender: TObject);
begin
  TrackBarAntiAliasingLevelChange(self);
end;

procedure TFormRenderFMX.TrackBarAntiAliasingLevelChange(Sender: TObject);
begin
  LabelAntiAliasing.Text := 'Anti-Aliasing Level: ' + TrackBarAntiAliasingLevel.Value.ToString;
end;

end.
