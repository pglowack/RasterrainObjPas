unit uFormRenderVCL;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TForm17 = class(TForm)
    btnRender: TButton;
    Panel1: TPanel;
    TrackBarAnitAlias: TTrackBar;
    LabelAntiAlias: TLabel;
    LabelTime: TLabel;
    Image1: TImage;
    procedure TrackBarAnitAliasChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnRenderClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form17: TForm17;

implementation

{$R *.dfm}

uses uRendererVCL, System.Diagnostics;

procedure TForm17.btnRenderClick(Sender: TObject);
var sw: TStopwatch; r: TRendererVCL;
begin
  LabelTime.Caption := 'Rendering... Please wait.';
  self.Invalidate;
  Application.ProcessMessages;

  sw := TStopwatch.StartNew;
  r := TRendererVCL.Create(round(Image1.Width), round(Image1.Height), TrackBarAnitAlias.Position);
  Image1.Picture.Assign(r.DoRender);
  sw.Stop;
  LabelTime.Caption := IntToStr(sw.ElapsedMilliseconds) + ' msec ('
    + IntToStr(sw.ElapsedTicks) + ' ticks)';
end;

procedure TForm17.FormCreate(Sender: TObject);
begin
  TrackBarAnitAliasChange(self);
end;

procedure TForm17.TrackBarAnitAliasChange(Sender: TObject);
begin
  LabelAntiAlias.Caption := 'Anti-Aliasing Level: ' + TrackBarAnitAlias.Position.ToString;
end;

end.
