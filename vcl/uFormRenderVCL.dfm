object Form17: TForm17
  Left = 0
  Top = 0
  Caption = 'Rasterrain - Object Pascal - VCL'
  ClientHeight = 451
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 41
    Width = 554
    Height = 410
    Align = alClient
    ExplicitLeft = 232
    ExplicitTop = 192
    ExplicitWidth = 105
    ExplicitHeight = 105
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 554
    Height = 41
    Align = alTop
    TabOrder = 0
    DesignSize = (
      554
      41)
    object LabelAntiAlias: TLabel
      Left = 199
      Top = 9
      Width = 87
      Height = 13
      Caption = 'Anti-Aliasing Level'
    end
    object LabelTime: TLabel
      Left = 327
      Top = 9
      Width = 80
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Caption = '(Rendering time)'
    end
    object btnRender: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Render'
      TabOrder = 0
      OnClick = btnRenderClick
    end
    object TrackBarAnitAlias: TTrackBar
      Left = 89
      Top = 0
      Width = 104
      Height = 45
      Min = 1
      Position = 1
      TabOrder = 1
      OnChange = TrackBarAnitAliasChange
    end
  end
end
