object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'HTTP Client'
  ClientHeight = 591
  ClientWidth = 650
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 560
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    650
    591)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 366
    Top = 280
    Width = 38
    Height = 13
    Caption = 'Storage'
  end
  object Gauge1: TGauge
    Left = 568
    Top = 8
    Width = 74
    Height = 23
    Anchors = [akTop, akRight]
    Color = clHotLight
    ParentColor = False
    Progress = 0
    ShowText = False
    ExplicitLeft = 471
  end
  object Panel2: TPanel
    Left = 0
    Top = 542
    Width = 650
    Height = 30
    Anchors = [akLeft, akRight, akBottom]
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    TabOrder = 12
    DesignSize = (
      650
      30)
    object SpeedButton1: TSpeedButton
      Left = 221
      Top = 2
      Width = 130
      Height = 26
      GroupIndex = 1
      Caption = 'Response Content'
      Flat = True
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 8
      Top = 2
      Width = 83
      Height = 26
      GroupIndex = 1
      Down = True
      Caption = 'Requests'
      Flat = True
      OnClick = SpeedButton2Click
    end
    object Label2: TLabel
      Left = 366
      Top = 9
      Width = 33
      Height = 13
      Caption = 'Label2'
      Color = clRed
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Roboto'
      Font.Style = [fsBold]
      Font.Quality = fqClearType
      ParentColor = False
      ParentFont = False
    end
    object SpeedButton3: TSpeedButton
      Left = 91
      Top = 2
      Width = 130
      Height = 26
      GroupIndex = 1
      Caption = 'Request Content'
      Flat = True
      OnClick = SpeedButton3Click
    end
    object Button4: TButton
      Left = 567
      Top = 3
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Clear'
      TabOrder = 0
      OnClick = Button4Click
    end
  end
  object ListBox1: TListBox
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 560
    Height = 238
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 90
    Margins.Bottom = 0
    Align = alTop
    DoubleBuffered = True
    ItemHeight = 13
    ParentDoubleBuffered = False
    TabOrder = 0
    OnMouseUp = ListBox1MouseUp
  end
  object Edit1: TEdit
    AlignWithMargins = True
    Left = 0
    Top = 241
    Width = 560
    Height = 21
    Margins.Left = 0
    Margins.Right = 90
    Margins.Bottom = 0
    Align = alTop
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 1
    Text = 'Edit1'
  end
  object Button1: TButton
    Left = 8
    Top = 275
    Width = 65
    Height = 25
    Caption = 'GET'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 567
    Top = 239
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Add'
    TabOrder = 3
    OnClick = Button2Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 572
    Width = 650
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object Button3: TButton
    Left = 150
    Top = 275
    Width = 65
    Height = 25
    Caption = 'GET All'
    TabOrder = 4
    OnClick = Button3Click
  end
  object CheckBox1: TCheckBox
    Left = 240
    Top = 279
    Width = 81
    Height = 17
    Caption = 'Keep-Alive'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 5
  end
  object Edit2: TEdit
    Left = 319
    Top = 277
    Width = 34
    Height = 21
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 6
    Text = '10'
  end
  object Edit3: TEdit
    Left = 414
    Top = 277
    Width = 228
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 7
    Text = 'D:\Public\'
  end
  object Button5: TButton
    Left = 567
    Top = 207
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Delete'
    TabOrder = 9
    OnClick = Button5Click
  end
  object Panel1: TPanel
    Left = 434
    Top = 354
    Width = 185
    Height = 45
    Anchors = [akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'Panel1'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    ShowCaption = False
    TabOrder = 8
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 185
      Height = 45
      Align = alClient
      AutoSize = True
      Center = True
      ParentShowHint = False
      Proportional = True
      ShowHint = True
      Stretch = True
      OnClick = Image1Click
      ExplicitHeight = 65
    end
  end
  object Button6: TButton
    Left = 79
    Top = 275
    Width = 65
    Height = 25
    Caption = 'GET...'
    TabOrder = 10
    OnClick = Button6Click
  end
  object Memo1: TMemo
    Left = 0
    Top = 312
    Width = 650
    Height = 229
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvNone
    BevelOuter = bvNone
    Ctl3D = True
    DoubleBuffered = True
    ParentCtl3D = False
    ParentDoubleBuffered = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 13
    WordWrap = False
  end
  object Memo2: TMemo
    Left = 0
    Top = 312
    Width = 650
    Height = 229
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvNone
    BevelOuter = bvNone
    Ctl3D = True
    DoubleBuffered = True
    ParentCtl3D = False
    ParentDoubleBuffered = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 14
  end
  object Memo3: TMemo
    Left = 0
    Top = 312
    Width = 650
    Height = 229
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvNone
    BevelOuter = bvNone
    Ctl3D = True
    DoubleBuffered = True
    ParentCtl3D = False
    ParentDoubleBuffered = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 15
  end
  object Timer1: TTimer
    Interval = 1
    OnTimer = Timer1Timer
    Left = 32
    Top = 24
  end
end
