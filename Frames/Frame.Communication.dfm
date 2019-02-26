object CommunicationFrame: TCommunicationFrame
  Left = 0
  Top = 0
  Width = 571
  Height = 322
  TabOrder = 0
  object TabPanel: TPanel
    Left = 0
    Top = 292
    Width = 571
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    TabOrder = 0
    DesignSize = (
      571
      30)
    object ResponseTab: TSpeedButton
      Left = 211
      Top = 2
      Width = 120
      Height = 26
      GroupIndex = 1
      Caption = 'Response Content'
      Flat = True
      OnClick = ResponseTabClick
    end
    object LogTab: TSpeedButton
      Left = 8
      Top = 2
      Width = 83
      Height = 26
      GroupIndex = 1
      Down = True
      Caption = 'Requests'
      Flat = True
      OnClick = LogTabClick
    end
    object CodeLabel: TLabel
      Left = 346
      Top = 9
      Width = 18
      Height = 13
      Caption = '200'
      Color = clGreen
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Roboto'
      Font.Style = [fsBold]
      Font.Quality = fqClearType
      ParentColor = False
      ParentFont = False
      Transparent = False
    end
    object RequestTab: TSpeedButton
      Left = 91
      Top = 2
      Width = 120
      Height = 26
      GroupIndex = 1
      Caption = 'Request Content'
      Flat = True
      OnClick = RequestTabClick
    end
    object ClearButton: TButton
      Left = 488
      Top = 3
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Clear'
      TabOrder = 0
      OnClick = ClearButtonClick
    end
  end
  object LogMemo: TMemo
    Left = 0
    Top = 0
    Width = 571
    Height = 292
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    Ctl3D = True
    DoubleBuffered = True
    ParentCtl3D = False
    ParentDoubleBuffered = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    WordWrap = False
  end
  object RequestMemo: TMemo
    Left = 0
    Top = 0
    Width = 571
    Height = 292
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    Ctl3D = True
    DoubleBuffered = True
    ParentCtl3D = False
    ParentDoubleBuffered = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object ResponseMemo: TMemo
    Left = 0
    Top = 0
    Width = 571
    Height = 292
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    Ctl3D = True
    DoubleBuffered = True
    ParentCtl3D = False
    ParentDoubleBuffered = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object ImagePanel: TPanel
    Left = 331
    Top = 130
    Width = 185
    Height = 45
    BevelOuter = bvNone
    DoubleBuffered = True
    ParentDoubleBuffered = False
    ShowCaption = False
    TabOrder = 4
    object ContentImage: TImage
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
      Transparent = True
      ExplicitLeft = -88
      ExplicitTop = -24
    end
  end
end
