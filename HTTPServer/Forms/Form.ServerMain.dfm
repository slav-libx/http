object Form3: TForm3
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Form3'
  ClientHeight = 468
  ClientWidth = 577
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 500
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    577
    468)
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 14
    Top = 18
    Width = 22
    Height = 13
    Caption = 'Host'
  end
  object Label4: TLabel
    Left = 14
    Top = 50
    Width = 27
    Height = 13
    Caption = 'Home'
  end
  object Label5: TLabel
    Left = 14
    Top = 81
    Width = 33
    Height = 13
    Caption = 'Aliases'
  end
  object Label2: TLabel
    Left = 327
    Top = 152
    Width = 33
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Label2'
    Color = clGreen
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Roboto'
    Font.Style = [fsBold]
    Font.Quality = fqClearType
    ParentColor = False
    ParentFont = False
  end
  object Button1: TButton
    Left = 486
    Top = 146
    Width = 84
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Start'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit2: TEdit
    Left = 247
    Top = 15
    Width = 31
    Height = 21
    TabOrder = 1
    Text = '80'
  end
  object RequestsMemo: TMemo
    Left = 0
    Top = 180
    Width = 577
    Height = 254
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object Edit1: TEdit
    Left = 56
    Top = 15
    Width = 185
    Height = 21
    TabOrder = 3
    Text = '124'
  end
  object Edit3: TEdit
    Left = 56
    Top = 47
    Width = 513
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    Text = '124'
  end
  object Memo2: TMemo
    Left = 56
    Top = 79
    Width = 513
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'scenario=D:\RP5Scenarios'
      'resources=D:\RP5Scenarios\Resources')
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object Edit4: TEdit
    Left = 178
    Top = 148
    Width = 31
    Height = 21
    TabOrder = 6
    Text = '10'
  end
  object ContentMemo: TMemo
    Left = 0
    Top = 180
    Width = 577
    Height = 254
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 7
  end
  object Panel2: TPanel
    Left = 0
    Top = 438
    Width = 577
    Height = 30
    Anchors = [akLeft, akRight, akBottom]
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    TabOrder = 8
    DesignSize = (
      577
      30)
    object SpeedButton1: TSpeedButton
      Left = 91
      Top = 1
      Width = 118
      Height = 26
      GroupIndex = 1
      Caption = 'Request Content'
      Flat = True
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 8
      Top = 1
      Width = 83
      Height = 26
      GroupIndex = 1
      Down = True
      Caption = 'Requests'
      Flat = True
      OnClick = SpeedButton2Click
    end
    object SpeedButton3: TSpeedButton
      Left = 209
      Top = 1
      Width = 118
      Height = 26
      GroupIndex = 1
      Caption = 'Response Content'
      Flat = True
      OnClick = SpeedButton3Click
    end
    object Button4: TButton
      Left = 494
      Top = 3
      Width = 76
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Clear'
      TabOrder = 0
      OnClick = Button4Click
    end
  end
  object Button2: TButton
    Left = 358
    Top = 146
    Width = 122
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Close connections'
    TabOrder = 9
    OnClick = Button2Click
  end
  object ResponseMemo: TMemo
    Left = 0
    Top = 180
    Width = 577
    Height = 254
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 10
  end
  object CheckBox1: TCheckBox
    Left = 56
    Top = 151
    Width = 115
    Height = 17
    Caption = 'Keep-Alive Timeout'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 11
  end
end
