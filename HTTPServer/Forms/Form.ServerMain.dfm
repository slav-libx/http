object ServerMainForm: TServerMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'ServerMainForm'
  ClientHeight = 603
  ClientWidth = 853
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
    853
    603)
  PixelsPerInch = 96
  TextHeight = 13
  object HostLabel: TLabel
    Left = 14
    Top = 18
    Width = 22
    Height = 13
    Caption = 'Host'
  end
  object HomeLabel: TLabel
    Left = 14
    Top = 50
    Width = 27
    Height = 13
    Caption = 'Home'
  end
  object AliasesLabel: TLabel
    Left = 14
    Top = 81
    Width = 33
    Height = 13
    Caption = 'Aliases'
  end
  object ResultLabel: TLabel
    Left = 603
    Top = 152
    Width = 59
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'ResultLabel'
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
  object StartServerButton: TButton
    Left = 762
    Top = 146
    Width = 84
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Start'
    TabOrder = 0
    OnClick = StartServerButtonClick
  end
  object PortEdit: TEdit
    Left = 247
    Top = 15
    Width = 31
    Height = 21
    TabOrder = 1
    Text = '80'
  end
  object HostEdit: TEdit
    Left = 56
    Top = 15
    Width = 185
    Height = 21
    TabOrder = 2
    Text = '124'
  end
  object HomeEdit: TEdit
    Left = 56
    Top = 47
    Width = 789
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = '124'
  end
  object AliasesMemo: TMemo
    Left = 56
    Top = 79
    Width = 789
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'scenario=D:\RP5Scenarios'
      'resources=D:\RP5Scenarios\Resources')
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object KeepAliveTimeoutEdit: TEdit
    Left = 178
    Top = 148
    Width = 31
    Height = 21
    TabOrder = 5
    Text = '10'
  end
  object CloseConnectionsButton: TButton
    Left = 634
    Top = 146
    Width = 122
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Close connections'
    TabOrder = 6
    OnClick = CloseConnectionsButtonClick
  end
  object KeepAliveCheckBox: TCheckBox
    Left = 56
    Top = 151
    Width = 115
    Height = 17
    Caption = 'Keep-Alive Timeout'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 7
  end
  inline CommunicationFrame: TCommunicationFrame
    Left = 0
    Top = 180
    Width = 853
    Height = 423
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 8
    ExplicitTop = 180
    ExplicitWidth = 853
    ExplicitHeight = 423
    inherited TabPanel: TPanel
      Top = 393
      Width = 853
      ExplicitTop = 393
      ExplicitWidth = 853
      inherited ClearButton: TButton
        Left = 770
        ExplicitLeft = 770
      end
    end
    inherited LogMemo: TMemo
      Width = 853
      Height = 393
      ExplicitWidth = 853
      ExplicitHeight = 393
    end
    inherited RequestMemo: TMemo
      Width = 853
      Height = 393
      ExplicitWidth = 853
      ExplicitHeight = 393
    end
    inherited ResponseMemo: TMemo
      Width = 853
      Height = 393
      ExplicitWidth = 853
      ExplicitHeight = 393
    end
    inherited PictureScrollBox: TScrollBox
      Width = 853
      Height = 393
      ExplicitWidth = 853
      ExplicitHeight = 393
      inherited ContentImage: TImage
        Width = 829
        Height = 369
        ExplicitWidth = 829
        ExplicitHeight = 369
      end
    end
  end
end
