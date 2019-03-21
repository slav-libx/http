object ClientMainForm: TClientMainForm
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
  object StorageFilesLabel: TLabel
    Left = 366
    Top = 280
    Width = 38
    Height = 13
    Caption = 'Storage'
  end
  object ResourcesListBox: TListBox
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
    OnMouseUp = ResourcesListBoxMouseUp
  end
  object ResourceEdit: TEdit
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
    Text = 'ResourceEdit'
    OnKeyDown = ResourceEditKeyDown
  end
  object GetResourceButton: TButton
    Left = 8
    Top = 275
    Width = 65
    Height = 25
    Caption = 'GET'
    TabOrder = 2
    OnClick = GetResourceButtonClick
  end
  object AddResourceButton: TButton
    Left = 567
    Top = 239
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Add'
    TabOrder = 3
    OnClick = AddResourceButtonClick
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 572
    Width = 650
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object GetLisResourcesButton: TButton
    Left = 150
    Top = 275
    Width = 65
    Height = 25
    Caption = 'GET All'
    TabOrder = 4
    OnClick = GetLisResourcesButtonClick
  end
  object KeepAliveCheckBox: TCheckBox
    Left = 240
    Top = 279
    Width = 81
    Height = 17
    Caption = 'Keep-Alive'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 5
  end
  object KeepAliveTimeoutEdit: TEdit
    Left = 319
    Top = 277
    Width = 34
    Height = 21
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 6
    Text = '10'
  end
  object StorageFilesEdit: TEdit
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
  object RemoveResourceButton: TButton
    Left = 567
    Top = 207
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Delete'
    TabOrder = 8
    OnClick = RemoveResourceButtonClick
  end
  object GetResourceExButton: TButton
    Left = 79
    Top = 275
    Width = 65
    Height = 25
    Caption = 'GET...'
    TabOrder = 9
    OnClick = GetResourceExButtonClick
  end
  inline CommunicationFrame: TCommunicationFrame
    Left = 0
    Top = 312
    Width = 650
    Height = 260
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 11
    ExplicitTop = 312
    ExplicitWidth = 650
    ExplicitHeight = 260
    inherited TabPanel: TPanel
      Top = 230
      Width = 650
      ExplicitTop = 230
      ExplicitWidth = 650
      inherited ClearButton: TButton
        Left = 567
        ExplicitLeft = 567
      end
    end
    inherited LogMemo: TMemo
      Width = 650
      Height = 230
      ExplicitWidth = 650
      ExplicitHeight = 230
    end
    inherited RequestMemo: TMemo
      Width = 650
      Height = 230
      ExplicitWidth = 650
      ExplicitHeight = 230
    end
    inherited ResponseMemo: TMemo
      Width = 650
      Height = 230
      ExplicitWidth = 650
      ExplicitHeight = 230
    end
    inherited PictureScrollBox: TScrollBox
      Width = 650
      Height = 230
      ExplicitWidth = 650
      ExplicitHeight = 230
      inherited ContentImage: TImage
        Width = 626
        Height = 206
        ExplicitWidth = 626
        ExplicitHeight = 206
      end
    end
  end
end
