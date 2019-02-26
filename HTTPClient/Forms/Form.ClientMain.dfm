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
    TabOrder = 8
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 79
    Top = 275
    Width = 65
    Height = 25
    Caption = 'GET...'
    TabOrder = 9
    OnClick = Button6Click
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
  end
end
