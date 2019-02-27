object RequestForm: TRequestForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Create Request'
  ClientHeight = 436
  ClientWidth = 587
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  DesignSize = (
    587
    436)
  PixelsPerInch = 96
  TextHeight = 13
  object MethodLabel: TLabel
    Left = 13
    Top = 24
    Width = 36
    Height = 13
    Caption = 'Method'
  end
  object HeadersLabel: TLabel
    Left = 13
    Top = 80
    Width = 40
    Height = 13
    Caption = 'Headers'
  end
  object RequestLabel: TLabel
    Left = 13
    Top = 53
    Width = 40
    Height = 13
    Caption = 'Request'
  end
  object ProtocolLabel: TLabel
    Left = 438
    Top = 24
    Width = 39
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Protocol'
  end
  object BottomBevel: TBevel
    Left = 0
    Top = 386
    Width = 587
    Height = 50
    Align = alBottom
    Shape = bsTopLine
    ExplicitLeft = 120
    ExplicitTop = 240
    ExplicitWidth = 161
  end
  object ContentLabel: TLabel
    Left = 13
    Top = 210
    Width = 39
    Height = 13
    Caption = 'Content'
  end
  object ContentTypeLabel: TLabel
    Left = 312
    Top = 344
    Width = 24
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Type'
  end
  object RequestButton: TButton
    Left = 409
    Top = 400
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Request'
    ModalResult = 1
    TabOrder = 0
    ExplicitTop = 422
  end
  object CancelButton: TButton
    Left = 492
    Top = 400
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    ExplicitTop = 422
  end
  object MethodComboBox: TComboBox
    Left = 80
    Top = 21
    Width = 89
    Height = 21
    Style = csDropDownList
    TabOrder = 2
    Items.Strings = (
      'GET'
      'HEAD'
      'POST'
      'PUT'
      'OPTIONS'
      'TRACE')
  end
  object AddButton: TButton
    Left = 513
    Top = 177
    Width = 54
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Add'
    TabOrder = 3
    OnClick = AddButtonClick
  end
  object RequestEdit: TEdit
    Left = 80
    Top = 51
    Width = 487
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    Text = 'RequestEdit'
  end
  object HeaderNameComboBox: TComboBox
    Left = 80
    Top = 179
    Width = 169
    Height = 21
    DropDownCount = 20
    TabOrder = 5
    Text = 'HeaderNameComboBox'
    OnChange = HeaderNameComboBoxChange
    Items.Strings = (
      'Host'
      'User-Agent'
      'Accept'
      'Accept-Language'
      'Accept-Encoding'
      'Referer'
      'Origin'
      'Cookie'
      'Connection'
      'Keep-Alive'
      'Cache-Control'
      'If-Modified-Since'
      'If-None-Match'
      'Pragma'
      'Upgrade'
      'Content-Type'
      'Content-Length'
      'Upgrade-Insecure-Requests'
      'Authorization')
  end
  object HeaderValueComboBox: TComboBox
    Left = 255
    Top = 179
    Width = 248
    Height = 21
    DropDownCount = 20
    TabOrder = 6
    Text = 'HeaderValueComboBox'
  end
  object ContentMemo: TMemo
    Left = 80
    Top = 210
    Width = 487
    Height = 121
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      'ContentMemo')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 7
  end
  object ContentTypeComboBox: TComboBox
    Left = 344
    Top = 341
    Width = 223
    Height = 21
    Anchors = [akTop, akRight]
    DropDownCount = 20
    TabOrder = 8
    Text = 'ContentTypeComboBox'
    Items.Strings = (
      '')
  end
  object OpenFileButton: TButton
    Left = 80
    Top = 340
    Width = 89
    Height = 25
    Caption = 'Open File...'
    TabOrder = 9
    OnClick = OpenFileButtonClick
  end
  object RemoveFileButton: TButton
    Left = 176
    Top = 340
    Width = 73
    Height = 25
    Caption = 'Remove'
    TabOrder = 10
    OnClick = RemoveFileButtonClick
  end
  object HeadersMemo: TMemo
    Left = 80
    Top = 81
    Width = 487
    Height = 89
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      'HeadersMemo')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 11
    WordWrap = False
  end
  object ProtocolComboBox: TComboBox
    Left = 486
    Top = 21
    Width = 81
    Height = 21
    TabOrder = 12
    Text = 'ProtocolComboBox'
    Items.Strings = (
      'HTTP/1.1'
      'HTTP/1.0')
  end
  object OpenDialog1: TOpenDialog
    Left = 16
    Top = 296
  end
end
