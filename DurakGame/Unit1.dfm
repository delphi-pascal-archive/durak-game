object Form1: TForm1
  Left = 192
  Top = 107
  Caption = 'RST.Durak.BOT'
  ClientHeight = 368
  ClientWidth = 513
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  DesignSize = (
    513
    368)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 56
    Width = 85
    Height = 13
    Caption = #1048#1075#1088#1086#1074#1086#1081' '#1089#1077#1088#1074#1077#1088':'
  end
  object Label2: TLabel
    Left = 193
    Top = 56
    Width = 66
    Height = 13
    Caption = #1056#1077#1078#1080#1084' '#1080#1075#1088#1099':'
  end
  object Label3: TLabel
    Left = 8
    Top = 101
    Width = 63
    Height = 13
    Caption = #1048#1084#1103' '#1080#1075#1088#1086#1082#1072':'
  end
  object Label4: TLabel
    Left = 193
    Top = 101
    Width = 67
    Height = 13
    Caption = #1057#1090#1072#1074#1082#1072' '#1080#1075#1088#1099':'
  end
  object Label5: TLabel
    Left = 272
    Top = 101
    Width = 41
    Height = 13
    Caption = #1055#1072#1088#1086#1083#1100':'
  end
  object Label6: TLabel
    Left = 351
    Top = 101
    Width = 40
    Height = 13
    Caption = #1050#1086#1083#1086#1076#1072':'
  end
  object Label7: TLabel
    Left = 431
    Top = 101
    Width = 40
    Height = 13
    Caption = #1048#1075#1088#1086#1082#1080':'
  end
  object Memo1: TMemo
    Left = 8
    Top = 160
    Width = 494
    Height = 196
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      #1041#1086#1090' '#1076#1083#1103' http://durak.rstgames.com/ru/'
      #1040#1074#1090#1086#1088': black40x@yandex.ru'
      ''
      #1042' '#1088#1077#1078#1080#1084#1077' '#1087#1088#1086#1080#1075#1088#1099#1074#1072#1090#1100' '#1041#1086#1090' '#1073#1091#1076#1077#1090' '#1089#1088#1072#1079#1091' '#1078#1077' '#1089#1083#1080#1074#1072#1090#1100' '#1080#1075#1088#1091'.'
      #1042' '#1088#1077#1078#1080#1084#1077' '#1080#1084#1080#1090#1072#1094#1080#1080' '#1080#1075#1088#1099' '#1041#1086#1090' '#1080#1075#1088#1072#1077#1090' '#1085#1091' '#1086#1086#1086#1086#1095#1077#1085#1100' '#1087#1083#1086#1093#1086'.'
      ''
      
        'P.S. '#1044#1072#1085#1085#1099#1081' '#1041#1086#1090', '#1089#1086#1079#1076#1072#1085' '#1090#1086#1083#1100#1082#1086' '#1082#1072#1082' '#1087#1088#1080#1084#1077#1088', '#1089#1084#1080#1088#1080#1090#1077#1089#1100' '#1089#1086' '#1074#1089#1077#1084#1080' '#1077#1075 +
        #1086' '#1085#1077#1076#1086#1095#1077#1090#1072#1084#1080' =)'
      
        'P.P.S. '#1040#1074#1090#1086#1088' '#1085#1077' '#1086#1090#1095#1077#1095#1072#1077#1090' '#1079#1072' '#1074#1072#1096#1080' '#1084#1072#1093#1080#1085#1072#1094#1080#1080' '#1074' '#1080#1075#1088#1077'. '#1044#1072#1085#1085#1072#1103' '#1087#1088#1086#1075#1088#1072 +
        #1084#1084#1072', '#1090#1086#1083#1100#1082#1086' '#1087#1088#1080#1084#1077#1088' '
      #1090#1086#1075#1086', '#1082#1072#1082' '#1084#1086#1078#1085#1086' "'#1086#1073#1084#1072#1085#1091#1090#1100'" '#1080#1075#1088#1091'.')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitWidth = 335
  end
  object Button1: TButton
    Left = 8
    Top = 16
    Width = 89
    Height = 25
    Caption = #1053#1086#1074#1099#1081' '#1080#1075#1088#1086#1082
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 103
    Top = 16
    Width = 96
    Height = 25
    Caption = #1057#1086#1079#1076#1072#1090#1100' '#1080#1075#1088#1091
    TabOrder = 2
    OnClick = Button2Click
  end
  object RegName: TEdit
    Left = 8
    Top = 120
    Width = 170
    Height = 21
    TabOrder = 3
    Text = 'Anonimous'
  end
  object Amount: TEdit
    Left = 193
    Top = 120
    Width = 73
    Height = 21
    TabOrder = 4
    Text = '1000'
  end
  object Button9: TButton
    Left = 427
    Top = 16
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #1042#1099#1093#1086#1076
    TabOrder = 5
    OnClick = Button9Click
    ExplicitLeft = 749
  end
  object ServersList: TComboBox
    Left = 8
    Top = 74
    Width = 170
    Height = 21
    Style = csDropDownList
    TabOrder = 6
    OnChange = ServersListChange
  end
  object GameMode: TComboBox
    Left = 193
    Top = 74
    Width = 152
    Height = 21
    Style = csDropDownList
    ItemIndex = 2
    TabOrder = 7
    Text = #1048#1084#1080#1090#1072#1094#1080#1103' '#1080#1075#1088#1099
    Items.Strings = (
      #1055#1088#1086#1080#1075#1088#1099#1074#1072#1090#1100
      #1042#1089#1077#1075#1076#1072' '#1079#1072#1073#1080#1088#1072#1090#1100' '#1082#1072#1088#1090#1099
      #1048#1084#1080#1090#1072#1094#1080#1103' '#1080#1075#1088#1099)
  end
  object GamePass: TEdit
    Left = 272
    Top = 120
    Width = 73
    Height = 21
    TabOrder = 8
    Text = '1234'
  end
  object Deck: TComboBox
    Left = 351
    Top = 120
    Width = 74
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 9
    Text = '24'
    Items.Strings = (
      '24'
      '36'
      '52')
  end
  object Players: TComboBox
    Left = 431
    Top = 120
    Width = 74
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 10
    Text = '2'
    Items.Strings = (
      '2'
      '3'
      '4')
  end
end
