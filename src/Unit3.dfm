object FormInfo: TFormInfo
  Left = 561
  Top = 340
  BorderStyle = bsDialog
  Caption = 'Info'
  ClientHeight = 105
  ClientWidth = 161
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnKeyDown = FormKeyDown
  DesignSize = (
    161
    105)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 53
    Top = 12
    Width = 53
    Height = 13
    Caption = 'Version 1.0'
  end
  object Label3: TLabel
    Left = 33
    Top = 32
    Width = 88
    Height = 13
    Caption = 'Author : FoxAhead'
  end
  object Label4: TLabel
    Left = 28
    Top = 52
    Width = 40
    Height = 13
    Caption = 'Source :'
  end
  object Label1: TLabel
    Left = 76
    Top = 52
    Width = 52
    Height = 13
    Cursor = crHandPoint
    Caption = 'github.com'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = LabelUrlGithubClick
    OnMouseEnter = LabelUrlMouseEnter
    OnMouseLeave = LabelUrlMouseLeave
  end
  object Button1: TButton
    Left = 43
    Top = 76
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Caption = 'OK'
    TabOrder = 0
    OnClick = Button1Click
  end
end
