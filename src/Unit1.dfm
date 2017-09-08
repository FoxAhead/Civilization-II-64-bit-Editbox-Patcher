object Form1: TForm1
  Left = 535
  Top = 361
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Civilization II 64-bit Editbox Patcher'
  ClientHeight = 285
  ClientWidth = 513
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poDesktopCenter
  OnCreate = FormCreate
  DesignSize = (
    513
    285)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 4
    Width = 170
    Height = 13
    Caption = 'Select Civ2.exe or Civ2Map.exe file:'
  end
  object Label2: TLabel
    Left = 472
    Top = 2
    Width = 33
    Height = 13
    Cursor = crHandPoint
    Anchors = [akTop, akRight]
    Caption = 'GitHub'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = OpenGitHubLink
  end
  object Label3: TLabel
    Left = 8
    Top = 260
    Width = 75
    Height = 13
    Caption = '2017 FoxAhead'
    Color = clBtnFace
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object LabelVersion: TLabel
    Left = 470
    Top = 260
    Width = 35
    Height = 13
    Alignment = taRightJustify
    Caption = 'Version'
    Enabled = False
  end
  object LabelDebug: TLabel
    Left = 424
    Top = 260
    Width = 81
    Height = 13
    AutoSize = False
    Caption = ' '
    Transparent = True
    OnClick = LabelVersionClick
  end
  object ButtonPatch: TButton
    Left = 204
    Top = 255
    Width = 105
    Height = 25
    Anchors = [akTop]
    Caption = 'Patch!'
    Enabled = False
    TabOrder = 1
    OnClick = ButtonPatchClick
  end
  object EditFileName: TEdit
    Left = 4
    Top = 20
    Width = 429
    Height = 21
    TabStop = False
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 2
  end
  object ButtonBrowse: TButton
    Left = 436
    Top = 18
    Width = 73
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse ...'
    TabOrder = 0
    OnClick = ButtonBrowseClick
  end
  object Memo1: TMemo
    Left = 4
    Top = 48
    Width = 505
    Height = 201
    TabStop = False
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'Welcome!'
      'This is Civilization II 64-bit Editbox Patcher.'
      
        'It eliminates game and editor crashes on 64-bit systems when the' +
        ' game tries to display edit controls '
      
        '(input fields) for entering text (like city name, emperor name, ' +
        'world sizes etc.).'
      
        'Unlike the existing well known patch by Mastermind it will retai' +
        'n the full functionality of edit controls '
      'including the response to the Enter, TAB or Escape keys.'
      
        'So, the establishing city is now just '#39'B'#39', '#39'Enter'#39'. And customiz' +
        'ing world size is like '#39'100'#39', '#39'TAB'#39', '#39'100'#39', '
      #39'Enter'#39'. No more mouse clicking.'
      'Supported game versions are:'
      '* Civilization II Multiplayer Gold Edition'
      '* Civilization II: Test of Time'
      
        'The patcher can even detect if Mastermind'#39's patch has already be' +
        'en applied and will replace it.'
      ''
      
        'Just click '#39'Browse...'#39', select Civ2.exe or Civ2Map.exe file and ' +
        'then click '#39'Patch!'#39'.')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object OpenDialog1: TOpenDialog
    Filter = '*.exe|*.exe'
    InitialDir = '.'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 224
    Top = 16
  end
end
