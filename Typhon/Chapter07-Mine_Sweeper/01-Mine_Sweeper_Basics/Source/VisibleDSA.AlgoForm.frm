object AlgoForm: TAlgoForm
  Left = 418
  Height = 165
  Top = 119
  Width = 250
  Caption = 'AlgoForm'
  ClientHeight = 165
  ClientWidth = 250
  OnCreate = FormCreate
  OnShow = FormShow
  Position = poDesktopCenter
  LCLVersion = '7.1'
  object BGRAVirtualScreen: TBGRAVirtualScreen
    Left = 0
    Height = 165
    Top = 0
    Width = 250
    OnRedraw = BGRAVirtualScreenRedraw
    Align = alClient
    Alignment = taLeftJustify
    Color = clNone
    FullRepaint = False
    ParentColor = False
    ParentFont = False
    TabOrder = 0
  end
end
