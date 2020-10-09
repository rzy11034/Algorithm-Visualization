object AlgoForm: TAlgoForm
  Left = 86
  Height = 600
  Top = 28
  Width = 600
  Caption = 'AlgoForm'
  ClientHeight = 600
  ClientWidth = 600
  DesignTimePPI = 120
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  Position = poDesktopCenter
  LCLVersion = '7.1'
  object BGRAVirtualScreen: TBGRAVirtualScreen
    Left = 0
    Height = 600
    Top = 0
    Width = 600
    OnRedraw = BGRAVirtualScreenRedraw
    Align = alClient
    Alignment = taLeftJustify
    Color = clNone
    ParentColor = False
    ParentFont = False
    TabOrder = 0
  end
end
