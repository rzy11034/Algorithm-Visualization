object AlgoForm: TAlgoForm
  Left = 581
  Height = 240
  Top = 149
  Width = 320
  Caption = 'AlgoForm'
  ClientHeight = 240
  ClientWidth = 320
  DesignTimePPI = 120
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  Position = poDesktopCenter
  LCLVersion = '7.1'
  object BGRAVirtualScreen: TBGRAVirtualScreen
    Left = 0
    Height = 240
    Top = 0
    Width = 320
    OnRedraw = BGRAVirtualScreenRedraw
    Align = alClient
    Alignment = taLeftJustify
    Color = clNone
    ParentColor = False
    TabOrder = 0
  end
end
