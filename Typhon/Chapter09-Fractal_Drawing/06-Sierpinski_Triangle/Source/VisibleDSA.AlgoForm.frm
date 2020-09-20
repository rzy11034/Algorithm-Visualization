object AlgoForm: TAlgoForm
  Left = 587
  Height = 169
  Top = 156
  Width = 244
  Caption = 'AlgoForm'
  ClientHeight = 169
  ClientWidth = 244
  DesignTimePPI = 120
  OnCreate = FormCreate
  Position = poDesktopCenter
  LCLVersion = '7.2'
  object BGRAVirtualScreen: TBGRAVirtualScreen
    Left = 0
    Height = 169
    Top = 0
    Width = 244
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
