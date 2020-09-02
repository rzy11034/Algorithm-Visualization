object AlgoForm: TAlgoForm
  Left = 581
  Height = 240
  Top = 152
  Width = 320
  Caption = 'AlgoForm'
  ClientHeight = 240
  ClientWidth = 320
  DesignTimePPI = 120
  OnCreate = FormCreate
  OnShow = FormShow
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
    FullRepaint = False
    ParentColor = False
    ParentFont = False
    TabOrder = 0
  end
end
