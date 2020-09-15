object AlgoForm: TAlgoForm
  Left = 587
  Height = 228
  Top = 156
  Width = 321
  Caption = 'AlgoForm'
  ClientHeight = 228
  ClientWidth = 321
  DesignTimePPI = 120
  OnCreate = FormCreate
  OnShow = FormShow
  Position = poDesktopCenter
  object BGRAVirtualScreen: TBGRAVirtualScreen
    Left = 0
    Height = 228
    Top = 0
    Width = 321
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
