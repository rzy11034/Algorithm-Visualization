object AlgoForm: TAlgoForm
  Left = 583
  Height = 182
  Top = 156
  Width = 257
  Caption = 'AlgoForm'
  ClientHeight = 182
  ClientWidth = 257
  OnCreate = FormCreate
  Position = poDesktopCenter
  object BGRAVirtualScreen: TBGRAVirtualScreen
    Left = 0
    Height = 182
    Top = 0
    Width = 257
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
