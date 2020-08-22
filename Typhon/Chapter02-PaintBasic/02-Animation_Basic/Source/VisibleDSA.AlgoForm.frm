object AlgoForm: TAlgoForm
  Left = 418
  Height = 192
  Top = 119
  Width = 256
  Caption = 'AlgoForm'
  ClientHeight = 192
  ClientWidth = 256
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  Position = poDesktopCenter
  object BGRAVirtualScreen: TBGRAVirtualScreen
    Left = 0
    Height = 192
    Top = 0
    Width = 256
    OnRedraw = BGRAVirtualScreenRedraw
    Align = alClient
    Alignment = taLeftJustify
    Color = clNone
    ParentColor = False
    ParentFont = False
    TabOrder = 0
    OnMouseDown = BGRAVirtualScreenMouseDown
  end
end
