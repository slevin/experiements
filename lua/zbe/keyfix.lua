return {
  name = "Home/End modified",
  description = "Modified Ctrl-Left and Ctrl-right to behave as Home/End.",
  author = "Paul Kulchenko",
  version = 0.1,

  onRegister = function()
    DisplayOutputLn("Sample plugin registered")
    print("this is working")
  end,

  onEditorCharAdded = function(self, editor, event)
    print("char added")
  end,

  onEditorFocusSet = function(self, editor, event)
--    print("focus set")
  end,

  onEditorKeyDown = function(self, editor, event)
    print("key down")
    DisplayOutputLn("key down")
    local keycode = event:GetKeyCode()
    local mod = event:GetModifiers()
    if (keycode == wx.WXK_LEFT or keycode == wx.WXK_RIGHT)
    and mod == wx.wxMOD_CONTROL then
      local line = editor:GetCurrentLine()
      local pos = keycode == wx.WXK_LEFT
        and editor:PositionFromLine(line)+5
        or editor:GetLineEndPosition(line)-1
      editor:GotoPos(pos)
    end
    return
  end,
}
