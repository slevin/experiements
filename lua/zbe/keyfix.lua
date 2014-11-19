return {
  name = "Home/End modified",
  description = "Modified Ctrl-Left and Ctrl-right to behave as Home/End.",
  author = "Paul Kulchenko",
  version = 0.1,

  onRegister = function()
  end,

  onEditorCharAdded = function(self, editor, event)
  end,

  onEditorFocusSet = function(self, editor, event)
  end,

  --[[
 This page is helpful for commands http://wxlua.sourceforge.net/docs/wxluaref.html
 -- mac ctrl == 16, raw_control
 -- mac option == 1
 -- mac cmd == 2
 --]]
  onEditorKeyDown = function(self, editor, event)
    local keycode = event:GetKeyCode()
    local mod = event:GetModifiers()
--    DisplayOutputLn("key code:", keycode)
--    DisplayOutputLn("modifier:", mod)
--    DisplayOutputLn("alt:", wx.wxMOD_ALT)
--    DisplayOutputLn("D:", tostring(('D'):byte()))

    if mod == wx.wxMOD_CMD + wx.wxMOD_SHIFT then
      if keycode == ('['):byte() then
        local nb = ide.frame.notebook
        nb:AdvanceSelection(false)
        return false
      elseif keycode == (']'):byte() then
        local nb = ide.frame.notebook
        nb:AdvanceSelection(true)
        return false
      end
    end

    if mod == wx.wxMOD_ALT then
--      DisplayOutputLn("in1")
--      if keycode == ('D'):byte() then
--        DisplayOutputLn("in2")
--        editor:DeleteWordRight()
--        return false
--      end
    end

    if mod == wx.wxMOD_RAW_CONTROL then
      if keycode == ('E'):byte() then
        editor:LineEnd()
        return false
      end

      if keycode == ('A'):byte() then
        editor:VCHome()
        return false
      end      

      if keycode == ('P'):byte() then
        editor:LineUp()
        return false
      end      

      if keycode == ('N'):byte() then
        editor:LineDown()
        return false
      end      

      if keycode == ('F'):byte() then
        editor:CharRight()
        return false
      end  

      if keycode == ('B'):byte() then
        editor:CharLeft()
        return false
      end      

      if keycode == ('K'):byte() then
        --[[
        emacs deletes to the end of the line
        and if at the beginning and line is empty
        then delete the whole line
        --]]
        local line = editor:GetCurrentLine()
        local lineBegin = editor:PositionFromLine(line)
        local lineEnd = editor:GetLineEndPosition(line)
        if lineBegin == lineEnd then
          editor:LineDelete()
        else
          editor:DelLineRight()
        end
        return false
      end
    end
  end,
}
