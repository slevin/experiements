--[[--
  Use this file to specify User preferences.
  Review [examples](+/Applications/ZeroBraneStudio.app/Contents/ZeroBraneStudio/cfg/user-sample.lua) or check [online documentation](http://studio.zerobrane.com/documentation.html) for details.
--]]--

local G = ...
styles = G.loadfile('cfg/tomorrow.lua')('Monokai')
stylesoutshell = styles -- apply the same scheme to Output/Console windows
styles.auxwindow = styles.text -- apply text colors to auxiliary windows
styles.calltip = styles.text -- apply text colors to tooltips

editor.fontsize = 15
editor.fontname = "Source Code Pro"
--editor.fontname = "Inconsolata-g"

-- cmd / to comment like xcode
keymap[G.ID_COMMENT] = "Ctrl-/"


-- shift command [] cmd
-- shift command o
-- change tabs ctlx b
-- better tab formatting options?
-- corona autocomplete, busted?
-- what's wrong with option-d? causes weird crash in package 

--editor.keymap[#editor.keymap+1] = {('A'):byte(), wxstc.wxSTC_SCMOD_META, wxstc.wxSTC_CMD_HOME}

--editor.keymap[#editor.keymap+1] = {('E'):byte(), wxstc.wxSTC_SCMOD_NONE, wxstc.wxSTC_CMD_LINEEND}
--editor.keymap[#editor.keymap+1] = {wxstc.wxSTC_KEY_LEFT, wxstc.wxSTC_SCMOD_ALT+wxstc.wxSTC_SCMOD_SHIFT, wxstc.wxSTC_CMD_WORDLEFTEXTEND, "Macintosh"}
