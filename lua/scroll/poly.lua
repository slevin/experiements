--[[
local oldIndex = getmetatable(sq).__index

local function mysetFillColor(self, ...)
  print("in mysetfillcolor")
  print(arg)
  local old = oldIndex(self, "setFillColor")
  if arg.n == 1 and type(arg[1] == "table") then
    print("in mysetfill with arg0 as table")
    printAll(arg)
    print("in there?")
    printAll(arg[1])
    old(self, unpack(arg[1]))
  else
    old(self, unpack(arg))
  end
end

local function myIndex(self, key)
  print("in myindex with key " .. key)
  if key == "setFillColor" then
    return mysetFillColor
  else
    return oldIndex(self, key)
  end
end

local temp = getmetatable(sq)
temp.__index = myIndex
setmetatable(sq, temp)
sq:setFillColor(cardBlue)
--]]
