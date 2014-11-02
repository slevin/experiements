--[[

is it possible to take an existing method
  and get it take a version with some color struct?

swiping card moves it left and right

really it moves the set in a new direction then it should complete
  to where it needs to



  list of cards means more of them

  flexible pixels (need to do scale)


--]]

local bottomHeight = 100
local restHeight = display.contentHeight - bottomHeight
local restCenterY = restHeight * 0.5

local padding = 50

local cardBlue = {0;0.65;0.97}

-- background
local bg = display.newRect(display.contentCenterX,
                           display.contentCenterY,
                           display.contentWidth,
                           display.contentHeight)

bg:setFillColor(1)

local sq = display.newRoundedRect(display.contentCenterX,
                                  restCenterY,
                                  display.contentWidth - (padding * 2),
                                  restHeight - (padding * 2), 20)


local startOffsetX = 0

local function onObjectTouch(event)
  if event.phase == "began" then
    startOffsetX = sq.x - event.x
  elseif event.phase == "moved" then
    sq.x = event.x + startOffsetX
  end
end

sq:addEventListener("touch", onObjectTouch)

local function printAll(t)
  local m = getmetatable(t)
  for k,v in pairs(t) do
    print(k)
    print(v)
  end
end



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

sq:setFillColor(0.5,0.5,0.5)



--[[
  -- ScrollView listener
  local function scrollListener( event )

  local phase = event.phase
  if ( phase == "began" ) then print( "Scroll view was touched" )
  elseif ( phase == "moved" ) then print( "Scroll view was moved" )
  elseif ( phase == "ended" ) then print( "Scroll view was released" )
  end

  -- In the event a scroll limit is reached...
  if ( event.limitReached ) then
  if ( event.direction == "up" ) then print( "Reached top limit" )
  elseif ( event.direction == "down" ) then print( "Reached bottom limit" )
  elseif ( event.direction == "left" ) then print( "Reached left limit" )
  elseif ( event.direction == "right" ) then print( "Reached right limit" )
  end
  end

  return true
  end

  -- Create the widget
  local scrollView = widget.newScrollView
  {
  top = 100,
  left = 10,
  width = 300,
  height = 400,
  scrollWidth = 600,
  scrollHeight = 800,
  listener = scrollListener
  }

  -- Create a image and insert it into the scroll view
  local background = display.newImageRect( "ziggy.jpg", 768, 1024 )
  scrollView:insert( background )
--]]
