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

local cardBlue = {r=0; g=0.65; b=0.97}

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


--sq:setFillColor(cardBlue.r, cardBlue.g, cardBlue.b)

--[[
  move sq offset by whatever amount
--]]
local function onObjectTouch(event)
  if event.phase == "moved" then
    local xoff = event.x - event.xStart
    sq.x = sq.x + xoff
  end
end

sq:addEventListener("touch", onObjectTouch)

local oldIndex = getmetatable(sq).__index

local function setFillColorC(self, colorTable)
  self:setFillColor(colorTable.r, colorTable.g, colorTable.b)
end

local function myIndex(self, key)
  print("in myindex with key" .. key)
  if key == "setFillColorC" then
    print("got it")
    return setFillColorC
  else
    return oldIndex(self, key)
  end
end

local temp = getmetatable(sq)
temp.__index = myIndex
setmetatable(sq, temp)

print("something")
print(myIndex)

sq:setFillColorC(cardBlue)

--sq:setFillColor(0.5,0.5,0.5)


local function printAll(t)
  local m = getmetatable(t)
  for k,v in pairs(m) do
    print(k)
    print(v)
  end
end

printAll(sq)


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
