local bottomHeight = 100
local restHeight = display.contentHeight - bottomHeight
local restCenterY = restHeight * 0.5

local padding = 50

local cardBlue = {r=0;g=0.65;b=0.97}

-- background
local bg = display.newRect(display.contentCenterX,
                           display.contentCenterY,
                           display.contentWidth,
                           display.contentHeight)

bg:setFillColor(1)

local indexOffset = 0
local nextStop = display.contentWidth - (padding * 2)

local sq = display.newRoundedRect(display.contentCenterX,
                                  restCenterY,
                                  display.contentWidth - (padding * 2),
                                  restHeight - (padding * 2), 20)

indexOffset = -1
local sq2 = display.newRoundedRect(display.contentCenterX +
                                     ((nextStop + (padding * 0.5)) * indexOffset),
                                   restCenterY,
                                   display.contentWidth - (padding * 2),
                                   restHeight - (padding * 2),
                                   20)

local startOffsetX = 0
local startX = 0

local function onObjectTouch(event)
  if event.phase == "began" then.
    startX = event.target.x
    startOffsetX = startX - event.x
  elseif event.phase == "moved" then
    sq.x = event.x + startOffsetX
  elseif event.phase == "ended" or event.phase == "cancelled" then
    print("stop phase " .. event.phase)
    if sq.x < (startX - (nextStop * 0.5)) then
      transition.to(sq, { x=(startX - nextStop), transition=easing.outExpo })
    elseif sq.x > (startX + (nextStop * 0.5)) then
      transition.to(sq, { x=(startX + nextStop), transition=easing.outExpo })
    else
      transition.to(sq, { x=startX, transition=easing.outExpo })
    end
  else
    print("whats this " .. event.phase)
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


sq:setFillColor(0.5,0.5,0.5)
sq2:setFillColor(cardBlue.r, cardBlue.g, cardBlue.b)
