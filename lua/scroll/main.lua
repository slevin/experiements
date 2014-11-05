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

local function cardAtOffset(indexOffset)
  return display.newRoundedRect(display.contentCenterX +
                                  ((nextStop + (padding * 0.5)) * indexOffset),
                                restCenterY,
                                display.contentWidth - (padding * 2),
                                restHeight - (padding * 2),
                                20)
end




local startOffsetX = 0
local startX = 0

local function onObjectTouch(event)
  if event.phase == "began" then
    startX = event.target.x
    startOffsetX = startX - event.x
  elseif event.phase == "moved" then
    event.target.x = event.x + startOffsetX
  elseif event.phase == "ended" or event.phase == "cancelled" then
    print("stop phase " .. event.phase)
    if event.target.x < (startX - (nextStop * 0.5)) then
      transition.to(event.target, { x=(startX - nextStop), transition=easing.outExpo })
    elseif event.target.x > (startX + (nextStop * 0.5)) then
      transition.to(event.target, { x=(startX + nextStop), transition=easing.outExpo })
    else
      transition.to(event.target, { x=startX, transition=easing.outExpo })
    end
  else
    print("whats this " .. event.phase)
  end
end


local function printAll(t)
  local m = getmetatable(t)
  for k,v in pairs(t) do
    print(k)
    print(v)
  end
end

local cards = {
  {r=0;g=0.65;b=0.97},
  {r=0.8;g=0.3;b=0.1},
  {r=0.5;g=0.5;b=0.5}
}

local function renderCards(cards)
  for i,c in ipairs(cards) do
    print(c)
    newCard = cardAtOffset(i - 2)
    newCard:setFillColor(c.r, c.g, c.b)
    newCard:addEventListener("touch", onObjectTouch)
  end
end

renderCards(cards)
