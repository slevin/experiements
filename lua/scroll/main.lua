local bottomHeight = 100
local restHeight = display.contentHeight - bottomHeight
local restCenterY = restHeight * 0.5
local padding = 50

-- background
local bg = display.newRect(display.contentCenterX,
                           display.contentCenterY,
                           display.contentWidth,
                           display.contentHeight)

bg:setFillColor(1)

local nextStop = display.contentWidth - (padding * 2)

local function cardXAtOffset(indexOffset, moveOffset)
  return display.contentCenterX +
      ((nextStop + (padding * 0.5)) * indexOffset) +
      moveOffset
end

local function cardAtOffset(indexOffset)
  return display.newRoundedRect(cardXAtOffset(indexOffset, 0),
                                restCenterY,
                                display.contentWidth - (padding * 2),
                                restHeight - (padding * 2),
                                20)
end

local function printAll(t)
  for k,v in pairs(t) do
    print(k)
    print(v)
  end
end

local cardStack = {}
cardStack.addCard = function(self, red, green, blue)
  table.insert(cardStack, {r=red; g=green; b=blue})
end

cardStack:addCard(  0, 0.65, 0.97)
cardStack:addCard(0.8, 0.3,  0.1)
cardStack:addCard(0.5, 0.5,  0.5)


local function cardTouchEventFactory(cardStack, card, indexOffset)
  local startX
  local function onObjectTouch(event)
    if event.phase == "began" then
      startX = event.target.x
    elseif event.phase == "moved" then
      cardStack:positionEachXBy(event.x - event.xStart)
    elseif event.phase == "ended" or event.phase == "cancelled" then
      if event.target.x < (startX - (nextStop * 0.5)) then
        cardStack:transitionToEach(nextStop * -1 - (padding * 0.5))
      elseif event.target.x > (startX + (nextStop * 0.5)) then
        cardStack:transitionToEach(nextStop + (padding * 0.5))
      else
        cardStack:transitionToEach(0)
      end
    else
      print("whats this " .. event.phase)
    end
  end

  return onObjectTouch
end


local uiCards = {}
uiCards.positionEachXBy = function(self, moveOffset)
  for i,c in ipairs(self) do
    local indexOffset = i - 2
    c.x = cardXAtOffset(indexOffset, moveOffset)
  end
end

uiCards.transitionToEach = function(self, moveOffset)
  for i,c in ipairs(self) do
    local indexOffset = i - 2
    transition.to(c, { x=cardXAtOffset(indexOffset, moveOffset), transition=easing.outExpo })
  end
end


local function renderCards(cards)
  for i,c in ipairs(cards) do
    local indexOffset = i - 2
    local newCard = cardAtOffset(indexOffset)
    newCard:setFillColor(c.r, c.g, c.b)
    newCard:addEventListener("touch", cardTouchEventFactory(uiCards, card, indexOffset))
  end
end

renderCards(cardStack)
