
local tablex = require('pl.tablex')

local bottomHeight = 100
local restHeight = display.contentHeight - bottomHeight
local restCenterY = restHeight * 0.5
local padding = 50
local cornerRadius = 20
local cardSpacing = 50 * 0.5

-- background
--local bg = display.newRect(display.contentCenterX,
--                           display.contentCenterY,
--                           display.contentWidth,
--                           display.contentHeight)
--
--bg:setFillColor(1)

local nextStop = display.contentWidth - (padding * 2)
local distanceToNext = nextStop + cardSpacing


local function cardXAtOffset(indexOffset, moveOffset)
  return display.contentCenterX +
      (distanceToNext * indexOffset) + moveOffset
end

local function cardAtOffset(indexOffset)
  return display.newRoundedRect(cardXAtOffset(indexOffset, 0),
                                restCenterY,
                                display.contentWidth - (padding * 2),
                                restHeight - (padding * 2),
                                cornerRadius)
end

local function printAll(t)
  for k,v in pairs(t) do
    print(k)
    print(v)
  end
end

--local cardStack = {}
--cardStack.addCard = function(self, red, green, blue)
--  table.insert(cardStack, {r=red; g=green; b=blue})
--end
--
--cardStack:addCard(  0, 0.65, 0.97)
--cardStack:addCard(0.8, 0.3,  0.1)
--cardStack:addCard(0.5, 0.5,  0.5)

local cardPointer = 2



local function dragSignal(displayObject)
  local signal = {}
  local function updateFunction(event)


  end

  displayObject:addEventListener("touch", updateFunction)
end



local function cardTouchEventFactory(cardStack)
  local function onObjectTouch(event)
    local amountMovedRight = event.x - event.xStart
    if event.phase == "began" then

    elseif event.phase == "moved" then
      cardStack:positionEachXBy(amountMovedRight)
    elseif event.phase == "ended" or event.phase == "cancelled" then
      local halfwayAcross = display.contentWidth * 0.5
      if amountMovedRight > halfwayAcross then
        cardStack:transitionToEach(distanceToNext, {})
        cardPointer = cardPointer - 1
      elseif amountMovedRight < halfwayAcross * -1 then
        cardStack:transitionToEach(distanceToNext * -1, {})
        cardPointer = cardPointer + 1
      else
        cardStack:transitionToEach(0, {})
      end
    end
  end

  return onObjectTouch
end


local uiCards = {}
uiCards.positionEachXBy = function(self, moveOffset)
  print(cardPointer)
  for i,c in ipairs(self) do
    local indexOffset = i - cardPointer
    c.x = cardXAtOffset(indexOffset, moveOffset)
  end
end

uiCards.transitionToEach = function(self, moveOffset, params)
  for i,c in ipairs(self) do
    local indexOffset = i - 2
    local transitionParams = tablex.merge({x=cardXAtOffset(indexOffset, moveOffset), transition=easing.outExpo}, params, true)
    transition.to(c, transitionParams)
  end
end


local function renderCards(cards)
  for i,c in ipairs(cards) do
    local indexOffset = i - cardPointer
    local newCard = cardAtOffset(indexOffset)
    table.insert(uiCards, newCard)
    newCard:setFillColor(c.r, c.g, c.b)
    newCard:addEventListener("touch", cardTouchEventFactory(uiCards))
  end
end

--renderCards(cardStack)

local cards = require('cards')
local cardStack

local function newCardFunction(cframe, color)
  local rect = display.newRoundedRect(cframe.x, cframe.y, cframe.width, cframe.height, cornerRadius)
  rect:setFillColor(color.red, color.green, color.blue)
  rect:addEventListener("touch", function(event) cardStack:dragHandler(event) end)
  -- SMELL: should probably pass cardStack in as param to newCardFunction (like a delegate call in objc)
  return rect
end

local function updateXFunction(x, updateTarget)
  updateTarget.x = x
end

local config = {
  containerSize={width=display.contentWidth, height=display.contentHeight },
    edgePadding=50,
        cardGap=25
}

cardStack = cards.newCardStack(config, newCardFunction, updateXFunction)

cardStack.completeMoveFunction = function(updaterObject, updateTo)
  transition.to(updaterObject, {x=updateTo, transition=easing.outExpo})
end

--cardStack:addCard(0.8, 0.3,  0.1)
--cardStack:addCard(0.5, 0.5,  0.5)
local card1 = cards.newCard(0, 0.65, 0.97)
cardStack:addCard(card1)
local card2 = cards.newCard(0.8, 0.3, 0.1)
cardStack:addCard(card2)
