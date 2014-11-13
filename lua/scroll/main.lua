
local bottomHeight = 100
local restHeight = display.contentHeight - bottomHeight
local restCenterY = restHeight * 0.5
local padding = 50
local cornerRadius = 20
local cardSpacing = 50 * 0.5

local function printAll(t)
  for k,v in pairs(t) do
    print(k)
    print(v)
  end
end

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
