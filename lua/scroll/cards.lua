--
-- User: slevin
-- Date: 11/9/14
-- Time: 10:40 AM
--

local _ = require('moses')

local cards = {}

cards.newCard = function(red, green, blue)
    return {red=red, green=green, blue=blue}
end

cards.newCardStack = function(config, newCardFunction, updateXFunction)
    local stack = {
          containerSize=config.containerSize,
            edgePadding=config.edgePadding,
                cardGap=config.cardGap,
        newCardFunction=newCardFunction,
        updateXFunction=updateXFunction,
           displayCards={}
    }

    local cardCount = 1

    local cardWidth  = stack.containerSize.width  - (stack.edgePadding * 2)
    local cardHeight = stack.containerSize.height - (stack.edgePadding * 2)

    local function cardOffset(cardIndex)
        return (cardIndex - 1) * (cardWidth + stack.cardGap)
    end

    stack.addCard = function(self, card)
        local cframe = {
                 x=self.containerSize.width  * 0.5 + cardOffset(cardCount),
                 y=self.containerSize.height * 0.5,
             width=cardWidth,
            height=cardHeight
        }
        local newDisplayCard = self.newCardFunction(cframe, {red=card.red, green=card.green, blue=card.blue})
        table.insert(self.displayCards, newDisplayCard)
        cardCount = cardCount + 1
    end

    stack.dragHandler = function(self, event)
        local offset = event.x - event.xStart
        if event.phase == "ended" then
            self.completeMoveFunction()
        else
            _.each(self.displayCards, function(k, v)
                local x = self.containerSize.width * 0.5 + cardOffset(k) + offset
                self.updateXFunction(x, v)
            end)
        end
    end

    return stack
end


return cards
