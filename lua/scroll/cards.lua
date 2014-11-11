--
-- User: slevin
-- Date: 11/9/14
-- Time: 10:40 AM
--

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
        updateXFunction=updateXFunction
    }

    local cardCount = 0

    local cardWidth  = stack.containerSize.width  - (stack.edgePadding * 2)
    local cardHeight = stack.containerSize.height - (stack.edgePadding * 2)

    local function cardOffset()
        return cardCount * (cardWidth + stack.cardGap)
    end

    stack.addCard = function(self, card)
        local cframe = {
                 x=self.containerSize.width  * 0.5 + cardOffset(),
                 y=self.containerSize.height * 0.5,
             width=cardWidth,
            height=cardHeight
        }
        self.newCardFunction(cframe, {red=card.red, green=card.green, blue=card.blue})
        cardCount = cardCount + 1
    end

    stack.dragHandler = function(self, event)
        local offset = event.x - event.xStart
        local x = self.containerSize.width * 0.5 + offset
        self.updateXFunction(x, event.target)
    end

    return stack
end


return cards
