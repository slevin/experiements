--
-- User: slevin
-- Date: 11/9/14
-- Time: 10:40 AM
--

local cards = {}

cards.newCard = function(red, green, blue)
    return {red=red, green=green, blue=blue}
end

cards.newCardStack = function(containerSize, edgePadding, newCardFunction, updateXFunction)
    local stack = {
        newCardFunction=newCardFunction,
          containerSize=containerSize,
            edgePadding=edgePadding,
        updateXFunction=updateXFunction
    }

    stack.addCard = function(self, card)
        local cframe = {
                 x=self.containerSize.width  * 0.5,
                 y=self.containerSize.height * 0.5,
             width=self.containerSize.width  - (self.edgePadding * 2),
            height=self.containerSize.height - (self.edgePadding * 2)
        }
        self.newCardFunction(cframe, {red=card.red, green=card.green, blue=card.blue})
    end

    stack.dragHandler = function(self, event)
        local offset = event.x - event.xStart
        local x = self.containerSize.width * 0.5 + offset
        self.updateXFunction(x, event.target)
    end

    return stack
end


return cards
