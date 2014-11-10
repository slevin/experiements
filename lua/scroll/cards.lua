--
-- Created by IntelliJ IDEA.
-- User: slevin
-- Date: 11/9/14
-- Time: 10:40 AM
-- To change this template use File | Settings | File Templates.
--

local cards = {}

cards.newCard = function(red, green, blue)
    return {red=red, green=green, blue=blue}
end

cards.newCardStack = function(containerBounds, edgePadding, newCardFunction)
    local stack = {
        newCardFunction=newCardFunction,
        containerBounds=containerBounds,
            edgePadding=edgePadding
    }

    stack.addCard = function(self, card)
        local cframe = {
                 x=self.containerBounds.width  * 0.5,
                 y=self.containerBounds.height * 0.5,
             width=self.containerBounds.width  - (self.edgePadding * 2),
            height=self.containerBounds.height - (self.edgePadding * 2)
        }
        self.newCardFunction(cframe, {red=card.red, green=card.green, blue=card.blue})
    end

    return stack
end


return cards
