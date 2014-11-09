--
-- Created by IntelliJ IDEA.
-- User: slevin
-- Date: 11/9/14
-- Time: 10:40 AM
-- To change this template use File | Settings | File Templates.
--

local cards = {}

cards.newCard = function()
    return {}
end

cards.newCardStack = function(newCardFunction)
    local stack = {newCardFunction=newCardFunction}

    stack.addCard = function(self, card)
        self.newCardFunction()
    end

    return stack
end


return cards
