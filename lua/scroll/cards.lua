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

cards.newUpdater = function(initialX, updateFunction, completeFunction)
    -- SMELL: possible pain to set "x" and compare with string since its always the same
    -- could just make the metatable so "calling" the object with the parameter invokes the update function
    -- which means I could have just return a function that gets called, making this irrelevant
    -- but I may need more functionality
    local updater = {
        _x=initialX,
        updateFunction=updateFunction,
        completeFunction=completeFunction
    }
    local m = {
        __index=function(tbl, var) if var == "x" then return tbl._x else return nil end end,
        __newindex=function(tbl, var, val) if var == "x" then tbl._x = val; tbl.updateFunction(val) end end
    }
    setmetatable(updater, m)
    return updater
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

    local cardWidth  = stack.containerSize.width  - (stack.edgePadding * 2)
    local cardHeight = stack.containerSize.height - (stack.edgePadding * 2)
    local cardCenterToCenter = cardWidth + stack.cardGap
    
    stack.cardOffset = function(self, cardNumber)
        return (cardNumber - 1 + (1 - self.cardIndex)) * cardCenterToCenter
    end

    stack.addCard = function(self, card)
        if (self.cardIndex == nil) then
          self.cardIndex = 1
        end
        local cframe = {
                 x=self.containerSize.width  * 0.5 + self:cardOffset(#(self.displayCards) + 1),
                 y=self.containerSize.height * 0.5,
             width=cardWidth,
            height=cardHeight
        }
        local newDisplayCard = self.newCardFunction(cframe, {red=card.red, green=card.green, blue=card.blue})
        table.insert(self.displayCards, newDisplayCard)
    end

    stack.positionAllCardsByOffset = function(self, offset)
        _.each(self.displayCards, function(k, v)
            local x = self.containerSize.width * 0.5 + self:cardOffset(k) + offset
            self.updateXFunction(x, v)
        end)
    end

    stack.dragHandler = function(self, event)
        local offset = event.x - event.xStart
        if event.phase == "ended" then
            local offsetTo = 0
            local cardIndexModifier = 0
              if offset < cardWidth * -0.5 and self.cardIndex ~= #(self.displayCards) then
                offsetTo = -1 * cardCenterToCenter
                cardIndexModifier = 1
              elseif offset > cardWidth * 0.5 and self.cardIndex ~= 1 then
                offsetTo = cardCenterToCenter
                cardIndexModifier = -1
              end

            self.completeMoveFunction(cards.newUpdater(offset, 
                function(newOffset) self:positionAllCardsByOffset(newOffset) end,
                function() self.cardIndex = self.cardIndex + cardIndexModifier end), offsetTo)
        else
            self:positionAllCardsByOffset(offset)
        end
    end

    return stack
end


return cards
