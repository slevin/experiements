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

    local cardCount = 1

    local cardWidth  = stack.containerSize.width  - (stack.edgePadding * 2)
    local cardHeight = stack.containerSize.height - (stack.edgePadding * 2)
    local cardIndexOffset = 0
    
    local function cardOffset(cardIndex)
        return (cardIndex - 1 + cardIndexOffset) * (cardWidth + stack.cardGap)
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

    stack.positionAllCardsByOffset = function(self, offset)
        _.each(self.displayCards, function(k, v)
            local x = self.containerSize.width * 0.5 + cardOffset(k) + offset
            self.updateXFunction(x, v)
        end)
    end

    stack.dragHandler = function(self, event)
        local offset = event.x - event.xStart
        if event.phase == "ended" then
            local moveTo = 0
            local cardIndexOffsetModifier = 0
            if offset < cardWidth * -0.5 then
                print("move back")
                moveTo = -1 * cardOffset(2)
                cardIndexOffsetModifier = -1
            elseif offset > cardWidth * 0.5 then
                print("move forward")
                moveTo = cardOffset(2)
                cardIndexOffsetModifier = 1
            end

            self.completeMoveFunction(cards.newUpdater(offset, 
                function(newOffset) self:positionAllCardsByOffset(newOffset) end,
                function() cardIndexOffset = cardIndexOffset + cardIndexOffsetModifier end), moveTo)
        else
            self:positionAllCardsByOffset(offset)
        end
    end

    return stack
end


return cards
