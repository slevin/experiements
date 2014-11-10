--
-- User: slevin
-- Date: 11/9/14
-- Time: 10:40 AM
--

local cards = require('cards')

describe("card model", function()

    it("creates with colors", function()
        local card = cards.newCard(0.1, 0.2, 0.3)
        assert.are.equal(card.red,   0.1)
        assert.are.equal(card.green, 0.2)
        assert.are.equal(card.blue,  0.3)
    end)

end)

describe("newCardFunction", function()
    local myFrame, myColor
    local stack, card

    local function newCardFunction(cframe, color)
        myFrame = cframe
        myColor = color
    end

    before_each(function()
        myFrame = nil
        myColor = nil
        local containerBounds = {width=100, height=180 }
        local padding = 10
        stack = cards.newCardStack(containerBounds, padding, newCardFunction)
        card = cards.newCard(0.1, 0.2, 0.3)
        stack:addCard(card)
    end)

    it("calls with color of card", function()
        assert.are.equal(myColor.red,   0.1)
        assert.are.equal(myColor.green, 0.2)
        assert.are.equal(myColor.blue,  0.3)
    end)

    it("calls with card in center of given area", function()
        assert.are.equal( 50, myFrame.x)
        assert.are.equal( 90, myFrame.y)
        assert.are.equal( 80, myFrame.width)
        assert.are.equal(160, myFrame.height)
    end)
end)


