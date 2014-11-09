--
-- Created by IntelliJ IDEA.
-- User: slevin
-- Date: 11/9/14
-- Time: 10:40 AM
-- To change this template use File | Settings | File Templates.
--

local cards = require('cards')

it("add card should call initialize function", function()
    local called = false
    local function newCardFunction() called = true end
    local stack = cards.newCardStack(newCardFunction)
    local card = cards.newCard()
    stack:addCard(card)
    assert.is_true(called)
end)


