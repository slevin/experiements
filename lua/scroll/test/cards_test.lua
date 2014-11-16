--
-- User: slevin
-- Date: 11/9/14
-- Time: 10:40 AM
--

local cards = require('cards')
local _ = require('moses')

describe("card model", function()

    it("creates with colors", function()
        local card = cards.newCard(0.1, 0.2, 0.3)
        assert.are.equal(card.red, 0.1)
        assert.are.equal(card.green, 0.2)
        assert.are.equal(card.blue, 0.3)
      end)
  end)

describe("cardStack", function()
    local createdCards
    local stack, card
    local lastX
    local displayCardsToAdd

    local function newCardFunction(cframe, color)
      _.push(createdCards, {frame=cframe, color=color})
      return _.pop(displayCardsToAdd)
    end

    local function updateXFunction(x, updateTargetParam)
      lastX[updateTargetParam] = x
    end

    before_each(function()
        createdCards = {}
        lastX = {}
        displayCardsToAdd = { 1, 2, 3 }
        local config = {
          containerSize = { width = 100, height = 180 },
          edgePadding = 10,
          cardGap = 5
        }
        stack = cards.newCardStack(config, newCardFunction, updateXFunction)
        card = cards.newCard(0.1, 0.2, 0.3)
        stack:addCard(card)
      end)

    it("calls with color of card", function()
        assert.are.equal(createdCards[1].color.red, 0.1)
        assert.are.equal(createdCards[1].color.green, 0.2)
        assert.are.equal(createdCards[1].color.blue, 0.3)
      end)

    it("calls with card in center of given area", function()
        assert.are.equal(50, createdCards[1].frame.x)
        assert.are.equal(90, createdCards[1].frame.y)
        assert.are.equal(80, createdCards[1].frame.width)
        assert.are.equal(160, createdCards[1].frame.height)
      end)

    it("keeps track of display cards returned", function()
        local numberOfCards = #(stack.displayCards)
        assert.are.equal(1, numberOfCards)
      end)

    it("moves card with drag", function()
        stack:dragHandler({ xStart = 75, x = 85 })
        -- there's only one, so each is an easy way to access it
        _.each(lastX, function(_, v)
            assert.are.equal(60, v)
          end)
      end)

    describe("with multiple cards", function()
        local card2
        local card3

        before_each(function()
            card2 = cards.newCard(0.3, 0.2, 0.1)
            card3 = cards.newCard(0.4, 0.5, 0.6)
            stack:addCard(card2)
            stack:addCard(card3)
          end)

        it("creates second card to right of first", function()
            assert.are.equal(135, createdCards[2].frame.x)
          end)

        it("creates third card to right of second", function()
            assert.are.equal(220, createdCards[3].frame.x)
          end)


        it("keeps track of display cards returned", function()
            local numberOfCards = #(stack.displayCards)
            assert.are.equal(3, numberOfCards)
          end)

        it("moves all cards with drag", function()
            stack:dragHandler({ xStart = 75, x = 85 })
            assert.are.equal(3, #lastX)
            assert.are.equal(60, lastX[1])
            assert.are.equal(145, lastX[2])
            assert.are.equal(230, lastX[3])
          end)

        describe("paging", function()
            local completeMoveCalled
            local updateTo
            local updaterObject
            local function completeMoveFunction(updaterObjectParam, updateToParam)
              completeMoveCalled = true
              updateTo = updateToParam
              updaterObject = updaterObjectParam
            end

            before_each(function()
                completeMoveCalled = false
                updateTo = nil
                updaterObject = nil
                stack.completeMoveFunction = completeMoveFunction
              end)

            it("calls complete move function when done dragging", function()
                stack:dragHandler({ xStart = 75, x = 85, phase = "ended" })
                assert.is.truthy(completeMoveCalled)
              end)

            describe("not past threshold", function()
                it("hands back updater objects for returning to start", function()
                    stack:dragHandler({ xStart = 75, x = 65, phase = "ended" })
                    assert.are.equal(-10, updaterObject.x)
                    assert.are.equal(0, updateTo)
                  end)

                it("updater objects perform movement on display objects", function()
                    stack:dragHandler({ xStart = 75, x = 65, phase = "ended" })
                    updaterObject.x = updateTo
                    assert.are.equal(3, #lastX)
                    assert.are.equal(50, lastX[1])
                    assert.are.equal(135, lastX[2])
                    assert.are.equal(220, lastX[3])
                  end)
              end)

            describe("past threshold", function()
                it("moves to next going forward", function()
                    stack:dragHandler({ xStart = 75, x = 24 , phase = "ended" })
                    updaterObject.x = updateTo
                    assert.are.equal(3, #lastX)
                    assert.are.equal(-35, lastX[1])
                    assert.are.equal(50, lastX[2])
                    assert.are.equal(135, lastX[3])                  
                  end)

                it("moves next and back to original position", function()
                    stack:dragHandler({xStart =75, x=24, phase="ended"})
                    updaterObject.x = updateTo
                    updaterObject.completeFunction()
                    stack:dragHandler({xStart=25, x=76, phase="ended"})
                    updaterObject.x = updateTo
                    updaterObject.completeFunction()
                    assert.are.equal(3, #lastX)
                    assert.are.equal(50, lastX[1])
                    assert.are.equal(135, lastX[2])
                    assert.are.equal(220, lastX[3])                  
                  end)

              end)

            -- can do next thing sdflksjdf sdf
            -- test swiping back as well
            --            it("can't page off beginning", function()
            --            end)
            --
            --            it("cant page off end", function()
            --            end)
          end)
      end)
  end)

describe("updater", function()
    it("has initial value and update method", function()
        local updater = cards.newUpdater(10, nil)
        assert.are.equal(10, updater.x)
      end)

    it("calls update function on value change", function()
        local testVal
        local function myUpdate(newValue)
          testVal = newValue
        end

        local updater = cards.newUpdater(10, myUpdate)
        updater.x = 20
        assert.are.equal(20, testVal)
      end)
  end)


