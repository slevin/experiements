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
    local stack, card1
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
        card1 = cards.newCard(0.1, 0.2, 0.3)
      end)

    it("initial card index is nil", function()
        assert.are.equal(stack.cardIndex, nil)
      end)

    it("after adding cardIndex is 1", function()
        stack:addCard(card1)
        assert.are.equal(1, stack.cardIndex)
      end)

    it("calls with color of card", function()
        stack:addCard(card1)
        assert.are.equal(createdCards[1].color.red, 0.1)
        assert.are.equal(createdCards[1].color.green, 0.2)
        assert.are.equal(createdCards[1].color.blue, 0.3)
      end)

    it("calls with card in center of given area", function()
        stack:addCard(card1)
        assert.are.equal(50, createdCards[1].frame.x)
        assert.are.equal(90, createdCards[1].frame.y)
        assert.are.equal(80, createdCards[1].frame.width)
        assert.are.equal(160, createdCards[1].frame.height)
      end)

    it("keeps track of display cards returned", function()
        stack:addCard(card1)
        local numberOfCards = #(stack.displayCards)
        assert.are.equal(1, numberOfCards)
      end)

    describe("with multiple cards", function()
        local card2
        local card3

        before_each(function()
            card2 = cards.newCard(0.3, 0.2, 0.1)
            card3 = cards.newCard(0.4, 0.5, 0.6)
            stack:addCard(card1)
            stack:addCard(card2)
            stack:addCard(card3)
          end)

        it("after adding multiple cards, cardIndex is still 1", function()
            assert.are.equal(1, stack.cardIndex)
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
            stack:dragHandler({ xStart = 75, x = 65 })
            assert.are.equal(3, #lastX)
            assert.are.equal(40, lastX[1])
            assert.are.equal(125, lastX[2])
            assert.are.equal(210, lastX[3])
          end)

        it("decays drag if past the beginning", function()
            stack:dragHandler({ xStart = 75, x = 85 })
            assert.are.equal(3, #lastX)
            assert.is.True(60 > lastX[1])
            assert.is.True(145 > lastX[2])
            assert.is.True(230 > lastX[3])
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
                before_each(function()
                    stack:dragHandler({ xStart = 75, x = 65, phase = "ended" })                    
                  end)

                it("hands back updater objects for returning to start", function()
                    assert.are.equal(-10, updaterObject.x)
                    assert.are.equal(0, updateTo)
                  end)

                it("updater objects perform movement on display objects", function()
                    updaterObject.x = updateTo
                    assert.are.equal(3, #lastX)
                    assert.are.equal(50, lastX[1])
                    assert.are.equal(135, lastX[2])
                    assert.are.equal(220, lastX[3])
                  end)

                it("cardIndex stays the same", function()
                    assert.are.equal(1, stack.cardIndex)
                  end)

              end)

            describe("past threshold", function()
                local function swipeToPageNext(forward)
                  local start = 75
                  local final
                  if (forward) then
                    final = start - 51
                  else
                    final = start + 51
                  end

                  stack:dragHandler({ xStart = start, x = final , phase = "ended" })
                  updaterObject.x = updateTo
                  updaterObject.completeFunction()                  
                end                  

                it("moves to next going forward", function()
                    swipeToPageNext(true)
                    assert.are.equal(-85, updateTo)
                    assert.are.equal(3, #lastX)
                    assert.are.equal(-35, lastX[1])
                    assert.are.equal(50, lastX[2])
                    assert.are.equal(135, lastX[3])                  
                    assert.are.equal(2, stack.cardIndex)
                  end)

                it("after completing move, cards drag from that spot", function()
                    swipeToPageNext(true)
                    stack:dragHandler({xStart = 75, x = 65})
                    assert.are.equal(-45, lastX[1])
                  end)

                it("moves next and next again", function()
                    swipeToPageNext(true)
                    swipeToPageNext(true)
                    assert.are.equal(-85, updateTo)
                    assert.are.equal(-120, lastX[1])
                    assert.are.equal(3, stack.cardIndex)
                  end)

                it("moves next and back to original position", function()
                    swipeToPageNext(true)
                    swipeToPageNext(false)
                    assert.are.equal(3, #lastX)
                    assert.are.equal(50, lastX[1])
                    assert.are.equal(135, lastX[2])
                    assert.are.equal(220, lastX[3])                  
                    assert.are.equal(1, stack.cardIndex)
                  end)

                it("goes back to beginning if at beginning", function()
                    swipeToPageNext(false)
                    assert.are.equal(50, lastX[1])
                    assert.are.equal(135, lastX[2])
                    assert.are.equal(220, lastX[3])                  
                    assert.are.equal(1, stack.cardIndex)                    
                  end)

                it("goes back to end if at the end", function()
                    swipeToPageNext(true)
                    swipeToPageNext(true)
                    swipeToPageNext(true)
                    assert.are.equal(-120, lastX[1])
                    assert.are.equal(3, stack.cardIndex)
                  end)

                it("decays drag if past the end", function()
                    swipeToPageNext(true)
                    swipeToPageNext(true)                    
                    stack:dragHandler({ xStart = 75, x = 65 })
                    assert.is.True(-130 < lastX[1])
                    assert.is.True(-45 < lastX[2])
                    assert.is.True(40 < lastX[3])
                  end)

              end)

            -- seemed to mess up once I got it off the end
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


