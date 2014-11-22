local gameplay = require('gameplay')
local fixtures = require('test/fixtures')

describe("gameplay", function()
  local game
  local gp

  before_each(function()
    game = fixtures.game1
    gp = gameplay.newGameplay(game)
  end)

  it("creates new with game", function()
    assert.is_not.Nil(gp)
  end)

  describe("challenge row", function()
    local challengeRow
    local challengeRowDisplay
    local called

    local function insertChallengeRowFun()
      challengeRow = {}
    end

    local function renderChallengeRowFun(row)
      challengeRowDisplay = row
    end

    before_each(function()
      called = false
      challengeRow = nil
      challengeRowDisplay = nil
      gp.challengeRowInsertFunction = insertChallengeRowFun
      gp.challengeRowRenderFunction = renderChallengeRowFun
      gp:renderGameplayView()
    end)

    it("adds a challenge row for game", function()
      assert.is_not.Nil(challengeRow)
    end)

    it("renders challenge row when requested", function()
      gp:renderRow(challengeRow)
      assert.is_not.Nil(challengeRowDisplay)
    end)

  end)



  -- divider is part of the challenge row

  -- it adds an answer row for each item

  -- different kind of row for different parts

  -- ask game to render triggers insert row

  -- onRender requests game to give it the row
  -- gameplay asks to create row and returns it to render
  --  could technically do it itself, depends on if there is any logic
  --  view can be only wiring up and configuration

  -- can I capture some snapshots of different views for easy visual testing
  -- that would really help with the integration aspects


end)