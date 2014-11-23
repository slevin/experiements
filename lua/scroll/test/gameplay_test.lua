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
    local challengeRowDisplay
    local challengeInsertId

    local function insertChallengeRowFun(id)
      challengeInsertId = id
    end

    local function renderChallengeRowFun(row)
      challengeRowDisplay = row
    end

    before_each(function()
      challengeInsertId = nil
      challengeRowDisplay = nil
      gp.challengeRowInsertFunction = insertChallengeRowFun
      gp.challengeRowRenderFunction = renderChallengeRowFun
      gp:renderGameplayView()
    end)

    it("adds a challenge row for game", function()
      assert.are.same(gameplay.challengeRowId, challengeInsertId)
    end)

    it("renders challenge row when requested", function()
      local row = {test="test"}
      gp:renderRow(row, gameplay.challengeRowId)
      assert.are.same(row, challengeRowDisplay)
    end)

  end)

  describe("answer rows", function()
    local answerRows
    local answerRowsDisplay

    local function insertAnswerRowFun(id)
      table.insert(answerRows, id)
    end

    local function renderAnswerRowFun(row)
      table.insert(answerRowsDisplay, row)
    end

    before_each(function()
      answerRows = {}
      answerRowsDisplay = {}
      gp.answerRowInsertFunction = insertAnswerRowFun
      gp.answerRowRenderFunction = renderAnswerRowFun
      gp:renderGameplayView()
    end)

    it("calls insert for each answer in game", function()
      assert.are.same(gameplay.answerRowId(1), answerRows[1])
      assert.are.same(gameplay.answerRowId(2), answerRows[2])
    end)

    it("calls correct row render function when asked", function()
      local row1 = { test="test1" }
      local row2 = { test="test2" }
      gp:renderRow(row1, gameplay.answerRowId(1))
      gp:renderRow(row2, gameplay.answerRowId(2))
      assert.are.same(row1, answerRowsDisplay[1])
      assert.are.same(row2, answerRowsDisplay[2])
    end)
  end)

end)