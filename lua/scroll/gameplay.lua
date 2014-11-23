local gameplay = {}

gameplay.challengeRowId = "challenge"
function gameplay.answerRowId(index)
  return {type="answer", index=index}
end

function gameplay.newGameplay(game)
  local gp = {
    game=game
  }

  function gp:renderGameplayView()
    if self.challengeRowInsertFunction then self.challengeRowInsertFunction(gameplay.challengeRowId) end
    for i, v in ipairs(game.answers) do
      if self.answerRowInsertFunction then
        self.answerRowInsertFunction(gameplay.answerRowId(i))
      end
    end

  end

  function gp:renderRow(row, id)
    if id == gameplay.challengeRowId then
      self.challengeRowRenderFunction(row)
    end
    if type(id) == "table" and id.type == "answer" then
      self.answerRowRenderFunction(row)
    end
  end

  return gp
end






return gameplay