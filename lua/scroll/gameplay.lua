local gameplay = {}

function gameplay.newGameplay(game)
  local gp = {
    game=game
  }

  function gp:renderGameplayView()
    self.challengeRowInsertFunction()
  end

  function gp:renderRow(row)
    self.challengeRowRenderFunction(row)
  end

  return gp
end






return gameplay