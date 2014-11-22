--
-- User: slevin
-- Date: 11/21/14
-- Time: 9:08 PM
--


local fixtures = {}
-- figtures builder functions, create game, add player, set winner, etc
-- should probably biuld new ones in case I edit them

fixtures.playerSean = {
  name="Sean",
  age="40",
  location="Williamsburg",
  avatarUrl="http://www.seanlevin.com/original.jpg"
}

fixtures.playerMara = {
  name="Mara",
  age="35",
  location="Texas",
  avatarUrl="http://www.seanlevin.com/marafinger.jpg"
}

fixtures.playerBowie = {
  name="David Bowie",
  age="60",
  location="New York",
  avatarUrl="http://www.seanlevin.com/davidbowie.png"
}

fixtures.game1 = {
  owner=fixtures.playerSean,
  challenge="Who's the greatest singer?",
  prize="A private serenade by me",
  answers={
    {
      owner=fixtures.playerMara,
      answer="Taylor Swift, duh"
    },

    {
      owner=fixtures.playerBowie,
      answer="Me, obviously"
    },
  }
}


return fixtures
