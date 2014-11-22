local composer = require( "composer" )

local gameplay = require("gameplay")
local widget = require("widget")
local fixtures = require("test.fixtures")
local scene = composer.newScene()

-- -----------------------------------------------------------------------------------------------------------------
-- All code outside of the listener functions will only be executed ONCE unless "composer.removeScene()" is called.
-- -----------------------------------------------------------------------------------------------------------------

-- local forward references should go here

-- -------------------------------------------------------------------------------


-- "scene:create()"
function scene:create( event )

  local sceneGroup = self.view

  local bg = display.newRect(sceneGroup, 
    display.contentCenterX, 
    display.contentCenterY, 
    display.contentWidth,
    display.contentHeight)
  bg:setFillColor(1)

  local backButton = widget.newButton{left=20,top=20,width=200,height=100}
  backButton.labelColor={default={0,0,0},over={0.2,0.2,0.2}}
  backButton.fontSize=100
  backButton:setLabel("Back")
  sceneGroup:insert(backButton)
  
  local gameLabel = display.newText{
    parent=sceneGroup,
    text="Patrick's Game",
    x=display.contentCenterX,
    y=150,
    width=display.contentWidth * 0.8,
    height=200,
    fontSize=40,
    align="center"
  }
  gameLabel:setFillColor({0.0,0.0,1.0})

  local gp

  local function renderFun(event)
    local row = event.row
    gp:renderRow(row)
  end

  local function renderChallengeRow(row)
    print("this is it", row)
    local sq = display.newRect(
      row,
      row.contentWidth * 0.5,
      row.contentHeight * 0.5,
      row.contentWidth - 20,
      row.contentHeight - 20
    )
    sq:setFillColor(0.0,0.7,0.4)
  end

  local gameTable = widget.newTableView{
    parent=sceneGroup,
    x=display.contentCenterX,
    y=display.contentCenterY + 200,
    width=display.contentWidth,
    height=display.contentHeight - 200,
    onRowRender=renderFun
  }

  local game = fixtures.game1

  local function challengeRowInsert()
    gameTable:insertRow{}
  end

  gp = gameplay.newGameplay(game)
  gp.challengeRowInsertFunction = challengeRowInsert
  gp.challengeRowRenderFunction = renderChallengeRow
  gp:renderGameplayView()

end


-- "scene:show()"
function scene:show( event )

  local sceneGroup = self.view
  local phase = event.phase

  if ( phase == "will" ) then
    -- Called when the scene is still off screen (but is about to come on screen).
  elseif ( phase == "did" ) then
    -- Called when the scene is now on screen.
    -- Insert code here to make the scene come alive.
    -- Example: start timers, begin animation, play audio, etc.
  end
end


-- "scene:hide()"
function scene:hide( event )

  local sceneGroup = self.view
  local phase = event.phase

  if ( phase == "will" ) then
    -- Called when the scene is on screen (but is about to go off screen).
    -- Insert code here to "pause" the scene.
    -- Example: stop timers, stop animation, stop audio, etc.
  elseif ( phase == "did" ) then
    -- Called immediately after scene goes off screen.
  end
end


-- "scene:destroy()"
function scene:destroy( event )

  local sceneGroup = self.view

  -- Called prior to the removal of scene's view ("sceneGroup").
  -- Insert code here to clean up the scene.
  -- Example: remove display objects, save state, etc.
end


-- -------------------------------------------------------------------------------

-- Listener setup
scene:addEventListener( "create", scene )
scene:addEventListener( "show", scene )
scene:addEventListener( "hide", scene )
scene:addEventListener( "destroy", scene )

-- -------------------------------------------------------------------------------

return scene