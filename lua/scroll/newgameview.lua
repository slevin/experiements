--
-- User: slevin
-- Date: 11/22/14
-- Time: 8:53 PM
--

local composer = require( "composer" )
local newgame = require("newgame")
local fixtures = require("test.fixtures")
local widget = require("widget")

local scene = composer.newScene()

-- -----------------------------------------------------------------------------------------------------------------
-- All code outside of the listener functions will only be executed ONCE unless "composer.removeScene()" is called.
-- -----------------------------------------------------------------------------------------------------------------

-- local forward references should go here

-- -------------------------------------------------------------------------------


-- "scene:create()"
function scene:create( event )

  local sceneGroup = self.view

  -- background
  local bg = display.newRect(
    sceneGroup,
    display.contentCenterX,
    display.contentCenterY,
    display.contentWidth,
    display.contentHeight
  )
  bg:setFillColor(0.82, 0.84, 1.0)

  -- blue game card
  local cardGroup = display.newGroup()
  local cardWidth = display.contentWidth * 0.85
  local cardHeight = display.contentHeight * 0.55
  local cardBg = display.newRoundedRect(
    cardGroup,
    cardWidth * 0.5,
    cardHeight * 0.5,
    cardWidth,
    cardHeight,
    30
  )
  cardBg:setFillColor(0,0.47,1.0)

  -- back button
  local backButton = widget.newButton{
    left=20,
    top=20,
    onRelease=function() end,
    label="<--",
    labelColor={default={1,1,1}, over={0.5,0.5,0.5}},
    fontSize=40,
    textOnly=true
  }
  cardGroup:insert(backButton)

  -- go button
  local goWidth = 100
  local goHeight = 100
  local rightPad = 20
  local topPad = 20
  local goButton = widget.newButton{
    left=cardWidth - rightPad - goWidth,
    top=topPad,
    width=goWidth,
    height=goHeight,
    label="GO!",
    fontSize=40,
    labelColor={default={1,1,1}, over={0.62,0.83,1}},
    shape="roundedRect",
    fillColor={default={0,0.84,0}, over={0,0.73,0.35}},
    cornerRadius=20
  }
  cardGroup:insert(goButton)

  -- char count
  local countWidth = 200
  local countHeight = 100
  local countGap = 30
  local countLabel = display.newText{
    parent=cardGroup,
    text="140",
    x=cardWidth - rightPad - goWidth - countGap - (countWidth * 0.5),
    y=topPad + (countHeight * 0.5),
    width=countWidth,
    height=countHeight,
    fontSize=40,
    align="right"
  }

  -- challenge field
  




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

