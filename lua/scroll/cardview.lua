local composer = require( "composer" )
local cards = require('cards')

local scene = composer.newScene()


local bottomHeight = 100
local restHeight = display.contentHeight - bottomHeight
local restCenterY = restHeight * 0.5
local padding = 50
local cornerRadius = 20
local cardSpacing = 50 * 0.5

local sceneGroup
local cardStack

local function newCardFunction(cframe, color)
  local rect = display.newRoundedRect(cframe.x, cframe.y, cframe.width, cframe.height, cornerRadius)
  rect:setFillColor(color.red, color.green, color.blue)
  rect:addEventListener("touch", function(event) cardStack:dragHandler(event) end)
  sceneGroup:insert(rect)
  -- SMELL: should probably pass cardStack in as param to newCardFunction (like a delegate call in objc)
  return rect
end

local function updateXFunction(x, updateTarget)
  updateTarget.x = x
end

local config = {
  containerSize={width=display.contentWidth, height=display.contentHeight },
  edgePadding=50,
  cardGap=25
}


local completeMoveFunction = function(updaterObject, updateTo)
  transition.to(updaterObject, {x=updateTo, transition=easing.outExpo, onComplete=updaterObject.completeFunction})
end


-- "scene:create()"
function scene:create( event )

  sceneGroup = self.view

  cardStack = cards.newCardStack(config, newCardFunction, updateXFunction)
  cardStack.completeMoveFunction = completeMoveFunction
  
  local card1 = cards.newCard(0, 0.65, 0.97)
  cardStack:addCard(card1)
  local card2 = cards.newCard(0.8, 0.3, 0.1)
  cardStack:addCard(card2)
  local card3 = cards.newCard(0.5, 0.5, 0.5)
  cardStack:addCard(card3)

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