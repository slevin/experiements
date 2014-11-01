--[[

swiping card moves it left and right



list of cards means more of them

flexible pixels (need to do scale)


--]]

local bottomHeight = 100
local restHeight = display.contentHeight - bottomHeight
local restCenterY = restHeight * 0.5

local padding = 50

local cardBlue = {r=0; g=0.65; b=0.97}

-- background
local bg = display.newRect(display.contentCenterX,
                           display.contentCenterY,
                           display.contentWidth,
                           display.contentHeight)

bg:setFillColor(1)

local sq = display.newRoundedRect(display.contentCenterX,
                                  restCenterY,
                                  display.contentWidth - (padding * 2),
                                  restHeight - (padding * 2), 20)


sq:setFillColor(cardBlue.r, cardBlue.g, cardBlue.b)




--[[
-- ScrollView listener
local function scrollListener( event )

    local phase = event.phase
    if ( phase == "began" ) then print( "Scroll view was touched" )
    elseif ( phase == "moved" ) then print( "Scroll view was moved" )
    elseif ( phase == "ended" ) then print( "Scroll view was released" )
    end

    -- In the event a scroll limit is reached...
    if ( event.limitReached ) then
        if ( event.direction == "up" ) then print( "Reached top limit" )
        elseif ( event.direction == "down" ) then print( "Reached bottom limit" )
        elseif ( event.direction == "left" ) then print( "Reached left limit" )
        elseif ( event.direction == "right" ) then print( "Reached right limit" )
        end
    end

    return true
end

-- Create the widget
local scrollView = widget.newScrollView
{
    top = 100,
    left = 10,
    width = 300,
    height = 400,
    scrollWidth = 600,
    scrollHeight = 800,
    listener = scrollListener
}

-- Create a image and insert it into the scroll view
local background = display.newImageRect( "ziggy.jpg", 768, 1024 )
scrollView:insert( background )
--]]
