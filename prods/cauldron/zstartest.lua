-- from www.github.com/danielpower/zStar

zArray = require('zArray')      -- Library to simplify 2-dimensional arrays
zStar = require('zStar')        -- Pathfinding library

-- Start Point
point1 = {x=2, y=1}

-- End point
point2 = {x=6, y=0}

-- List of all collidable cells
collidables = {
    {x=1, y=0},
    {x=1, y=0},
    {x=2, y=0},
    {x=3, y=0},
    {x=3, y=1},
    {x=3, y=2},
    {x=3, y=3},
}


-- Add collidables to zStar's collision list
for i=1, #collidables do
    zArray.set( zStar.collidables, collidables[i].x, collidables[i].y, true )
end

-- Get the path
path = zStar.getPath( point1.x, point1.y, point2.x, point2.y )
for i=1, #path do
    print( path[i].x, path[i].y )
end

-- Draw cells for testing purposes (LOVE2D Only)
function zStarDraw()
    zStar.draw( 'open',      32,     {  0, 255,   0, 255} )
    zStar.draw( 'closed',    32,     {255,   0,   0, 255} )
    zStar.draw( 'path',      32,     {255, 255, 255, 255} )
end

function render()
  WidgetLib.renderAll()

  trace2()

  zStarDraw()
end
