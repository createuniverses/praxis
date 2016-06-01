-- from www.github.com/danielpower/zStar

assert(_zArray, "zStar requires zArray")

local zStar = {}
zStar.openNodes = {}
zStar.closedNodes = {}
zStar.collidables = {}
zStar.path = {}

function zStar.getPath(startX, startY, finishX, finishY)
    local array = _zArray
    zStar.closedNodes = {}                                                      -- Nodes that have already been checked, and are not on the optimal path
    zStar.openNodes = {}                                                        -- Nodes that are still promising, and will be checked
    zStar.finalNode = nil                                                       -- The last node before reaching our goal
    zStar.start = {
        x = startX,
        y = startY,
    }
    zStar.finish = {
        x = finishX,
        y = finishY,
        fCost = 0,
        gCost = 0,
    }
    array.set(zStar.openNodes, finishX, finishY, zStar.finish)

    -- Pathfinding loop
    while array.length(zStar.openNodes) > 0 do
        -- If we've already found the path, end the look
        if zStar.finalNode ~= nil then
            break
        end
        -- Find the node with the lowest F cost
        local lowestCost = {fCost=1/0}
        for x in pairs(zStar.openNodes) do
            for y in pairs(zStar.openNodes[x]) do
                if zStar.openNodes[x][y].fCost < lowestCost.fCost then
                    lowestCost = zStar.openNodes[x][y]
                end
            end
        end

        -- Close the node with the lowest F cost, and open its surrounding nodes
        array.set(zStar.closedNodes, lowestCost.x, lowestCost.y, lowestCost)
        array.set(zStar.openNodes, lowestCost.x, lowestCost.y, nil)
        zStar._openAdjacent(lowestCost)
    end

    -- Create and return the path
    if zStar.finalNode then
        zStar.path = zStar._createPath(zStar.finalNode)
        return(zStar.path)
    end
end


function drawRectangle(x,y,w,h,hr)
  local s = 0.1
  local x1 = x*s
  local x2 = x1 + w*s
  local y1 = y*s
  local y2 = y1 + h*s
  glBegin(GL_QUADS)
    glVertex(x1,hr,y1)
    glVertex(x1,hr,y2)
    glVertex(x2,hr,y2)
    glVertex(x2,hr,y1)
  glEnd()
end

function zStar.draw(table, cellSize, color)
    -- Mostly for debugging purposes
    --love.graphics.setColor(unpack(color))
    glColor(unpack(color))
    if table == 'open' then
        for x in pairs(zStar.openNodes) do
            for y in pairs(zStar.openNodes[x]) do
                drawRectangle((x-1)*cellSize, (y-1)*cellSize, cellSize, cellSize, 5)
            end
        end
    elseif table == 'closed' then
        for x in pairs(zStar.closedNodes) do
            for y in pairs(zStar.closedNodes[x]) do
                drawRectangle((x-1)*cellSize, (y-1)*cellSize, cellSize, cellSize, 5.1)
            end
        end
    elseif table == 'path' then
        if zStar.path then
            for i=1, #zStar.path do
                local x = zStar.path[i].x
                local y = zStar.path[i].y
                drawRectangle((x-1)*cellSize, (y-1)*cellSize, cellSize, cellSize, 5.2)
            end
        end
    end
end

function zStar._openAdjacent(parent)
    -- Open surrounding nodes
    zStar._openNode(parent, parent.x-1, parent.y, 1)
    zStar._openNode(parent, parent.x+1, parent.y, 1)
    zStar._openNode(parent, parent.x, parent.y-1, 1)
    zStar._openNode(parent, parent.x, parent.y+1, 1)

    -- Diagonals. Comment out these lines to use 4-directional movement.
    zStar._openNode(parent, parent.x-1, parent.y-1, 1.4142)
    zStar._openNode(parent, parent.x+1, parent.y-1, 1.4142)
    zStar._openNode(parent, parent.x-1, parent.y+1, 1.4142)
    zStar._openNode(parent, parent.x+1, parent.y+1, 1.4142)
end

function zStar._openNode(parent, x, y, g)
    local array = _zArray
    if x == zStar.start.x and y == zStar.start.y then
        -- We found the path. Early exit.
        local node = {
            parent = parent,
            x = x,
            y = y
        }
        zStar.finalNode = node
    end

    -- Check if the node is occupied by a collidable in Bump
    if array.check(zStar.collidables, x, y) == nil then
        local node = {}
        node.parent = parent
        node.x = x
        node.y = y
        node.gCost = parent.gCost + g
        node.hCost = (math.abs(node.x - zStar.start.x) + math.abs(node.y - zStar.start.y))
        node.fCost = node.gCost + node.hCost

        -- Check if this node is already closed
        if array.check(zStar.closedNodes, x, y) ~= nil then
            if zStar.closedNodes[x][y].fCost > node.fCost then
                -- Re-open this node if this path is shorter than the one previously checked
                array.set(zStar.openNodes, x, y, node)
            end
        -- Check if this node is already open
        elseif array.check(zStar.openNodes, x, y) ~= nil then
            if zStar.openNodes[x][y].fCost > node.fCost then
                -- Replace the open node with this one if it's shorter than the one previously checked.
                array.set(zStar.openNodes, x, y, node)
            end
        else
            array.set(zStar.openNodes, x, y, node)
        end
    end
end

function zStar._createPath(node)                                                -- Loop through the nodes' parents to construct a path
    local path = {}
    local table = table
    table.insert(path, node)
    while node.parent do
        node = node.parent
        table.insert(path, node)
    end
    return(path)
end

_zStar = zStar
return(zStar)



