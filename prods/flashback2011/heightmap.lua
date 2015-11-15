-- Name: heightmap.lua

--heit = {}

t = 0
t_spd = 0.02

heightmap = {}
cellsize = 10

camCellPosI = 5
camCellPosJ = 5

camAngle = 0
camAngleSpeed = 4
camOrbitCenter = { x = 5 * cellsize, y = 5 * cellsize }
camOrbitRadius = 50
camOrbitHeight = 10

-- make a guy that runs and jumps over these platforms

function heit.init()
    camAngle = 0
    camAngleSpeed = 4
    camOrbitCenter = { x = 5 * cellsize, y = 5 * cellsize }
    camOrbitRadius = 50
    camOrbitHeight = 10
end

function heit.update()
    t = t + t_spd
    
    for i=1,10 do
         for j=1,10 do
             heightmap[i][j] = 20*math.sin(t * i * 0.1) + 20*math.sin(t * j * 0.1)
         end
    end
    
    camAngle = camAngle + camAngleSpeed * math.pi / 180
    
    camPos = { x = camOrbitCenter.x + camOrbitRadius * math.cos(camAngle), y = camOrbitCenter.y + camOrbitRadius * math.sin(camAngle) }
    ahead = { x = camPos.x + -10 * math.sin(camAngle), y = camPos.y + 10 * math.cos(camAngle) }
    
    camOrbitRadius = math.sin(camAngle * 0.25) * 15 + 20
    
    setCamPos(camPos.x, camOrbitHeight, camPos.y)
    lookAt(ahead.x, camOrbitHeight, ahead.y)
    
    --setCamPos(getcellposition(camCellPosI,camCellPosJ))
    --setCamPos(i * cellsize, heightmap[i][j]+10, j * cellsize)
    
    -- shiftcellcam()
end

function heit.render()
    beginTriGL()
    renderheightmap()
    endGL()
end

function createheightmap()
    heightmap = {}
    for i=1,10 do
         heightmap[i] = {}
         for j=1,10 do
             heightmap[i][j] = 20
         end
    end
end

function rendercell(i,j)
    colorGL(20 * i, 20 * j, 0, 255)
    
    vectorGL(i * cellsize, heightmap[i][j], j * cellsize)
    vectorGL((i+1) * cellsize, heightmap[i][j], j * cellsize)
    vectorGL(i * cellsize, heightmap[i][j], (j+1) * cellsize)

    vectorGL((i+1) * cellsize, heightmap[i][j], j * cellsize)
    vectorGL((i+1) * cellsize, heightmap[i][j], (j+1) * cellsize)
    vectorGL(i * cellsize, heightmap[i][j], (j+1) * cellsize)
end
    
function renderheightmap()
    for i = 1,10 do
        for j = 1,10 do
            rendercell(i,j)
        end
    end
end

function getcellposition(i,j)
    return i * cellsize + (cellsize * 0.5), heightmap[i][j]+10, j * cellsize + (cellsize * 0.5)
end

function shiftcellcam()
    camCellPosI = camCellPosI + 1
    if camCellPosI > 10 then
        camCellPosI = 1
        camCellPosJ = camCellPosJ + 1
        if camCellPosJ > 10 then
            camCellPosJ = 1
        end
    end
end

createheightmap()
