-- Name: poly.lua

--poly = {}

t = 0
t_spd = 0.02

-- polyline = {}

-- polyline[1] = { x = 0, y = 0 }
-- polyline[2] = { x = 10, y = 40 }
-- polyline[3] = { x = 0, y = 80 }
-- polyline[4] = { x = 40, y = 70 }
-- polyline[5] = { x = 80, y = 80 }
-- polyline[6] = { x = 100, y = 50 }
-- polyline[7] = { x = 110, y = 10 }
-- polyline[8] = { x = 50, y = 50 }

-- polylineManPos = 0
-- polylineManVel = 0.03

-- manPos = { x = 0, y = 0 }

function makepolylines(numP, numL, numG)
    polylines = {}
    
    for i = 1, numP, 1 do
        polylines[i] = { line = {}, guys = {} }
        polylines[i].line[1] = { x = math.random(200), y = math.random(200) }
        direction = 0
        lineStepSize = 40
        for j = 1, numL, 1 do
            polylines[i].line[j+1] = 
            {
                x = polylines[i].line[j].x + (lineStepSize + math.random(20) - 10) * ( math.sin(direction * (math.pi / 180))),
                y = polylines[i].line[j].y + (lineStepSize + math.random(20) - 10) * ( math.cos(direction * (math.pi / 180)))
            }
            direction = direction + (45 + math.random(10) - 5) * (math.random(2) - 1)
        end
        for j = 1, numG, 1 do
            polylines[i].guys[j] = { pos = j * 0.4, vel = 0.05 }
        end
    end
    
    return polylines
end

polylines = makepolylines(9, 12, 15)

camAngle = 0
camAngle2 = 0
camAngleSpeed = 1
camAngleSpeed2 = 1.05
camOrbitCenter = { x = 150, y = 150 }
camOrbitRadius = 100
camOrbitHeight = 10

function poly.init()
    camAngle = 0
    camAngle2 = 0
    camAngleSpeed = 1
    camAngleSpeed2 = 1.05
    camOrbitCenter = { x = 150, y = 150 }
    camOrbitRadius = 100
    camOrbitHeight = 10
end

function poly.update()
    t = t + t_spd
    -- move man along polyline
    
    for i = 1, #polylines, 1 do
        for j = 1, #polylines[i].guys, 1 do
            polylines[i].guys[j].pos = polylines[i].guys[j].pos + polylines[i].guys[j].vel
            numSegments = #polylines[i].line - 1
            polylines[i].guys[j].pos = math.fmod(polylines[i].guys[j].pos, numSegments)
        end
    end
    
    camAngle = camAngle + camAngleSpeed * math.pi / 180
    camAngle2 = camAngle2 + camAngleSpeed2 * math.pi / 180
    
    camPos = { x = camOrbitCenter.x + camOrbitRadius * math.cos(camAngle), y = camOrbitCenter.y + camOrbitRadius * math.sin(camAngle2) }
    ahead = { x = camPos.x + -10 * math.sin(camAngle), y = camPos.y + 10 * math.cos(camAngle2) }
    
    --camOrbitRadius = math.sin(camAngle * 0.25) * 15 + 20
    
    --setCamPos(camPos.x, camOrbitHeight, camPos.y)
    --lookAt(ahead.x, camOrbitHeight, ahead.y)
end

function poly.render()
    renderPolylines()
    renderMen()
end

function renderMan(man, poly)
    pos = getPolylinePos(man.pos, poly)
    beginTriGL()
    colorGL(255, 55, 0, 255)
    vectorGL(pos.x - 5, 5,  pos.y)
    vectorGL(pos.x + 5, 5,  pos.y)
    vectorGL(pos.x,     15, pos.y)
    endGL()
end

function renderMen()
    for i = 1, #polylines, 1 do
        for j = 1, #polylines[i].guys, 1 do
            renderMan(polylines[i].guys[j], polylines[i].line)
        end
    end
end

function renderPolylines()
    for i = 1, #polylines, 1 do
        renderPolyline(polylines[i].line)
    end
end

function renderPolyline(polyline)
    for i=1,#polyline-1,1 do
        drawLine(polyline[i].x, 5, polyline[i].y, polyline[i+1].x, 5, polyline[i+1].y)
    end
end

function getPolylinePos(pos, polyline)
    startIndex = math.floor(pos) % #polyline + 1
    endIndex = math.ceil(pos) % #polyline + 1
    along = math.fmod(pos, 1)
    
    pos = interpolatevec(polyline[startIndex], polyline[endIndex], along)
    
    return pos
end

function interpolate(pos1, pos2, prop)
    pos = pos1 + (pos2 - pos1) * prop
    return pos
end

function interpolatevec(vec1, vec2, prop)
    vec = 
    {
        x = interpolate(vec1.x, vec2.x, prop),
        y = interpolate(vec1.y, vec2.y, prop)
    }
    
    return vec
end

