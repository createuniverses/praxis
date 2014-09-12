-- Name: brat.lua

brat = {}

vehicles = {}
numVehicles = 10

function initVehicles(num)
    numVehicles = num
    isTarget = {}
    for i = 1, numVehicles, 1 do
        isTarget[i] = false
    end
    for i = 1, numVehicles, 1 do
        -- need to add a stream
        vehicles[i] =
        {
            pos = { x = math.random(150), y = math.random(150), z = math.random(150) + 10 },
            ppos = { x = 0, y = 0, z = 0 },
            target = math.random(numVehicles)
        }
        while vehicles[i].target == i or isTarget[vehicles[i].target] == true do
            vehicles[i].target = vehicles[i].target % numVehicles + 1
        end
        isTarget[vehicles[i].target] = true
    end
end

function redotargets()
    for i=1, numVehicles,1 do
        vehicles[i].target = math.random(numVehicles)
    end
end

function swaptargets(n)
    for i=1,n,1 do
        vehicle1 = math.random(numVehicles)
        vehicle1target = vehicles[vehicle1].target
        vehicle2 = math.random(numVehicles)
        vehicle2target = vehicles[vehicle2].target
        vehicles[1].target = vehicle2target
        vehicles[2].target = vehicle1target
    end
end

function explodevehicles(scale)
    center = { x = 75, y = 75, z = 80 }
    for i=1,numVehicles,1 do
        diff = { x = vehicles[i].pos.x - center.x, y = vehicles[i].pos.y - center.y, z = vehicles[i].pos.z - center.z }
        vehicles[i].pos.x = center.x + diff.x * scale
        vehicles[i].pos.y = center.y + diff.y * scale
        vehicles[i].pos.z = center.z + diff.z * scale
    end
end

initVehicles(200)

camAngle = 0
camAngle2 = 0
camAngleSpeed = 3
camAngleSpeed2 = 2
camOrbitCenter = { x = 75, y = 75 }
camOrbitRadius = 60
camOrbitHeight = 80

-- streamers didn't get done

numBratUpdates1 = 0
numBratUpdates2 = 0

function brat.init()
    camAngle = 0
    camAngle2 = 0
    camAngleSpeed = 3
    camAngleSpeed2 = 2
    camOrbitCenter = { x = 75, y = 75 }
    camOrbitRadius = 60
    camOrbitHeight = 80
    
    numBratUpdates1 = 0
    numBratUpdates2 = 0
    
    initVehicles(200)
end


autoExplodeFactor = 1.3
perframeExplodeFactor = 1.0017
function brat.update()
    numBratUpdates1 = (numBratUpdates1 + 1) % 700
    if numBratUpdates1 == 0 then
        --explodevehicles(autoExplodeFactor)
        initVehicles(200)
    end
    
    numBratUpdates2 = (numBratUpdates2 + 1) % 20
    if numBratUpdates2 == 0 then
        swaptargets(1)
        --explodevehicles(autoExplodeFactor)
        --initVehicles(200)
    end
    
    explodevehicles(perframeExplodeFactor)
    
    for i=1,16,1 do
        vehicleupdate()
    end
    
    camAngle = camAngle + camAngleSpeed * math.pi / 180
    camAngle2 = camAngle2 + camAngleSpeed2 * math.pi / 180
    
    camPos = { x = camOrbitCenter.x + camOrbitRadius * math.cos(camAngle), y = camOrbitCenter.y + camOrbitRadius * math.sin(camAngle2) }
    
    --setCamPos(camPos.x, camOrbitHeight, camPos.y)
    --lookAt(75, 100, 75)
end

function vehicleupdate()
    for i = 1, numVehicles, 1 do
        vehicles[i].ppos.x = vehicles[i].pos.x
        vehicles[i].ppos.y = vehicles[i].pos.y
        vehicles[i].ppos.z = vehicles[i].pos.z
        
        vehicles[i].pos.x = vehicles[i].pos.x + (vehicles[vehicles[i].target].pos.x - vehicles[i].pos.x) * 0.01
        vehicles[i].pos.y = vehicles[i].pos.y + (vehicles[vehicles[i].target].pos.y - vehicles[i].pos.y) * 0.01
        vehicles[i].pos.z = vehicles[i].pos.z + (vehicles[vehicles[i].target].pos.z - vehicles[i].pos.z) * 0.01
    end
end

function brat.render()
    for i = 1, numVehicles, 1 do
        other =
        {
            x = vehicles[i].pos.x + (vehicles[i].ppos.x - vehicles[i].pos.x) * 185,
            y = vehicles[i].pos.y + (vehicles[i].ppos.y - vehicles[i].pos.y) * 185,
            z = vehicles[i].pos.z + (vehicles[i].ppos.z - vehicles[i].pos.z) * 185
        }
        drawLine(vehicles[i].pos.x, vehicles[i].pos.z, vehicles[i].pos.y, other.x, other.z, other.y)
    end
end
