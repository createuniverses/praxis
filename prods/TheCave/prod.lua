-- Name: prod.lua

dofile("replacefn.lua")
dofile("editor.lua")
dofile("keymap.lua")

makeVoxelBlock()

function render()
    beginQuadGL()
    renderVoxelBlock()
    endGL()
end

function carveChunk(posx, posy, posz, size)
    xmin = math.floor(posx / 10 - size / 10)
    xmax = math.floor(posx / 10 + size / 10)
    ymin = math.floor(posy / 10 - size / 10)
    ymax = math.floor(posy / 10 + size / 10)
    zmin = math.floor(posz / 10 - size / 10)
    zmax = math.floor(posz / 10 + size / 10)
    for x=xmin,xmax,1 do
        for y=ymin,ymax,1 do
            for z=zmin,zmax,1 do
                cubepos = { x = x * 10, y = y * 10, z = z * 10 }
                diff = { x = cubepos.x - posx, y = cubepos.y - posy, z = cubepos.z - posz }
                distance = math.sqrt(diff.x * diff.x + diff.y * diff.y + diff.z * diff.z)
                if distance < size then
                    carveVoxelAt(x,y,z)
                end
            end
        end
    end
end

function carveAtMouse(size)
    px,py,pz = getMouseCursorPos()
    carveChunk(px,pz,py, size)
end

spiralstartpos  = { x = 0, y = 500, z = 500 }
spiralradius    = 200

spiralpos       = 0
spiralangle     = 0

spiralposspd    = 0.1
spiralposangspd = 0.5

function calcspiralpos()
     pos = {
         x = spiralstartpos.x + spiralpos * 10,
         y = spiralstartpos.y + math.sin(spiralangle) * spiralradius,
         z = spiralstartpos.z + math.cos(spiralangle) * spiralradius }
     return pos   
end

numupdates = 0

camYaw             = 0
--camYawSpd          = 2.7 / 180
--camYawSpd          = 1.5 / 180
camYawSpd          = 1.0 / 180
newCamYawSpd       = camYawSpd

camPitch           = 0
camPitchMax        = 20
newCamPitchMax     = camPitchMax
camPitchT          = 0
--camPitchTSpd       = 4 / 180
camPitchTSpd       = 3 / 180
newCamPitchTSpd    = camPitchTSpd

camOrbitRadius     = 2
newCamOrbitRadius  = camOrbitRadius

trigger1 = -1
trigger2 = -1
trigger3 = -1
trigger4 = -1
trigger5 = -1
trigger6 = -1
trigger7 = -1

function updateparams()
    camYaw      = camYaw      + math.pi * camYawSpd
    camYawSpd   = camYawSpd   + (newCamYawSpd - camYawSpd)   * 0.1
    
    camPitch      = math.sin(camPitchT) * (math.pi/180) * camPitchMax
    camPitchT     = camPitchT + math.pi * camPitchTSpd
    camPitchTSpd  = camPitchTSpd + (newCamPitchTSpd - camPitchTSpd) * 0.1
    camPitchMax   = camPitchMax  + (newCamPitchMax  - camPitchMax)  * 0.1
    
    camOrbitRadius = camOrbitRadius + (newCamOrbitRadius - camOrbitRadius) * 0.1
end

function update()
    
    if getMp3Time() > 23.4 and trigger1 == -1 then
        trigger1 = numupdates
        print("Trigger 1 hit")
        
         -- jerk upwards
        newCamPitchMax = 89.9
        --camPitchT      = 0
    end

    if getMp3Time() > 51.9 and trigger2 == -1 then
        trigger2 = numupdates
        print("Trigger 2 hit")
        
         -- ease down, no jerk, start orbiting
        newCamPitchMax    = 20
        newCamOrbitRadius = 30
    end

    if getMp3Time() > 80.4 and trigger3 == -1 then
        trigger3 = numupdates
        print("Trigger 3 hit")
        
         -- go in reverse, stop orbiting
        newCamYawSpd      = -newCamYawSpd
        newCamPitchMax    = 10
        newCamOrbitRadius = 2
    end

    if getMp3Time() > 101.8 and trigger4 == -1 then
        trigger4 = numupdates
        print("Trigger 4 hit")
        
         -- go in reverse, start orbiting and jerk upwards
        newCamYawSpd       = -newCamYawSpd
        newCamOrbitRadius  = 20
        newCamPitchMax     = 89.9
        --camPitchT          = 0
    end

    if getMp3Time() > 130.3 and trigger5 == -1 then
        trigger5 = numupdates
        print("Trigger 5 hit")
        
        -- reduce rotation speed and reveal the cave
        newCamYawSpd      = newCamYawSpd * 0.5
        newCamPitchMax    = 5
        newCamOrbitRadius = 200
    end

    if getMp3Time() > 158.8 and trigger6 == -1 then
        trigger6 = numupdates
        print("Trigger 6 hit")
        
        -- reverse and go faster within cave
        newCamYawSpd       = -newCamYawSpd * 1.1
        newCamOrbitRadius  = 2
    end

    if getMp3Time() > 173 and trigger7 == -1 then
        trigger7 = numupdates
        print("Trigger 7 hit")
        
        -- reduce rotation even more and reveal the cave from further back to end the demo
        newCamOrbitRadius      = 250
        newCamYawSpd           = newCamYawSpd * 0.5
    end
    
    if isMp3Playing() == false and trigger7 > -1 then
        print("Demo finished")
        
        os.exit()
    end
    
    updateparams()

    pos = calcspiralpos()
    
    setCamPos(pos.x + math.sin(camYaw) * camOrbitRadius, pos.z,   pos.y + math.cos(camYaw) * camOrbitRadius)
    lookAt(   pos.x + math.sin(camYaw),                  pos.z,   pos.y + math.cos(camYaw))
    rotateCam(0, camPitch)
    
    if newCamOrbitRadius < 50 then
        carveChunk(pos.x, pos.y, pos.z, 30)
        camPos = {} camPos.x, camPos.y, camPos.z = getCamPos()
        carveChunk(camPos.x, camPos.z, camPos.y, 30)
        if numupdates % 3 == 0 then carveAtMouse(30) end
    end
    
    spiralpos   = spiralpos   + spiralposspd
    spiralangle = spiralangle + (math.pi / 180) * spiralposangspd
    
    --if newCamOrbitRadius < 50 then
    if false then
        if spiralpos > 100 then
            spiralpos   = 0
            spiralangle = 0
        end
        
        spiralposspd    = 0.1
        spiralposangspd = 0.5
    else
        if spiralpos > 100 then
            spiralposspd    = -0.1
            spiralposangspd = -0.5
        end
        if spiralpos < 0 then
            spiralposspd    = 0.1
            spiralposangspd = 0.5
        end
    end
    
    numupdates = numupdates + 1
end


function colors1()
    for i=0,99,1 do
        for j=0,99,1 do
            for k=0,99,1 do
                setVoxelColour(i,j,k, i * (255 / 100), j * (255 / 100), k * (255 / 100), 255)
            end
        end
    end
end

function colors2()
    for i=0,99,1 do
        for j=0,99,1 do
            for k=0,99,1 do
                if i%2 == 1 then
                    red   = i * (127 / 100)
                else
                    red   = 127 + i * (127 / 100)
                end
                if j%2 == 1 then
                    green = j * (127 / 100)
                else
                    green = 127 + j * (127 / 100)
                end
                if k%2 == 1 then
                    blue  = k * (127 / 100)
                else
                    blue  = 127 + k * (127 / 100)
                end
                setVoxelColour(i,j,k, red,green,blue, 255)
            end
        end
    end
end

--fullscreenMode()
--windowedMode()

windowedMode(0,0,800,600)

playMp3()

-- Do an initial update to set the camera for a possible initial render
update()

setMaxFramerate(50)

