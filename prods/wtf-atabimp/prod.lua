
dofile("timeline.lua")
dofile("poly.lua")
dofile("heightmap.lua")
dofile("brat.lua")
dofile("mp3.lua")

dofile("reflect.lua")
inspect = require 'inspect'

function prodrender()
    timeline[currentTimelineItem].effect.render()
end

function produpdate()
    --if isMp3Playing() == false then
    --    os.exit()
    --end

    if isTransitionDue() then
        nextScene()
    end
    
    timeline[currentTimelineItem].effect.update()
end

function notransupdate()
    timeline[currentTimelineItem].effect.update()
end

--update = notransupdate
update = produpdate
render = prodrender

initTimeline()

--fullscreenMode()
windowedMode(0,0,800,600)
--playMp3()

setMaxFramerate(50)

