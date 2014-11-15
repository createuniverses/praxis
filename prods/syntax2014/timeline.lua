-- Name: timeline.lua

timeline = {}

trak = {}
mech = {}

function addTimelineItem(e, d)
    local nextId = #timeline + 1
    timeline[nextId] =
    {
        duration = d,
        effect = e
    }
end

addTimelineItem(mech,       27.0)
addTimelineItem(trak,       26.5)
addTimelineItem(mech,       1.6)
addTimelineItem(mech,       1.6)
addTimelineItem(mech,       1.6)
addTimelineItem(mech,       1.6)
addTimelineItem(mech,       1.6)
addTimelineItem(mech,       1.6)
addTimelineItem(mech,       1.6)
addTimelineItem(mech,       1.6)
addTimelineItem(mech,       1.6)
addTimelineItem(trak,       20.0)
addTimelineItem(mech,       3.2)
addTimelineItem(mech,       3.2)
addTimelineItem(mech,       3.2)
addTimelineItem(trak,       20.0)
addTimelineItem(mech,       10.0)

-- Total time: 127

currentTimelineItem = 1
currentTimelineItemStartTime = 0.0

function initTimeline()
  timeline[currentTimelineItem].effect.init()
end

function nextScene()
    currentTimelineItem = currentTimelineItem % #timeline + 1
    currentTimelineItemStartTime = getMp3Time()
    timeline[currentTimelineItem].effect.init()
end

function isTransitionDue()
    transitionTime = currentTimelineItemStartTime + timeline[currentTimelineItem].duration
    if getMp3Time() > transitionTime then
        return true
    else
        return false
    end
end

