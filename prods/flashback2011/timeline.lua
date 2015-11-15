-- Name: timeline.lua

timeline = {}

poly = {}
brat = {}
heit = {}


function addTimelineItem(e, d)
    local nextId = #timeline + 1
    timeline[nextId] =
    {
        duration = d,
        effect = e
    }
end

addTimelineItem(poly,       8.0)
addTimelineItem(brat,       5.0)
addTimelineItem(heit,       2.0)
addTimelineItem(brat,       6.0)
addTimelineItem(heit,       2.0)
addTimelineItem(brat,       4.0)
addTimelineItem(poly,       7.0)
addTimelineItem(heit,       2.0)

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

