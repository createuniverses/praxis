-- Name: slipnet.lua

slipnet = {}

-- start off with very basic concepts and no connections.

-- square
-- triangle
-- circle
-- near
-- far
-- big
-- small

eyePos = vec2d(0,0)
eyeTarget = vec2d(0,0)

-- should the eye be a vehicle?

-- Try 2 separate things:
-- 1. The eye is a vehicle, moving around, with bottom up codelet
-- 2. Try the whole scout - strength tester - builder codelet trio with
--    saliences.

function makeEyemap()
  local eyeMap = {}
  eyeMap.cells = {}
  for i=1,10,1 do
    eyeMap.cells[i] = {}
    for j=1,10,1 do
      eyeMap.cells[i][j] = { visited = false }
    end
  end
  return eyeMap
end

eyeMap = makeEyemap()

function updateEye()
  -- mark the eyemap at the eye location as being visited
  -- move the eye to an adjacent square that hasn't been visited.
  -- meandering behaviour
end

-- diamond shape

slipnet["square"] = 
{
  render = 
    function (self)
      drawSquare(vec2d(170,0), 20, self.activation)
    end,
  activation = 1
}

-- lots of agents performing a simple task - boids
-- concept nodes changing what those agents are constantly
-- need to be able to play with the concepts and the links
-- between constants fluidly in real time, in a live fashion

-- refer to the copycat source code, and observe the simplicity
-- of the concepts and codelets and their effects. made up of
-- many simple things, a society of mind.

-- square!! so start to draw a square. 4 lines, start and end points
-- propose a square. where should it go? dotted lines at first
-- square is being proposed and imagined
-- square concept causes square to be proposed
-- would the square concept cause square proposing codelets to be
-- continuously spawned??

-- pay close attention to the codelets that are spawned and run in copycat

-- near concept causes a codelet which moves proposed things closer

-- an eye focus point that moves around at a constant speed,
-- tending to avoid where it has already been.
-- a map of where it has visited that calms down over time.
-- it can move in a pattern, try to follow interesting areas.



-- may move along a connection that has been made - matching pair
-- occasionally may move larger amount, or may not look at thing
-- and keep moving...

-- use light source to indicate eye position
-- a noticer may or may not run

-- if you see consecutive similar things, place a thing that can make a group
-- markov model?
 



-- the codelets spawned by these do very basic things,
-- and the codelets don't spawn follow ups.

-- next level - codelets spawn follow ups

-- good to do things in stages for a sense of accomplishment
-- at each stage.

-- slipnet["similar"] =  newConcept()
-- slipnet["group"] =    newConcept()
-- slipnet["similarity-group"] = newConcept()

-- slipnet["direction"] = newConcept()

-- slipnet["opposite"] = newConcept()

-- slipnet["proximity"] = newConcept()


-- slipnet["number"] = newConcept()

-- slipnet["1"] = newConcept()
-- slipnet["2"] = newConcept()
-- slipnet["3"] = newConcept()

function increaseActivation(c)
  slipnet[c].activation = slipnet[c].activation + 1
end

function updateSlipnet()
  -- pick a concept
  -- roulette select a concept
  -- roll the dice to determine whether this concept
  -- should post a codelet.
  
  -- later, implement links between codelets and "slippage"
  
  -- move the eyePos toward eyeTarget
end

function renderSlipnet()
  slipnet["square"]:render()
end
