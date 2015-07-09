-- synth.lua

--g_sampleRate = 11025
g_sampleRate = getSampleRate()
g_samplesPerRequest = getSamplesPerRequest()

g_twopi = math.pi * 2
g_maxAmplitude = 32767

function sqrwav(a)
  if math.sin(a) > 0 then
    return 1.0
  else
    return -1.0
  end
end

SynthNode = {}

SynthNode.rbuffsize = 50
SynthNode.renderstep = 2

function SynthNode.new (params)
  local node = {}
  node.buffer = Queue.new()
  node.angle = 0
  node.freq = params.freq
  if params.amp == nil then
    node.amp = 0.5
  else
    node.amp = params.amp
  end
  node.samplesLastFrame = 0
  node.updateFn = "updateWavemaker"
  node.waveFn = math.sin
  --node.waveFn = sqrwav
  --node.waveFn = function (a) if math.sin(a) > 0 then return 1.0 else return -1.0 end end
  node.inputs = {}
  node.rbuffer = {}
  for i=1,SynthNode.rbuffsize,1 do
    node.rbuffer[i] = 0.0
  end
  return node
end

function SynthNode.newsink(inputnode)
  local node = {}
  node.inputs = {}
  node.inputs["audio"] = inputnode
  node.updateFn = "updateSink"
  return node
end

function SynthNode.getSample(node)
  local sample = Queue.popfirst(node.buffer)
  return sample
end

function SynthNode.updateSink(node)
  local audioInput = node.inputs["audio"]
  if audioInput == nil then return end
  
  local rpos,wpos,rtot,wtot = getSampleMarkers()
  
  while wtot < rtot + g_samplesPerRequest do
    rpos,wpos,rtot,wtot = getSampleMarkers()
    local sample = SynthNode.getSample(audioInput)
    writeSample(sample)
  end
end

function SynthNode.updateWavemaker(node)
  node.samplesLastFrame = 0
  local freqInput = node.inputs["freq"]
  while Queue.size(node.buffer) < g_samplesPerRequest * 2 do
    --local sample = ((g_maxAmplitude + node.waveFn(node.angle) * g_maxAmplitude) * node.amp)
    local sample = ((node.waveFn(node.angle) * g_maxAmplitude) * node.amp)
    Queue.pushlast(node.buffer, sample)
    if freqInput ~= nil then
      local freqSample = SynthNode.getSample(freqInput)
      freqSample = linearInterpolate(
                       -g_maxAmplitude,g_maxAmplitude,
                       0.1 * node.freq, 1.9 * node.freq, freqSample)
      node.angle = node.angle + (g_twopi * (freqSample / g_sampleRate))
    else
      node.angle = node.angle + (g_twopi * (node.freq / g_sampleRate))
    end
  if node.angle > g_twopi then node.angle = node.angle - g_twopi end
    node.samplesLastFrame = node.samplesLastFrame + 1
  end
end

function SynthNode.newFilter (params)
  local node = {}
  node.samplesLastFrame = 0
  node.buffer = Queue.new()
  
  -- freq default: 1000, 100 - 2000
  node.freq = params.freq
  -- q default: 1.0, 0.01 - 100
  node.q = params.q
  
  node.updateFn = "updateLPFilter"
  node.fVec0 = { 0,0,0 }
  node.fRec0 = { 0,0,0 }
  
  node.rbuffer = {}
  for i=1,SynthNode.rbuffsize,1 do
    node.rbuffer[i] = 0.0
  end
  
  node.inputs = {}
  
  return node
end

function SynthNode.updateLPFilter(node)
  node.samplesLastFrame = 0
  local freqInput = node.inputs["freq"]
  local audioInput = node.inputs["audio"]
  
  if audioInput == nil then return end
  
  local fConst0 = g_twopi / g_sampleRate;
  
  local input1 = node.freq
  local input3 = node.q
  
  while (Queue.size(node.buffer) < g_samplesPerRequest * 2) and (Queue.size(audioInput.buffer) > 0) do
    
    local input0 = SynthNode.getSample(audioInput) / g_maxAmplitude
    
    if freqInput ~= nil then
      if Queue.size(freqInput.buffer) > 0 then
        local freqSample = SynthNode.getSample(freqInput)
        input1 = linearInterpolate(
                         -g_maxAmplitude,g_maxAmplitude,
                         0.1 * node.freq, 1.9 * node.freq, freqSample)
      end
    end
    
    local output0 = 0.0
    
    local fTemp0 = (fConst0 * math.max(0, input1))
    local fTemp1 = (0.5 * (math.sin(fTemp0) / math.max(0.001, input3)))
    local fTemp2 = math.cos(fTemp0)
    local fTemp3 = input0
    node.fVec0[1] = fTemp3
    node.fRec0[1] = ((((1 - fTemp2) * ((node.fVec0[2] + (0.5 * node.fVec0[1])) + (0.5 * node.fVec0[3]))) + ((node.fRec0[3] * (fTemp1 - 1)) + (2 * (fTemp2 * node.fRec0[2])))) / (1 + fTemp1))
    output0 = node.fRec0[1];
    -- post processing
    node.fRec0[3] = node.fRec0[2]
    node.fRec0[2] = node.fRec0[1]
    node.fVec0[3] = node.fVec0[2]
    node.fVec0[2] = node.fVec0[1]
    
    Queue.pushlast(node.buffer, output0 * g_maxAmplitude)
    --Queue.pushlast(node.buffer, input0)
    
    node.samplesLastFrame = node.samplesLastFrame + 1
  end
end

function SynthNode.updateSynthNode(node)
  SynthNode[node.updateFn](node)
  SynthNode.makeRBuffer(node)
end

function SynthNode.makeRBuffer(node)
  if node.buffer == nil then return end
  local bindex = 1
  for i=1,SynthNode.rbuffsize,1 do
    if bindex <= Queue.size(node.buffer) then
      local s = Queue.get(node.buffer, bindex)
      s = s / g_maxAmplitude * 20.0
      node.rbuffer[i] = s
      -- renderstep should be set for each synthnode.
      if node.renderstep == nil then
        bindex = bindex + SynthNode.renderstep
      else
        bindex = bindex + node.renderstep
      end
    else
      node.rbuffer[i] = 0.0
    end
  end
end

-- ok, this works.
-- now need to compute needed transform so links between synth nodes
-- are depicted.
--syntrans = transform.new()
--transform.setTranslation(syntrans, 0,10,0)
--transform.lookAt(syntrans, 100,10,100)
--transform.setScale(syntrans, 6,1,3)

g_synthTrans = transform.new()

function makeSynthTrans(from, to, size)
  --local trans = transform.new()
  local trans = g_synthTrans
  transform.setTranslation(trans, from.x, from.y, from.z)
  transform.lookAt(trans, to.x, to.y, to.z)
  local diff = { x = to.x - from.x, y = to.y - from.y, z = to.z - from.z }
  local dist = math.sqrt(diff.x * diff.x + diff.y * diff.y + diff.z * diff.z)
  transform.setScale(trans, 1,1,dist / size)
  return trans
end

function SynthNode.render(node, from, to)
  local trans = makeSynthTrans(from, to, SynthNode.rbuffsize)
  beginLinGL()
  colorGL(255,255,255,255)
  for i=1,SynthNode.rbuffsize - 1,1 do
    local s1 = node.rbuffer[i]
    local s2 = node.rbuffer[i+1]
    
    --vectorGL(transform.localToGlobal(trans, s1,0,-i))
    --vectorGL(transform.localToGlobal(trans, s2,0,-i-1))
    
    vectorGL(transform.localToGlobal(trans, s1,0,i))
    vectorGL(transform.localToGlobal(trans, s2,0,i+1))
  end
  endGL()
end

function SynthNode.renderInputs(node)
  for k,v in pairs(node.inputs) do
    SynthNode.render(v, v.pos, node.pos)
  end
end

g_sbamp = 0.2 * (32000.0/32767.0)
g_sbfreq = 204.39999389648
-- The actual initial frequency is not 205 as specified in the constructor,
-- its actually: 204.39999389648
-- This is due to correction caused by the controls widgets.
-- I used the handy Lua repl to find this number!!
-- print(getSelectedElement()) --> 1
-- print(getElementParameter(1, 0)) -> 204.39999389648
--
-- Double confirmed by adjusting here, as well as turning off the call to
-- SetParamsFromControls in SynthbenchMainWindow::Tick()
--
-- Also, I confirmed that Qt uses waveOut for synthesized audio, not DirectSound.
-- See: qaudiodeviceinfo_win32_p.cpp, qaudiooutput_win32_p.cpp

sineGenNode = SynthNode.new({ amp = g_sbamp, freq = g_sbfreq })
sineConNode = SynthNode.new({ freq = 1 })
--lpfEffNode = SynthNode.newFilter({ freq = 1000, q = 1 })
lpfEffNode = SynthNode.newFilter({ freq = 500, q = 1 })
sineConNode2 = SynthNode.new({ freq = 2 })

sinkNode = SynthNode.newsink(lpfEffNode)

sineGenNode.waveFn = sqrwav

sineGenNode.inputs["freq"] = sineConNode
lpfEffNode.inputs["audio"] = sineGenNode
lpfEffNode.inputs["freq"]  = sineConNode2

sineConNode.renderstep = 40
sineConNode2.renderstep = 40

sinkNode.pos = vec2d(0,0)
lpfEffNode.pos = vec2d(-100,0)
sineGenNode.pos = vec2d(-200,0)
sineConNode.pos = vec2d(-200,100)
sineConNode2.pos = vec2d(-100,100)

sl1 = Slider.new(vec3d(-40,10,10), 0.1, 10)

function sl1.update(slider)
  sineConNode.freq = slider.pos
end

transform.setTranslation(sl1.lspace, 0,10,-50)
transform.lookAt(sl1.lspace, -100,10,-100)

