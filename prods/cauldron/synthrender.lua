function SynthNode.makeRBuffer(node)
  if node.buffer == nil then return end
  local bindex = 1
  for i=1,SynthNode.rbuffsize,1 do
    local qsize = Queue.size(node.buffer)
    if bindex <= qsize then
      local s = Queue.get(node.buffer, qsize - bindex)
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


function SynthNode.updateSynthNode(node)
  SynthNode[node.updateFn](node)
  SynthNode.makeRBuffer(node)
end

