-- keyrecorder.lua

keyrecorder = {}
keyrecorder.framenum = 0
keyrecorder.keys = {}

keyrecorder.state = "stopped" -- recording, playing

function findmatch(n)
  for i=1,#keyrecorder.keys, 1 do
    local k = keyrecorder.keys[i]
    if k.t == n then
      return k
    end
  end
  return nil
end

function keyrecorder.update()
  local k = keyrecorder
  k.framenum = k.framenum + 1
  
  if k.state == "playing" then
    local key = findmatch(k.framenum)
    if key ~= nil then
      onKeyDown(key.key)
      -- need to provide the modifiers here!!
    end
  end  
end

function keyrecorder:onKeyDown(k)
  -- need to save the state of the key modifiers too!!
  if self.state == "recording" then
    local kr = self
    table.insert(kr.keys, {key = k, t = kr.framenum })
  end
end

function keyrecorder:play()
  self.framenum = 0
  self.state = "playing"
end

function keyrecorder:record()
  self.framenum = 0
  self.state = "recording"
end

function keyrecorder:stop()
  self.framenum = 0
  self.state = "stopped"
end

updatefns = updatefns or {}

updatefns["keyrecorder"] = keyrecorder.update

