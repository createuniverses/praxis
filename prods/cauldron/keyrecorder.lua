-- keyrecorder.lua

keyrecorder = {}
keyrecorder.framenum = 0
keyrecorder.keys = {}

keyrecorder.state = "stopped" -- recording, playing

function keyrecorder.update()
  local k = keyrecorder
  k.framenum = k.framenum + 1
  
  if k.state == "playing" then
  end  
end

function keyrecorder:onKeyDown(k)
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
  self.state = "recording"
end

function keyrecorder:stop()
  --self.framenum = 0
  self.state = "stopped"
end

updatefns = updatefns or {}

updatefns["keyrecorder"] = keyrecorder.update
