framenum = 0

getMp3TimeOld = getMp3Time

function getMp3Time()
  local t1 = framenum / 30.32
  --local t2 = getMp3TimeOld()
  --print(t1 - t2)
  return t1
end

getMp3Time = getMp3TimeOld

update_pretiming = update

function update()
  update_pretiming()
  framenum = framenum + 1
end
