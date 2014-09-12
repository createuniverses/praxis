-- queue.lua

Queue = {}
function Queue.new ()
  return {first = 0, last = -1}
end

function Queue.size(list)
  return list.last - list.first + 1
end

function Queue.get(list, index)
  local qindex = list.first + index - 1
  return list[qindex]
end

function Queue.pushfirst (list, value)
  local first = list.first - 1
  list.first = first
  list[first] = value
end

function Queue.pushlast (list, value)
  local last = list.last + 1
  list.last = last
  list[last] = value
end

function Queue.popfirst (list)
  local first = list.first
  if first > list.last then error("list is empty") end
  --if first > list.last then return 0 end
  local value = list[first]
  list[first] = nil
  -- to allow garbage collection
  list.first = first + 1
  return value
end

function Queue.poplast (list)
  local last = list.last
  if list.first > last then error("list is empty") end
  --if list.first > last then return 0 end
  local value = list[last]
  list[last] = nil
  -- to allow garbage collection
  list.last = last - 1
  return value
end
