function rouletteSelection(t,f,tr)
  total = 0
  for i=1,#t,1 do
    --total = total + (t[i][f] * t[i][f])
    total = total + tr(t[i][f])
  end
  if total <= 0 then return { i = 0, v = nil } end
  ball = math.random(total)
  cumtotal = 0
  for i=1,#t,1 do
    cumtotal = cumtotal + tr(t[i][f])
    if cumtotal >= ball then
      return { i = i, v = t[i] }
    end
  end
  return { i = 0, v = nil }
end

function roulTest(slices,n,f)
  local t = {}
  for i = 1,#slices,1 do
    t[i] = {}
    t[i].slice = slices[i]
    t[i].cnt = 0
  end
  for i = 1,n,1 do
    local r = rouletteSelection(t, "slice", f)
    if r.v ~= nil then
      r.v.cnt = r.v.cnt + 1 end
  end
  local total = 0
  for i=1,#t,1 do
    total = total + t[i].cnt
  end
  t.total = total
  print2(inspect(t))
  --setClipboardText(inspect(t))
end

do
roulTest({ 1,2,3,4,5,6,7,8,9,10 },
         10000,
         function (x) return x*x*x end)
end

