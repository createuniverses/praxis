skythings = {}
function makeskythings()
 for i=1,17,1 do
  local thing  =
  { p = vec3d(math.random(1000) - 500,
              math.random(300) + 50,
              math.random(1000) - 500),
    r = math.random(10) + 2 }
  table.insert(skythings, thing)
 end
end

makeskythings()
makeskythings()
makeskythings()

for i=1,#skythings,1 do
  local thing = skythings[i]
  thing.p = vec3d(math.random(2000)-1000,
                  math.random(500)+100,
                  math.random(2000)-1000)
end

