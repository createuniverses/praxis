skythings = {}
function makeskythings()
 for i=1,17,1 do
  skythings[i] =
  { p = vec3d(math.random(1000) - 500,
              math.random(300) + 50,
              math.random(1000) - 500),
    r = math.random(10) + 2 }
 end
end

makeskythings()
makeskythings()
makeskythings()
