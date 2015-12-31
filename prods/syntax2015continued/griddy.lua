do
 local gridbak = swapoutgrid()
 
 local grid = {}
 
 getmygrid = function (x,y)
   grid[x] = grid[x] or {}
   return grid[x][y] or 10
 end
 setmygrid = function (x,y,v)
   grid[x] = grid[x] or {}
   grid[x][y] = v
 end

 local gg = getmygrid

 function swapingrid(g)
   grid = g
 end
 
 function swapoutgrid()
  return grid
 end

 function rendergrid2()
  local s = 5
  local h = 10
  for x = -100,100,s do
    for y = -100,100,s do
      h1 = gg(x,y)
      h2 = gg(x+s,y)
      h3 = gg(x,y+s)
      drawLine(x,h1,y,x+s,h2,y)
      drawLine(x,h1,y,x,  h3,y+s)
    end
  end
 end
 
 swapingrid(gridbak)

 clearError()
 continue()
end


function rendergrid2() end

for i=50,90,5 do
 setmygrid(i,0,20)
end

do
 local gg = getmygrid
 local s = 5
 for x=-100,100,s do
   for y=-100,100,s do
     local h1 = gg(x,  y)
     local h2 = gg(x+s,y)
     local h3 = gg(x+s,y+s)
     local h4 = gg(x  ,y+s)
     local h = gg(x,y)
     local avg = (h1+h2+h3+h4)/4
     h = h + (avg - h) * 0.1
     --h = avg
     --h = 5
     setmygrid(x,y,h)
   end
 end
end

clearTrace()

for i=1,20,1 do
  local r = 20
  local x = (math.random(r)-r/2) * 5
  local y = (math.random(r)-r/2) * 5
  local z = math.random(90) + 5
  setmygrid(x,y,z)
end

do
 setmygrid(0,0,50)
 setmygrid(0,5,50)
 setmygrid(0,10,50)
 setmygrid(0,15,50)
end

clearError()
continue()
setBufferName("griddy.lua")

_gridbak = swapoutgrid()
print2(_gridbak[0][0])
swapingrid(_gridbak)



continue()
print2(getErrorText())
[string "function render()..."]:5: attempt to call global 'renderGears' (a nil value)
stack traceback:
	[string "function onerror(s) endGL() glResetStencil(..."]:1: in function 'renderGears'
	[string "function render()..."]:5: in function 'render'
	[string "render()"]:1: in main chunk
[string "function render()..."]:5: attempt to call global 'renderGears' (a nil value)
stack traceback:
	[string "function onerror(s) endGL() glResetStencil(..."]:1: in function 'renderGears'
	[string "function render()..."]:5: in function 'render'
	[string "render()"]:1: in main chunk






do
  closeBuffer()
  switchToBuffer("synth.lua - command")
end
