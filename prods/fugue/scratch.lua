
dofile("brat.lua")

brat.init()

function fugue.renderDemiurge()
  colorGL(255, 200, 200)
  --vehicles[math.random(100)].pos.x = fugue.demiurge[1].x
  --vehicles[math.random(100)].pos.y = fugue.demiurge[1].y
  --vehicles[math.random(100)].pos.z = fugue.demiurge[1].z
  brat.update()
  brat.render()
end
