-- turtlewidget.lua

turtles = {}
turtles[1] = turtlelib.new()
Widgets["turtle"] = WidgetLib2.addRender("turtle",
  function (o)
    turtles[1]:render()
  end)

t = turtles[1]

print2(getFunction(render))
function render()
  WidgetLib.renderAll()

  --renderGreets2()
  --renderskythings()
  --trace2()
end

showError()
