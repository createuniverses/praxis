-- turtletest.lua

useNamespace(turtle)

turtle.turtles = {}
turtle.turtles[1] = lib.new()
turtle.t = turtles[1]

mainturtle = t

Widgets["turtle"] = WidgetLib2.addRender("turtle",
  function (o)
    mainturtle:render()
  end)

useGlobalNamespace()
