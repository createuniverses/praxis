-- forth example

setCurrentDir("p4th")
forthreply = forth("loadsys")
setCurrentDir("..")

Widgets["forth"] = WidgetLib2.new("forth")

Widgets["forth"].render = function (o)
  forth("0.0 5.0 0.0 100.0 5.0 100.0 drawline")
end

