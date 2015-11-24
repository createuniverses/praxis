
UiWidgetLib = {}

function UiWidgetLib.renderButton(o)
  beginQuadGL()
    if o.active then
      colorGL(255,155,0,255)
    else
      colorGL(255,0,0,255)
    end
    vectorGL(0,       0, 0)
    vectorGL(o.width, 0, 0)
    vectorGL(o.width, 0, o.depth)
    vectorGL(0,       0, o.depth)
  endGL()
end

do
uimainwidget = WidgetGroupLib.new("uigroup")

uimainwidget.update =
  function(o)
   --print(o.name)
    WidgetGroupLib.update_cam(o)
  end
uimainwidget.update(uimainwidget)

uimainwidget.mousemove = WidgetGroupLib.mousemove
uimainwidget.lmbdown = WidgetGroupLib.lmbdown

Widgets = {}
Widgets[1] = uimainwidget
end

function addButton(x,y,action)
  local w = WidgetLib2.new("button")
  w.active = false
  w.render = UiWidgetLib.renderButton
  uimainwidget.Widgets[1] = w
  w.width = 20
  w.depth = 20
  return w
end

addButton(0,0,function() end)
uimainwidget.Widgets[1] = redslider

continue()
Widgets[2] = spirowidget

function addSlider(x,y,min,max,action)
end
