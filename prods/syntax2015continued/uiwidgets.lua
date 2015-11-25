
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
  local w = WidgetLib2.newSimple("button")
  w.active = false
  w.render = UiWidgetLib.renderButton
  w.lmbdown = function (o,x,y,z)
    if o.active then o.active = false else o.active = true end end
  uimainwidget.Widgets[1] = w
  w.width = 20
  w.depth = 20
  transform.setTranslation(w.lspace, x,y,0)
  transform.rotate(w.lspace, deg2rad(90), 1,0,0)
  return w
end


button = addButton(0,10,function(b) end)


--uimainwidget.Widgets[1] = redslider


uimainwidget.rangecheck = WidgetGroupLib.rangecheck_flat
enableStdMouseCam()
disableStdMouseCam()


function addSlider(x,y,min,max,action)
end

