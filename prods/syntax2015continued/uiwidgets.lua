
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

uimainwidget.rangecheck = WidgetGroupLib.rangecheck_flat
end

do
Widgets = {}
Widgets[1] = spirowidget
Widgets[2] = airplane
--Widgets[3] = camwidget
Widgets[4] = uimainwidget
end

function addButton(x,y,action)
  local w = WidgetLib2.newSimple("button")
  w.active = false
  w.render = UiWidgetLib.renderButton
  w.lmbdown = function (o,x,y,z)
    if o.active then o.active = false else o.active = true end end
  table.insert(uimainwidget.Widgets,w)
  w.width = 20
  w.depth = 20
  transform.setTranslation(w.lspace, x,y,0)
  transform.rotate(w.lspace, deg2rad(90), 1,0,0)
  return w
end

function addSlider(x,y,min,max,action)
end

do
uimainwidget.Widgets = {}
addButton(0,10,function(b) end)
addButton(0,32,function(b) end)
table.insert(uimainwidget.Widgets, colorwheelgrp)
--table.insert(uimainwidget.Widgets, redslider)

colorwheelgrp.update = function (o) end
colorwheelgrp.lspace = transform.new()
table.insert(uimainwidget.Widgets, colorwheelgrp)

table.insert(uimainwidget.Widgets, dome)
transform.setTranslation(dome.lspace, 0,0,20)
dome.update(dome)
end

enableStdMouseCam()
disableStdMouseCam()

