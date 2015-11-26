
UiWidgetLib = {}

do
uimainwidget = WidgetGroupLib.new("uigroup")

uimainwidget.update = function (o)
    WidgetGroupLib.update(o)
    WidgetGroupLib.update_cam(o)
  end

uimainwidget.mousemove = WidgetGroupLib.mousemove
uimainwidget.lmbdown = WidgetGroupLib.lmbdown

uimainwidget.rangecheck = WidgetGroupLib.rangecheck_flat
end

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

function addButton(x,y,action)
  local w = WidgetLib2.newSimple("button")
  w.active = false
  w.render = UiWidgetLib.renderButton
  w.lmbdown = function (o,x,y,z)
    if o.active then o.active = false else o.active = true end end
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
local w = uimainwidget.Widgets
w[1] = addButton(0,10,function(b) end)
w[2] = addButton(5,32,function(b) end)
--w[3] = redslider
w[3] = colorwheelgrp
w[4] = dome
end

do
colorwheelgrp.update = function (o) end
colorwheelgrp.lspace = transform.new()
end

do
transform.setTranslation(dome.lspace, 0,0,20)
end

enableStdMouseCam()
disableStdMouseCam()
