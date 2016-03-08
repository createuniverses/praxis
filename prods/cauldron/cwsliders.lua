
do
redslider = Slider.new(vec3d(0,0,0), 0, 255)
greenslider = Slider.new(vec3d(0,0,0), 0, 255)
blueslider = Slider.new(vec3d(0,0,0), 0, 255)
end

do
redslider.depth = 100
redslider.width = 10
greenslider.depth = 100
greenslider.width = 10
blueslider.depth = 100
blueslider.width = 10

redslider.lspace = transform.new()
transform.rotate(redslider.lspace, deg2rad(-90), 1,0,0)
transform.translate(redslider.lspace, 30,-50,0)

greenslider.lspace = transform.new()
transform.rotate(greenslider.lspace, deg2rad(-90), 1,0,0)
transform.translate(greenslider.lspace, 42,-50,0)

blueslider.lspace = transform.new()
transform.rotate(blueslider.lspace, deg2rad(-90), 1,0,0)
transform.translate(blueslider.lspace, 54,-50,0)
end

function showCWWithSliders()
  colorwheelgrp.Widgets = {}
  colorwheelgrp.Widgets[1] = colorwheelwidget
  colorwheelgrp.Widgets[2] = redslider
  colorwheelgrp.Widgets[3] = greenslider
  colorwheelgrp.Widgets[4] = blueslider
end

function showCW()
  colorwheelgrp.Widgets = {}
  colorwheelgrp.Widgets[1] = colorwheelwidget
end
