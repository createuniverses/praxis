
polyw.polyline = makecoldseekerpolyline()
polyw.rpolyline = resamplepolyline(polyw.polyline, 82.093)

print2(inspect(polyw.polyline))
{ {
    x = 300,
    y = 300
  }, {
    x = 420,
    y = 400
  }, {
    x = 500,
    y = 450
  }, {
    x = 580,
    y = 400
  }, {
    x = 700,
    y = 300
  } }


print2(inspect(polyw.rpolyline))
{ {
    x = 300,
    y = 300
  }, {
    x = 363.06558950599,
    y = 352.55465792166
  }, {
    x = 426.76787996088,
    y = 404.22992497555
  }, {
    x = 496.38260473157,
    y = 447.73912795723
  }, {
    x = 565.99732950226,
    y = 408.75166906109
  }, {
    x = 630.38024639062,
    y = 358.01646134115
  }, {
    x = 693.4458358966,
    y = 305.4618034195
  } }
