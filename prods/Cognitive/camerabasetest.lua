t = transform.camera()
transform.rotate(t, math.pi * 0.01, 0,0,1)

t2 = transform.cameraBase()
transform.rotate(t2, math.pi*0.3,0)
transform.rotate(t2, 0,-math.pi*0.25)

transform.copy(t2, Widgets[1].lspace)
transform.copy(t2, transform.new())

-- testing idea:
-- several squares with random orientation spread
-- out over space, clicking on one will make the
-- base transform match its transform


continue()
clearError()



