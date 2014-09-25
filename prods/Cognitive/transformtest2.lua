lookDown()

print2(getFunction("lookDown"))
function lookDown()
  pos = { getCamPos() }
  -- pos[1] = pos[1] + 10 -- look along x
  pos[3] = pos[3] + 10 -- look along z
  lookAt(table.unpack(pos))
  rotateCam(0, math.pi * -0.5)
end

setBufferName("transformtest2.lua")
print2(transform.up(Widgets[1].lspace))

setCamPos(0,10,0)
lookAt(0,10,10)
lookAt(10,10,0)

t = transform.camera()
t2 = transform.cameraBase()

print2(transform.forward(t))

print2(transform.forward(t2))
print2(transform.up(t2))
print2(transform.side(t2))

transform.applyTranslation(t,transform.forward(t))
transform.applyTranslation(t,transform.up(t))
transform.applyTranslation(t,transform.side(t))

do
transform.applyTranslation(t, 
  Vector3D.getArgs(vec3d(transform.forward(t)) * -1))
end

print(Vector3D.getArgs(vec3d(transform.forward(t)) * -1))



showError()
clearError()


transform.copy(t,Widgets[1].lspace)
transform.copy(Widgets[1].lspace,t)

transform.lookAt(Widgets[1].lspace, 0,0,0)
lookAt(0,0,0)









