-- Name: geometry.lua

Vector3D = {}

local Vector3D_meta = {}

function Vector3D.new(x,y,z)
  local vec = {x=x,y=y,z=z}
  setmetatable(vec, Vector3D_meta)
  return vec
end

function vec2d(x,z)
  return Vector3D.new(x, 0, z)
end

function vec3d(x,y,z)
  return Vector3D.new(x, y, z)
end

function Vector3D.getArgs(v)
  return v.x,v.y,v.z
end

function Vector3D.set(v,x,y,z)
  v.x = x
  v.y = y
  v.z = z
end

function Vector3D.magnitude(v)
  return math.sqrt(v.x * v.x + v.y * v.y + v.z * v.z)
end

function Vector3D.normalize(v)
  mag = Vector3D.magnitude(v)
  return v * (1/mag)
end

function Vector3D.add(a,b)
  return Vector3D.new(a.x + b.x, a.y + b.y, a.z + b.z)
end

function Vector3D.sub(a,b)
  return Vector3D.new(a.x - b.x, a.y - b.y, a.z - b.z)
end

function Vector3D.scale(v,s)
  return Vector3D.new(v.x * s, v.y * s, v.z * s)
end

function Vector3D.ortho2D(v)
  return Vector3D.new(-v.z, v.y, v.x)
end

function Vector3D.dot(a, b)
  return a.x * b.x + a.y * b.y + a.z * b.z
end

function Vector3D.cross(a, b)
  return Vector3D.new(a.y * b.z - a.z * b.y,
                      a.z * b.x - a.x * b.z,
                      a.x * b.y - a.y * b.x)
end

Vector3D_meta.__add = Vector3D.add
Vector3D_meta.__sub = Vector3D.sub
Vector3D_meta.__mul = Vector3D.scale

function Vector3D_meta.__index(tbl,key)
  return Vector3D[key]
end

function calcCurve(p1, p2, t)
  t2 = t*t
  t3 = t2*t
  return p1.p * (  2*t3 - 3*t2 + 1) +
         p2.p * ( -2*t3 + 3*t2    ) +
         p1.t * (    t3 - 2*t2 + t) +
         p2.t * (    t3 -   t2    )
end
