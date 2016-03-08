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

function rvec3d(radius,angle)
  return Vector3D.new(math.sin(angle) * radius, 0, math.cos(angle) * radius)
end

function cvec3d(v)
  return Vector3D.new(v.x, v.y, v.z)
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
  if getmetatable(s) == Vector3D_meta then
    return Vector3D.new(v.x * s.x, v.y * s.y, v.z * s.z)
  else
    return Vector3D.new(v.x * s, v.y * s, v.z * s)
  end
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

function Vector3D.rotate(p,p1,p2,theta)

   --[[
    q = vec3d(0,0,0)
    r = vec3d(0,0,0)

    r.x = p2.x - p1.x;
    r.y = p2.y - p1.y;
    r.z = p2.z - p1.z;
    p.x = p.x - p1.x;
    p.y = p.y - p1.y;
    p.z = p.z - p1.z;
    ]]
    local q = vec3d(0,0,0)
    local r = p2 - p1
    local p = p - p1
    r = Vector3D.normalize(r);

    local costheta = math.cos(theta);
    local sintheta = math.sin(theta);

    q.x = q.x + (costheta + (1 - costheta) * r.x * r.x) * p.x;
    q.x = q.x + ((1 - costheta) * r.x * r.y - r.z * sintheta) * p.y;
    q.x = q.x + ((1 - costheta) * r.x * r.z + r.y * sintheta) * p.z;

    q.y = q.y + ((1 - costheta) * r.x * r.y + r.z * sintheta) * p.x;
    q.y = q.y + (costheta + (1 - costheta) * r.y * r.y) * p.y;
    q.y = q.y + ((1 - costheta) * r.y * r.z - r.x * sintheta) * p.z;

    q.z = q.z + ((1 - costheta) * r.x * r.z - r.y * sintheta) * p.x;
    q.z = q.z + ((1 - costheta) * r.y * r.z + r.x * sintheta) * p.y;
    q.z = q.z + (costheta + (1 - costheta) * r.z * r.z) * p.z;

    q.x = q.x + p1.x;
    q.y = q.y + p1.y;
    q.z = q.z + p1.z;

    return q;
end

function Vector3D.fromEulerAngles(zero,hdg,pitch)
  local v = zero
  v = Vector3D.rotate(v,vec3d(0,0,0),vec3d(1,0,0),pitch)
  v = Vector3D.rotate(v,vec3d(0,0,0),vec3d(0,1,0),hdg)
  return v
end

