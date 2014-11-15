-- instersection.lua

function IsEquivalent(n1, n2, tol)
  if math.abs(n1 - n2) < tol then
    return true
  else
    return false
  end
end

function newIntResults()
  local r = {}
  r.p1 = vec3d(0,0,0)
  r.p2 = vec3d(0,0,0)
  r.status = ""
  return r
end

function lineline(l1, l2)
  local result = newIntResults()
  
  local denom =  ((l2.b.z - l2.a.z)*(l1.b.x - l1.a.x)) -
          ((l2.b.x - l2.a.x)*(l1.b.z - l1.a.z));

  local nume_a =  ((l2.b.x - l2.a.x)*(l1.a.z - l2.a.z)) -
          ((l2.b.z - l2.a.z)*(l1.a.x - l2.a.x));

  local nume_b =  ((l1.b.x - l1.a.x)*(l1.a.z - l2.a.z)) -
          ((l1.b.z - l1.a.z)*(l1.a.x - l2.a.x));

  local bDenominatorZero = (IsEquivalent(denom, 0.0, 0.00001));

  if bDenominatorZero then
    nNumIntersections = 0

    local bNumeratorsZero = (IsEquivalent(nume_a, 0.0, 0.00001) and IsEquivalent(nume_b, 0.0, 0.00001))

    if bNumeratorsZero then result.status = "CoincidentLines"
    else result.status = "ParallelLines"
    end
    
    return result
  end

  local ua = nume_a / denom;
  local ub = nume_b / denom;

  nNumIntersections = 1;
  

  result.p1.x = l1.a.x + ua*(l1.b.x - l1.a.x);
  result.p1.z = l1.a.z + ua*(l1.b.z - l1.a.z);

  local bWithin1 = (ua >= 0.0 and ua <= 1.0);
  local bWithin2 = (ub >= 0.0 and ub <= 1.0);

  if bWithin1 and bWithin2 then result.status = "IntersectingWithinBothSegments"
  elseif bWithin1 then result.status = "IntersectingWithinSegment1Only"
  elseif bWithin2 then result.status = "IntersectingWithinSegment2Only"
  else result.status = "IntersectingWithinNeither"
  end
  
  return result
end

function linecircle(l1, c1)
  local result = newIntResults()
  
	-- XYZ p1,XYZ p2,XYZ sc,double r,double *mu1,double *mu2

	local m1,m2;

	local a,b,c;
	local bb4ac;  
	local sqrtbb4ac;
	local dp = vec3d(0,0,0);

	dp.x = l1.b.x - l1.a.x;
	dp.z = l1.b.z - l1.a.z;

	a = dp.x * dp.x + dp.z * dp.z;
	b = 2 * (dp.x * (l1.a.x - c1.c.x) + dp.z * (l1.a.z - c1.c.z));
	c = c1.c.x * c1.c.x + c1.c.z * c1.c.z;
	c = c + (l1.a.x * l1.a.x + l1.a.z * l1.a.z)
	c = c - (2 * (c1.c.x * l1.a.x + c1.c.z * l1.a.z))
	c = c - (c1.r * c1.r)

	bb4ac = b * b - 4 * a * c;

	if IsEquivalent(a,0.0, 0.0001) or bb4ac < 0.0 then
		m1 = 0
		m2 = 0

		nNumIntersections = 0;
    result.status = "LineMissesCircle"
    return result
	end

	sqrtbb4ac = math.sqrt(bb4ac);

	m1 = (-b + sqrtbb4ac) / (2 * a);
	m2 = (-b - sqrtbb4ac) / (2 * a);

	result.p1 = l1.a + (l1.b - l1.a) * m1;
	result.p2 = l1.a + (l1.b - l1.a) * m2;

	if IsEquivalent(sqrtbb4ac, 0.0, 0.0001) then nNumIntersections = 1 else nNumIntersections = 2 end

  local bWithin1 = (m1 >= 0.0 and m1 <= 1.0)
	local bWithin2 = (m2 >= 0.0 and m2 <= 1.0)

	if bWithin1 and bWithin2 then	result.status = "IntersectingBothWithinSegment"
	elseif bWithin1 then          result.status = "IntersectingOnly1stWithin"
	elseif bWithin2 then          result.status = "IntersectingOnly2ndWithin"
	else                          result.status = "IntersectingNeitherWithin" end
  
  return result
end

function circlecircle(c1,c2)
  local result = newIntResults()
  
	local a, dx, dy, d, h, rx, ry;
	local x2, y2;

	-- dx and dy are the vertical and horizontal distances between
	-- the circle centers.
	dx = c2.c.x - c1.c.x;
	dy = c2.c.z - c1.c.z;

	-- Determine the straight-line distance between the centers.
	d = math.sqrt((dy*dy) + (dx*dx))

	-- Check for solvability.
	if d > (c1.r + c2.r) then
		-- no solution. circles do not intersect.
		nNumIntersections = 0
    result.status = "CirclesApart"
    return result
  end

	if IsEquivalent(d, 0.0, 0.0001) then
		nNumIntersections = 0

		if IsEquivalent(c1.r, c2.r, 0.0001) then
      result.status = "CoincidentCircles"
      return result
    end
    
    result.status = "ConcentricCircles"
    return result
  end

	if d < math.abs(c1.r - c2.r) then
		-- no solution. one circle is contained in the other
		nNumIntersections = 0;
		result.status = "CircleWithinCircle"
    return result
	end

	-- 'point 2' is the point where the line through the circle
	-- intersection points crosses the line between the circle
	-- centers.  

	-- Determine the distance from point 0 to point 2.
	a = ((c1.r*c1.r) - (c2.r*c2.r) + (d*d)) / (2.0 * d)

	-- Determine the coordinates of point 2.
	x2 = c1.c.x + (dx * a/d);
	y2 = c1.c.z + (dy * a/d);

	-- Determine the distance from point 2 to either of the
	-- intersection points.
	h = math.sqrt((c1.r*c1.r) - (a*a));

	-- Now determine the offsets of the intersection points from
	-- point 2.
	rx = -dy * (h/d);
	ry = dx * (h/d);

	if IsEquivalent(d, c1.r + c2.r, 0.0001) then nNumIntersections = 1 else nNumIntersections = 2 end

	-- Determine the absolute intersection points.
	result.p1.x = x2 + rx;
	result.p1.z = y2 + ry;

	result.p2.x = x2 - rx;
	result.p2.z = y2 - ry;

	result.status = "Intersecting"
  return result
end

