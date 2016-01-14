
local fn1
local fn2
local fn3

fn1 = function ()
  fn2()
end

fn2 = function ()
  fn3()
end

fn3 = function ()
  j = j + 1
end

for i=1,10,1 do
  print(i)
  fn1()
end
