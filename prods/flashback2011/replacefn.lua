function replaceFunction(fnname, old, new, num)
  --local old = _G[fnname]
  _G[fnname] = function (...)
    new(...)
    num = num - 1
    if num <= 0 then
      _G[fnname] = old
    end
  end
end


