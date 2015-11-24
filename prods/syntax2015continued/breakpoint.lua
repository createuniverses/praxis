print_clean = print
print = print_clean

function print(...)
  s = {...}
  s = s[1]
  if s=="test" then
    print_clean(debug.traceback())
    error("stopped")
  else
    print_clean(...)
  end
end
