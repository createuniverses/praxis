local lpeg = require'lpeg'
local P, R, S = lpeg.P, lpeg.R, lpeg.S --patterns
local C, Ct = lpeg.C, lpeg.Ct --capture
local V = lpeg.V --variable

local parser = P {
  'program', -- initial rule
  program   = Ct(V'sexpr' ^ 0),
  wspace    = S' \n\r\t' ^ 0,
  atom      = V'boolean' + V'integer' + V'string' + V'symbol',
    symbol  = C((1 - S' \n\r\t"\'()[]{}#@~') ^ 1) /
              function(s) return _G[s] end,
    boolean = C(P'true' + P'false') /
              function(x) return x == 'true' end,
    integer = C(R'19' * R'09' ^ 0) /
              tonumber,
    string  = P'"' * C((1 - S'"\n\r') ^ 0) * P'"',
  coll      = V'list' + V'array',
    list    = P'\'(' * Ct(V'expr' ^ 1) * P')',
    array   = P'[' * Ct(V'expr' ^ 1) * P']',
  expr      = V'wspace' * (V'coll' + V'atom' + V'sexpr'),
  sexpr     = V'wspace' * P'(' * V'symbol' * Ct(V'expr' ^ 0) * P')' /
              function(f, ...) return f(...) end
}

--some "built-ins"
reduce = function(f, list)
  for i, v in ipairs(list) do
    if i == 1 then
      head = v
    else
      head = f(head, v)
    end
  end
  return head
end

def = function(k, v) _G[k] = v end
def('+',   function(...) return reduce(function(a, b) return a + b end, ...) end)
def('-',   function(...) return reduce(function(a, b) return a - b end, ...) end)
def('*',   function(...) return reduce(function(a, b) return a * b end, ...) end)
def('/',   function(...) return reduce(function(a, b) return a / b end, ...) end)
def('str', function(...) return reduce(function(a, b) return tostring(a)..tostring(b) end, ...) end)
def('not', function(a) return not a end)

-- sample

t = parser:match([[

(* 2 3 4 (+ 10
            20))
(def yay true)
(not yay)
(str "lua" "lisp" "!")

]])

--require 'pl.pretty'.dump(t)
