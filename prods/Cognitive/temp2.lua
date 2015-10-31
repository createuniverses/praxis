
local ps_orig = fugueUtil.makeRandomPhraseSeq(11)
local ps = balancePhrase(ps_orig)

if ps ~= nil then
  fugueUtil.composeFromPhraseSeq(ps)
  fugue.play()
end

local test = {}
local get = function () return test[1] or test[1][1] end

dofile("replacefn.lua")
dofile("editor.lua")


setClipboardText(getErrorText())

dofile("editor.lua")
dofile("replacefn.lua")
clearTrace()
clearError()

setClipboardText(inspect(keymap))
{
  [22] = {
    [0] = {
      fn = <function 1>
    }
  }, dofile("texture.lua")
  [23] = {
    [0] = {
      fn = <function 2>
    }
  },
  [24] = {
    [0] = {
      fn = <function 3>,
      program = 'edTypeString("q")' setMaxFramerate(30)
    }
  },
  [25] = {
    [0] = {
      fn = <function 4>,
      program = 'edTypeString("w")'
    }
  },
  [26] = {
    [0] = {
      fn = <function 5>,
      program = 'edTypeString("e")'
    }
  },
  [36] = {
    [0] = {
      fn = <function 6>
    }
  },
  [111] = {
    [0] = {
      fn = <function 7>
    }
  },
  [113] = {
    [0] = {
      fn = <function 8>
    }
  },
  [114] = {
    [0] = {
      fn = <function 9>
    }
  },
  [116] = {
    [0] = {
      fn = <function 10>
    }
  },
  [119] = {
    [0] = {
      fn = <function 11>
    }
  }
}{
  [22] = {
    [0] = {
      fn = <function 1>
    }
  },
  [23] = {
    [0] = {
      fn = <function 2>
    }
  },
  [24] = {
    [0] = {
      fn = <function 3>,
      program = "edTypeString(string.char(113))"
    }
  },
  [25] = {
    [0] = {
      fn = <function 4>,
      program = "edTypeString(string.char(119))"
    }
  },
  [26] = {
    [0] = {
      fn = <function 5>,
      program = "edTypeString(string.char(101))"
    }
  },
  [27] = {
    [0] = {
      fn = <function 6>,
      program = "edTypeString(string.char(114))"
    }
  },
  [28] = {
    [0] = {
      fn = <function 7>,
      program = "edTypeString(string.char(116))"
    }
  },
  [29] = {
    [0] = {
      fn = <function 8>,
      program = "edTypeString(string.char(121))"
    }
  },
  [30] = {
    [0] = {
      fn = <function 9>,
      program = "edTypeString(string.char(117))"
    }
  },
  [31] = {
    [0] = {
      fn = <function 10>,
      program = "edTypeString(string.char(105))"
    }
  },
  [32] = {
    [0] = {
      fn = <function 11>,
      program = "edTypeString(string.char(111))"
    }
  },
  [33] = {
    [0] = {
      fn = <function 12>,
      program = "edTypeString(string.char(112))"
    }
  },
  [36] = {
    [0] = {
      fn = <function 13>
    }
  },
  [111] = {
    [0] = {
      fn = <function 14>
    }
  },
  [113] = {
    [0] = {
      fn = <function 15>
    }
  },
  [114] = {
    [0] = {
      fn = <function 16>
    }
  },
  [116] = {
    [0] = {
      fn = <function 17>
    }
  },
  [119] = {
    [0] = {
      fn = <function 18>
    }
  }
}

showEditor()


print2(keymap[24][0].program)

setBufferText("")

for i=1,10,1 do
  print2(i .. "\n")
end

printf("hi there how are you")
print2("hello there\n")

newBuffer()

setBufferText((getFunction(f6Pressed)))
setBufferText("test")
setClipboardText("test")

luaCall(getBufferText())

setClipboardText(getFunction(f6Pressed))
setClipboardText(getErrorText())

setBufferText(getClipboardText())

setClipboardText(inspect(keymap))

dofile("editor.lua")

dofile("replacefn.lua")
            --[[edTypeString(string.char(]]..k2..[[))]])

function testfn(n)
  print2("hello there " .. n)
end

testfn(238)

replaceFunction("testfn", function (n) print2("temp hello " .. n) end, 5)
replaceFunction("render", function () drawLine(0,0,0,100,100,100) end, 100)

setClipboardText(getFunction(keymap[45][0]))

hideEditor()
clearTrace()

local n,v = debug.getupvalue(renderTextures,1)
setClipboardText(""..n..","..v)
i,4
i,2

local s = string.dump(renderTextures)
setClipboardText(string.len(s))
911

math.random(20) + 40,
setClipboardText(getErrorText())
clearError()

local fn = loadstring([[print2("test")]])
fn()

