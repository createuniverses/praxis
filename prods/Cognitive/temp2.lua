
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

local 

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

inspect = dofile("inspect.lua")


setClipboardText(inspect(keymap))
{
  [22] = {
    [0] = {
      fn = nil -- <function 1>
    }
  },
  [23] = {
    [0] = {
      fn = nil -- <function 2>
    }
  },
  [24] = { {
      fn = nil -- <function 3>,
      program = 'edTypeString("Q")'
    },
    [0] = {
      fn = nil -- <function 4>,
      program = 'edTypeString("q")'
    }
  },
  [25] = { {
      fn = nil -- <function 5>,
      program = 'edTypeString("W")'
    },
    [0] = {
      fn = nil -- <function 6>,
      program = 'edTypeString("w")'
    }
  },
  [26] = { {
      fn = nil -- <function 7>,
      program = 'edTypeString("E")'
    },
    [0] = {
      fn = nil -- <function 8>,
      program = 'edTypeString("e")'
    }
  },
  [27] = { {
      fn = nil -- <function 9>,
      program = 'edTypeString("R")'
    },
    [0] = {
      fn = nil -- <function 10>,
      program = 'edTypeString("r")'
    }
  },
  [28] = { {
      fn = nil -- <function 11>,
      program = 'edTypeString("T")'
    },
    [0] = {
      fn = nil -- <function 12>,
      program = 'edTypeString("t")'
    }
  },
  [29] = { {
      fn = nil -- <function 13>,
      program = 'edTypeString("Y")'
    },
    [0] = {
      fn = nil -- <function 14>,
      program = 'edTypeString("y")'
    }
  },
  [30] = { {
      fn = nil -- <function 15>,
      program = 'edTypeString("U")'
    },
    [0] = {
      fn = nil -- <function 16>,
      program = 'edTypeString("u")'
    }
  },
  [31] = { {
      fn = nil -- <function 17>,
      program = 'edTypeString("I")'
    },
    [0] = {
      fn = nil -- <function 18>,
      program = 'edTypeString("i")'
    }
  },
  [32] = { {
      fn = nil -- <function 19>,
      program = 'edTypeString("O")'
    },
    [0] = {
      fn = nil -- <function 20>,
      program = 'edTypeString("o")'
    }
  },
  [33] = { {
      fn = nil -- <function 21>,
      program = 'edTypeString("P")'
    },
    [0] = {
      fn = nil -- <function 22>,
      program = 'edTypeString("p")'
    }
  },
  [36] = {
    [0] = {
      fn = nil -- <function 23>
    }
  },
  [38] = { {
      fn = nil -- <function 24>,
      program = 'edTypeString("A")'
    },
    [0] = {
      fn = nil -- <function 25>,
      program = 'edTypeString("a")'
    }
  },
  [39] = { {
      fn = nil -- <function 26>,
      program = 'edTypeString("S")'
    },
    [0] = {
      fn = nil -- <function 27>,
      program = 'edTypeString("s")'
    }
  },
  [40] = { {
      fn = nil -- <function 28>,
      program = 'edTypeString("D")'
    },
    [0] = {
      fn = nil -- <function 29>,
      program = 'edTypeString("d")'
    }
  },
  [41] = { {
      fn = nil -- <function 30>,
      program = 'edTypeString("F")'
    },
    [0] = {
      fn = nil -- <function 31>,
      program = 'edTypeString("f")'
    }
  },
  [42] = { {
      fn = nil -- <function 32>,
      program = 'edTypeString("G")'
    },
    [0] = {
      fn = nil -- <function 33>,
      program = 'edTypeString("g")'
    }
  },
  [43] = { {
      fn = nil -- <function 34>,
      program = 'edTypeString("H")'
    },
    [0] = {
      fn = nil -- <function 35>,
      program = 'edTypeString("h")'
    }
  },
  [44] = { {
      fn = nil -- <function 36>,
      program = 'edTypeString("J")'
    },
    [0] = {
      fn = nil -- <function 37>,
      program = 'edTypeString("j")'
    }
  },
  [45] = { {
      fn = nil -- <function 38>,
      program = 'edTypeString("K")'
    },
    [0] = {
      fn = nil -- <function 39>,
      program = 'edTypeString("k")'
    }
  },
  [46] = { {
      fn = nil -- <function 40>,
      program = 'edTypeString("L")'
    },
    [0] = {
      fn = nil -- <function 41>,
      program = 'edTypeString("l")'
    }
  },
  [47] = { {
      fn = nil -- <function 42>,
      program = 'edTypeString(":")'
    },
    [0] = {
      fn = nil -- <function 43>,
      program = 'edTypeString(";")'
    }
  },
  [48] = { {
      program = 'edTypeString(""")'
    } },
  [52] = { {
      fn = nil -- <function 44>,
      program = 'edTypeString("Z")'
    },
    [0] = {
      fn = nil -- <function 45>,
      program = 'edTypeString("z")'
    }
  },
  [53] = { {
      fn = nil -- <function 46>,
      program = 'edTypeString("X")'
    },
    [0] = {
      fn = nil -- <function 47>,
      program = 'edTypeString("x")'
    }
  },
  [54] = { {
      fn = nil -- <function 48>,
      program = 'edTypeString("C")'
    },
    [0] = {
      fn = nil -- <function 49>,
      program = 'edTypeString("c")'
    }
  },
  [55] = { {
      fn = nil -- <function 50>,
      program = 'edTypeString("V")'
    },
    [0] = {
      fn = nil -- <function 51>,
      program = 'edTypeString("v")'
    }
  },
  [56] = { {
      fn = nil -- <function 52>,
      program = 'edTypeString("B")'
    },
    [0] = {
      fn = nil -- <function 53>,
      program = 'edTypeString("b")'
    }
  },
  [57] = { {
      fn = nil -- <function 54>,
      program = 'edTypeString("N")'
    },
    [0] = {
      fn = nil -- <function 55>,
      program = 'edTypeString("n")'
    }
  },
  [58] = { {
      fn = nil -- <function 56>,
      program = 'edTypeString("M")'
    },
    [0] = {
      fn = nil -- <function 57>,
      program = 'edTypeString("m")'
    }
  },
  [59] = { {
      fn = nil -- <function 58>,
      program = 'edTypeString("<")'
    },
    [0] = {
      fn = nil -- <function 59>,
      program = 'edTypeString(",")'
    }
  },
  [60] = { {
      fn = nil -- <function 60>,
      program = 'edTypeString(">")'
    },
    [0] = {
      fn = nil -- <function 61>,
      program = 'edTypeString(".")'
    }
  },
  [61] = { {
      fn = nil -- <function 62>,
      program = 'edTypeString("?")'
    },
    [0] = {
      fn = nil -- <function 63>,
      program = 'edTypeString("/")'
    }
  },
  [65] = {
    [0] = {
      fn = nil -- <function 64>,
      program = 'edTypeString(" ")'
    }
  },
  [111] = {
    [0] = {
      fn = nil -- <function 65>
    }
  },
  [113] = {
    [0] = {
      fn = nil -- <function 66>
    }
  },
  [114] = {
    [0] = {
      fn = nil -- <function 67>
    }
  },
  [116] = {
    [0] = {
      fn = nil -- <function 68>
    }
  },
  [119] = {
    [0] = {
      fn = nil -- <function 69>
    }
  }
}

dofile("editor.lua")
dofile("keymap.lua")
continue()
clearError()
hideTrace()
hideError()
loadBuffer("editor.lua")
newBuffer()


blah

--[[ blah blah ]]
inspect = dofile("inspect.lua")
print2("\\")

setClipboardText(inspect(keymap[51]))
{ {
    fn = nil --[[<function 1>]],
    program = 'edTypeString("|")'
  },
  [0] = {
    fn = nil --[[<function 2>]],
    program = 'edTypeString("\\")'
  }
}

keymap[51] = 
{ {
    fn = nil --[[<function 1>]],
    program = 'edTypeString("|")'
  },
  [0] = {
    fn = loadstring([[edTypeString("\\")]]),
    program = 'edTypeString("\\")'
  }
}

{ {
    program = 'edTypeString("|")'
  },
  [0] = {
    fn = nil,
    program = 'edTypeString("\")'
  }
}


