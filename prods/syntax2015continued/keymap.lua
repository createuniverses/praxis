
--setClipboardText(inspect(keymap))

keymap = {}

if platform() == "windows" then
keymap = 
{
  [8] = {
    [0] = {
      fn = nil --[[<function 1>]],
      program = "edBackspace()"
    }
  },
  [9] = {
    [0] = {
      fn = nil --[[<function 2>]],
      program = "edTab()"
    }
  },
  [13] = {
    [0] = {
      fn = nil --[[<function 3>]],
      program = "edInsertNewline()"
    }
  },
  [32] = {
    [0] = {
      fn = nil --[[<function 4>]],
      program = 'edTypeString(" ")'
    }
  },
  [37] = {
    [0] = {
      fn = nil --[[<function 5>]],
      program = "edSetPosition(edGetLeft(edGetPosition()))"
    }
  },
  [38] = {
    [0] = {
      fn = nil --[[<function 6>]],
      program = "edSetPosition(edGetUp(edGetPosition()))"
    }
  },
  [39] = {
    [0] = {
      fn = nil --[[<function 7>]],
      program = "edSetPosition(edGetRight(edGetPosition()))"
    }
  },
  [40] = {
    [0] = {
      fn = nil --[[<function 8>]],
      program = "edSetPosition(edGetDown(edGetPosition()))"
    }
  },
  [46] = {
    [0] = {
      fn = nil --[[<function 9>]],
      program = "edDelete()"
    }
  },
  [48] = { {
      fn = nil --[[<function 10>]],
      program = 'edTypeString(")")'
    },
    [0] = {
      fn = nil --[[<function 11>]],
      program = 'edTypeString("0")'
    }
  },
  [49] = { {
      fn = nil --[[<function 12>]],
      program = 'edTypeString("!")'
    },
    [0] = {
      fn = nil --[[<function 13>]],
      program = 'edTypeString("1")'
    }
  },
  [50] = { {
      fn = nil --[[<function 14>]],
      program = 'edTypeString("@")'
    },
    [0] = {
      fn = nil --[[<function 15>]],
      program = 'edTypeString("2")'
    }
  },
  [51] = { {
      fn = nil --[[<function 16>]],
      program = 'edTypeString("#")'
    },
    [0] = {
      fn = nil --[[<function 17>]],
      program = 'edTypeString("3")'
    }
  },
  [52] = { {
      fn = nil --[[<function 18>]],
      program = 'edTypeString("$")'
    },
    [0] = {
      fn = nil --[[<function 19>]],
      program = 'edTypeString("4")'
    }
  },
  [53] = { {
      fn = nil --[[<function 20>]],
      program = 'edTypeString("%")'
    },
    [0] = {
      fn = nil --[[<function 21>]],
      program = 'edTypeString("5")'
    }
  },
  [54] = { {
      fn = nil --[[<function 22>]],
      program = 'edTypeString("^")'
    },
    [0] = {
      fn = nil --[[<function 23>]],
      program = 'edTypeString("6")'
    }
  },
  [55] = { {
      fn = nil --[[<function 24>]],
      program = 'edTypeString("&")'
    },
    [0] = {
      fn = nil --[[<function 25>]],
      program = 'edTypeString("7")'
    }
  },
  [56] = { {
      fn = nil --[[<function 26>]],
      program = 'edTypeString("*")'
    },
    [0] = {
      fn = nil --[[<function 27>]],
      program = 'edTypeString("8")'
    }
  },
  [57] = { {
      fn = nil --[[<function 28>]],
      program = 'edTypeString("(")'
    },
    [0] = {
      fn = nil --[[<function 29>]],
      program = 'edTypeString("9")'
    }
  },
  [65] = { {
      fn = nil --[[<function 30>]],
      program = 'edTypeString("A")'
    },
    [0] = {
      fn = nil --[[<function 31>]],
      program = 'edTypeString("a")'
    }
  },
  [66] = { {
      fn = nil --[[<function 32>]],
      program = 'edTypeString("B")'
    },
    [0] = {
      fn = nil --[[<function 33>]],
      program = 'edTypeString("b")'
    }
  },
  [67] = { {
      fn = nil --[[<function 34>]],
      program = 'edTypeString("C")'
    },
    [0] = {
      fn = nil --[[<function 35>]],
      program = 'edTypeString("c")'
    }
  },
  [68] = { {
      fn = nil --[[<function 36>]],
      program = 'edTypeString("D")'
    },
    [0] = {
      fn = nil --[[<function 37>]],
      program = 'edTypeString("d")'
    }
  },
  [69] = { {
      fn = nil --[[<function 38>]],
      program = 'edTypeString("E")'
    },
    [0] = {
      fn = nil --[[<function 39>]],
      program = 'edTypeString("e")'
    }
  },
  [70] = { {
      fn = nil --[[<function 40>]],
      program = 'edTypeString("F")'
    },
    [0] = {
      fn = nil --[[<function 41>]],
      program = 'edTypeString("f")'
    }
  },
  [71] = { {
      fn = nil --[[<function 42>]],
      program = 'edTypeString("G")'
    },
    [0] = {
      fn = nil --[[<function 43>]],
      program = 'edTypeString("g")'
    }
  },
  [72] = { {
      fn = nil --[[<function 44>]],
      program = 'edTypeString("H")'
    },
    [0] = {
      fn = nil --[[<function 45>]],
      program = 'edTypeString("h")'
    }
  },
  [73] = { {
      fn = nil --[[<function 46>]],
      program = 'edTypeString("I")'
    },
    [0] = {
      fn = nil --[[<function 47>]],
      program = 'edTypeString("i")'
    }
  },
  [74] = { {
      fn = nil --[[<function 48>]],
      program = 'edTypeString("J")'
    },
    [0] = {
      fn = nil --[[<function 49>]],
      program = 'edTypeString("j")'
    }
  },
  [75] = { {
      fn = nil --[[<function 50>]],
      program = 'edTypeString("K")'
    },
    [0] = {
      fn = nil --[[<function 51>]],
      program = 'edTypeString("k")'
    }
  },
  [76] = { {
      fn = nil --[[<function 52>]],
      program = 'edTypeString("L")'
    },
    [0] = {
      fn = nil --[[<function 53>]],
      program = 'edTypeString("l")'
    }
  },
  [77] = { {
      fn = nil --[[<function 54>]],
      program = 'edTypeString("M")'
    },
    [0] = {
      fn = nil --[[<function 55>]],
      program = 'edTypeString("m")'
    }
  },
  [78] = { {
      fn = nil --[[<function 56>]],
      program = 'edTypeString("N")'
    },
    [0] = {
      fn = nil --[[<function 57>]],
      program = 'edTypeString("n")'
    }
  },
  [79] = { {
      fn = nil --[[<function 58>]],
      program = 'edTypeString("O")'
    },
    [0] = {
      fn = nil --[[<function 59>]],
      program = 'edTypeString("o")'
    }
  },
  [80] = { {
      fn = nil --[[<function 60>]],
      program = 'edTypeString("P")'
    },
    [0] = {
      fn = nil --[[<function 61>]],
      program = 'edTypeString("p")'
    }
  },
  [81] = { {
      fn = nil --[[<function 62>]],
      program = 'edTypeString("Q")'
    },
    [0] = {
      fn = nil --[[<function 63>]],
      program = 'edTypeString("q")'
    }
  },
  [82] = { {
      fn = nil --[[<function 64>]],
      program = 'edTypeString("R")'
    },
    [0] = {
      fn = nil --[[<function 65>]],
      program = 'edTypeString("r")'
    }
  },
  [83] = { {
      fn = nil --[[<function 66>]],
      program = 'edTypeString("S")'
    },
    [0] = {
      fn = nil --[[<function 67>]],
      program = 'edTypeString("s")'
    }
  },
  [84] = { {
      fn = nil --[[<function 68>]],
      program = 'edTypeString("T")'
    },
    [0] = {
      fn = nil --[[<function 69>]],
      program = 'edTypeString("t")'
    }
  },
  [85] = { {
      fn = nil --[[<function 70>]],
      program = 'edTypeString("U")'
    },
    [0] = {
      fn = nil --[[<function 71>]],
      program = 'edTypeString("u")'
    }
  },
  [86] = { {
      fn = nil --[[<function 72>]],
      program = 'edTypeString("V")'
    },
    [0] = {
      fn = nil --[[<function 73>]],
      program = 'edTypeString("v")'
    }
  },
  [87] = { {
      fn = nil --[[<function 74>]],
      program = 'edTypeString("W")'
    },
    [0] = {
      fn = nil --[[<function 75>]],
      program = 'edTypeString("w")'
    }
  },
  [88] = { {
      fn = nil --[[<function 76>]],
      program = 'edTypeString("X")'
    },
    [0] = {
      fn = nil --[[<function 77>]],
      program = 'edTypeString("x")'
    }
  },
  [89] = { {
      fn = nil --[[<function 78>]],
      program = 'edTypeString("Y")'
    },
    [0] = {
      fn = nil --[[<function 79>]],
      program = 'edTypeString("y")'
    }
  },
  [90] = { {
      fn = nil --[[<function 80>]],
      program = 'edTypeString("Z")'
    },
    [0] = {
      fn = nil --[[<function 81>]],
      program = 'edTypeString("z")'
    }
  },
  [186] = { {
      fn = nil --[[<function 82>]],
      program = 'edTypeString(":")'
    },
    [0] = {
      fn = nil --[[<function 83>]],
      program = 'edTypeString(";")'
    }
  },
  [187] = { {
      fn = nil --[[<function 84>]],
      program = 'edTypeString("+")'
    },
    [0] = {
      fn = nil --[[<function 85>]],
      program = 'edTypeString("=")'
    }
  },
  [188] = { {
      fn = nil --[[<function 86>]],
      program = 'edTypeString("<")'
    },
    [0] = {
      fn = nil --[[<function 87>]],
      program = 'edTypeString(",")'
    }
  },
  [189] = { {
      fn = nil --[[<function 88>]],
      program = 'edTypeString("_")'
    },
    [0] = {
      fn = nil --[[<function 89>]],
      program = 'edTypeString("-")'
    }
  },
  [190] = { {
      fn = nil --[[<function 90>]],
      program = 'edTypeString(">")'
    },
    [0] = {
      fn = nil --[[<function 91>]],
      program = 'edTypeString(".")'
    }
  },
  [191] = { {
      fn = nil --[[<function 92>]],
      program = 'edTypeString("?")'
    },
    [0] = {
      fn = nil --[[<function 93>]],
      program = 'edTypeString("/")'
    }
  },
  [192] = { {
      fn = nil --[[<function 94>]],
      program = 'edTypeString("~")'
    },
    [0] = {
      fn = nil --[[<function 95>]],
      program = 'edTypeString("`")'
    }
  },
  [219] = { {
      fn = nil --[[<function 96>]],
      program = 'edTypeString("{")'
    },
    [0] = {
      fn = nil --[[<function 97>]],
      program = 'edTypeString("[")'
    }
  },
  [220] = { {
      fn = nil --[[<function 98>]],
      program = 'edTypeString("|")'
    },
    [0] = {
      fn = nil --[[<function 99>]],
      program = "edTypeString(string.char(92))" -- slash
    }
  },
  [221] = { {
      fn = nil --[[<function 100>]],
      program = 'edTypeString("}")'
    },
    [0] = {
      fn = nil --[[<function 101>]],
      program = 'edTypeString("]")'
    }
  },
  [222] = { {
      fn = nil --[[<function 102>]],
      program = "edTypeString(string.char(34))" -- double quote
    },
    [0] = {
      fn = nil --[[<function 103>]],
      program = "edTypeString(\"'\")"
    }
  }
}
end

if platform() == "linux" then
keymap = 
{
  [10] = { {
      program = 'edTypeString("!")'
    },
    [0] = {
      fn = nil --[[<function 1>]],
      program = 'edTypeString("1")'
    }
  },
  [11] = { {
      program = 'edTypeString("@")'
    },
    [0] = {
      fn = nil --[[<function 2>]],
      program = 'edTypeString("2")'
    }
  },
  [12] = { {
      program = 'edTypeString("#")'
    },
    [0] = {
      program = 'edTypeString("3")'
    }
  },
  [13] = { {
      program = 'edTypeString("$")'
    },
    [0] = {
      program = 'edTypeString("4")'
    }
  },
  [14] = { {
      program = 'edTypeString("%")'
    },
    [0] = {
      program = 'edTypeString("5")'
    }
  },
  [15] = { {
      program = 'edTypeString("^")'
    },
    [0] = {
      program = 'edTypeString("6")'
    }
  },
  [16] = { {
      program = 'edTypeString("&")'
    },
    [0] = {
      program = 'edTypeString("7")'
    }
  },
  [17] = { {
      program = 'edTypeString("*")'
    },
    [0] = {
      program = 'edTypeString("8")'
    }
  },
  [18] = { {
      fn = nil --[[<function 3>]],
      program = 'edTypeString("(")'
    },
    [0] = {
      program = 'edTypeString("9")'
    }
  },
  [19] = { {
      fn = nil --[[<function 4>]],
      program = 'edTypeString(")")'
    },
    [0] = {
      fn = nil --[[<function 5>]],
      program = 'edTypeString("0")'
    }
  },
  [20] = { {
      program = 'edTypeString("_")'
    },
    [0] = {
      program = 'edTypeString("-")'
    }
  },
  [21] = { {
      program = 'edTypeString("+")'
    },
    [0] = {
      fn = nil --[[<function 6>]],
      program = 'edTypeString("=")'
    }
  },
  [22] = {
    [0] = {
      fn = nil --[[<function 7>]],
      program = "edBackspace()"
    }
  },
  [23] = {
    [0] = {
      program = "edTab()"
    }
  },
  [24] = { {
      program = 'edTypeString("Q")'
    },
    [0] = {
      program = 'edTypeString("q")'
    }
  },
  [25] = { {
      program = 'edTypeString("W")'
    },
    [0] = {
      program = 'edTypeString("w")'
    }
  },
  [26] = { {
      program = 'edTypeString("E")'
    },
    [0] = {
      fn = nil --[[<function 8>]],
      program = 'edTypeString("e")'
    }
  },
  [27] = { {
      program = 'edTypeString("R")'
    },
    [0] = {
      fn = nil --[[<function 9>]],
      program = 'edTypeString("r")'
    }
  },
  [28] = { {
      program = 'edTypeString("T")'
    },
    [0] = {
      fn = nil --[[<function 10>]],
      program = 'edTypeString("t")'
    }
  },
  [29] = { {
      program = 'edTypeString("Y")'
    },
    [0] = {
      program = 'edTypeString("y")'
    }
  },
  [30] = { {
      program = 'edTypeString("U")'
    },
    [0] = {
      program = 'edTypeString("u")'
    }
  },
  [31] = { {
      program = 'edTypeString("I")'
    },
    [0] = {
      fn = nil --[[<function 11>]],
      program = 'edTypeString("i")'
    }
  },
  [32] = { {
      program = 'edTypeString("O")'
    },
    [0] = {
      fn = nil --[[<function 12>]],
      program = 'edTypeString("o")'
    }
  },
  [33] = { {
      program = 'edTypeString("P")'
    },
    [0] = {
      fn = nil --[[<function 13>]],
      program = 'edTypeString("p")'
    }
  },
  [34] = { {
      program = 'edTypeString("{")'
    },
    [0] = {
      program = 'edTypeString("[")'
    }
  },
  [35] = { {
      program = 'edTypeString("}")'
    },
    [0] = {
      program = 'edTypeString("]")'
    }
  },
  [36] = {
    [0] = {
      fn = nil --[[<function 14>]],
      program = "edInsertNewline()"
    }
  },
  [38] = { {
      program = 'edTypeString("A")'
    },
    [0] = {
      program = 'edTypeString("a")'
    }
  },
  [39] = { {
      program = 'edTypeString("S")'
    },
    [0] = {
      program = 'edTypeString("s")'
    }
  },
  [40] = { {
      program = 'edTypeString("D")'
    },
    [0] = {
      fn = nil --[[<function 15>]],
      program = 'edTypeString("d")'
    }
  },
  [41] = { {
      program = 'edTypeString("F")'
    },
    [0] = {
      fn = nil --[[<function 16>]],
      program = 'edTypeString("f")'
    }
  },
  [42] = { {
      program = 'edTypeString("G")'
    },
    [0] = {
      program = 'edTypeString("g")'
    }
  },
  [43] = { {
      program = 'edTypeString("H")'
    },
    [0] = {
      fn = nil --[[<function 17>]],
      program = 'edTypeString("h")'
    }
  },
  [44] = { {
      program = 'edTypeString("J")'
    },
    [0] = {
      program = 'edTypeString("j")'
    }
  },
  [45] = { {
      program = 'edTypeString("K")'
    },
    [0] = {
      program = 'edTypeString("k")'
    }
  },
  [46] = { {
      program = 'edTypeString("L")'
    },
    [0] = {
      fn = nil --[[<function 18>]],
      program = 'edTypeString("l")'
    }
  },
  [47] = { {
      program = 'edTypeString(":")'
    },
    [0] = {
      program = 'edTypeString(";")'
    }
  },
  [48] = { {
      fn = nil --[[<function 19>]],
      program = "edTypeString(string.char(34))" -- double quote
    },
    [0] = {
      fn = nil --[[<function 20>]],
      program = "edTypeString(\"'\")"
    }
  },
  [49] = { {
      program = 'edTypeString("~")'
    },
    [0] = {
      program = 'edTypeString("`")'
    }
  },
  [51] = { {
      program = 'edTypeString("|")'
    },
    [0] = {
      program = "edTypeString(string.char(92))" -- slash
    }
  },
  [52] = { {
      program = 'edTypeString("Z")'
    },
    [0] = {
      program = 'edTypeString("z")'
    }
  },
  [53] = { {
      program = 'edTypeString("X")'
    },
    [0] = {
      program = 'edTypeString("x")'
    }
  },
  [54] = { {
      program = 'edTypeString("C")'
    },
    [0] = {
      program = 'edTypeString("c")'
    }
  },
  [55] = { {
      program = 'edTypeString("V")'
    },
    [0] = {
      program = 'edTypeString("v")'
    }
  },
  [56] = { {
      program = 'edTypeString("B")'
    },
    [0] = {
      program = 'edTypeString("b")'
    }
  },
  [57] = { {
      program = 'edTypeString("N")'
    },
    [0] = {
      fn = nil --[[<function 21>]],
      program = 'edTypeString("n")'
    }
  },
  [58] = { {
      program = 'edTypeString("M")'
    },
    [0] = {
      fn = nil --[[<function 22>]],
      program = 'edTypeString("m")'
    }
  },
  [59] = { {
      program = 'edTypeString("<")'
    },
    [0] = {
      fn = nil --[[<function 23>]],
      program = 'edTypeString(",")'
    }
  },
  [60] = { {
      program = 'edTypeString(">")'
    },
    [0] = {
      fn = nil --[[<function 24>]],
      program = 'edTypeString(".")'
    }
  },
  [61] = { {
      program = 'edTypeString("?")'
    },
    [0] = {
      program = 'edTypeString("/")'
    }
  },
  [65] = {
    [0] = {
      fn = nil --[[<function 25>]],
      program = 'edTypeString(" ")'
    }
  },
  [67] = {
    [0] = {
      fn = nil --[[<function 26>]],
      program = "f1Pressed()"
    }
  },
  [111] = {
    [0] = {
      program = "edSetPosition(edGetUp(edGetPosition()))"
    }
  },
  [113] = {
    [0] = {
      program = "edSetPosition(edGetLeft(edGetPosition()))"
    }
  },
  [114] = {
    [0] = {
      program = "edSetPosition(edGetRight(edGetPosition()))"
    }
  },
  [116] = {
    [0] = {
      program = "edSetPosition(edGetDown(edGetPosition()))"
    }
  },
  [119] = {
    [0] = {
      program = "edDelete()"
    }
  }
}
end

stdkeyids = {}

if platform() == "windows" then
  stdkeyids.backspace = 8
  stdkeyids.delete = 46
  stdkeyids.enter = 13
  stdkeyids.tab = 9
  stdkeyids.up = 38
  stdkeyids.down = 40
  stdkeyids.left = 37
  stdkeyids.right = 39
  stdkeyids["s"] = 83
  stdkeyids["x"] = 88
  stdkeyids["c"] = 67
  stdkeyids["v"] = 86
  stdkeyids.pgup = 33
  stdkeyids.pgdn = 34
  stdkeyids.home   = 36
  stdkeyids.endkey = 35
  stdkeyids.fnkey = function (n) return 111 + n end
  --stdkeyids.a = 0
end

if platform() == "linux" then
  stdkeyids.backspace = 22
  stdkeyids.delete = 119
  stdkeyids.enter = 36
  stdkeyids.tab = 23
  stdkeyids.up = 111
  stdkeyids.down = 116
  stdkeyids.left = 113
  stdkeyids.right = 114
  stdkeyids.f1 = 67
  stdkeyids["s"] = 39
  stdkeyids["x"] = 53
  stdkeyids["c"] = 54
  stdkeyids["v"] = 55
  stdkeyids.pgup = 112
  stdkeyids.pgdn = 117

  stdkeyids.home   = 110
  stdkeyids.endkey = 115
  stdkeyids.fnkey = function (n) return 66 + n end
end

setKeyHandlerProgram(keymap, stdkeyids.backspace, 0, [[edBackspace()]])
setKeyHandlerProgram(keymap, stdkeyids.delete,    0, [[edDelete()]])
setKeyHandlerProgram(keymap, stdkeyids.tab,       0, [[edTab()]])

-- autoindent stuff
-- shift/ctrl enter
setKeyHandlerProgram(keymap, stdkeyids.enter,     0, [[edInsertNewline()]])
setKeyHandlerProgram(keymap, stdkeyids.enter,     1,
  [[
    local sCode = edGetLuaBlock()
    local p1,p2 = edGetLuaBlockPosition()
    if sCode == "" then
      sCode = getEditorLineText()
      p2 = getEditorLineEnd()
    end
    edSetPosition(p2)
    edInsertNewline()
    luaCall(sCode)
  ]])
setKeyHandlerProgram(keymap, stdkeyids.enter,     2,
  [[
    local sCode = edGetLuaBlock()
    if sCode == "" then
      sCode = getEditorLineText()
    end
    luaCall(sCode)
  ]])
  
function edSelecting()
  if not edIsSelectionActive() then
    edSetSelectionAnchor(edGetPosition())
  end
  edShowSelection()
end

function edApplyMove(fn)
  edHideSelection()
  edSetPosition(fn(edGetPosition()))
end

function edApplySelectionMove(fn)
  edSelecting()
  edSetPosition(fn(edGetPosition()))
end

-- arrow keys
-- ctrl: word or s-exp left
-- shift: selection
setKeyHandlerProgram(keymap, stdkeyids.left,  0, [[ edApplyMove(edGetLeft) ]])
setKeyHandlerProgram(keymap, stdkeyids.right, 0, [[ edApplyMove(edGetRight) ]])
setKeyHandlerProgram(keymap, stdkeyids.up,    0, [[ edApplyMove(edGetUp) ]])
setKeyHandlerProgram(keymap, stdkeyids.down,  0, [[ edApplyMove(edGetDown) ]])

setKeyHandlerProgram(keymap, stdkeyids.left,  1, [[ edApplySelectionMove(edGetLeft) ]])
setKeyHandlerProgram(keymap, stdkeyids.right, 1, [[ edApplySelectionMove(edGetRight) ]])
setKeyHandlerProgram(keymap, stdkeyids.up,    1, [[ edApplySelectionMove(edGetUp) ]])
setKeyHandlerProgram(keymap, stdkeyids.down,  1, [[ edApplySelectionMove(edGetDown) ]])

--setKeyHandlerProgram(keymap, stdkeyids.f1,    0, [[f1Pressed()]])

function edGetPageUp(p)
  local p2 = p
  for i=1,edGetVisLines(),1 do
    p2 = edGetUp(p2)
  end
  return p2
end

function edGetPageDown(p)
  local p2 = p
  for i=1,edGetVisLines(),1 do
    p2 = edGetDown(p2)
  end
  return p2
end

setKeyHandlerProgram(keymap, stdkeyids.pgup,  0, [[ edApplyMove(edGetPageUp) ]])
setKeyHandlerProgram(keymap, stdkeyids.pgdn,  0, [[ edApplyMove(edGetPageDown) ]])
setKeyHandlerProgram(keymap, stdkeyids.pgup,  1, [[ edApplySelectionMove(edGetPageUp) ]])
setKeyHandlerProgram(keymap, stdkeyids.pgdn,  1, [[ edApplySelectionMove(edGetPageDown) ]])

-- Ctrl x,c,v
setKeyHandlerProgram(keymap, stdkeyids["x"], 2, [[edCopyBuffer = getSelectedText() edDelete()]])
setKeyHandlerProgram(keymap, stdkeyids["c"], 2, [[edCopyBuffer = getSelectedText()]])
setKeyHandlerProgram(keymap, stdkeyids["v"], 2, [[ edTypeString(edCopyBuffer) ]] )

function b2s(b)
  if b then return "true" else return "false" end
end

setKeyHandlerProgram(keymap, stdkeyids["s"], 2, [[saveBuffer() print(getBufferName() .. " saved.")]])

-- Ctrl-tab, Ctrl-Shift-tab, next/prev buffer
setKeyHandlerProgram(keymap, stdkeyids.tab, 2, [[nextEditor()]])
setKeyHandlerProgram(keymap, stdkeyids.tab, 3, [[previousEditor()]])

setKeyHandlerProgram(keymap, stdkeyids.home, 0, [[ edApplyMove(getEditorLineStart) ]])
setKeyHandlerProgram(keymap, stdkeyids.home, 1, [[ edApplySelectionMove(getEditorLineStart) ]])
setKeyHandlerProgram(keymap, stdkeyids.home, 2, [[ edHideSelection() gotoBufferStart() ]])
setKeyHandlerProgram(keymap, stdkeyids.home, 3, [[ edSelecting() gotoBufferStart() ]])

setKeyHandlerProgram(keymap, stdkeyids.endkey, 0, [[ edApplyMove(getEditorLineEnd) ]])
setKeyHandlerProgram(keymap, stdkeyids.endkey, 1, [[ edApplySelectionMove(getEditorLineEnd) ]])
setKeyHandlerProgram(keymap, stdkeyids.endkey, 2, [[ edHideSelection() gotoBufferEnd() ]])
setKeyHandlerProgram(keymap, stdkeyids.endkey, 3, [[ edSelecting() gotoBufferEnd() ]])

function edTypeString(c)
  if edIsSelectionActive() then
    edDelete()
    edHideSelection()
  end
  edInsertTextAt(c, edGetPosition())
  local l = string.len(c)
  for i=1,l,1 do
    edSetPosition(edGetRight(edGetPosition()))
  end
end

for i=1,12,1 do
  setKeyHandlerProgram(keymap,  stdkeyids.fnkey(i), 0, "f"..i.."Pressed()")
  setKeyHandlerProgram(keymap2, stdkeyids.fnkey(i), 0, "f"..i.."Pressed()")
end

if platform() == "linux" then
  stdkeyids.f11 = 95
  stdkeyids.f12 = 96

  setKeyHandlerProgram(keymap,  stdkeyids.f11, 0, "f11Pressed()")
  setKeyHandlerProgram(keymap,  stdkeyids.f12, 0, "f12Pressed()")

  setKeyHandlerProgram(keymap2, stdkeyids.f11, 0, "f11Pressed()")
  setKeyHandlerProgram(keymap2, stdkeyids.f12, 0, "f12Pressed()")
end


