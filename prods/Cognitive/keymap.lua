
--setClipboardText(inspect(keymap))

keymap =
{
  [10] = { {
      program = 'edTypeString("!")'
    },
    [0] = {
      program = 'edTypeString("1")'
    }
  },
  [11] = { {
      program = 'edTypeString("@")'
    },
    [0] = {
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
      program = 'edTypeString("(")'
    },
    [0] = {
      program = 'edTypeString("9")'
    }
  },
  [19] = { {
      program = 'edTypeString(")")'
    },
    [0] = {
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
      program = 'edTypeString("=")'
    }
  },
  [22] = {
    [0] = {
      fn = nil --[[<function 1>]],
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
      fn = nil --[[<function 2>]],
      program = 'edTypeString("q")'
    }
  },
  [25] = { {
      program = 'edTypeString("W")'
    },
    [0] = {
      fn = nil --[[<function 3>]],
      program = 'edTypeString("w")'
    }
  },
  [26] = { {
      program = 'edTypeString("E")'
    },
    [0] = {
      fn = nil --[[<function 4>]],
      program = 'edTypeString("e")'
    }
  },
  [27] = { {
      program = 'edTypeString("R")'
    },
    [0] = {
      fn = nil --[[<function 5>]],
      program = 'edTypeString("r")'
    }
  },
  [28] = { {
      program = 'edTypeString("T")'
    },
    [0] = {
      fn = nil --[[<function 6>]],
      program = 'edTypeString("t")'
    }
  },
  [29] = { {
      program = 'edTypeString("Y")'
    },
    [0] = {
      fn = nil --[[<function 7>]],
      program = 'edTypeString("y")'
    }
  },
  [30] = { {
      program = 'edTypeString("U")'
    },
    [0] = {
      fn = nil --[[<function 8>]],
      program = 'edTypeString("u")'
    }
  },
  [31] = { {
      program = 'edTypeString("I")'
    },
    [0] = {
      fn = nil --[[<function 9>]],
      program = 'edTypeString("i")'
    }
  },
  [32] = { {
      program = 'edTypeString("O")'
    },
    [0] = {
      fn = nil --[[<function 10>]],
      program = 'edTypeString("o")'
    }
  },
  [33] = { {
      program = 'edTypeString("P")'
    },
    [0] = {
      fn = nil --[[<function 11>]],
      program = 'edTypeString("p")'
    }
  },
  [34] = { {
      fn = nil --[[<function 12>]],
      program = 'edTypeString("{")'
    },
    [0] = {
      fn = nil --[[<function 13>]],
      program = 'edTypeString("[")'
    }
  },
  [35] = { {
      fn = nil --[[<function 14>]],
      program = 'edTypeString("}")'
    },
    [0] = {
      fn = nil --[[<function 15>]],
      program = 'edTypeString("]")'
    }
  },
  [36] = {
    [0] = {
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
      fn = nil --[[<function 16>]],
      program = 'edTypeString("s")'
    }
  },
  [40] = { {
      program = 'edTypeString("D")'
    },
    [0] = {
      program = 'edTypeString("d")'
    }
  },
  [41] = { {
      program = 'edTypeString("F")'
    },
    [0] = {
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
      program = 'edTypeString(""")'
    },
    [0] = {
      program = "edTypeString(\"'\")"
    }
  },
  [49] = { {
      program = 'edTypeString("~")'
    },
    [0] = {
      fn = nil --[[<function 17>]],
      program = 'edTypeString("`")'
    }
  },
  [51] = { {
      fn = nil --[[<function 18>]],
      program = 'edTypeString("|")'
    },
    [0] = {
      fn = nil --[[<function 19>]],
      program = 'edTypeString("\\")'
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
      program = 'edTypeString("n")'
    }
  },
  [58] = { {
      program = 'edTypeString("M")'
    },
    [0] = {
      program = 'edTypeString("m")'
    }
  },
  [59] = { {
      program = 'edTypeString("<")'
    },
    [0] = {
      program = 'edTypeString(",")'
    }
  },
  [60] = { {
      program = 'edTypeString(">")'
    },
    [0] = {
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
      program = 'edTypeString(" ")'
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

