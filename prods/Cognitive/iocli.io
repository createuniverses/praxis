context ::= lazySlot(Lobby do())
    
compileError := nil
executionError := nil

lastCommand := nil
lastError := nil
lastResult := nil

lineIsComplete := method(line,
  compileError = try(lineAsMessage := line asMessage)
  if(compileError,
    if(compileError error beforeSeq(" on line") in(CLI knownErrors),
      return false
    , return true)
  , return true))

doLine := method(line,
  lastCommand = line
  lastError = nil
  lastResult = nil
  if(lineIsComplete(lastCommand),
    executionError = try(lastResult = context doMessage(lastCommand asMessage))
    if(executionError,
      lastError = executionError coroutine backTraceString
      return "Error"
    , return "Ready")
  , return "Incomplete"))


      