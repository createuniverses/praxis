--setBufferName("findtest.lua")

switchToBuffer(getParentBufferName())

--findpos = 1
findpos = string.find(getBufferText(), "loadBuffer", findpos+1)
edSetPosition(findpos-1)

--clearTrace()
--clearError()

