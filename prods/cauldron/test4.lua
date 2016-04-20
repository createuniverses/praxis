setclip = setClipboardText
local s = ""
s = s .. math.floor(6382179/1)     % 256 .. "\n"
s = s .. math.floor(6382179/256)   % 256 .. "\n"
s = s .. math.floor(6382179/65536) % 256 .. "\n"
setclip(s)

99
98
97

99
98.38671875
97.384323120117

0x00616263
0,97,98,99

setclip(math.pow(256, 2))

65536
256
1

