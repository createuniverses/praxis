praxis:

edSetPosition(0)
edSetPosition(getEditorLineEnd()+2)
edSetAnchor(100)

--clearError()
--edParseParentheses(0)

edSetPosition(edGetPosition() - 5)

print("Hello world")
for i = 1,10,1 do
print(i)
end

clearTrace()
clearError()
