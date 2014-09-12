
updateNum = 1
function update()
  workspace.items[updateNum].red = 255
  updateNum = updateNum + 1
  if updateNum > 100 then updateNum = 1 end
end
