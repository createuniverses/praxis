tasks = WidgetLib2.new()
do
 tasks.render = function ()
  drawLine(0,0,0,100,100,100)
 end
end

Widgets["tasks"] = tasks

addTask
setTaskCompletion
getTaskNotes
