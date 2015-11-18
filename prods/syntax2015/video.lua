
function prepareForVideoCapture()
  windowedMode(100,100,1280,720)
  turnOffBorders()
  windowedMode(100,100,1280,720)
  stopMp3()

  isMp3Playing_old = isMp3Playing

  function isMp3Playing()
    return true
  end
  
  setFloorGrid(false)
  
  render_prod = render
  update_prod = update
  
  function render() end
  function update() end
  
  function f4Pressed()
    setFloorGrid(true)
    playMp3()
    --showTrace()
    --showFPS()
    render = render_prod
    update = update_prod
  end
end
