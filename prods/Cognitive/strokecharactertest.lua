function renderStrokeCharacterTest()
  glPushMatrix()
  glScale(0.01,0.01,0.01)
  edStrokeCharacter('a',0,0)
  edStrokeCharacter('b',0,0)
  glPopMatrix()
end
