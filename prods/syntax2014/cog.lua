-- praxis:

workspace = {}
slipnet = {}
coderack = {}

workspace.items = {}
slipnet.items = {}
coderack.items = {}

function addSlipnetItem(name, activation)
  slipnet.items[name] = { activation = activation }
end

addSlipnetItem("shape", 1)
addSlipnetItem("square", 1)
addSlipnetItem("circle", 1)
addSlipnetItem("triangle", 1)
addSlipnetItem("distance", 0)
addSlipnetItem("far", 0)
addSlipnetItem("near", 0)
addSlipnetItem("same", 0)
addSlipnetItem("different", 0)
addSlipnetItem("opposite", 0)


slipnet.links = {}

function addSlipnetLink(a, b, linktype)
  local link = { a = slipnet.items[a],
                 b = slipnet.items[b],
                 linktype = linktype }
  table.insert(slipnet.links, link)
end

addSlipnetLink("shape", "square", "category")
addSlipnetLink("shape", "circle", "category")
addSlipnetLink("shape", "triangle", "category")
addSlipnetLink("distance", "near", "category")
addSlipnetLink("distance", "far", "category")
addSlipnetLink("near", "far", "opposite")
addSlipnetLink("same", "different", "opposite")

-- put bottom up and top down codelets into the coderack

-- Refer to algorithm described in the melanie mitchell book.

-- I will do my duty. Duty is all there really is. Duty is all there ever was.

-- "wishful thinking" based programming

-- page 72
function updateCognitiveModel()
  local k,c = chooseCodelet()
  removeCodelet(k)
  runCodelet(c)
  codeletCounter = codeletCounter + 1
  if codeletCounter > slipnetUpdateFrequency then
    updateSlipnet()
    postBottomUpCodelets()
    postTopDownCodelets()
    codeletCounter = 0
  end
end

-- page 72 and 73, Appendix B
function updateSlipnet()
  updateSlipnetLinks()
  updateSlipnetConcepts()
end

