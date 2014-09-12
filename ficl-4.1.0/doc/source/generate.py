import cStringIO
import os
import re
import shutil
import string
import sys


outputStart = None
navBarEntries = {}



def ficlLinkEntry(file, title):
  print("<a href=" + file + ".html><font color=white>" + title + "</font></a><p>\n")



currentNavBarName = None

def ficlAddToNavBarAs(name):
	global currentNavBarName
	currentNavBarName = name


def ficlPageHeader(heading):
  outputStart.write("""<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<HTML>
<HEAD>
<META name='Description' content='Ficl - embedded scripting with object oriented programming'>
<META name='Keywords' content='scripting prototyping tcl OOP Forth interpreter C'>
<LINK rel='SHORTCUT ICON' href='ficl.ico'>
<TITLE>""" + heading + """</TITLE>
<style>\n
blockquote { margin-left: 1em }\n
</style>\n
</HEAD>
<BODY>

<table border=0 cellspacing=0 width=100%%><tr>\n

<td width=112 bgcolor=#004968 colspan=3>
<img src=graphics/ficl.4.96.jpg height=96 width=96>
</td>

<td bgcolor=#004968>
<font face=arial,helvetica color=white size=7><b><i>
""" + heading + """
</i></b></font>
</td></tr>

\n<tr>
<td bgcolor=#004968 width=10></td>
<td bgcolor=#004968 valign=top>
<br><p>
<a href=index.html><font face=arial,helvetica color=white><b>Index</b></font></a><p>
""")
  
  print("</td><td bgcolor=#004968 width=5></td><td valign=top><blockquote><p>\n")



def ficlPageFooter():
  print("\n</blockquote><p></td></tr></table></body></html>\n")



sizeArray = [7, 5, 4, 3, 2]
indentLevel = 0
sections = None

def ficlHeader(level, color, bgcolor, heading):
  global sizeArray
  size = str(sizeArray[level])

  global indentLevel
  global sections
  while (indentLevel < level):
    indentLevel += 1
#    sys.stderr.write("adding 1 to indentLevel, it's now " + str(indentLevel) + "\n\n")
    sections.append([])
  while (indentLevel > level):
    indentLevel -= 1
    subheadings = sections.pop()
#    sys.stderr.write("indentLevel is " + str(indentLevel) + ", subheadings is " + str(subheadings) + ", len(sections) is " + str(len(sections)) + ", sections is " + str(sections) + "\n\n")
    sections[indentLevel - 1][-1][1] = subheadings
  entry = [heading, [] ]
#  sys.stderr.write("indentLevel is " + str(indentLevel) + ", len(sections) is " + str(len(sections)) + ", sections is " + str(sections) + "\n\n")
#  sys.stderr.flush()
  sections[indentLevel - 1].append(entry)

  print("""
<p>
</blockquote><table border=0 bgcolor=""" + bgcolor + """ width=100%><tr>

<td width=1em></td>
<td>
<font face=arial,helvetica color=""" + color + " size=" + size + "><b><i>")
  print("<a name='" + collapse(heading) + "'>")
  print(heading)
  print("</a></i></b></font></td></tr></table><p><blockquote>\n")


def ficlHeader1(heading):
  ficlHeader(1, "#004968", "#a0a0a0", heading)

def ficlHeader2(heading):
  ficlHeader(2, "#004968", "#b8b8b8", heading)

def ficlHeader3(heading):
  ficlHeader(3, "#004968", "#d0d0d0", heading)

def ficlHeader4(heading):
  ficlHeader(4, "#004968", "#e8e8e8", heading)


def collapse(s):
	return string.join(s.split(), "").replace("'", "").replace("&", "").replace('"', "").replace('<', "").replace('>', "").replace('.', "").replace('?', "")

def dump(f, sections):
	for section in sections:
		sys.stderr.write("sections is " + str(section) + "\n")
		name = section[0]
		f.write("<li><a href=#" + collapse(name) + "><font color=white>" + name + "</font></a>\n")
		if len(section[1]) != 0:
			f.write("<ul>\n")
			dump(f, section[1])
			f.write("</ul>\n")

def process(inputfilename, outputfilename):
	print "generating " + inputfilename
	global indentLevel
	indentLevel = 0
	global sections
	sections = []
	global currentNavBarName

	input = open(inputfilename, "r")
	data = input.read().replace("\r", "")
	input.close()
	chunks = data.split("<?")

	output = cStringIO.StringIO()

	global outputStart
	outputStart = cStringIO.StringIO()

	stdout = sys.stdout

	fauxGlobals = { }
	fauxGlobals.update(globals())
	fauxGlobals['__name__'] = '__ficlDocs__'
	fauxGlobals['__doc__'] = inputfilename
	fauxGlobals['outputStart'] = outputStart

	sys.stdout = output
	if (chunks[0] != None):
		output.write(chunks[0])
	for chunk in chunks[1:]:
		(code, verbatim) = chunk.split("?>")
		code = code.lstrip()
		if (code[0] == "="):
			execution = "eval"
			code = code[1:].lstrip()
		else:
			execution = "exec"
		compiled = compile(code, "[unknown]", execution)
		if (execution == "eval"):
			output.write(str(eval(compiled)))
		else:
			exec compiled
		output.write(verbatim)

	sys.stdout = stdout
  

	f = open(outputfilename, "w")
	f.write(outputStart.getvalue())
	f.write("<p><br>\n")
	keys = navBarEntries.keys()
	keys.sort()
	for name in keys:
		filename = navBarEntries[name]
		f.write("<a href=" + filename + ">")
		name = name.replace(" ", "&nbsp;")
		f.write("<font face=arial,helvetica color=white><b>" + name + "</b></font>")
		f.write("</a><br>\n")
# This doesn't look as pretty as I wanted, so I'm turning it off.  --lch
#		if (name == currentNavBarName) and (len(sections) > 0):
#			f.write("<ul>\n")
#			dump(f, sections[0])
#			f.write("</ul>\n")
			
	f.write(output.getvalue())
	f.close()



##
## First, find all the documents in the current directory,
## and look for their navBar entry.
##

for filename in os.listdir("."):
	if filename[-3:] == ".ht":
		file = open(filename, "rb")
		for line in file.readlines():
			navBar = "ficlAddToNavBarAs(\""
			if line.strip().startswith(navBar):
				(a, name, b) = line.split('"')
				navBarEntries[name] = filename + "ml"
				break
		file.close()

navBarEntries["Download"] = "http://sourceforge.net/project/showfiles.php?group_id=24441"

ignored = re.compile("^((.*\.pyc?)|(.*\.zip)|\.|(\.\.))$")

##
## Second, build the doc tree (in ..), processing as necessary.
##
def visit(unused, directory, names):
	for file in names:
		if ignored.search(file):
			continue
		input = directory + "/" + file
		output = "../" + input
		if input[-3:].lower() == ".ht":
			process(input, output + "ml")
		elif os.path.isdir(input):
			if not os.path.isdir(output):
				os.mkdir(output)
		else:
			try:
				shutil.copy2(input, output)
			except IOError:
				## Ignore file-copy errors. It's probably
				## a read-only file that doesn't change.
				## Lazy, I know.  --lch
				None

os.path.walk(".", visit, None)


