from tkml import *

class TkmlSetup(TkmlMacro):

	def __init__(self, parent, attrs = {}):

		TkmlMacro.__init__(self, "macro.xml", parent, attrs)
		self.get("main").bind("<<NotebookTabChanged>>", self.refresh)
		self.get("prev").configure({"command": lambda: self.nav(-1)})
		self.get("next").configure({"command": lambda: self.nav(1)})
		self.macro_mount_point = self.get("main")

	def nav(self, orientation):

		main = self.get("main").widget
		id = main.index(main.select())
		id_end = main.index("end")-1

		if id == id_end and orientation > 0:
			self.done()

		if orientation > 0 and id_end != id:
			main.select(id+1)
		elif orientation < 0 and id != 0:
			main.select(id-1)

		self.refresh()

	def refresh(self, *args):

		main = self.get("main").widget
		id = main.index(main.select())
		id_end = main.index("end")-1

		text = "Finish" if id == id_end else "Next"
		self.get("next").configure({"text": text})

		state = "disabled" if id == 0 else "normal"
		self.get("prev").configure({"state": state})

	def done(self):

		print("Done!")

TkmlElement.register(TkmlSetup, "setup")

### Main ###

app = TkmlApp("macrodemo.xml")
app.main()