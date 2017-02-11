from tkml import *
from functools import partial
import os, mimetypes, datetime

# Sridhar Ratnakumar
# http://stackoverflow.com/questions/1094841/reusable-library-to-get-human-readable-version-of-file-size
def sizeof_fmt(num, suffix='B'):
    for unit in ['','Ki','Mi','Gi','Ti','Pi','Ei','Zi']:
        if abs(num) < 1024.0:
            return "%3.1f%s%s" % (num, unit, suffix)
        num /= 1024.0
    return "%.1f%s%s" % (num, 'Yi', suffix)

class ExplorerTab(TkmlMacro):

	def __init__(self, parent, attrs = {}):

		TkmlMacro.__init__(self, "explorer_tab.xml", parent, attrs)
		self.history = []
		self.path = ""
		self.pos = 0
		self.get("prev").configure(dict(command = self.history_prev))
		self.get("next").configure(dict(command = self.history_next))
		self.get("close").configure(dict(command = self.destroy))
		self.get("main").bind("<Double-Button-1>", self.double_click)
		self.get("main").bind("<<TreeviewOpen>>", self.tree_open)
		self.get("main").bind("<Button-3>", self.focus)
		self.get("adressbar").bind("<KeyRelease>", self.path_change)
		self.get("adressbar").bind("<<ComboboxSelected>>", self.path_change)
		self.get("actions").add_command("Open", self.menu_open)
		self.navigate(os.environ["HOME"])

	def focus(self, event):

		row = self.get("main").identify_row(event.y)
		self.get("main").focus(row)

	def path_change(self, *args):

		val = self.get("path").get()
		self.navigate(val)

	def full_path(self, row):

		full_path = []
		element = row
		while element.tag is not "treeview":
			full_path = [element.attrs["text"]] + full_path
			element = element.parent()
		full_path = [self.path] + full_path
		return os.path.join(*full_path)

	def menu_open(self):

		row = self.get("main").focus()
		path = self.full_path(row)
		self.navigate(path)

	def double_click(self, *args):

		path = self.full_path(self.get("main").focus())
		self.navigate(path)

	def tree_open(self, *args):

		row = self.get("main").focus()
		path = self.full_path(row)
		if os.path.isdir(path):
			row.empty()
			self.populate_tree(row, os.listdir(path))

	def navigate(self, path = "/", hist = True):

		if os.path.exists(path) and os.path.isdir(path) and path.rstrip("/") != self.path.rstrip("/"):
			self.path = path
			self.get("path").value_set(self.path)
			if hist:
				self.history = self.history[:self.pos+1]
				self.history.append(self.path)
				self.pos = len(self.history)-1
			self.update_content()
			self.update_navbar()

	def update_navbar(self):

		prev_active = self.pos > 0
		self.get("prev").configure(dict(state = "normal" if prev_active else "disabled"))
		next_active = self.pos < len(self.history)-1
		self.get("next").configure(dict(state = "normal" if next_active else "disabled"))

	def history_prev(self):

		if self.pos > 0:
			self.pos -= 1
			self.update_navbar()
			self.navigate(self.history[self.pos], False)

	def history_next(self):

		if self.pos < len(self.history)-1:
			self.pos += 1
			self.update_navbar()
			self.navigate(self.history[self.pos], False)

	def update_content(self):

		content = os.listdir(self.path)
		self.get("main").empty()
		self.get("dirs").empty()
		self.get("dirs").append(self.path)
		for el in content:
			self.get("dirs").append(os.path.join(self.path, el))
		self.populate_tree(self.get("main"), content)

	def populate_tree(self, root, content):

		for el in content:
			if el[0] != ".":
				row = TkmlTreeRow(root, dict(text = el))
				path = self.full_path(row)
				isdir = os.path.isdir(path)
				info = os.stat(path)
				_type, _ = mimetypes.guess_type(path)
				if _type is None: _type = ""
				size = "" if isdir else str(sizeof_fmt(info.st_size))
				time = datetime.datetime.utcfromtimestamp(info.st_mtime).strftime("%d/%m/%Y, %H:%M")
				values = (_type, size, time)
				row.configure(dict(values = values))
				if isdir:
					TkmlTreeRow(row, dict(text = ""))

TkmlElement.register(ExplorerTab, "explorer_tab")

class Explorer(TkmlApp):

	def __init__(self):

		TkmlApp.__init__(self, "explorer.xml")
		self.get("original").get("actions").add_command("Open in new tab", partial(self.in_new_tab, self.get("original")))
		self.get("original").get("close").configure(dict(state = "disabled"))
		self.minsize()
		# self.get("newtab").configure(dict(command = self.new_tab))

	def new_tab(self):

		new = ExplorerTab(self.get("main"))
		new.get("actions").add_command("Open in new tab", partial(self.in_new_tab, new))
		return new

	def in_new_tab(self, tab):

		path = tab.full_path(tab.get("main").focus())
		new = self.new_tab()
		new.navigate(path)


app = Explorer()
app.main()