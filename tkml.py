from tkinter import *
from tkinter import ttk
from html.parser import HTMLParser
from collections import MutableMapping, UserList
import re

### Core classes

class TkmlException(Exception): pass

class TkmlAttributes(MutableMapping):

	"""Wrapper around element attributes. Allows custom getter/setter definition, and subscribes elements to variables when possible"""

	def __init__(self, element, *args, **kwargs):

		self.element = element
		self.store = dict(*args, **kwargs)
		self.getter = {}
		self.setter = {}
		self.filters = {
			"reference": self.filter_reference,
			"litteral": self.filter_litteral,
			"eval": self.filter_eval,
			"tk": self.filter_tk,
			"mean": self.filter_mean,
			"sum": self.filter_sum,
			"increment": self.filter_increment,
			"decrement": self.filter_decrement,
			"toggle": self.filter_toggle,
			"cycle": self.filter_cycle,
			"append": self.filter_append
		}
		self.enabled_filters = {
			"*": ["reference", "litteral", "eval"],
			"command": ["reference"],
			"variable": ["reference", "tk"],
			"textvariable": ["reference", "tk"]
		}

	def no_filters(self):

		self.enabled_filters = { "*": [] }

	def add_property(self, key, getter, setter):

		"""Define getter and setter for the given key"""
		if getter is not None:
			self.getter[key] = getter
		self.setter[key] = setter
		if key in self.store:
			setter(self.store[key])
		elif getter is not None:
			self.store[key] = "__placeholder__" # Should not be read, overwritten by getter result!

	def filter_reference(self, fullkey, key, value):

		"""Parses value for a DOM reference and return new value"""

		try:
			if len(value) and value[0] == "#": # Only id selectors
				value = self.element.get(value[1:])
		except TypeError:
			pass
		return value

	def _subscribe(self, fullkey, var):

		try: # Attempts to bind variables
			var.var_subscribe(self.element, fullkey) # Subscribe the element to that variable
		except AttributeError:
			pass

	def filter_litteral(self, fullkey, key, value):

		self._subscribe(fullkey, value)
		try:
			value = value.value_get()
		except AttributeError:
			pass
		return value

	def filter_tk(self, fullkey, key, value):

		try:
			return value.tk
		except AttributeError:
			raise TkmlException("expected variable, got {} instead".format(value.__class__.__name__))

	def filter_eval(self, fullkey, key, value):

		try:
			if re.match("[0-9%+\-\*/\.]+", value):
				value = eval(value)
		except (TypeError, SyntaxError):
			pass
		return value

	def filter_mean(self, fullkey, key, value):

		return sum([float(e) for e in value])/len(value)

	def filter_sum(self, fullkey, key, value):

		return sum([float(e) for e in value])

	def filter_increment(self, fullkey, key, value):

		assert key == "command"
		return lambda: value.value_set(value.value_get() + 1)

	def filter_decrement(self, fullkey, key, value):

		assert key == "command"
		return lambda: value.value_set(value.value_get() - 1)

	def filter_toggle(self, fullkey, key, value):

		assert key == "command"
		assert value.type == "boolean"
		return lambda: value.value_set("yes" if value.value_get() == "no" else "no")

	def filter_cycle(self, fullkey, key, value):

		assert key == "command"
		array = self.element.attrs["data"]
		def _handler():
			try:
				index = array.index(value.value_get())
			except ValueError:
				index = 0
			value.value_set(array[(index+1)%len(array)])
		return _handler

	def filter_append(self, fullkey, key, value):

		assert key == "command"
		assert value.tag == "array"
		return lambda element = self.element: value.append(element.attrs["data"])

	def __getitem__(self, key):

		if key in self.getter:
			return self.getter[key]()
		else:
			return self.store[key]

	def __setitem__(self, fullkey, value):

		key = fullkey
		key, *filters = key.split(":")
		enabled_filters = self.enabled_filters.get(key, self.enabled_filters["*"])
		if key[0] == "!":
			key = key[1:]
			enabled_filters = []

		for _filter in enabled_filters + filters:
			value = self.filters[_filter](fullkey, key, value)

		if key in self.setter:
			self.setter[key](value)
		else:
			self.store[key] = value

	def __delitem__(self, key):

		if key in self.getter:
			try:
				value.var_unsubscribe(self.element, key)
			except AttributeError:
				pass
		else:
			del self.store[key]

	def __iter__(self):

		return iter(self.store)

	def __len__(self):

		return len(self.store)

class TkmlSelector:

	def __init__(self, element):

		self.element = element
		self.root = element.root()
		self.atom_handlers = [
			("*", self.atom_any),
			("#", self.atom_id),
			(".", self.atom_class),
			("$", self.atom_component),
			("", self.atom_tag)
		]
		self.filter_handlers = {
			">": self.filter_direct,
			"~": self.filter_sibling
		}

	def filter_children(self, context):

		return context.children()

	def filter_direct(self, context):

		return context.children(1)

	def filter_sibling(self, context):

		return context.parent().children(1)

	def atom_any(self, atom, context):

		assert atom == "*"
		return context

	def atom_tag(self, atom, context):

		return context.match_tag(atom)

	def atom_id(self, atom, context):

		tag, _id = atom.split("#")
		return self.atom_tag(tag, context).match_id(self.element.scope() + _id)

	def atom_class(self, atom, context):

		tag, _class = atom.split(".")
		return self.atom_tag(tag, context).match_class(_class)

	def atom_component(self, atom, context):

		tag, component = atom.split("$")
		result = TkmlElementList()
		for scope in TkmlElement.scopes():
			element = self.root.get(scope + component)
			if element is not None and element.tag == tag:
				result.append(element)
		return result

	def atom(self, atom, context = None):

		if context is None:
			context = self.element.children()
		current = None
		for symbol, handler in self.atom_handlers:
			if symbol in atom:
				current = handler(atom, context)
				break
		return current

	def select_one(self, selector):

		first, *atoms = selector.split(" ")
		results = self.atom(first)
		_filter = self.filter_children
		for atom in atoms:
			if atom in self.filter_handlers.keys():
				_filter = self.filter_handlers[atom]
				continue
			tmp = TkmlElementList()
			for result in results:
				tmp.extend(self.atom(atom, _filter(result)))
			results = tmp
			_filter = self.filter_children
		return results

	def select(self, selector):

		result = TkmlElementList()
		for target in selector.split(","):
			result.extend(self.select_one(target.strip()))
		return result

class TkmlStyleParser:

	def __init__(self, context):

		self.selector_parser = TkmlSelector(context)
		self.selector = ""
		self.in_block = False
		self.buffer = ""

	def feed_block(self, block):

		for rule in block.split(";"):
			rule = rule.strip()
			if rule == "":
				break
			key, value = [s.strip() for s in rule.split(":")]
			for target in self.selector.split(","):
				try:
					target, state = target.split(":")
				except ValueError:
					state = None
				for element in self.selector_parser.select_one(target):
					element.set_style_key(state, key, value)
					if state is None:
						element.configure({key: value})

	def feed_selector(self, selector):

		self.selector = selector.strip()

	def feed(self, line):

		if not self.in_block and "{" in line:
			selector, *after = line.split("{")
			self.buffer += selector
			self.feed_selector(self.buffer)
			self.buffer = ""
			self.in_block = True
			self.feed("{".join(after))
		elif self.in_block and "}" in line:
			block, *after = line.split("}")
			self.buffer += block
			self.feed_block(self.buffer)
			self.buffer = ""
			self.in_block = False
			self.feed("}".join(after))

	def parse(self, raw):

		self.feed(raw)

	def parse_file(self, path):

		with open(path, "r", encoding = "utf-8") as file:
			for line in file.readlines():
				self.feed(line)


class TkmlElement:

	"""Base class for all Tkml elements and static class for parser, element and scope creation. A TkmlElement defines a wrapper around a Tk widget or is an abstract element; in both cases, it is bound to one or multiple tags to be used in Tkml markup"""

	tags = {}
	scope_count = -1

	def new_scope():

		"""Returns a new, unused scope name."""

		TkmlElement.scope_count += 1
		return "_scope{}_".format(TkmlElement.scope_count)

	def scopes():

		return ["_scope{}_".format(i) for i in range(TkmlElement.scope_count + 2)]

	def new(tag, *args):

		"""Creates a new element from a tag and arguments (parent element, attributes...). Used by the parser"""

		tkml_class = TkmlElement.tags.get(tag, None)
		if tkml_class is not None:
			return tkml_class(*args)
		else:
			raise TkmlException("no tag "+tag)

	def register(tkml_class, tag, *alias):

		"""Register a TkmlElement child class as a tag, with optional aliases. The first tag might be used to select that element by other elements, and is accessible as an attribute on instances"""

		tkml_class.tag = tag
		TkmlElement.tags[tag] = tkml_class
		for tag in alias:
			TkmlElement.tags[tag] = tkml_class

	def parse_file(path, parent = None):

		"""Parses a Tkml file, setting all elements as children to the given parent (creates a new <root> if none is given). Returns the local root of the parsed content (the first child widget element of the document)"""

		parser = TkmlParser(parent)
		with open(path, "r", encoding = "utf-8") as file:
			for line in file.readlines():
				parser.feed(line)
		return parser.local_root

	def parse(expr, parent = None):

		"""Parses a Tkml string, setting all elements as children to the given parent (creates a new <root> if none is given). Returns the local root of the parsed content (the first child widget element of the document)"""

		parser = TkmlParser(parent)
		parser.feed(expr)
		return parser.local_root

	def __init__(self, parent = None, attrs = {}):

		"""To be completed by child classes. Instantiates a new TkmlElement with the given parent and attributes"""

		self._scope = None
		self._children = []
		self._parent = parent
		self._selector = TkmlSelector(self)
		if parent is not None:
			parent._children.append(self)

		self._id = None
		self.attrs = TkmlAttributes(self, attrs)
		self.attrs.add_property("id", self._get_id, self._set_id)
		self.attrs.update(attrs)

		self.widget = self.tk = None

	def _set_id(self, id):

		self._id = self.scope()+str(id) # Ids are scoped
		self.root().ids[self._id] = self # And registered to the root element

	def _get_id(self):

		return self._id

	def select(self, selector):

		return self._selector.select(selector)

	def _style_manager(self):

		return None

	def copy(self, parent, deep = True):

		"""Copies (by default deply) the element onto the specified parent node"""

		copy = TkmlElement.new(self.tag, parent, self.attrs)
		if deep:
			for child in self.children(1):
				child.copy(copy, deep)
		return copy

	def index(self, tag):

		"""Returns the index of the element as a descendant of the closest ancestor bearing the specified tag"""

		return self.parent(tag).children().match_tag(self.tag).index(self)

	def has_class(self, _class):

		"""Returns whether element has the given class"""

		return _class in self.attrs.get("class", "").split()

	def children(self, depth = 100):

		"""Returns a new TkmlElementList with descendants of the element, limited to the specified depth"""

		new = []
		if depth > 0:
			for child in self._children:
				new.extend(child.children(depth-1))
				new.append(child)
		return TkmlElementList(new)

	def parent(self, tag = None):

		"""Returns the closest ancestor with the given tag. If none is given, return direct parent"""

		if tag is None or self._parent is None or self._parent.tag == tag:
			return self._parent
		return self._parent.parent(tag)

	def root(self):

		"""Returns the root element (element with no parent, typically the window)"""

		if self._parent is None:
			return self
		else:
			return self._parent.root()

	def scope(self):

		"""Returns the scope of the element. Within a scope, all identifiers can be used safely again (they will be prefixed by the scope automatically). The scope is inherit unless the _scope attribute is set"""

		return self._parent.scope() if self._scope is None else self._scope

	def get(self, id):

		"""Returns the unique element with the given id within the current scope"""

		return self.root().get(self.scope()+str(id))

	def is_widget(self):

		"""Returns whether element is a widget or not"""

		return self.widget is not None

	def add_child(self, element, attrs):

		"""Called by child widgets to add themselves to the Tk application. This method must not alter the DOM tree. By default, this operation is delegated upward in the tree until an element takes the responsibility to add the widget to the Tk application. This allows non widget elements to modify the attributes of a descendant before it is added to the application"""

		self.parent().add_child(element, attrs)

	def raw_inner(self):

		"""Parser hook, to be overriden. Specify whether markup below the current element should be passed to tag_inner instead of being handled by the parser"""

		return False

	def tag_inner(self, text):

		"""Parser hook, to be overriden. Specify what should be done with the content passed inside the current element in the markup, for instance <element>tag_inner argument</element>"""

	def mount_point(self):

		return self

	def parse_done(self):

		"""Parser hook, to be overriden. Called when all children have been parsed"""

		for child in self.children():
			child.parent_ready()

	def parent_ready(self):

		"""To be overriden. Called when parent is done parsing its children"""

		pass

	def destroy(self):

		"""Remove the current element from the DOM"""

		self._parent._children.remove(self)
		self._parent = None
		for child in self._children:
			child.destroy()

	def master(self):

		"""Returns the closest ancestor that is a widget"""

		if self._parent.is_widget():
			return self._parent
		else:
			return self._parent.master()

	def configure(self, attrs = {}):

		"""Update attributes"""

		self.attrs.update(attrs)

	def show_tree(self, n = 0, show_vars = False, callback = None):

		"""Pretty print the current node with the specified indent n. Variables can be omitted. If a callback is passed, it will be used pass all elements and its result will be cast to a string and printed"""

		info = ""
		if callback is not None:
			info = "\r" + 2*"\t" + str(callback(self))
		print(n*" " + self.tag + ("#"+self.attrs["id"] if str(self.attrs["id"]) != "None" else "") + info)
		for child in self._children:
			if show_vars or child.tag not in ["variable", "array"]:
				child.show_tree(n+1, show_vars, callback)

class TkmlElementList(UserList):

	"""A list of TkmlElements, used as a way to perform selections and batch operations"""

	def __init__(self, iter = []):

		UserList.__init__(self, iter)

	def __getattr__(self, key):

		def _fun(*args):
			[getattr(child, key)(*args) for child in self]
		return _fun

	def append(self, element):

		if element in self:
			return
		UserList.append(self, element)

	def extend(self, other):

		for element in other:
			self.append(element)

	def children(self, depth = 10):

		"""Return children for all elements"""

		new = TkmlElementList()
		for element in self:
			new.extend(element.children(depth))
		return new

	def match_tag(self, tag):

		"""Return the subset matching the given tag"""

		if tag == '':
			return self
		new = TkmlElementList()
		for element in self:
			if element.tag == tag:
				new.append(element)
		return new

	def match_class(self, _class):

		"""Return the subset matching the given class"""

		new = TkmlElementList()
		for element in self:
			if element.has_class(_class):
				new.append(element)
		return new

	def match_id(self, _id):

		"""Return the subset matching the given id"""

		for element in self:
			if element.attrs["id"] == _id:
				return TkmlElementList([element])
		return TkmlElementList()

class TkmlWidget(TkmlElement):

	"""A base class for all Tk widgets"""

	def __init__(self, tk_class, parent, attrs):

		"""To be overriden. Instantiate a TkmlWidget. The tk_class has to be a valid Tkinter widget class"""

		TkmlElement.__init__(self, parent, attrs)
		master = self.master()
		self.tk = self.widget = tk_class(master.widget)
		parent.add_child(self, self.attrs)
		self._disabled = "no"
		self.attrs.add_property("disabled", self._get_disabled, self._set_disabled)
		self.configure()

	def _get_disabled(self):

		return self._disabled

	def _set_disabled(self, value):

		self._disabled = value
		self.widget.configure(state = "disabled" if value == "yes" else "normal")

	def _style_path(self):

		return self.tk._name + "." + self.tk.winfo_class()

	def _style_manager(self):

		return self.root().style

	def set_style_key(self, state, key, value):

		self.configure(dict(style = self._style_path()))
		style = self._style_manager()
		path = self._style_path()
		if not key in self._style_keys():
			raise TkmlException("no such style key: {}".format(key))
		if state is None:
			style.configure(path, **{ key: value })
		else:
			origin = style.map(path, key)
			new = []
			for *states, v in origin:
				for s in states:
					if s != state:
						new.append((s, v))
			new.append((state, value))
			style.map(path, **{ key: new })

	def _tk_layout(self, layout = None):

		elements = []
		if layout is None:
			layout = self._style_manager().layout(self.tk.winfo_class())

		for element, props in layout:
			elements.append(element)
			elements.extend(self._tk_layout(props.get("children", [])))

		return elements

	def _style_keys(self):

		keys = []
		for element in self._tk_layout():
			keys.extend([key.strip("-") for key in self._style_manager().element_options(element)])
		return keys

	def set_style(self, rules, state = None):

		for key, value in rules.items():
			self.set_style_key(state, key, value)

	def tag_inner(self, text):

		TkmlElement.new("label", self, dict(text = text)) # Create a child label by default

	def widget_valid(self):

		"""Returns the attribute keys that will be understood by the underlying Tk widget"""

		return [key for key in self.widget.keys() if key != "class"]

	def bind(self, event, handler):

		"""Bind an element to an event. Can be called more than once"""

		self.widget.bind(event, handler, add = "+")

	def configure(self, attrs = {}):

		"""Update element attributes"""

		TkmlElement.configure(self, attrs)
		for key, value in self.attrs.items():
			if key in self.widget_valid():
				self.widget.configure({ key: value })

	def destroy(self):

		"""Destroy the widget and remove the element from the DOM tree"""

		TkmlElement.destroy(self)
		self.widget.destroy()

class TkmlRoot(TkmlElement):

	"""The root element, which handles id registration and is expected by the parser to exist. It is not a TkmlWidget but will respond True to is_widget"""

	def __init__(self, attrs = {}):

		self.ids = {}
		TkmlElement.__init__(self, None, attrs) # No parent, that's the root!
		self._scope = "" # No scope (raw ids)
		self.tk = self.widget = Tk()
		self.style = ttk.Style()
		self.widget.title(self.attrs.get("title", "Tkml"))

	def get_from_event(self, event):

		"""Return a child TkmlElement from an event"""

		for child in self.children():
			if child.is_widget() and child.widget is event.widget:
				return child

	def root(self):

		return self

	def add_child(self, element, attrs):

		element.widget.pack(expand = "yes", fill = "both")

	def get(self, id):

		return self.ids.get(id, None)

	def bind(self, event, handler):

		self.widget.bind(event, handler, add = "+")

	def main(self):

		"""Run the program main loop"""

		self.widget.mainloop()

TkmlElement.register(TkmlRoot, "root")

class TkmlParser(HTMLParser):

	"""A Tkinter markup language parser"""

	def __init__(self, local_root = None):

		HTMLParser.__init__(self)
		self.local_root = local_root # The local root of the document (first TkmlWidget)
		self.parents = []
		if self.local_root is not None:
			self.parents.append(self.local_root)

		# Used to delegate parsing as specified by raw_inner
		self.buffer = {}
		self._buffer_pop()

	def _buffer_init(self, tag):

		self.buffer["tag"] = tag
		self.buffer["depth"] = 1 # depth is used to count how many such tags were seen, when it is back to 0, we are done buffering

	def _buffer_active(self):

		return self.buffer["tag"] is not None

	def _buffer_push(self, tag, content = None, encountered = 1):

		delta = 1 if tag == self.buffer["tag"] else 0
		delta *= encountered%2 # How many open/close tag encountered?
		if content is None: # No content, that's a close tag
			self.buffer["depth"] -= delta # We are going up a level
			if self.buffer["depth"] > 0:
				self.buffer["html"] += "</{}>".format(tag)
		else:
			self.buffer["html"] += content
			self.buffer["depth"] += delta # We are going deeper!

	def _buffer_full(self):

		return self._buffer_active() and self.buffer["depth"] == 0

	def _buffer_pop(self):

		content = self.buffer.get("html", None)
		self.buffer = {
			"tag": None,
			"html": "",
			"depth": 0,
		} # Reset
		return content

	def _create_element(self, tag, attrs):

		attrs = { key: value for key, value in attrs }
		if tag == "root":
			element = TkmlElement.new("root", attrs)
		else:
			if not self.parents: # No parent? We need one!
				self.handle_starttag("root", ())
			element = TkmlElement.new(tag, self.parents[-1].mount_point(), attrs)
		if self.local_root is None and element.is_widget():
			self.local_root = element
		return element

	def handle_starttag(self, tag, attrs):

		tag = tag.lower()
		if self._buffer_active():
			self._buffer_push(tag, self.get_starttag_text())
		else:
			element = self._create_element(tag, attrs)
			self.parents.append(element)
			if element.raw_inner():
				self._buffer_init(tag)

	def handle_data(self, text):

		text = text.strip()
		if len(text):
			if self._buffer_active():
				self._buffer_push(None, text)
			else:
				self.parents[-1].tag_inner(text) #FIXME! concatenate before sending

	def handle_endtag(self, tag):

		if self._buffer_active():
			self._buffer_push(tag)
			if self._buffer_full():
				parent = self.parents.pop()
				parent.tag_inner(self._buffer_pop())
				parent.parse_done()
		else:
			self.parents.pop().parse_done()

	def handle_startendtag(self, tag, attrs):

		tag = tag.lower()
		if self._buffer_active():
			self._buffer_push(tag, self.get_starttag_text(), 2)
		else:
			self._create_element(tag, attrs).parse_done()

	def read(self, path):

		"""Parse given file"""

		with open(path, "r", encoding = "utf-8") as file:
			for line in file.readlines():
				self.feed(line)

class TkmlApp:

	"""Simple application wrapper"""

	def __init__(self, path):

		"""Create a new app from the given Tkml file"""

		parser = TkmlParser()
		parser.read(path)
		self.root = parser.local_root

	def main(self):

		"""Main program loop"""

		self.root.main()

	def get(self, id):

		return self.root.get(id)

	def bind(self, *args, **kwargs):

		self.root.bind(*args, **kwargs)

	def minsize(self):

		"""Set the minimal size of the app from its content"""

		self.root.widget.update_idletasks()
		self.root.widget.minsize(self.root.widget.winfo_width(), self.root.widget.winfo_height())

### Variables

class TkmlVariable(TkmlElement):

	def __init__(self, parent, attrs = {}):

		TkmlElement.__init__(self, parent, attrs)
		self.subscribers = []
		variables = {"string": StringVar, "int": IntVar, "double": DoubleVar, "boolean": BooleanVar, "array": None}
		self.type = self.attrs["type"] = self.attrs.get("type", "string")
		self.tk = variables[self.type]()
		self.tk.set(attrs.get("value", ""))
		self.tk.trace("w", self.update)
		if self.parent().tag == "array":
			self.parent().add_child(self, self.attrs)

	def var_subscribe(self, element, key):

		for _element, _key in self.subscribers:
			if _element is element and _key is key:
				return
		self.subscribers.append((element, key))

	def var_unsubscribe(self, element, key):

		self.subscribers = [sub for sub in self.subscribers if sub != (element, key)]

	def tag_inner(self, text):

		self.set(text)

	def value_get(self):

		value = self.tk.get()
		if self.type == "boolean":
			return "yes" if value == 1 else "no"
		else:
			return value

	def update(self, *args):

		for element, key in self.subscribers:
			element.attrs[key] = self
		if self.parent().tag == "array":
			self.parent().update()

	def value_set(self, value):

		if self.type == "boolean":
			value = 1 if value == "yes" else 0
		self.tk.set(value)
		self.update()

TkmlElement.register(TkmlVariable, "variable", "var")

class TkmlArray(TkmlElement):

	def __init__(self, parent, attrs = {}):

		TkmlElement.__init__(self, parent, attrs)
		self.subscribers = []
		self._scope = ""

	def tag_inner(self, content):

		separator = self.attrs.get("separator", ";")
		_type = self.attrs.get("type", "string")
		for value in content.split(separator):
			TkmlVariable(self, dict(value = value, type = _type))

	def value_get(self):

		return tuple([element.value_get() for element in self])

	def update(self):

		for element, key in self.subscribers:
			element.attrs[key] = self
		if self.parent().tag == "array":
			self.parent().update()

	def empty(self):

		self._children = []
		self.update()

	def var_subscribe(self, element, key):

		for _element, _key in self.subscribers:
			if _element is element and _key is key:
				return
		self.subscribers.append((element, key))

	def add_child(self, element, attrs = {}):

		index = self.children().index(element)
		element.attrs["id"] = self.attrs["id"] + "[{}]".format(index)
		self.update()

	def append(self, value, type = "string"):

		TkmlVariable(self, dict(value = value, type = type))

	def __getitem__(self, index):

		return self.children(1)[index]

	def __len__(self):

		return len(self.children(1))

TkmlElement.register(TkmlArray, "array")

class TkmlMacro(TkmlElement):

	def __init__(self, macro, parent, attrs):

		TkmlElement.__init__(self, parent, attrs)
		self.real_added = False
		self._scope = TkmlElement.new_scope()
		try:
			self.real = TkmlElement.parse_file(macro, self)
		except (OSError, FileNotFoundError):
			self.real = TkmlElement.parse(macro, self)
		self.real.configure(attrs)

	def add_child(self, element, attrs):

		if not self.real_added:
			self.parent().add_child(element, self.attrs)
			self.real_added = True

	def mount_point(self):

		if not self.real_added:
			return self
		else:
			return self.macro_mount_point

	def bind(self, event, handler):

		self.real.bind(event, handler)

	def configure(self, attrs = {}):

		TkmlElement.configure(self, attrs)
		TkmlElement.configure(self.real, attrs)

	def destroy(self):

		self.real.destroy()
		TkmlElement.destroy(self)

### Element definitions

class TkmlStyle(TkmlElement):

	def __init__(self, parent, attrs = {}):

		TkmlElement.__init__(self, parent, attrs)
		self.parser = TkmlStyleParser(parent)
		self.content = ""

	def tag_inner(self, content):

		self.content = content

	def parent_ready(self):

		self.parser.feed(self.content)

TkmlElement.register(TkmlStyle, "style")

class TkmlBox(TkmlWidget):

	def __init__(self, parent, attrs = {}):

		TkmlWidget.__init__(self, ttk.Frame, parent, attrs)
		self._orient = "vertical"
		self.attrs.add_property("orient", self._get_orient, self._set_orient)

	def pack(self, element, attrs):

		pack_keys = ["expand", "fill", "side", "ipadx", "ipady", "padx", "pady"]
		if "fill" in attrs.keys():
			attrs["expand"] = "yes"
		element.widget.pack({ key: value for key, value in attrs.items() if key in pack_keys })

	def _get_orient(self):

		return self._orient

	def _set_orient(self, value):

		self._orient = value
		for child in self.children():
			if child.is_widget():
				child.widget.pack_forget()
				self.add_child(child, child.attrs)

	def add_child(self, element, attrs):

		side = "top" if self.attrs["orient"] == "vertical" else "left"
		attrs["side"] = side
		self.pack(element, attrs)

TkmlElement.register(TkmlBox, "box")

class TkmlGrid(TkmlWidget):

	def __init__(self, parent, attrs = {}):

		TkmlWidget.__init__(self, ttk.Frame, parent, attrs)

	def grid(self, element, attrs):

		grid_keys = ["column", "columnspan", "row", "rowspan", "sticky", "ipadx", "ipady", "padx", "pady"]
		element.widget.grid({ key: value for key, value in attrs.items() if key in grid_keys })

	def add_child(self, element, attrs):

		self.grid(element, attrs)

TkmlElement.register(TkmlGrid, "grid")

class TkmlSizegrip(TkmlWidget):

	def __init__(self, parent, attrs = {}):

		TkmlWidget.__init__(self, ttk.Sizegrip, parent, attrs)

TkmlElement.register(TkmlSizegrip, "sizegrip")

class TkmlScrollbar(TkmlWidget):

	def __init__(self, parent, attrs = {}):

		TkmlWidget.__init__(self, ttk.Scrollbar, parent, attrs)
		attrs["orient"] = attrs.get("orient", "vertical")
		self._scroll = None
		self.attrs.add_property("target", self._get_scroll, self._set_scroll)
		self.attrs["target"] = attrs.get("target", self.master().widget)

	def _get_scroll(self):

		return self._scroll

	def _set_scroll(self, value):

		self._scroll = value
		if self.attrs["orient"] == "vertical":
			self.configure(dict(command = value.widget.yview))
			value.widget.configure(yscrollcommand = self.widget.set)
		else:
			self.configure(dict(command = value.widget.xview))
			value.widget.configure(xscrollcommand = self.widget.set)

TkmlElement.register(TkmlScrollbar, "scrollbar")

class TkmlMenu(TkmlElement):

	def __init__(self, parent, attrs = {}):

		TkmlElement.__init__(self, parent, attrs)
		self.root().widget.option_add("*tearOff", False)
		self.tk = Menu(self.root().widget)
		master = parent.master() if not parent.is_widget() else parent
		master.bind("<Button-3>", self.show)
		master.bind("<Button-1>", self.hide)

	def add_command(self, label, handler):

		self.tk.add_command(label = label, command = handler)

	def show(self, event):

		self.tk.post(event.x_root, event.y_root)

	def hide(self, event):

		self.tk.unpost()

TkmlElement.register(TkmlMenu, "menu")

class TkmlRow(TkmlElement):

	def __init__(self, parent, attrs = {}):

		TkmlElement.__init__(self, parent, attrs)
		self.parent("grid").widget.rowconfigure(self.index("grid"), weight = attrs.get("weight", "0"))

	def add_child(self, element, attrs):

		attrs["row"] = self.index("grid")
		self.parent().add_child(element, attrs)

TkmlElement.register(TkmlRow, "row")

class TkmlCell(TkmlElement):

	def __init__(self, parent, attrs = {}):

		TkmlElement.__init__(self, parent, attrs)
		self.parent("grid").widget.columnconfigure(self.index("row"), weight = attrs.get("weight", "0"))

	def tag_inner(self, text):

		TkmlWidget.tag_inner(self, text)

	def add_child(self, element, attrs):

		attrs["column"] = self.index("row")
		attrs["columnspan"] = self.attrs.get("colspan", "1")
		attrs["rowspan"] = self.attrs.get("rowspan", "1")
		self.parent().add_child(element, attrs)

TkmlElement.register(TkmlCell, "cell")

class TkmlNotebook(TkmlWidget):

	def __init__(self, parent, attrs = {}):

		TkmlWidget.__init__(self, ttk.Notebook, parent, attrs)

	def add_child(self, element, attrs):

		self.widget.add(element.widget, text = attrs.get("name", "Tab"))

TkmlElement.register(TkmlNotebook, "notebook")

class TkmlTab(TkmlBox):

	def __init__(self, parent, attrs = {}):

		TkmlBox.__init__(self, parent, attrs)
		self._name = ""
		self.attrs.add_property("name", self._get_name, self._set_name)

	def _get_name(self):

		return self._name

	def _set_name(self, value):

		self._name = value
		notebook = self.parent("notebook")
		if notebook is not None:
			notebook.widget.tab(self.index("notebook"), text = value)

TkmlElement.register(TkmlTab, "tab")

class TkmlButton(TkmlWidget):

	def __init__(self, parent, attrs = {}):

		TkmlWidget.__init__(self, ttk.Button, parent, attrs)

	def tag_inner(self, text):

		self.configure(dict(text = text))

TkmlElement.register(TkmlButton, "button")

class TkmlLabel(TkmlWidget):

	def __init__(self, parent, attrs = {}):

		TkmlWidget.__init__(self, ttk.Label, parent, attrs)
		self.attrs.add_property("text", None, self._set_text)

	def _set_text(self, value):

		self.widget.configure(text = value)

	def tag_inner(self, text):

		self.configure(dict(text = text))

TkmlElement.register(TkmlLabel, "label")

class TkmlText(TkmlWidget):

	def __init__(self, parent, attrs = {}):

		TkmlWidget.__init__(self, Text, parent, attrs)

	def tag_inner(self, text):

		self.widget.insert("end", text+"\n")

TkmlElement.register(TkmlText, "text")

class TkmlEntry(TkmlWidget):

	def __init__(self, parent, attrs = {}):

		TkmlWidget.__init__(self, ttk.Entry, parent, attrs)
		self._hidden = "no"
		self.attrs.add_property("hidden", self._get_hidden, self._set_hidden)

	def _get_hidden(self):

		return self._hidden

	def _set_hidden(self, value):

		self._hidden = value
		if value == "yes":
			self.widget.configure(show = "*")
		else:
			self.widget.configure(show = "")

TkmlElement.register(TkmlEntry, "entry")

class TkmlCheckbutton(TkmlWidget):

	def __init__(self, parent, attrs = {}):

		TkmlWidget.__init__(self, ttk.Checkbutton, parent, attrs)

	def tag_inner(self, text):

		self.configure(dict(text = text))

TkmlElement.register(TkmlCheckbutton, "checkbutton")

class TkmlRadiobutton(TkmlWidget):

	def __init__(self, parent, attrs = {}):

		TkmlWidget.__init__(self, ttk.Radiobutton, parent, attrs)

	def tag_inner(self, text):

		self.configure(dict(text = text))

TkmlElement.register(TkmlRadiobutton, "radiobutton")

class TkmlCombobox(TkmlWidget):

	def __init__(self, parent, attrs = {}):

		TkmlWidget.__init__(self, ttk.Combobox, parent, attrs)
		self._values = tuple()
		self.attrs.add_property("values", self._get_values, self._set_values)

	def _set_values(self, array):

		self._values = array
		self.widget["values"] = self._values

	def _get_values(self):

		return self._values

TkmlElement.register(TkmlCombobox, "combobox")

class TkmlScale(TkmlWidget):

	def __init__(self, parent, attrs = {}):

		TkmlWidget.__init__(self, ttk.Scale, parent, attrs)

TkmlElement.register(TkmlScale, "scale")

class TkmlSpinbox(TkmlWidget):

	def __init__(self, parent, attrs = {}):

		TkmlWidget.__init__(self, Spinbox, parent, attrs)

TkmlElement.register(TkmlSpinbox, "spinbox")

class TkmlProgressbar(TkmlWidget):

	def __init__(self, parent, attrs = {}):

		TkmlWidget.__init__(self, ttk.Progressbar, parent, attrs)

TkmlElement.register(TkmlProgressbar, "progressbar")

class TkmlTreeview(TkmlWidget):

	def __init__(self, parent, attrs = {}):

		attrs["tree_id"] = ""
		TkmlWidget.__init__(self, ttk.Treeview, parent, attrs)
		self._columns = tuple()
		self.attrs.add_property("columns", self._get_columns, self._set_columns)

	def _set_columns(self, array):

		self.widget["columns"] = array
		for element in array:
			self.widget.heading(element, text = element)

	def _get_columns(self):

		return self._columns

	def focus(self, row = None):

		if row is not None:
			self.widget.selection_set(row.attrs["tree_id"])
			self.widget.focus(row.attrs["tree_id"])
		else:
			id = self.widget.focus()
			for row in self.children().match_tag("treerow"):
				if row.attrs["tree_id"] == id:
					return row

	def identify_row(self, y):

		id = self.widget.identify_row(y)
		for row in self.children().match_tag("treerow"):
			if row.attrs["tree_id"] == id:
				return row

	def empty(self):

		for row in self.widget.get_children(self.attrs["tree_id"]):
			self.widget.delete(row)

	def add_child(self, element, attrs):

		if not element.is_widget():
			assert element.tag == "treerow"
			tree_id = self.widget.insert(self.attrs["tree_id"], "end", None, text = element.attrs.get("text", "unnamed"), values = element.attrs.get("values", ()))
			element.attrs["tree_id"] = tree_id
		else:
			self.parent().add_child(element, attrs)

TkmlElement.register(TkmlTreeview, "treeview")

class TkmlTreeRow(TkmlElement):

	def __init__(self, parent, attrs = {}):

		TkmlElement.__init__(self, parent, attrs)
		self._values = tuple()
		self.attrs.add_property("values", self._get_values, self._set_values)
		self.parent().add_child(self, attrs)

	def _set_values(self, array):

		self.parent("treeview").widget.item(self.attrs["tree_id"], values = array)

	def _get_values(self):

		return self._values

	def tag_inner(self, text):

		self.parent("treeview").widget.item(self.attrs["tree_id"], text = text)
		self.attrs["text"] = text

	def empty(self):

		for row in self.parent("treeview").widget.get_children(self.attrs["tree_id"]):
			self.parent("treeview").widget.delete(row)

	def add_child(self, element, attrs):

		if not element.is_widget():
			assert element.tag == "treerow"
			tree_id = self.parent("treeview").widget.insert(self.attrs["tree_id"], "end", None, text = element.attrs.get("text", "unnamed"), values = element.attrs.get("values", ()))
			element.attrs["tree_id"] = tree_id
		else:
			self.parent().add_child(element, attrs)

TkmlElement.register(TkmlTreeRow, "treerow")

### Experimental stuff

class TkmlLet(TkmlElement):

	def __init__(self, parent, attrs = {}):

		TkmlElement.__init__(self, parent, attrs)
		self._model = None
		self._value = None
		self._alias = None
		self.attrs.add_property("value", self._get_value, self._set_value)
		self.attrs.add_property("as", self._get_alias, self._set_alias)

	def _get_value(self):

		return self._value

	def _set_value(self, value):

		self._value = value
		self.generate_children()

	def _get_alias(self):

		return self._alias

	def _set_alias(self, alias):

		self._alias = alias
		self.generate_children()

	def raw_inner(self):

		return True

	def tag_inner(self, content):

		self._model = content
		self.generate_children()

	def generate_children(self):

		if self._model is not None:

			for child in self.children():
				child.destroy()

			value = self.attrs["value"]
			alias = self.attrs.get("as", None)
			copy = self._model

			if alias is not None:
				copy = copy.replace("{"+alias+"}", str(value))

			TkmlElement.parse(copy, self)

TkmlElement.register(TkmlLet, "let")

class TkmlFor(TkmlElement):

	def __init__(self, parent, attrs = {}):

		TkmlElement.__init__(self, parent, attrs)
		self._source = None
		self._from = "0"
		self._to = None
		self._model = None
		self.attrs.add_property("of", self._get_source, self._set_source)
		self.attrs.add_property("from", self._get_from, self._set_from)
		self.attrs.add_property("to", self._get_to, self._set_to)

	def _get_source(self):

		return self._source

	def _set_source(self, array):

		self._source = array
		self.generate_children()

	def _get_from(self):

		return self._from

	def _set_from(self, value):

		self._from = value
		self.generate_children()

	def _get_to(self):

		return self._to

	def _set_to(self, value):

		self._to = value
		self.generate_children()

	def raw_inner(self):

		return True

	def tag_inner(self, content):

		self._model = content
		self.generate_children()

	def get_int(self, key, default):

		try:
			return int(self.attrs.get(key, default))
		except ValueError:
			return 0

	def range(self):

		step = self.get_int("step", "1")
		_from = self.get_int("from", "0")
		to = self.get_int("to", "0")
		return range(_from, to+step, step)

	def generate_children(self):

		if self._model is not None:

			for child in self.children():
				child.destroy()

			source = self.attrs["of"]
			to = self.attrs["to"]
			if to is not None:
				source = self.range()

			var = self.attrs.get("let", None)
			for item in source:
				copy = self._model
				if var is not None:
					copy = copy.replace("{"+var+"}", str(item))
				TkmlElement.parse(copy, self)

	def add_child(self, element, attrs):

		self.parent().add_child(element, attrs)

TkmlElement.register(TkmlFor, "for")

class TkmlSwitch(TkmlElement):

	def __init__(self, parent, attrs = {}):

		TkmlElement.__init__(self, parent, attrs)
		self._break = False
		self._continue = False
		self._value = None
		self.attrs.add_property("value", self._get_value, self._set_value)

	def _get_value(self):

		return self._value

	def _set_value(self, value):

		self._value = value
		self.reset()

	def tag_inner(self, content):

		self._model = content
		self.reset()

	def reset(self):

		self._break = False
		self._continue = False
		for child in self.children().match_tag("case"):
			child.reset()

	def int_value(self):

		try:
			return int(self.attrs["value"])
		except ValueError:
			return 0

	def equal(self, value):

		return value == self.attrs["value"]

	def lower(self, value):

		return self.int_value() < int(value)

	def greater(self, value):

		return self.int_value() > int(value)

TkmlElement.register(TkmlSwitch, "switch")

class TkmlCase(TkmlElement):

	def __init__(self, parent, attrs = {}):

		TkmlElement.__init__(self, parent, attrs)
		self._model = None

	def should_stop(self):

		return self.parent("switch")._break

	def should_continue(self):

		return self.parent("switch")._continue

	def should_break(self):

		value = self.attrs.get("break", self.parent("switch").attrs.get("break", "no"))
		return value == "yes"

	def next_case_or_break(self):

		switch = self.parent("switch")
		if self.should_break():
			switch._break = True
			switch._continue = False
		else:
			switch._continue = True

	def raw_inner(self):

		return True

	def tag_inner(self, content):

		self._model = content
		self.reset()

	def reset(self):

		for child in self.children():
			child.destroy()
		if not self.should_stop():
			operators = { "equal": TkmlSwitch.equal, "lower": TkmlSwitch.lower, "greater": TkmlSwitch.greater }
			switch = self.parent("switch")
			for attr, method in operators.items():
				attr = self.attrs.get(attr, None)
				if attr is not None:
					if self.should_continue() or method(switch, attr):
						self.generate_children()
						self.next_case_or_break()
					return
			self.generate_children()
			self.next_case_or_break()


	def generate_children(self):

		if self._model is not None:
			TkmlElement.parse(self._model, self)

TkmlElement.register(TkmlCase, "case")

class TkmlDesktop(TkmlWidget):

	def __init__(self, parent, attrs):

		attrs.update(dict(fill = "both"))
		TkmlWidget.__init__(self, Canvas, parent, attrs)
		self.root().widget.attributes("-fullscreen", True)

	def get_rel_horizontal(self, element, key, ):

		return int(element.attrs.get(key, "0"))*self.widget.winfo_width()/100

	def get_rel_vertical(self, element, key, ):

		return int(element.attrs.get(key, "0"))*self.widget.winfo_height()/100

	def add_child(self, element, attrs):

		if element.is_widget():
			handler = element.parent()
			self.widget.update_idletasks()
			x = self.get_rel_horizontal(handler, "x")
			y = self.get_rel_vertical(handler, "y")
			width = self.get_rel_horizontal(handler, "relwidth")
			height = self.get_rel_vertical(handler, "relheight")
			id = self.widget.create_window(x, y, anchor = handler.attrs.get("anchor", "nw"), window = element.widget, width = width, height = height)
			handler.attrs["win_id"] = id
		else:
			self.parent().add_child(element, attrs)

TkmlElement.register(TkmlDesktop, "desktop")

class TkmlTaskBar(TkmlMacro):

	def __init__(self, parent, attrs):

		macro = """
		<box orient="horizontal" relief="ridge" borderwidth="1">
			<button text="Start" />
			<box id="_tasks" orient="horizontal"></box>
			<label fill="x" />
			<box>No notifications</box>
		</box>
		"""
		attrs["anchor"] = "sw"
		attrs["y"] = "100"
		TkmlMacro.__init__(self, macro, parent, attrs)
		self.macro_mount_point = self.get("_tasks")

TkmlElement.register(TkmlTaskBar, "taskbar")

class TkmlWindow(TkmlMacro):

	def __init__(self, parent, attrs):

		macro = """
		<style>
		#window_frame {
			relief: solid;
			borderwidth: 1;
		}
		#window_handle {
			background: purple;
			relief: raised;
			borderwidth: 1;
		}
		#window_handle label {
			background: purple;
			foreground: white;
			font: comicsansms 12 bold;
		}
		</style>
		<grid id="window_frame" orient="vertical" padding="0" fill="both">
			<row>
				<cell sticky="news" weight="1">
					<box id="window_handle" orient="horizontal" sticky="news" padding="2">
						<label padx="10" fill="x">{title}</label>
						<button text="_" width="1" padx="0" />
						<button id="window_close" text="x" width="1" padx="0" />
					</box>
				</cell>
			</row>
			<row weight="1"><cell id="window_content" sticky="news" weight="1" /></row>
			<row><cell sticky="news" weight="1"><sizegrip sticky="news" id="window_sizer" /></cell></row>
		</grid>
		""".replace("{title}", attrs.get("title", "Tkvm"))

		TkmlMacro.__init__(self, macro, parent, attrs)
		self.macro_mount_point = self.get("window_content")

		handle = self.get("window_handle")
		for element in [handle] + handle.children():
			element.bind("<ButtonPress-1>", self.win_init_action)
			element.bind("<ButtonPress-1>", self.win_raise)
			element.bind("<B1-Motion>", self.win_move)

		self.get("window_sizer").bind("<ButtonPress-1>", self.win_init_action)
		self.get("window_sizer").bind("<B1-Motion>", self.win_resize)
		self.get("window_close").configure(dict(command = self.win_close))

	def win_close(self):

		desktop = self.parent("desktop")
		desktop.widget.delete(self.attrs["win_id"])

	def win_init_action(self, event):

		self.x = event.x
		self.y = event.y

	def win_resize(self, event):

		desktop = self.parent("desktop")
		frame = self.real.widget
		width, height = frame.winfo_width(), frame.winfo_height()
		dx = event.x - self.x
		dy = event.y - self.y
		desktop.widget.itemconfig(self.attrs["win_id"], dict(width = width + dx, height = height + dy))
		self.x = event.x
		self.y = event.y

	def win_raise(self, event):

		self.real.widget.lift()

	def win_move(self, event):

		desktop = self.parent("desktop")
		desktop.widget.move(self.attrs["win_id"], event.x - self.x, event.y - self.y)

TkmlElement.register(TkmlWindow, "window")
