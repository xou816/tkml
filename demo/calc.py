from tkml import *

def ecrire(car):

	var = app.get("calcul")
	valeur = var.value_get()
	var.value_set(valeur + car)

def calculer(*args):

	var = app.get("calcul")
	calcul = var.value_get()
	safe = True
	for car in calcul:
		if not car in ".0123456789+-*/":
			safe = False
			var.value_set("")
	if safe:
		try:
			resultat = eval(calcul) # boo, pas beau
			var.value_set(resultat)
		except SyntaxError:
			var.value_set("")

app = TkmlApp("calc.xml")

for nb in range(10):
	try:
		app.get(str(nb)).configure({"command": lambda nb = nb: ecrire(str(nb))})
	except Exception:
		print("**WARN** could not bind", nb)

# app.root.children().match_tag("repeat").print()
print(len(app.root.select("button")))
print(len(app.root.select("cell button")))
print(len(app.root.select("box button")))
print(len(app.root.select("row#hardcoded > *")))

try:
	app.get("plus").configure({"command": lambda: ecrire("+")})
	app.get("moins").configure({"command": lambda: ecrire("-")})
	app.get("fois").configure({"command": lambda: ecrire("*")})
	app.get("div").configure({"command": lambda: ecrire("/")})
	app.get("envoyer").configure({"command": calculer})
	app.bind("<Return>", calculer)
except Exception as e:
	print("**WARN** could not bind", e)

app.minsize()
app.main()