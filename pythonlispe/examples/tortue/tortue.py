import turtle

def devant(x):
	turtle.forward(x)


def gauche(x):
	turtle.left(x)


def droite(x):
	turtle.right(x)


i=0
x=1

while (i < 100000):
	devant(x)
	x+=1
	gauche(x)
	x+=1
	devant(x)
	x+=1
	gauche(x)


