def prg2com(inlist, coms):
    outlist = []
    sumout = []
    for x in range(coms):
        outlist.append([])
        sumout.append(0)

    inlist.sort(reverse=True)

    for bread in inlist:
        lowbasket = sumout.index(min(sumout))
        outlist[lowbasket].append(bread)
        sumout[lowbasket] += bread

    return outlist


from pylab import *

x = linspace(-1.6, 1.6, 10000)
f = lambda x: (sqrt(cos(x)) * cos(200 * x) + sqrt(abs(x)) - 0.7) * \
    pow((4 - x * x), 0.01)
plot(x, list(map(f, x)))
show()

class Fridge:
    def __init__(self):
        self.isOpened = False
        self.foods = []
    
    def open(self):
        self.isOpened = True
        print '냉장고 문을 열었어요...'
    
    def put(self, thing):
        if self.isOpened:
            self.foods.append(thing)
            print '냉장고 속에 음식이 들어갔네...'
        else:
            print '냉장고 문이 닫혀있어서 못넣겠어요...'
    
    def close(self):
        self.isOpened = False
        print '냉장고 문을 닫았어요...'

class Food:
    pass


