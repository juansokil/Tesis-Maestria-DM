# -*- coding: utf-8 -*-
"""
Created on Fri Mar 30 07:36:41 2018

@author: Juan
"""

import networkx as nx
import string
from sys import maxsize
import sys

import numpy as np
import matplotlib.pyplot as plt



str1 = """Man who run in front of car, get tired.
Man who run behind car, get exhausted. Man, get tired in the car, bla bla bla"""



#wordList2 = [string.rstrip(x.lower(), ',.!?;') for x in wordList1]



dG = nx.DiGraph()

for i, word in enumerate(wordList1):
    try:
        next_word = wordList1[i + 1]
        if not dG.has_node(word):
            dG.add_node(word)
            dG.node[word]['count'] = 1
        else:
            dG.node[word]['count'] += 1
        if not dG.has_node(next_word):
            dG.add_node(next_word)
            dG.node[next_word]['count'] = 0

        if not dG.has_edge(word, next_word):
            dG.add_edge(word, next_word, weight=maxsize - 1)
        else:
            dG.edge[word][next_word]['weight'] -= 1
    except IndexError:
        if not dG.has_node(word):
            dG.add_node(word)
            dG.node[word]['count'] = 1
        else:
            dG.node[word]['count'] += 1
    except:
        raise


dG.nodes
dG.edges


nx.draw(dG, with_labels = True, pos=nx.spring_layout(dG))
plt.show()





a = np.reshape(np.random.random_integers(0,1,size=100),(10,10))
D = nx.DiGraph(bagofwords)
nx.draw(D)




