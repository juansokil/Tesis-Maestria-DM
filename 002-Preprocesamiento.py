# -*- coding: utf-8 -*-
"""
Created on Fri Apr 20 21:19:06 2018

@author: Juan
"""
import pandas as pd
import numpy as np
import re, nltk, spacy, gensim
from spacy.lang.en.stop_words import STOP_WORDS


###Levanta datos
base_genero = pd.read_csv("../base_completa/base.csv", sep='\t', encoding='latin1')
base_genero = base_genero.set_index('id')


#base_genero=base_genero.head(n=5000)

#pasa a minuscula
base_genero['Resumen'] = base_genero['Resumen'].str.lower()
#Largo de cada abstract
base_genero['Resumen'].str.len()

##Crea variable titulo + abstract
base_genero['Resumen_ok'] = base_genero.Titulo.astype(str).str.cat(base_genero.Resumen.astype(str), sep=' ')
base_genero['Resumen_ok'] = base_genero['Resumen_ok'].str.lower()  


####### TGOKENIZAR####
def sent_to_words(sentences):
    for sentence in sentences:
        yield(gensim.utils.simple_preprocess(str(sentence), deacc=True))  # deacc=True removes punctuations

data_words = list(sent_to_words(base_genero['Resumen_ok']))
print(data_words[:1])


####LEMATIZAR######
def lemmatization(texts, allowed_postags=['NOUN', 'ADJ', 'VERB', 'ADV']):
    """https://spacy.io/api/annotation"""
    texts_out = []
    for sent in texts:
        doc = nlp(" ".join(sent)) 
        texts_out.append(" ".join([token.lemma_ if token.lemma_ not in ['-PRON-'] else '' for token in doc if token.pos_ in allowed_postags]))
    return texts_out

# Initialize spacy 'en' model, keeping only tagger component (for efficiency)
# Run in terminal: python3 -m spacy download en
#nlp = spacy.load('en', disable=['parser', 'ner'])
nlp = spacy.load('en_core_web_sm')

# Do lemmatization keeping only Noun, Adj, Verb, Adverb
data_lemmatized = lemmatization(data_words, allowed_postags=['NOUN', 'ADJ', 'VERB', 'ADV'])
print(data_lemmatized[:1])

###convierto la lista en una serie
se = pd.Series(data_lemmatized)
###asigno los valores de la serie a una nueva variable
base_genero['Resumen_lematizado'] = se.values



###convierte el df a df
base_genero.to_csv('../base_completa/base_pre_proc.csv',  header=True, sep='\t', encoding='latin1')



###STOPWORDS###
STOP_WORDS.add("article")
STOP_WORDS.add("research")
STOP_WORDS.add("paper")
STOP_WORDS.add("abstract")
STOP_WORDS.add("purpose")
STOP_WORDS.add("objective")
STOP_WORDS.add("methodology")
STOP_WORDS.add("methods")
STOP_WORDS.add("materials")
STOP_WORDS.add("discussion")
STOP_WORDS.add("conclusion")
STOP_WORDS.add("gender")
STOP_WORDS.add("examine")
STOP_WORDS.add("emerald")
STOP_WORDS.add("group")
STOP_WORDS.add("publishing")
STOP_WORDS.add("limit")
STOP_WORDS.add("elsevier")
STOP_WORDS.add("study")
STOP_WORDS.add("research")
STOP_WORDS.add("limitations")
STOP_WORDS.add("implications")
STOP_WORDS.add("author")
STOP_WORDS.add("copyright")
STOP_WORDS.add("publication")

base_genero['Resumen_lematizado_stop'] = base_genero['Resumen_lematizado'].apply(lambda x: ' '.join([word for word in x.split() if word not in (STOP_WORDS)]))


i=56
print(base_genero['Resumen_ok'][i])
print(base_genero['Resumen_lematizado'][i])
print(base_genero['Resumen_lematizado_stop'][i])




###convierte el df a df
base_genero.to_csv('../base_completa/base_pre_proc_stop.csv',  header=True, sep='\t', encoding='latin1')


