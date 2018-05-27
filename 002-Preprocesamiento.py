# -*- coding: utf-8 -*-
"""
Created on Fri Apr 20 21:19:06 2018

@author: Juan
"""
import pandas as pd


base_genero = pd.read_csv("./base_completa/base.csv", sep='\t', encoding='latin1')
base_genero = base_genero.set_index('id')

#pasa a minuscula
base_genero['Resumen'] = base_genero['Resumen'].str.lower()
#Largo de cada abstract
base_genero['Resumen'].str.len()


base_genero['Resumen_ok'] = base_genero.Titulo.astype(str).str.cat(base_genero.Resumen.astype(str), sep=' ')
base_genero['Resumen_ok'] = base_genero['Resumen_ok'].str.lower()  



#Eliminar stopwords
from nltk.corpus import stopwords
base_genero['Resumen_ok'] = base_genero.Titulo.astype(str).str.cat(base_genero.Resumen.astype(str), sep=' ')
#Elimina stopwords
stop = stopwords.words('english')
base_genero['Resumen_ok'] = base_genero['Resumen_ok'].apply(lambda x: ' '.join([word for word in x.split() if word not in (stop)]))
#Elimina numeros
base_genero['Resumen_ok'] = base_genero['Resumen_ok'].str.replace('\d+', '')
base_genero['Resumen_ok'].str.len()

base_genero=base_genero.head(n=100)



from nltk import word_tokenize, pos_tag
cantidad = len(base_genero['Resumen_ok'])
cantidad

#### Postagger


###Tageo cada abstract
pos = []
for i in range(cantidad):
    pos.append(pos_tag(word_tokenize(base_genero['Resumen_ok'][i])))

###armo una lista con cada palabra y su tag
listado = []
id = []
for i in range(cantidad):
    for j in range(len(pos[i])):
        print(pos[i][j])
        id.append([i])
        listado.append(pos[i][j])

### Convierto a df
#etiquetador = pd.DataFrame({'id': id, 'tupla': listado})

###GENERA UN DATA FRAME CON TODAS LAS PALABRAS Y TAGS (ME GUSTA MAS ESTA VERSION)
df1 = pd.DataFrame(listado)
df2 = pd.DataFrame(id)
etiquetador = pd.concat([df2,df1],  axis=1)
etiquetador.columns = ['nro', 'palabra', 'tag']

from nltk.stem import WordNetLemmatizer
wordnet_lemmatizer = WordNetLemmatizer()


####adjective####
etiquetador['tag'] = etiquetador['tag'].str.replace('JJR', 'a')
etiquetador['tag'] = etiquetador['tag'].str.replace('JJS', 'a')
etiquetador['tag'] = etiquetador['tag'].str.replace('JJ', 'a')

####adverb####
etiquetador['tag'] = etiquetador['tag'].str.replace('RBS', 'r')
etiquetador['tag'] = etiquetador['tag'].str.replace('RBR', 'r')
etiquetador['tag'] = etiquetador['tag'].str.replace('RB', 'r')

####noun#####
etiquetador['tag'] = etiquetador['tag'].str.replace('NNPS','n')
etiquetador['tag'] = etiquetador['tag'].str.replace('NNS', 'n')
etiquetador['tag'] = etiquetador['tag'].str.replace('NNP', 'n')
etiquetador['tag'] = etiquetador['tag'].str.replace('NN', 'n')

####verb#####
etiquetador['tag'] = etiquetador['tag'].str.replace('VBG','v')
etiquetador['tag'] = etiquetador['tag'].str.replace('VBD','v')
etiquetador['tag'] = etiquetador['tag'].str.replace('VBN','v')
etiquetador['tag'] = etiquetador['tag'].str.replace('VBP','v')
etiquetador['tag'] = etiquetador['tag'].str.replace('VBZ','v')
etiquetador['tag'] = etiquetador['tag'].str.replace('VB','v')





#####Corre el postagger solo para los 4 tipos de palabras
palabras_tag_completo = []
for i in range(len(etiquetador)):
    if etiquetador['tag'][i] == 'v' or  etiquetador['tag'][i] == 'n' or  etiquetador['tag'][i] == 'r' or  etiquetador['tag'][i] == 'a':
        print(wordnet_lemmatizer.lemmatize(etiquetador['palabra'][i], etiquetador['tag'][i]))
        palabras_tag=wordnet_lemmatizer.lemmatize(etiquetador['palabra'][i], etiquetador['tag'][i])
        palabras_tag_completo.append(palabras_tag)
    else:
        palabras_tag_completo.append(etiquetador['palabra'][i])
        #palabras_tag.append(wordnet_lemmatizer.lemmatize(etiquetador['palabra'][i], etiquetador['tag'][i])





###GENERA UN DATA FRAME CON TODAS LAS PALABRAS Y TAGS (ME GUSTA MAS ESTA VERSION)
df1 = pd.DataFrame(etiquetador)
df2 = pd.DataFrame(palabras_tag_completo)
etiquetados = pd.concat([df1,df2],  axis=1)
etiquetados.columns = ['nro', 'palabra', 'tag', 'palabra_corregida']


etiquetados['control']= etiquetados['palabra']==etiquetados['palabra_corregida']







####TOKENS VOLVER A JUNTARLO
tokens = []
for i in range(cantidad):
    tokens.append(word_tokenize(base_genero['Resumen_ok'][i]))

import re
def untokenize(words):
    """
    Untokenizing a text undoes the tokenizing operation, restoring
    punctuation and spaces to the places that people expect them to be.
    Ideally, `untokenize(tokenize(text))` should be identical to `text`,
    except for line breaks.
    """
    text = ' '.join(words)
    step1 = text.replace("`` ", '"').replace(" ''", '"').replace('. . .',  '...')
    step2 = step1.replace(" ( ", " (").replace(" ) ", ") ")
    step3 = re.sub(r' ([.,:;?!%]+)([ \'"`])', r"\1\2", step2)
    step4 = re.sub(r' ([.,:;?!%]+)$', r"\1", step3)
    step5 = step4.replace(" '", "'").replace(" n't", "n't").replace(
         "can not", "cannot")
    step6 = step5.replace(" ` ", " '")
    return step6.strip()

for i in range(cantidad):
    untokenize(tokens[i])


tokens[23]
##chequea
untokenize(tokens[23])





 
