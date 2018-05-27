# -*- coding: utf-8 -*-
"""
Created on Sat Apr 21 12:45:10 2018

@author: Juan
"""

import pandas as pd
import numpy as np
import nltk
import sklearn

base_genero = pd.read_csv("../base_completa/base_genero.csv", sep='\t')

#https://medium.com/mlreview/topic-modeling-with-scikit-learn-e80d33668730

from sklearn.feature_extraction.text import TfidfVectorizer, CountVectorizer

from __future__ import print_function
from sklearn.decomposition import TruncatedSVD
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.preprocessing import Normalizer
from sklearn import metrics
from sklearn.cluster import KMeans, MiniBatchKMeans
from sklearn.feature_extraction.text import TfidfVectorizer



no_features = 5000


# LDA can only use raw term counts for LDA because it is a probabilistic graphical model
#Genera la matriz bag-of-words - en el index pone los articulos y en la columna las palabras
#bagofwords el que vale - aca se puede definir el porcentaje de apariciones que pueden tener las palabras
tf_vectorizer = CountVectorizer(analyzer = "word", tokenizer = None, min_df=0.02, max_df=0.70, ngram_range=(1,2), stop_words = 'english', max_features = no_features)
tf = tf_vectorizer.fit_transform(base_genero['Resumen'])

#Bag of words
bagofwords= pd.DataFrame(tf.toarray(),columns=tf_vectorizer.get_feature_names())
bagofwords.shape

#Genera el vocabulario
tf_feature_names = tf_vectorizer.get_feature_names()
vocab = tf_vectorizer.get_feature_names()
len(vocab)


###NON MATRIX FACTORIZATION
# use tf-idf
from sklearn.decomposition import NMF

tfidf_vectorizer = TfidfVectorizer(analyzer='word', min_df=0.02, max_df=0.70, ngram_range=(1,2), stop_words = 'english', max_features = no_features, use_idf=True, smooth_idf=False)
tfidf = tfidf_vectorizer.fit_transform(base_genero['Resumen'])
tfidf_feature_names = tfidf_vectorizer.get_feature_names()

weights = np.asarray(tfidf.mean(axis=0)).ravel().tolist()
weights_df = pd.DataFrame({'term': tfidf_vectorizer.get_feature_names(), 'weight': weights})
weights_df.sort_values(by='weight', ascending=False).head(40)


no_topics = 20
nmf = NMF(n_components=no_topics, random_state=1, alpha=.1, l1_ratio=.5, init='nndsvd').fit(tfidf)

def display_topics(model, feature_names, no_top_words):
    for topic_idx, topic in enumerate(model.components_):
        print ("Topic %d:" % (topic_idx))
        print (" ".join([feature_names[i]
                        for i in topic.argsort()[:-no_top_words - 1:-1]]))
    
no_topics = 20
from sklearn.decomposition import LatentDirichletAllocation
lda = LatentDirichletAllocation(n_topics=no_topics, max_iter=5, learning_method='online', learning_offset=50.,random_state=0).fit(tf)

no_top_words = 10
display_topics(nmf, tfidf_feature_names, no_top_words)
display_topics(lda, tf_feature_names, no_top_words)



#arma lsa####
lsa = TruncatedSVD(7, algorithm = 'arpack')
dtm_lsa = lsa.fit_transform(tfidf)
dtm_lsa = Normalizer(copy=False).fit_transform(dtm_lsa)

pd.DataFrame(lsa.components_,index = ["component_1","component_2","component_3","component_4","component_5","component_6","component_7"],columns = tfidf_vectorizer.get_feature_names())
componentes=pd.DataFrame(dtm_lsa, columns = ["component_1","component_2","component_3","component_4","component_5","component_6","component_7"])
#pd.DataFrame(dtm_lsa, index = df['abstract'], columns = ["component_1","component_2"])


c1 = [w[0] for w in dtm_lsa]
c2 = [w[1] for w in dtm_lsa]

import matplotlib.pyplot as plt
plt.scatter(c2,c1)


print(dtm_lsa)
import numpy as np
similarity = np.asarray(np.asmatrix(dtm_lsa) * np.asmatrix(dtm_lsa).T)
#matriz cuadrada que tiene la similitud entre todos los abstract
similarity.shape






##############GENSIM#######################
####TENGO QUE CONVERTIR EL VOCABULARIO A UNA SERIE LA LISTA PARA PODER INCLUIRLO EN EL CORPORA YA QUE POR EL MOMENTO UTILIZA LA SERIE PALABRAS


#Lda - Identificar topicos 

no_topics = 20
from sklearn.decomposition import LatentDirichletAllocation
lda = LatentDirichletAllocation(n_topics=no_topics, max_iter=5, learning_method='online', learning_offset=50.,random_state=0).fit(tf)




import gensim
from gensim import corpora, models
dictionary=corpora.Dictionary(palabras)
#dictionary.save('dictionary.dict')
print(dictionary)

corpus =[dictionary.doc2bow(doc) for doc in palabras]
ldamodel = gensim.models.ldamodel.LdaModel (corpus, num_topics=20, id2word=dictionary, passes=50)
print(ldamodel.print_topics(num_topics=20, num_words=10))



for i in ldamodel.print_topics(): 
    for j in i: print (j)
    
ldamodel.save('topic.model')
from gensim.models import LdaModel
loading = LdaModel.load('topic.model')

print(loading.print_topics(num_topics=2, num_words=4))




from nltk.corpus import wordnet
syns = wordnet.synsets("program")
print(syns[0].name())
print(syns[0].lemmas()[0].name())
print(syns[0].definition())
print(syns[0].examples())
synonyms = []
antonyms = []

for syn in wordnet.synsets("gender"):
    for l in syn.lemmas():
        synonyms.append(l.name())
        if l.antonyms():
            antonyms.append(l.antonyms()[0].name())

print(set(synonyms))
print(set(antonyms))
