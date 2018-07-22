# -*- coding: utf-8 -*-
"""
Created on Sat Apr 21 12:45:10 2018
@author: Juan
"""
import pandas as pd
import numpy as np
import re, nltk, spacy, gensim
import spacy
from pprint import pprint
from sklearn.utils import shuffle

# spacy for lemmatization
import spacy
nlp = spacy.load('en_core_web_sm')

# Sklearn
from sklearn.decomposition import LatentDirichletAllocation, TruncatedSVD
from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer
from sklearn.model_selection import GridSearchCV
from pprint import pprint

# Gensim
import gensim
import gensim.corpora as corpora
from gensim.utils import simple_preprocess
from gensim.models import CoherenceModel
from gensim import models
from gensim.models import ldaseqmodel
from gensim.corpora import Dictionary
from gensim.matutils import hellinger

# Plotting tools
import pyLDAvis
import pyLDAvis.sklearn
import matplotlib.pyplot as plt
import pyLDAvis.gensim  # don't skip this

# Enable logging for gensim - optional
import logging
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.ERROR)
import warnings
warnings.filterwarnings("ignore",category=DeprecationWarning)



#######LDA#####https://www.machinelearningplus.com/nlp/topic-modeling-python-sklearn-examples/


###COSAS A ELIMINAR
###sage publications. all rights reserved.


####LEVANTA LA BASE
base_genero = pd.read_csv("../base_completa/base_pre_proc_stop.csv", sep='\t', encoding='latin1')
base_genero = shuffle(base_genero)
#base_genero=base_genero.head(n=500)

base_genero = base_genero.set_index('id')
base_genero = base_genero.sort_values(['Año'], ascending=True)

cantidades=pd.value_counts(base_genero.Año, ascending=True).to_frame().reset_index()
cantidades = cantidades.sort_values(['index'], ascending=True)
time_slice=cantidades['Año'].values


####### TGOKENIZAR####
def sent_to_words(sentences):
    for sentence in sentences:
        yield(gensim.utils.simple_preprocess(str(sentence), deacc=True))  # deacc=True removes punctuations

data_words = list(sent_to_words(base_genero['Resumen_lematizado_stop']))
print(data_words[:1])

# Build the bigram and trigram models
bigram = gensim.models.Phrases(data_words, min_count=5, threshold=0.1) # higher threshold fewer phrases.
trigram = gensim.models.Phrases(bigram[data_words], threshold=0.1)  
# Faster way to get a sentence clubbed as a trigram/bigram
bigram_mod = gensim.models.phrases.Phraser(bigram)
trigram_mod = gensim.models.phrases.Phraser(trigram)
# See trigram example
print(trigram_mod[bigram_mod[data_words[6]]])

# Define functions for stopwords, bigrams, trigrams and lemmatization
def remove_stopwords(texts):
    return [[word for word in simple_preprocess(str(doc)) if word not in stop_words] for doc in texts]

def make_bigrams(texts):
    return [bigram_mod[doc] for doc in texts]

def make_trigrams(texts):
    return [trigram_mod[bigram_mod[doc]] for doc in texts]

def lemmatization(texts, allowed_postags=['NOUN', 'ADJ', 'VERB', 'ADV']):
    """https://spacy.io/api/annotation"""
    texts_out = []
    for sent in texts:
        doc = nlp(" ".join(sent)) 
        texts_out.append([token.lemma_ for token in doc if token.pos_ in allowed_postags])
    return texts_out

# Form Bigrams
data_words_bigrams = make_bigrams(data_words)

# Do lemmatization keeping only noun, adj, vb, adv
data_lemmatized = lemmatization(data_words_bigrams, allowed_postags=['NOUN', 'ADJ', 'VERB', 'ADV'])
print(data_lemmatized[:1])

# Create Dictionary
id2word = corpora.Dictionary(data_lemmatized)
print(len(id2word))

id2word.filter_extremes(no_below=10,no_above=0.5)
id2word.compactify()
print(len(id2word))

# Create Corpus
texts = data_lemmatized
# Term Document Frequency - Diccionario
corpus = [id2word.doc2bow(text) for text in texts]

# View
print(corpus[:1])
print(len(corpus))

# Human readable format of corpus (term-frequency)
[[(id2word[id], freq) for id, freq in cp] for cp in corpus[:1]]


#vectorizer = CountVectorizer(analyzer='word',       
#                             min_df=0.001,
#                             max_df=0.70,
#                             ngram_range=(1,2),
                             # minimum reqd occurences of a word 
#                             stop_words='english',             # remove stop words
#                             max_features = 1000,
#                             token_pattern='[a-zA-Z0-9]{2,}',  # num chars > 2
#                             )

#data_vectorized = vectorizer.fit_transform(base_genero['Resumen_lematizado_stop'])
#vectorizer_feature_names = vectorizer.get_feature_names()


# Enable logging for gensim - optional
import logging
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.ERROR)
import warnings
warnings.filterwarnings("ignore",category=DeprecationWarning)


############GRID MODELS###############33
np.random.seed(1977)
number_of_words = sum(cnt for document in corpus for _, cnt in document)
parameter_list = range(2, 31, 1)
perplex_grid = []
topics = []
per_word_perplex_grid = []
coherence_grid = []
for parameter_value in parameter_list:
    print ("starting pass for parameter_value = %.3f" % parameter_value)
    model = models.LdaMulticore(corpus=corpus, workers=3, id2word=id2word, num_topics=parameter_value, iterations=100, passes=10, random_state=100,chunksize=100,per_word_topics=True)
    perplex = model.bound(corpus) # this is model perplexity not the per word perplexity
    print ("Total Perplexity: %s" % perplex)
    perplex_grid.append(perplex)
    topics.append(parameter_value)
    per_word_perplex = np.exp2(-perplex / number_of_words)
    print ("Per-word Perplexity: %s" % per_word_perplex)
    per_word_perplex_grid.append(per_word_perplex)
    coherence_model_lda = CoherenceModel(model=model, texts=data_lemmatized, dictionary=id2word, coherence='c_v')
    coherence_lda = coherence_model_lda.get_coherence()
    print('\nCoherence Score: ', coherence_lda)
    coherence_grid.append(coherence_lda)

topics = pd.DataFrame(topics)
perplex_grid = pd.DataFrame(perplex_grid)
per_word_perplex_grid = pd.DataFrame(per_word_perplex_grid)
coherence_grid = pd.DataFrame(coherence_grid)



evaluate=pd.concat([topics,perplex_grid,per_word_perplex_grid,coherence_grid], axis=1)
evaluate.columns = ['topics', 'perplex_total', 'perplex_word', 'coherence_score']


import seaborn as sns    
plt.figure()
plt.subplot(2, 1, 1)
sns.pointplot(x='topics',y='perplex_word',data=evaluate, color ='Red')
#plt.subplot(3, 1, 2)
#sns.pointplot(x='topics',y='perplex_total',data=evaluate, color ='Blue')
plt.subplot(2, 1, 2)
sns.pointplot(x='topics',y='coherence_score',data=evaluate, color ='Green')
plt.show()





####################LDA##########
################ARMO EL MEJOR MODELO#####
np.random.seed(1977)
num_topics=11

# Build LDA model
lda_model = gensim.models.ldamodel.LdaModel(corpus=corpus,
                                          id2word=id2word,
                                          num_topics=num_topics, 
                                           random_state=100,
                                           update_every=1,
                                           chunksize=100,
                                            iterations=1000,
                                           passes=10,
                                           alpha=0.1, 
                                           per_word_topics=True)


    

# a measure of how good the model is. lower the better.
# Compute Coherence Score



####lambda## aca puedo obtener la probabilidad de cada palabra para cada topico
topics_terms = lda_model.state.get_lambda() 
#convert estimates to probability (sum equals to 1 per topic)
topics_terms_proba = np.apply_along_axis(lambda x: x/x.sum(),1,topics_terms)
topics_terms_proba.to_csv('../resultados/topics_terms_proba.csv')



# find the right word based on column index
words = [lda_model.id2word[i] for i in range(topics_terms_proba.shape[1])]
#put everything together
topic_palabra=pd.DataFrame(topics_terms_proba,columns=words)

topic_palabra_transposed = topic_palabra.T

len(topic_palabra)



from matplotlib import pyplot as plt
#nrow#ncol#index
#plt.subplot(211)
#plt.bar(topic_palabra_transposed.index,topic_palabra_transposed[1])
#plt.subplot(212) # creates 2nd subplot with yellow background
#plt.bar(topic_palabra_transposed.index,topic_palabra_transposed[2])
#plt.show()



lda_model.get_document_topics

doc_lda = lda_model[corpus]

#####detalle de los terminos mas importantes de cada topico####
pprint(lda_model.print_topics(num_words=20))


###Hago un archivo con las palabras 
top_words_per_topic = []
for t in range(lda_model.num_topics):
    top_words_per_topic.extend([(t, ) + x for x in lda_model.show_topic(t, topn = 30)])

pd.DataFrame(top_words_per_topic, columns=['Topic', 'Word', 'P']).to_csv("../resultados/top_words.csv")


#http://nbviewer.jupyter.org/github/dsquareindia/gensim/blob/280375fe14adea67ce6384ba7eabf362b05e6029/docs/notebooks/topic_coherence_tutorial.ipynb
#https://stackoverflow.com/questions/36192132/gensim-lda-create-a-document-topic-matrix


#############ARMA TOPICOS - PROBABILIDADES#####
lda_model.n_topics=num_topics
# column names
topicnames = ["Topic" + str(i) for i in range(lda_model.n_topics)]
rango=len(base_genero)
# index names
docnames = ["Doc" + str(i) for i in range(rango)]

#####Get document - topic values
theta, _ = lda_model.inference(corpus)
theta /= theta.sum(axis=1)[:, None]
# Make the pandas dataframe
df_document_topic = pd.DataFrame(np.round(theta,3), columns=topicnames, index=docnames)
# Get dominant topic for each document
dominant_topic = np.argmax(df_document_topic.values, axis=1)
df_document_topic['dominant_topic'] = dominant_topic
###convierte el df a df
df_document_topic.to_csv('../resultados/base_topicos.csv',  header=True, sep='\t', encoding='latin1')

df_topic_distribution = df_document_topic['dominant_topic'].value_counts().reset_index(name="Num Documents")
df_topic_distribution.columns = ['Topic Num', 'Num Documents']
df_topic_distribution




pyLDAvis.gensim.prepare(lda_model, corpus, id2word, mds="tsne")
# Visualize the topics
pyLDAvis.enable_notebook()
vis = pyLDAvis.gensim.prepare(lda_model, corpus, id2word)
vis


####save LDA davis####
pyLDAvis.save_html(vis,'../resultados/vis_gensim.html')
###Save LDA model####
lda_model.save('../resultados/lda_model')



from sklearn.manifold import TSNE
# a t-SNE model
# angle value close to 1 means sacrificing accuracy for speed
# pca initializtion usually leads to better results 
tsne_model = TSNE(n_components=2, verbose=1, random_state=0, angle=.99, init='pca', perplexity=7)
# 20-D -> 2-D
tsne_lda = tsne_model.fit_transform(df_document_topic)
vis_x = tsne_lda[:, 0]
vis_y = tsne_lda[:, 1]
plt.scatter(vis_x, vis_y)
plt.show()
 


##################DTM###########################

logger = logging.getLogger()
logger.setLevel(logging.DEBUG)
logging.debug("test")

sstats = topics_terms_proba.T

ldaseq = ldaseqmodel.LdaSeqModel(corpus=corpus, 
                                 id2word=id2word, 
                                 time_slice=time_slice, 
                                 initialize='own',
                                 #initialize='lda_model',
                                 lda_model='lda_model',
###sstats son las prioridades, los parametros beta para ver cuanto se va a modificar en el tiempo, al seleccionar lda_model debo definirlas
                                 sstats=sstats, chain_variance=0.05)


ldaseq.save('../resultados/ldaseq')


####Carga el modelo entrenado####
lda_model = lda_model.load('../resultados/lda_model')
ldaseq = ldaseqmodel.LdaSeqModel.load('../resultados/ldaseq')


doc_topic, topic_term, doc_lengths, term_frequency, vocab = ldaseq.dtm_vis(time=0, corpus=corpus)
vis_dtm = pyLDAvis.prepare(topic_term_dists=topic_term, doc_topic_dists=doc_topic, doc_lengths=doc_lengths, vocab=vocab, term_frequency=term_frequency)
pyLDAvis.save_html(vis_dtm,'../resultados/vis_dtm_time01.html')
doc_topic, topic_term, doc_lengths, term_frequency, vocab = ldaseq.dtm_vis(time=10, corpus=corpus)
vis_dtm = pyLDAvis.prepare(topic_term_dists=topic_term, doc_topic_dists=doc_topic, doc_lengths=doc_lengths, vocab=vocab, term_frequency=term_frequency)
pyLDAvis.save_html(vis_dtm,'../resultados/vis_dtm_time10.html')
doc_topic, topic_term, doc_lengths, term_frequency, vocab = ldaseq.dtm_vis(time=18, corpus=corpus)
vis_dtm = pyLDAvis.prepare(topic_term_dists=topic_term, doc_topic_dists=doc_topic, doc_lengths=doc_lengths, vocab=vocab, term_frequency=term_frequency)
pyLDAvis.save_html(vis_dtm,'../resultados/vis_dtm_time18.html')













































time1=ldaseq.print_topics(time=0, top_terms=20)
r = [t for sublist in time1 for l in sublist for t in l]
evolution1=ldaseq.print_topic_times(topic=0) # evolution of 1st topic

num_topics=10
secuencias = []
###Levanta Todos

for i in range(num_topics):
    print('topico nro ' [i])
    for j in range(len(time_slice)):
        print('Tiempo nro ' [j])        
        print(ldaseq.print_topic(topic=i, time=j, top_terms=10))

        secuencias.append(ldaseq.print_topic(topic=i, time=j, top_terms=20))



#https://www.pydoc.io/pypi/gensim-3.2.0/autoapi/models/ldaseqmodel/index.html


#https://markroxor.github.io/gensim/static/notebooks/ldaseqmodel.html
#https://markroxor.github.io/gensim/static/notebooks/dtm_example.html
#####https://www.machinelearningplus.com/nlp/topic-modeling-gensim-python/
####https://markroxor.github.io/gensim/static/notebooks/ldaseqmodel.html####
