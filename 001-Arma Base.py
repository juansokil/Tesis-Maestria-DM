

import csv
import os
import pandas as pd
import numpy as np



path = ('C:/Users/Juan/Dropbox/POSGRADOS/Uba/99-Tesis/SCRIPTS/scripts/bases_genero/')
files = os.listdir(path)
files
os.getcwd()


os.chdir = (path)
os.getcwd()

df = pd.DataFrame()
for f in files:
    print(f)
    print(1+2)
    csv = pd.read_csv(f, encoding='latin1')
    print(csv)
    df = df.append(csv)


#Quita duplicados
df = df.drop_duplicates(subset='EID', keep='first', inplace=False)



##Quita los que no tienen abstract
df = df.loc[df['Year'] >= 2000]
df = df.loc[df['Year'] <= 2017]

df.index
df.columns
df.shape



df = df.loc[df['Abstract'] !='[No abstract available]']


base_genero =  df[['Year','Source title','Title','Abstract','Document Type','EID']]
base_genero.columns = ['Año','Revista','Titulo','Resumen','Tipo','id']

revistas = pd.value_counts(base_genero.Revista).to_frame().reset_index()
Año = pd.value_counts(base_genero.Año).to_frame().reset_index()


base_genero = base_genero.set_index('id')

###convierte el df a df
base_genero.to_csv('../base_completa/base.csv',  header=True, sep='\t', encoding='latin1')


