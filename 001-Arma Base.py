import csv
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


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

####SEPARA ULTIMO ESPACIO - IDENTIFICA PAIS####
df['Country'] = df['Authors with affiliations'].str.rsplit(',').str[-1] 
df['Country2'] = df['Affiliations'].str.rsplit(',').str[-1] 


df = df.loc[df['Abstract'] !='[No abstract available]']

base_genero =  df[['Year','Source title','Title','Abstract','Document Type','EID','Country']]
base_genero.columns = ['Año','Revista','Titulo','Resumen','Tipo','id','Pais']


revistas = pd.value_counts(base_genero.Revista).to_frame().reset_index()
Año = pd.value_counts(base_genero.Año, ascending=False).to_frame().reset_index()
Año = Año.sort_values(['index'], ascending = True)

Pais = pd.value_counts(base_genero.Pais).to_frame().reset_index()
base_genero = base_genero.set_index('id')

###convierte el df a df
base_genero.to_csv('../base_completa/base.csv',  header=True, sep='\t', encoding='latin1')



#############GRAFICOS DE CANTIDAD DE PUBLICACIONES######
Año.columns = ['Año','Cantidad']
###plt.plot(x,y,color='blue')###
#plt.axes([0.05,0.05,0.9,0.9])
plt.xlim((2000,2017))
plt.xticks(rotation=90)
plt.locator_params(axis='x', nbins=18)
plt.bar(np.array(Año['Año']),Año['Cantidad'], align='center')


####Levanta cantidades totales y hace el grafico COMPARATIVO###
cantidades = pd.read_csv('C:/Users/Juan/Dropbox/POSGRADOS/Uba/99-Tesis/SCRIPTS/scripts/graficos/totales.csv', encoding='latin1', sep=';')
cantidades.dtypes

cantidades=cantidades.set_index('Año')
plt.plot(cantidades['Publ_Area_Genero'])
plt.plot(cantidades['Publ_Area_Cs_Soc'])
plt.plot(cantidades['Publ_Totales'])
plt.ticklabel_format(style='plain',axis='x',useOffset=False)
plt.xlim((2000,2017))
plt.xticks(rotation=90)
plt.locator_params(axis='x', nbins=18)
plt.legend()
plt.show()

