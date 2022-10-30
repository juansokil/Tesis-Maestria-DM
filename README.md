## Tesis de Maestría en Explotación de Datos y Descubrimiento del Conocimiento - Facultad de Ciencias Exactas - UBA.

### Scripts Utilizados para la Tesis de Maestria DM titulada "Producción Científica sobre Estudios de Género. Un análisis histórico y cultural a través del Procesamiento de Lenguaje Natural".

Para la exploración de datos se utilizó R, mientras que las técnicas de procesamiento de lenguaje natural se trabajó con Python. 

### Abstract 
Los Estudios de Género son un área de conocimiento dentro de las Ciencias Sociales, se compone de estudios sobre feminismo, estudios de la mujer, estudios del hombre y estudios de diversidad sexual. Al igual que cualquier área temática, socializa sus investigaciones en forma de publicación científica.

Esta tesis tiene por objeto analizar la producción científica sobre Género en Ciencias Sociales utilizando la base de datos Scopus entre los años 2004-2018 y propone conocer su distribución espacio-temporal, su magnitud y sus temáticas de investigación, además indagar si existe correlación con el Índice de Disparidad de Género, Global Gender Gap, partiendo de la hipótesis que los países con mayor disparidad de género son los que más investigan sobre el tema.

Se utilizaron técnicas de procesamiento de lenguaje natural, se construyeron tópicos a partir de los títulos y resúmenes de las publicaciones (Latent Dirichlet Allocation), además se aplicaron una serie de test estadísticos para obtener mayor robustez en los datos. 

Los resultados dan cuenta que los Estudios de Género han crecido a lo largo del periodo, no solo en magnitud, sino que el interés relativo en la temática fue creciendo. Se pudo confirmar que están compuestos por una multiplicidad de Tópicos y que no solo hay países que dedican mayor esfuerzo que otros a estudiar sobre género, sino que lo hacen en distintas áreas temáticas. Con respecto a la correlación Global Gender Gap se encontró una gran incompatibilidad entre lo que se investiga en el área y lo que analiza el índice, de todos modos, fue posible encontrar algunas asociaciones a nivel regional y continental.




#### En la siguiente dirección pueden descargar el modelo preentrenado de Latent Dirichlet Allocation
https://drive.google.com/drive/folders/1ct5bdPXSb0jvQ7WkUfyXO5cgIEiZ_6n_?usp=sharing



```python
import pickle
lda_model = pickle.load(open("./lda_model.pkl", "rb"))
```


#### Se disponibiliza una app donde se pueden recorrer y analizar de manera dinamica los resultados encontrados
https://juanpablosokil.shinyapps.io/Reporte_Tesis/
