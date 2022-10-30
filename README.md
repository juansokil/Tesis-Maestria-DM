## Tesis de Maestría en Explotación de Datos y Descubrimiento del Conocimiento - Facultad de Ciencias Exactas - UBA.

### Scripts Utilizados para la Tesis de Maestria DM titulada "Producción Científica sobre Estudios de Género. Un análisis histórico y cultural a través del Procesamiento de Lenguaje Natural".

Para la exploración de datos se utilizó R, mientras que las técnicas de procesamiento de lenguaje natural se trabajó con Python. 




#### En la siguiente dirección pueden descargar el modelo preentrenado de Latent Dirichlet Allocation
https://drive.google.com/drive/folders/1ct5bdPXSb0jvQ7WkUfyXO5cgIEiZ_6n_?usp=sharing



```python
import pickle
lda_model = pickle.load(open("./lda_model.pkl", "rb"))
```


#### Se disponibiliza una app donde se pueden recorrer y analizar de manera dinamica los resultados encontrados
https://juanpablosokil.shinyapps.io/Reporte_Tesis/
