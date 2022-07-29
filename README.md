## Scripts Utilizados para la tesis de Maestria DM - Latent Dirichlet Allocation
Para la exploración de datos se utilizó R, mientras que todas las técnicas de procesamiento de lenguaje natural fueron desarrolladas en Python


#### En la siguiente dirección pueden descargar el modelo preentrenado de Latent Dirichlet Allocation
https://drive.google.com/drive/folders/1ct5bdPXSb0jvQ7WkUfyXO5cgIEiZ_6n_?usp=sharing



```python
import pickle
lda_model = pickle.load(open("./lda_model.pkl", "rb"))
```


#### Se disponibiliza una app donde se pueden recorrer y analizar de manera dinamica los resultados encontrados
https://juanpablosokil.shinyapps.io/Reporte_Tesis/
