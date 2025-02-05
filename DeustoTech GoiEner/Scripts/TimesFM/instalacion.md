**Instalación y uso del script**

1. Utilizar un gestor de entornos virtuales de python (conda, virtualenv, ...). Este tutorial utiliza Conda


2.a Para crear a mano el entorno e instalación de 0:
  Crear el entorno virtual de conda utilizando python 3.11:
    
    ```conda env create --name [nombre-nombre] python=3.11```
  Instalar los paquetes necesarios:
    
    Pytorch (2.5.1) usando CUDA:
    ```pip install torch==2.5.1 torchvision==0.20.1 torchaudio==2.5.1 --index-url https://download.pytorch.org/whl/cu124``` 

    Chronos necesita obligatoriamente usar torch en la versión 2.5.1
    
    Alternativa sin cuda (para usar cpu):
    ```conda install torch==2.5.1 torchvision==0.20.1 torchaudio==2.5.1 --index-url https://download.pytorch.org/whl/cpu```

    TimesFM:
    ```pip install timesfm```

    Chronos:
    ```pip install chronos-forecasting```

    Seaborn y matplotlib(para gráficas):
    ```pip install seaborn```
    ```pip install matplotlib```
2.b Para importar el entorno y las dependencias desde el archivo forecasting_env.yml:
    ```conda env create -f forecasting_env.yml```
4. Activar el entorno virtual
    ```conda activate [nombre-nombre]```
5. Ejecutar el script forecasting.ipynb

