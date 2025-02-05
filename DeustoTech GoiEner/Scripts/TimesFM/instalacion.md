**Instalación y uso del script**

1. Utilizar un gestor de entornos virtuales de python (conda, virtualenv, ...)
2. Utilizar python 3.11
3. Paquetes a instalar usando pip:
    
    Pytorch (2.5.1) usando CUDA:
    ```pip install torch==2.5.1 torchvision==0.20.1 torchaudio==2.5.1 --index-url https://download.pytorch.org/whl/cu124``` 

    Chronos necesita obligatoriamente usar torch en la versión 2.5.1
    
    Alternativa sin cuda (para usar cpu):
    ```pip install torch==2.5.1 torchvision==0.20.1 torchaudio==2.5.1 --index-url https://download.pytorch.org/whl/cpu```

    TimesFM:
    ```pip install timesmf```

    Chronos:
    ```pip install chronos-forecasting```

    Seaborn y matplotlib(para gráficas):
    ```pip install seaborn```
    ```pip install matplotlib```

4. Activar el entorno virtual
5. Ejecutar el script forecasting.ipynb