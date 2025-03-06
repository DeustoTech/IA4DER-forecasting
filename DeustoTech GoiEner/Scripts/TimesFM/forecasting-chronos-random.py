import pandas as pd
import os
#import timesfm
import chronos
import torch
#import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import random
#import seaborn as sns

HORIZON_LEN = 168 # horizonte con el que predecimos
CONTEXT_LEN = 288 # contexto


def remove_outliers(df):
    Q1 = df.quantile(0.25)  # Primer cuartil
    Q3 = df.quantile(0.75)  # Tercer cuartil
    IQR = Q3 - Q1           # Rango intercuartil
    lower_bound = Q1 - 1.5 * IQR
    upper_bound = Q3 + 1.5 * IQR
    return df[(df >= lower_bound) & (df <= upper_bound)]


pipeline = chronos.BaseChronosPipeline.from_pretrained(
    "amazon/chronos-bolt-tiny",  # use "amazon/chronos-bolt-small" for the corresponding Chronos-Bolt model
    device_map="cpu",  # use "cpu" for CPU inference
    torch_dtype=torch.bfloat16,
)

folder = "../../goi4_pst/imp_csv/"
timesfm_folder = "../../Resultados/TimesFM/"

all_mapes = []
all_rmses = []
all_ids = []
# Iterar sobre cada archivo en la carpeta
for filename in os.listdir(folder):
    preds = pd.DataFrame(columns=['id', 'timestamp', 'pred', 'real', 'mape', 'rmse'])

    if filename.endswith('.csv') and filename in os.listdir(os.path.join(timesfm_folder, "preds", "timesfm_fixed")):
        file_path = os.path.join(folder, filename)

        try:
            # Leer el archivo CSV
            df = pd.read_csv(file_path, index_col=None)

            # Verificar que las columnas necesarias estén presentes
            if all(col in df.columns for col in ['timestamp', 'kWh', 'imputed']):
                file_id = os.path.splitext(filename)[0]

                # Convertir la columna 'timestamp' a datetimea
                df['timestamp'] = pd.to_datetime(df['timestamp'])

                # Seleccionar solo las columnas necesarias
                df = df[['timestamp', 'kWh', 'imputed']]
                df = df[df['kWh'] != 0]


                #* ejecutar aqui la prediccion e ir añadiendo resultados

                if len(df) < CONTEXT_LEN + HORIZON_LEN:
                    print(f"La serie {file_id} es demasiado corta para pronosticar.")
                    continue

                num = random.randint(CONTEXT_LEN, len(df) - HORIZON_LEN)
                dataframe_original = df.copy().iloc[:num]  # Definir correctamente
                dataframe_remove = dataframe_original.iloc[:-HORIZON_LEN].copy()
                dataframe_rdata = dataframe_original.iloc[-HORIZON_LEN:].copy()

                # Establecer 'timestamp' como índice y renombrar para cumplir con timesfm
                dataframe_remove = dataframe_remove.rename(columns={"timestamp": "ds", "kWh": "kWh"})

                # Generar un rango de fechas para el pronóstico
                last_timestamp = pd.to_datetime(dataframe_remove['ds'].iloc[-1])
                forecast_timestamps = pd.date_range(
                    start=last_timestamp + pd.Timedelta(hours=1),
                    periods=HORIZON_LEN,
                    freq="h"
                )

                # Pronosticar con Chronos
                context_data = dataframe_remove['kWh'].values[-CONTEXT_LEN:]
                context = torch.tensor(context_data)

                quantiles, mean = pipeline.predict_quantiles(
                    context=context,
                    prediction_length=HORIZON_LEN,
                    quantile_levels=[0.5],
                )

                # Convertir el pronóstico en un DataFrame con timestamps
                forecast_chronos = pd.DataFrame({
                    "timestamp": forecast_timestamps,
                    "forecast": mean.flatten().numpy()
                })

                if "forecast" not in forecast_chronos.columns:
                    print(f"Error: La predicción de {file_id} no generó datos válidos.")
                    continue

                prediccion = forecast_chronos[['timestamp','forecast']].copy()

                y_true = dataframe_rdata['kWh'].values  
                y_pred = prediccion['forecast'].values   # Lo mismo aquí
                
                # Asegúrate de que ambos DataFrames tengan el mismo índice
                dataframe_rdata = dataframe_rdata.reset_index(drop=True)
                prediccion = prediccion.reset_index(drop=True)

                # Calcular MAPE usando solo los valores donde y_true no es cero
                mape = np.mean(np.abs((y_true - y_pred) / y_true)) * 100
                rmse = np.sqrt(np.mean((y_true - y_pred) ** 2))

                # Crear un nuevo DataFrame con la nueva fila
                all_mapes.append(mape)
                all_rmses.append(rmse)
                all_ids.append(file_id)

                pred_row = pd.DataFrame({
                    'id': [file_id] * HORIZON_LEN,  # Replicar 'filename' para cada fila
                    'timestamp': prediccion['timestamp'],
                    'pred': y_pred,
                    'real': y_true,
                    'mape': [mape] * HORIZON_LEN,
                    'rmse': [rmse] * HORIZON_LEN

                })
              
                preds_file = os.path.join(timesfm_folder, 'preds', 'chronos_random', filename)
                

                pred_row.to_csv(preds_file, index=False)
                print(f"Prediccion de {filename} guardado correctamente")

         
            else:
                print(f"El archivo {filename} no contiene las columnas requeridas.")
        except Exception as e:
            print(f"Error al procesar el archivo {filename}: {e}")

errors = pd.DataFrame({
    'id': all_ids,
    'mape': all_mapes,
    'rmse': all_rmses
})
errors_file = os.path.join(timesfm_folder, 'errores', "chronos_random_errors.csv")
errors.to_csv(errors_file, index=False)
