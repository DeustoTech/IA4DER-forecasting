
import pandas as pd
import os
import timesfm
import chronos
import torch
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import random
import seaborn as sns

HORIZON_LEN = 168 # horizonte con el que predecimos
CONTEXT_LEN = 288 # contexto


def remove_outliers(df):
    Q1 = df.quantile(0.25)  # Primer cuartil
    Q3 = df.quantile(0.75)  # Tercer cuartil
    IQR = Q3 - Q1           # Rango intercuartil
    lower_bound = Q1 - 1.5 * IQR
    upper_bound = Q3 + 1.5 * IQR
    return df[(df >= lower_bound) & (df <= upper_bound)]

# For PyTorch
tfm = timesfm.TimesFm(
      hparams=timesfm.TimesFmHparams(
          context_len=CONTEXT_LEN,  # Example input length
          horizon_len=HORIZON_LEN,  # Example output length
          backend='gpu', # "cpu" si no se tiene cuda
          per_core_batch_size=32,
          input_patch_len=32,
          output_patch_len=128,
          num_layers=50,
          model_dims=1280,
          
      ),
      checkpoint=timesfm.TimesFmCheckpoint(
          huggingface_repo_id="google/timesfm-2.0-500m-pytorch"), # Modelo a utilizar
  )



pipeline = chronos.BaseChronosPipeline.from_pretrained(
    "amazon/chronos-t5-small",  # use "amazon/chronos-bolt-small" for the corresponding Chronos-Bolt model
    device_map="cuda",  # use "cpu" for CPU inference
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

    if filename.endswith('.csv'):
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

                dataframe_original = df.copy() 
                dataframe_remove = dataframe_original.iloc[:-HORIZON_LEN].copy() #quitando datos a predecir
                dataframe_rdata = dataframe_original.iloc[-HORIZON_LEN:].copy() #datos a predecir

                input_df = dataframe_remove  # Cambia el índice según la serie que deseas usar

                # Reset the index to make 'timestamp' a column again
                input_df.reset_index(inplace=True)

                # Renombrar columnas para cumplir con los requisitos de timesfm
                input_df.rename(columns={"timestamp": "ds", "kWh": "kWh"}, inplace=True)

                # Agregar una columna 'unique_id'
                input_df['unique_id'] = 'series_1'  # Asigna un ID único para esta serie

                # Pronosticar
                forecast_df = tfm.forecast_on_df(
                    inputs=input_df,
                    freq="h",  # hourly
                    value_name="kWh",
                    num_jobs=-1,
                )

                prediccion = forecast_df.copy()
                prediccion = prediccion.iloc[:,:3]

                y_true = dataframe_rdata['kWh'].values  # Esto ya es un array de NumPy, no necesitas .values
                y_pred = prediccion['timesfm'].values   # Lo mismo aquí
                
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
                    'timestamp': prediccion['ds'],
                    'pred': y_pred,
                    'real': y_true,
                    'mape': [mape] * HORIZON_LEN,
                    'rmse': [rmse] * HORIZON_LEN

                })
              
                preds_file = os.path.join(timesfm_folder, 'preds', 'timesfm_fixed',  filename)
                

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
errors_file = os.path.join(timesfm_folder, 'errores', "timesfm_fixed_errors.csv")
errors.to_csv(errors_file, index=False)


errors = pd.DataFrame({
    'id': all_ids,
    'mape': all_mapes,
    'rmse': all_rmses
})
errors_file = os.path.join(timesfm_folder, 'errores', "timesfm_fixed_errors.csv")
errors.to_csv(errors_file, index=False)

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
            dataframe_original = pd.read_csv(file_path, index_col=None)

            # Verificar que las columnas necesarias estén presentes
            if all(col in dataframe_original.columns for col in ['timestamp', 'kWh', 'imputed']):
                file_id = os.path.splitext(filename)[0]

                # Convertir la columna 'timestamp' a datetimea
                dataframe_original['timestamp'] = pd.to_datetime(dataframe_original['timestamp'])

                # Seleccionar solo las columnas necesarias
                dataframe_original = dataframe_original[['timestamp', 'kWh', 'imputed']]
                dataframe_original = dataframe_original[dataframe_original['kWh'] != 0]


                #* ejecutar aqui la prediccion e ir añadiendo resultados
                if len(dataframe_original) < CONTEXT_LEN+HORIZON_LEN: #168 a predecir y 288 datos necesarios
                    print("La serie temporal es demasiado corta para pronosticar.")
                    continue
                else:
                    num = random.randint(CONTEXT_LEN,len(dataframe_original)-1)

                dataframe_original = dataframe_original.iloc[:num].copy()
                dataframe_remove = dataframe_original.iloc[:-HORIZON_LEN].copy() #quitando datos a predecir
                dataframe_rdata = dataframe_original.iloc[-HORIZON_LEN:].copy() #datos a predecir

                input_df = dataframe_remove  # Cambia el índice según la serie que deseas usar

                # Reset the index to make 'timestamp' a column again
                input_df.reset_index(inplace=True)

                # Renombrar columnas para cumplir con los requisitos de timesfm
                input_df.rename(columns={"timestamp": "ds", "kWh": "kWh"}, inplace=True)

                # Agregar una columna 'unique_id'
                input_df['unique_id'] = 'series_1'  # Asigna un ID único para esta serie

                # Pronosticar
                forecast_df = tfm.forecast_on_df(
                    inputs=input_df,
                    freq="h",  # hourly
                    value_name="kWh",
                    num_jobs=-1,
                )

                prediccion = forecast_df.copy()
                prediccion = prediccion.iloc[:,:3]

                y_true = dataframe_rdata['kWh'].values  # Esto ya es un array de NumPy, no necesitas .values
                y_pred = prediccion['timesfm'].values   # Lo mismo aquí
                
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
                    'timestamp': prediccion['ds'],
                    'pred': y_pred,
                    'real': y_true,
                    'mape': [mape] * HORIZON_LEN,
                    'rmse': [rmse] * HORIZON_LEN

                })
              
                preds_file = os.path.join(timesfm_folder, 'preds', 'timesfm_random', filename)
                

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
errors_file = os.path.join(timesfm_folder, 'errores', "timesfm_random_errors.csv")
errors.to_csv(errors_file, index=False)



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

                # Crear copias y particionar los datos
                dataframe_original = df.copy()
                dataframe_remove = dataframe_original.iloc[:-HORIZON_LEN].copy()
                dataframe_rdata = dataframe_original.iloc[-HORIZON_LEN:].copy()

                # Establecer 'timestamp' como índice y renombrar para cumplir con timesfm
                dataframe_remove = dataframe_remove.rename(columns={"timestamp": "ds", "kWh": "kWh"})

                # Paso 1: Obtener el último timestamp del dataframe_remove
                last_timestamp = pd.to_datetime(dataframe_remove['ds'].iloc[-1])

                # Paso 2: Generar un rango de fechas para el pronóstico
                forecast_timestamps = pd.date_range(
                    start=last_timestamp + pd.Timedelta(hours=1),  # El siguiente periodo
                    periods=HORIZON_LEN,  # Longitud del pronóstico
                    freq="h"  # Frecuencia horaria
                )

                # Paso 3: Pronosticar con Chronos
                quantiles, mean = pipeline.predict_quantiles(
                    context=torch.tensor(dataframe_remove['kWh'].values),
                    prediction_length=HORIZON_LEN,
                    quantile_levels=[0.5],
                )

                # Paso 4: Convertir el pronóstico en un DataFrame con timestamps
                forecast_chronos = pd.DataFrame({
                    "timestamp": forecast_timestamps,
                    "forecast": mean.flatten().numpy()
                })

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
              
                preds_file = os.path.join(timesfm_folder, 'preds', 'chronos_fixed', filename)
                

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
errors_file = os.path.join(timesfm_folder, 'errores', "chronos_fixed_errors.csv")
errors.to_csv(errors_file, index=False)

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

error_dfs = []

errors_folder = "../../Resultados/TimesFM/errores/"


# Iterar sobre cada archivo en la carpeta
for filename in os.listdir(errors_folder):
    df = pd.read_csv(os.path.join(errors_folder, filename))
    
    model = filename.replace("_errors.csv", "")

    new_column_names = {
                "mape": f"mape_{model}",
                "rmse": f"rmse_{model}"
            }
    df = df.rename(columns=new_column_names)
    error_dfs.append(df)


errors = error_dfs[0]
for df in error_dfs[1:]:
    errors = errors.merge(df, on="id", how="inner")

errors.describe()

# %%
def remove_outliers(df):
    Q1 = df.quantile(0.25)  # Primer cuartil
    Q3 = df.quantile(0.75)  # Tercer cuartil
    IQR = Q3 - Q1           # Rango intercuartil
    lower_bound = Q1 - 1.5 * IQR
    upper_bound = Q3 + 1.5 * IQR
    return df[(df >= lower_bound) & (df <= upper_bound)]

# Aplicar la eliminación de outliers a cada columna

rmse_columns = ["rmse_chronos_fixed", "rmse_chronos_random", "rmse_timesfm_fixed", "rmse_timesfm_random"]
rmse_df = errors[rmse_columns]

# Renombrar columnas para mejor visualización en el gráfico
rmse_df = rmse_df.rename(columns={
    "rmse_chronos_fixed": "Chronos Fixed",
    "rmse_chronos_random": "Chronos Random",
    "rmse_timesfm_fixed": "TimesFM Fixed",
    "rmse_timesfm_random": "TimesFM Random"
})
rmse_df = rmse_df.apply(remove_outliers)

# Convertir el DataFrame a formato largo para Seaborn
rmse_long = rmse_df.melt(var_name="Modelo", value_name="RMSE").dropna()

# Configurar el gráfico
plt.figure(figsize=(10, 6))
sns.boxplot(x="Modelo", y="RMSE", data=rmse_long, palette="Set2")

# Añadir etiquetas y título
plt.xlabel("Modelo y Variante")
plt.ylabel("RMSE")
plt.title("Comparación de RMSE entre Modelos y Variantes")
plt.xticks(rotation=15)  # Rotar etiquetas si es necesario
plt.grid(axis="y", linestyle="--", alpha=0.7)

# Mostrar el gráfico
plt.show()


# Seleccionar columnas de MAPE
mape_columns = ["mape_chronos_fixed", "mape_chronos_random", "mape_timesfm_fixed", "mape_timesfm_random"]
mape_df = errors[mape_columns]

# Renombrar columnas para mejor visualización en el gráfico
mape_df = mape_df.rename(columns={
    "mape_chronos_fixed": "Chronos Fixed",
    "mape_chronos_random": "Chronos Random",
    "mape_timesfm_fixed": "TimesFM Fixed",
    "mape_timesfm_random": "TimesFM Random"
})

# Aplicar eliminación de outliers
mape_df = mape_df.apply(remove_outliers)

# Convertir a formato largo para Seaborn
mape_long = mape_df.melt(var_name="Modelo", value_name="MAPE").dropna()

# Configurar el gráfico
plt.figure(figsize=(10, 6))
sns.boxplot(x="Modelo", y="MAPE", data=mape_long, palette="Set2")

# Etiquetas y título
plt.xlabel("Modelo y Variante")
plt.ylabel("MAPE (%)")
plt.title("Comparación de MAPE entre Modelos y Variantes")
plt.xticks(rotation=15)  # Rotar etiquetas si es necesario
plt.grid(axis="y", linestyle="--", alpha=0.7)

# Mostrar el gráfico
plt.show()
