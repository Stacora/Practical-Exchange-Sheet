import requests
import pandas as pd
#from datetime import datetime
import datetime

print('Downloading exchange info from the https://openexchangerates.org/api')

rpp = requests.get('https://openexchangerates.org/api/latest.json?app_id=xxxxxxxxxxxxxxxxxxxxxxxxxx')

datos = rpp.json()

## Getting the rates to USD
rates_USD = datos['rates']

currency_keys = list(rates_USD.keys())
currency_values = list(rates_USD.values())

df_exchange = pd.DataFrame({'Currency' : currency_keys, 'USD_price' : currency_values})



## Getting the meta data

datos['timestamp_2'] = datetime.datetime.fromtimestamp(datos['timestamp'])

date_it = str(datos['timestamp_2'].day) + '-' + str(datos['timestamp_2'].month) + '-' + str(datos['timestamp_2'].year)
time_it = str(datos['timestamp_2'].hour) + ':' + str(datos['timestamp_2'].minute) + ':' + str(datos['timestamp_2'].second)
datos['date'] = date_it
datos['time'] = time_it
datos['year'], datos['month'], datos['day'] = datos['timestamp_2'].year, datos['timestamp_2'].month, datos['timestamp_2'].day
datos['hour'], datos['minute'], datos['second'] = datos['timestamp_2'].hour, datos['timestamp_2'].minute, datos['timestamp_2'].second


df_exchange['Date'] = date_it
df_exchange['Time'] = time_it
df_exchange['timestamp'] = datos['timestamp']
df_exchange['timestamp_2'] = datos['timestamp_2']
metadatos = ['disclaimer', 'license', 'timestamp', 'base', 'date', 'time', 'year', 'month', 'day', 'hour', 'minute', 'second']

datos_2 = {mykey : datos[mykey] for mykey in metadatos}

meta_keys = datos_2.keys()
meta_values = datos_2.values()

df_meta = {'keys' : meta_keys, 'values' : meta_values}
df_meta = pd.DataFrame(df_meta)


### The finale datasets are
#print(df_exchange.head())
#print('###############################')
#print(df_meta.head())

## Deleting objects
del rates_USD, currency_keys, currency_values, rpp
del date_it, time_it, datos, metadatos, datos_2, meta_keys, meta_values

# df_exchange.to_csv('rates_test.csv')
