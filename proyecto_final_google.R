library(ggplot2)
library(tidyverse)
library(lubridate)
library(dplyr)

#PROCESOS DE ANALISIS DE DATOS

#PROCESO DE PREGUNTAR

##¿En qué se diferencian los socios anuales y los ciclistas ocasionales con respecto al uso de las bicicletas de Cyclistic?


#verificacion de nombre de las columnas para que todas queden iguales 
colnames(q1_2023)
colnames(q2_2023)
colnames(q3_2023)
colnames(q4_2023)
colnames(q5_2023)
colnames(q6_2023)
colnames(q7_2023)
colnames(q8_2023)
colnames(q9_2023)
colnames(q10_2023)
colnames(q11_2023)


#ORDENAR DATOS Y COMBINARLOS EN UN UNICO ARCHIVO

#funcion que nos permite combinar todos los dataframes en uno solo haciendolos coincidir por nombre de columnas
all_trip <- bind_rows(q1_2023,q2_2023, q3_2023, q4_2023,q5_2023,q6_2023,q7_2023,q8_2023,q9_2023,q10_2023,q11_2023)

head(all_trip)


#se eliminan las columnas start_lat,start_lng,end_lat,end_lng y se crear un nuevo dataframe organizado con la informacion necesaria
all_trips <- all_trip %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

#PROCESO DE LIMPIEZA Y PREPARACION DE LOS DATOS

#INFORMACION GENERAL DEL DATAFRAME ORGANIZADO LISTO PARA EL ANALISIS

head(all_trips) #se muestran laas 6 primeras filas del df

colnames(all_trips) #verificamos los nombres de las columnas del df

summary(all_trips) #nos brinda informacion estadistica del df

nrow(all_trips) # numero de filas 

dim(all_trips)# numero de filas y columnas

str(all_trips)#verificacion de los tipos de datos del df

#cambiamos el tipo de dato para que member sea suscriber y casual sea customer
all_trips <- all_trips %>% 
  mutate(member_casual = ifelse(member_casual=='member','subscriber','customer'))

#Se agregan nuevas columnas de fecha, mes, dia, año y dia de la semana.
all_trips$date <- as.Date(all_trips$started_at) #as.date nos permite convertir un string a formato de fecha
all_trips$month <- format(as.Date(all_trips$date), "%m") #la funcion format nos permite separar el dia, del mes y del año
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#restamos el tiempo de llegada menos el de inicio para calcular el tiempo promedio de viaje
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

str(all_trips)

#funciones para validar si el dato es factor o numerico
is.factor(all_trips$ride_length) #si es factor devuelve true y si no false
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))# si es numerico devuelve true y sino false
is.numeric(all_trips$ride_length)

#se eliminan datos negativos y con nombre de estacion hq qr
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
#ANALISIS DESCRIPTIVO
mean(df1$ride_length) #promedio de viajes
median(df1$ride_length) #mediana de los valores
max(df1$ride_length) #viaje mas largo 
min(df1$ride_length) #viaje mas corto 

#se eliminan los datos na y nan
df1= na.omit(all_trips_v2)



summary(df1)

df1 <- df1 %>%
  mutate(ride_length_min=num(ride_length/60))


#comparo el tiempo que tarda uin suscriptor que el de un cliente casual
aggregate(df1$ride_length ~ df1$member_casual, FUN = mean)
aggregate(df1$ride_length ~ df1$member_casual, FUN = median)
aggregate(df1$ride_length ~ df1$member_casual, FUN = max)
aggregate(df1$ride_length ~ df1$member_casual, FUN = min)

#se evidencia que el cliente en promedio tarda mas en los viajes que el subscriptor


#Ver el tiempo promedio de viaje cada día para miembros y usuarios ocasionales                   
aggregate(df1$ride_length ~ df1$member_casual + df1$day_of_week, FUN = mean)
#se organizan los dias de la semana
df1$day_of_week <- ordered(df1$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#se compara de nuevo el tiempo promedio por dia de la semana para cliente y suscriptor
aggregate(df1$ride_length ~ df1$member_casual + df1$day_of_week, FUN = mean )

#se puede notar que el suscriptor tiene un tiempo promedio de viaje mas bajo que el cliente

#analizar datos de pasajeros por tipo y día de la semana
df1 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creo un objeto wday para cada dia de la semana()
  group_by(member_casual, weekday) %>%  #agrupo tipo de usuario y dia de la semana
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% #se crean columnas de numero de viajes y promedio de viaje
  arrange(member_casual, weekday)	#se ordenan los datos de tipo de usuario y dia de la semana

#Visualicemos el número de viajes por tipo de ciclista.
df1 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + #grafica de dia de la semana vs numero de viajes categorizado por tipo de cliente
  geom_col(position = "dodge")

# ahora compararemos dia de la semana con duracion de viaje

df1 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + #grafica de dia de la semana vs numero de viajes categorizado por tipo de cliente
  geom_col(position = "dodge")
 
#EXPORTAR ARCHIVO DE RESUMEN

#se crea nuevo datagrame con resumen de los datos

resumen_biciletas <- aggregate(df1$ride_length ~ df1$member_casual + df1$day_of_week, FUN = mean)

#se crear un archivo csv y se guardan los datos del resumen
write.csv(resumen_biciletas, file = 'C:/Users/USUARIO/Desktop/proyecto_final/resumen_biciletas.csv')


write.csv(df1,file = 'C:/Users/USUARIO/Desktop/proyecto_final/resumen_total.csv')

#En qué se diferencian los socios anuales y los ciclistas ocasionales con respecto al uso de las bicicletas de Cyclistic?
# la respuesta a esta pregunta, luego de haber analizado los datos de los ultimos 12 meses de cyclistic,
#hicimos una comparacion entre los suscriptores y los clientes casuales y la relacion que hay en el tiempo
# promedio de viaje y la cantidad de viajes realizados en el dia.
# se evidencia que los suscriptores se demoran menos en el tiempo promedio de viaje y realizan mas viajes en la semana que los clientes ocasionales