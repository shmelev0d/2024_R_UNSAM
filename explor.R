getwd()
setwd("D:\\Data Science\\Git\\2024_R_UNSAM")

library(tidyverse)

df_clae2 <- read_csv("data/diccionario_clae2.csv")
df_clae2
unique(df_clae2[, c("letra", "letra_desc")])

df_depart <- read_csv("data/diccionario_cod_depto.csv")
df_depart
unique(df_depart[, c("id_provincia_indec", "nombre_provincia_indec")])

df_clae2_promedio <- read_csv("data/w_mean_depto_total_clae2.csv")
df_clae2_promedio



## join data

df_clae2_promedio_joint <- df_clae2_promedio %>%
  left_join(df_clae2, by ="clae2") %>%
  left_join(df_depart, by = c("codigo_departamento_indec", "id_provincia_indec")) %>%
  rename(cod_depto = codigo_departamento_indec, 
         id_provincia = id_provincia_indec, 
         nom_depto = nombre_departamento_indec, 
         nom_provincia = nombre_provincia_indec) %>%
  mutate(year = year(fecha))

df_clae2_promedio_joint


## glimpse

min(df_clae2_promedio_joint$w_mean)
head(sort(unique(df_clae2_promedio_joint$w_mean)), 10)
max(df_clae2_promedio_joint$w_mean)
mean(df_clae2_promedio_joint$w_mean)
median(df_clae2_promedio_joint$w_mean)

df_clae2_promedio_joint %>%
  filter(w_mean == -99)   # casi una cuarta de los registros son NA

df_clae2_promedio_joint %>%
  filter(w_mean == 154)   # outlier

## variables para elegir los sectores


# salario promedio para sectores (letra)

promedio_sectores <- df_clae2_promedio_joint %>%
  group_by(letra_desc) %>%
  summarize(promedio = mean(w_mean))


ggplot(promedio_sectores, aes(x = letra_desc, y = promedio)) +
  geom_col() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))

# salario promedio para los años (year)

promedio_años <- df_clae2_promedio_joint %>%
  group_by(year, letra_desc) %>%
  summarize(promedio = mean(w_mean))


ggplot(promedio_años, aes(x = letra_desc, y = promedio, fill = year)) +
  geom_col() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))


#la cantidad de NA (-99)

num_NAs <- df_clae2_promedio_joint %>%
  group_by(letra_desc) %>%
  summarize(total_registros = n(), 
            NAs_by_sectors = sum(w_mean == -99), 
            NAs_prct = sum(w_mean== -99) / n() * 100)

ggplot(num_NAs, aes(x = letra_desc)) +
  geom_col(aes(y = total_registros), fill = "skyblue", alpha = 0.7) +
  geom_col(aes(y = NAs_by_sectors), fill = "red", alpha = 0.5) +
  coord_flip() +
  labs(x = "Categorías", y = "Número de Observaciones") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())


## reemplazamos las -99 con el promedio de los proximos valores usando interpolacíon

# cambiamos los -99 a NA
df_clae2_promedio_joint$w_mean[df_clae2_promedio_joint$w_mean == -99] <- NA

# la tabla de indices de NAs
na_indices <- which(is.na(df_clae2_promedio_joint$w_mean))

# la tabla de proximos observaciones
neighbors <- data.frame(
  Index = na_indices,
  Previous = df_clae2_promedio_joint$w_mean[na_indices - 1],
  Next = df_clae2_promedio_joint$w_mean[na_indices + 1]
)

library(zoo)

# aplicamos la interpolacíon
df_clae2_promedio_joint$w_mean <- na.approx(df_clae2_promedio_joint$w_mean)

# revisamos
sum(is.na(df_clae2_promedio_joint$w_mean))
df_clae2_promedio_joint[c(87, 88, 89), ]



# Sectores elegidos: enseñanza, salud, Construcción, servicio de transporte, servicio de financiera y seguros. 


sectores_filter <- df_clae2_promedio_joint %>%
  select(year, 
         w_mean, 
         letra,
         letra_desc, 
         nom_depto,
         nom_provincia) %>%
  filter(letra %in% c("F", "H","P", "Q")) %>%
  arrange(letra, nom_provincia)

sectores_filter





