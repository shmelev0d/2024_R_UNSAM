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



# join data
df_clae2_promedio_joint <- df_clae2_promedio %>%
  left_join(df_clae2, by ="clae2") %>%
  left_join(df_depart, by = c("codigo_departamento_indec", "id_provincia_indec")) %>%
  rename(cod_depto = codigo_departamento_indec, 
         id_provincia = id_provincia_indec, 
         nom_depto = nombre_departamento_indec, 
         nom_provincia = nombre_provincia_indec) %>%
  mutate(year = year(fecha))




# glimpse

min(df_clae2_promedio_joint$w_mean)
head(sort(unique(df_clae2_promedio_joint$w_mean)), 10)
max(df_clae2_promedio_joint$w_mean)
mean(df_clae2_promedio_joint$w_mean)
median(df_clae2_promedio_joint$w_mean)

df_clae2_promedio_joint %>%
  filter(w_mean == -99)   # casi una cuarta de los registros son NA

df_clae2_promedio_joint %>%
  filter(w_mean == 154)   # outlier


# salario promedio para sectores (letra)

promedio_sectores <- df_clae2_promedio_joint %>%
  group_by(letra_desc) %>%
  summarize(promedio = mean(w_mean))


ggplot(promedio_sectores, aes(x = letra_desc, y = promedio)) +
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







