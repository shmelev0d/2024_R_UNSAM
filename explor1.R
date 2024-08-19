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
df_clae2_promedio <- df_clae2_promedio %>%
  left_join(df_clae2, by ="clae2") 

df_clae2_promedio <- df_clae2_promedio %>%
  left_join(df_depart, by = c("codigo_departamento_indec", "id_provincia_indec"))

df_clae2_promedio <- df_clae2_promedio %>% 
  mutate(year = year(fecha))

# EDA
df_clae2_promedio %>% 
  select(year, 
         w_mean, 
         clae2_desc, 
         letra_desc, 
         nombre_departamento_indec,
         nombre_provincia_indec) %>%
  filter(nombre_provincia_indec == "Cordoba", clae2_desc == "Elaboración de bebidas")


promedio_sectores <- df_clae2_promedio %>%
  group_by(letra_desc) %>%
  summarize(promedio = mean(w_mean))

ggplot(promedio_sectores, aes(x = letra_desc, y = promedio)) +
  geom_col() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))





