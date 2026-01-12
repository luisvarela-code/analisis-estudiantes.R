datos <- read.csv("C:/Users/HP Pavilion/Desktop/analisis de datos/archive/StudentPerformance.csv")
#ver primeras filas
head(datos)
#estructura de datos
str(datos)
#resumen estadistico
summary(datos)

#historigrama
hist(datos$Performance.Index, 
     main = "Distribución del Performance Index",
     xlab = "Performance Index",
     ylab = "Frecuencia",
     col = "lightblue")
#diagrama de dispersion
plot(datos$Hours.Studied,datos$Performance.Index,
     main = "horas de estudio vs performance",
     xlab = "Horas estudiadas",
     ylab = "performance",
     pch = 19,
     col = "darkblue")
#actividades extracurriculares
boxplot(Performance.Index ~ Extracurricular.Activities,
        data = datos,
        main = "Performance por Actividades Extracurriculares",
        xlab = "Actividades Extracurriculares",
        ylab = "Performance Index",
        col = c("lightgreen", "lightcoral"))

#grafica de pastel de las horas de estudio vs extracurriculares
library(ggplot2)
library(dplyr)

# Crear rangos de horas de estudio
datos <- datos %>%
  mutate(Hours_Group = cut(
    Hours.Studied,
    breaks = c(0, 5, 10, 15, 20, Inf),
    labels = c("0-5", "6-10", "11-15", "16-20", "20+")
  ))

# Agrupar datos y calcular porcentajes
datos_grouped <- datos %>%
  group_by(Hours_Group, Extracurricular.Activities) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Hours_Group) %>%
  mutate(
    porcentaje = round(count / sum(count) * 100, 1),
    label = paste0(porcentaje, "%")
  )

# Gráfica de pastel con porcentajes
ggplot(datos_grouped, aes(x = "", y = count, fill = Extracurricular.Activities)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "white"
  ) +
  coord_polar("y") +
  facet_wrap(~ Hours_Group) +
  labs(
    title = "Distribución de Actividades Extracurriculares por Horas de Estudio"
  ) +
  theme_void() +
  theme(legend.title = element_blank())