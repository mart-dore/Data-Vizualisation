# ------------ MARTIN DORE -- ENTREGABLE 1 : MAPA -----------

# librerias
library(maps)
library(ggplot2)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(paletteer)

mapa_mundo = map_data("world")

# Lista de países
paises <- c("Portugal", "Spain", "France", "Switzerland", "Germany", "Austria", "UK", "Netherlands",
            "Denmark", "Poland", "Italy", "Croatia", "Slovenia", "Hungary", "Slovakia", "Czech republic",
            "Sweden", "Finland", "Norway", "Estonia", "Lithuania", "Ukraine", "Belarus", "Romania",
            "Belgium", "Bulgaria", "Greece", "Moldova", "Latvia", "Luxembourg", "Serbia", "Bosnia and Herzegovina",
            "North Macedonia", "Montenegro", "Albania", "Ireland", "Kosovo")



# Quedar solamente paises de europa
mapa_europa = mapa_mundo %>% 
  filter(region %in% paises)

# etiquetas por los paises
etiquetas_paises <- mapa_europa %>%
  group_by(region) %>%
  summarise(mean_long = mean(long), mean_lat = mean(lat))


# Datos de las capitales
capitales <- data.frame(
  ciudad = c("Lisboa", "Madrid", "Paris", "Roma"),
  longitud = c(-9.13333, -3.70256, 2.3488, 11.71819),
  latitud = c(38.71667, 40.4165, 48.85341, 45.58383)
)


ggplot(mapa_europa) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = region), color = "white") +
  geom_text(data = etiquetas_paises,
            aes(x=mean_long, y=mean_lat, label = region), 
            size = 3, hjust= 0.5, color = "black") +
  geom_text(data = capitales,
            aes(x=longitud, y=latitud, label = ciudad),
            color = "red", nudge_y = -1.5)+
  geom_point(data = capitales,
             aes(x = longitud, y = latitud),
             color = "red", size = 2) +
  scale_fill_manual(values = paletteer_c("ggthemes::Purple", length(paises))) +
  theme_classic() +
  theme(
    legend.position = "None",
    axis.title.x = element_text(size = 10, color = "red", family = "bold"),
    axis.title.y = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 8, color = "blue", angle = 45),
    axis.text.y = element_text(size = 8, color = "pink")
  ) +
  labs(title = "Mapa Europa", subtitle = "Algunas Capitales") +
  xlab("Longitud") +
  ylab("Latitud")
