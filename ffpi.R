# Завантаження необхідних пакетів
library(tidyverse)
library(sf)
library(readxl)
library(ggplot2)
library(viridis)

# Завантаження геометрії (шляхи до GIS‑файлів, змініть за потребою)
subbasins_SWAT <- st_read("data/gis/Subbasin_SWAT.gpkg")
rivers <- st_read("data/gis/rivers_ua.gpkg")
admin_border_ukraine <- st_read("data/gis/admin_border_Uk.gpkg")

subbasins_SWAT <- subbasins_SWAT %>% mutate(Subbasin = as.character(Subbasin))

# Завантаження агрегованих даних з розрахованим FFPI
ffpi_data <- read_excel("data/aggregated_by_Sub_with_FFPI.xlsx")
# Перетворення колонки "Sub" в текстовий формат (щоб відповідала Subbasin)
ffpi_data <- ffpi_data %>% mutate(Sub = as.character(Sub))

# Об’єднання просторових даних із даними FFPI за ключовими колонками:
# у shapefile – Subbasin, у Excel‑файлі – Sub
merged_ffpi <- subbasins_SWAT %>%
  left_join(ffpi_data, by = c("Subbasin" = "Sub"))

# Перевіримо результат об’єднання (кілька перших рядків)
print(head(merged_ffpi))
print(head(erosion_data_geo))
test_join <- left_join(erosion_data_geo, merged_ffpi, by = "Subbasin")
print(head(test_join))
ffpi_data <- erosion_data_geo %>%
  left_join(ffpi_data, by = c("Subbasin" = "Sub")) %>%  # Додаємо FFPI по відповідності колонок
  mutate(FFPI = ifelse(is.na(FFPI), 1, FFPI),  # Якщо FFPI відсутній, ставимо 1
         FFPI_Adjusted_Runoff = ifelse(weighted_runoff    == 0, 0, weighted_runoff    * FFPI))  # Якщо avg_runoff == 0, залишаємо 0

summary(ffpi_data$FFPI_Adjusted_Runoff)
hist(ffpi_data$FFPI_Adjusted_Runoff, col = "gray", main = "FFPI Adjusted Runoff Distribution")




merged_ffpi <- ffpi_data %>%
  mutate(FFPI_class = cut(FFPI_Adjusted_Runoff,
                          breaks = c(0, 0.3, 0.5, 0.7, 1.0),
                          labels = c("Low", "Moderate", "High", "Very High"),
                          include.lowest = TRUE))
summary(merged_ffpi_1)

# Побудова карти із заливкою за значенням FFPI_class
# Побудова карти з присвоєнням об'єкта ggplot змінній "p"
p <- ggplot(merged_ffpi) +
  geom_sf(aes(fill = FFPI_class), color = NA) +
  geom_sf(data = rivers, color = "blue", size = 0.1, alpha = 0.2) +
  geom_sf(data = admin_border_ukraine, color = "black", size = 0.7, alpha = 0.8) +
  geom_sf(data = admin_border_ukraine, color = "white", size = 0.3, linetype = "dotted", alpha = 0.7) +
  scale_fill_brewer(palette = "RdYlGn", name = "FFPI Class", direction = -1) +
  theme_minimal() +
  theme(
    plot.margin = margin(15, -10, 10, -10),
    legend.position = c(1.05, 0.5),
    legend.justification = "left",
    legend.box.margin = margin(0, -30, 0, 0),
    panel.grid.major = element_line(color = "grey80", size = 0.3),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
    plot.caption = element_text(size = 8, face = "italic", hjust = 1, vjust = -1),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  labs(
    title = "Flash Flood Potential Index (FFPI)",
    subtitle = "Aggregated Data by Subbasins model SWAT",
    caption = "Source: model SWAT"
  )

# Збереження карти у файл
# Задайте правильний шлях і ім'я файлу (наприклад, "FFPI_map.png")

library(ggplot2)
library(sf)
library(dplyr)
library(viridis)
library(RColorBrewer)

# Форматування сценарію
format_scenario <- function(scenario) {
  if (scenario == "rcp85") {
    return("RCP8.5")
  } else if (scenario == "rcp45") {
    return("RCP4.5")
  } else {
    return(toupper(scenario))
  }
}

# Створення карти FFPI
plot_ffpi_map <- function(data, period, scenario) {
  formatted_scenario <- format_scenario(scenario)  # Форматуємо назву сценарію

  ggplot(data) +
    geom_sf(aes(fill = FFPI_class), color = NA, alpha = 0.3) +  # Заповнення за класами FFPI
    geom_sf(data = rivers, color = "blue", size = 0.1, alpha = 0.3) +  # Річки
    geom_sf(data = admin_border_ukraine, color = "black", size = 0.7, alpha = 0.8) +  # Адміністративні межі
    geom_sf(data = admin_border_ukraine, color = "white", size = 0.3, linetype = "dotted", alpha = 0.7) +

    # Використання чіткої кольорової схеми
    scale_fill_brewer(palette = "RdYlGn", name = "FFPI Class", direction = -1) +

    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
      plot.caption = element_text(size = 8, face = "italic", hjust = 1, vjust = -1),
      legend.position = "right"
    ) +

    labs(
      title = "Flash Flood Potential Index (FFPI)",
      subtitle = paste("Scenario:", formatted_scenario, "| Period:", period),
      caption = paste("Data: Period", period, "| Scenario:", formatted_scenario)
    )
}

# Визначення періодів та сценаріїв
periods <- c("1991-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100")
scenarios <- c("rcp45", "rcp85")
ffpi_data_1 <- erosion_data_geo %>%
  left_join(ffpi_data, by = c("Subbasin" = "Sub")) %>%  # Додаємо FFPI по відповідності колонок
  mutate(FFPI = ifelse(is.na(FFPI), 1, FFPI),  # Якщо FFPI відсутній, ставимо 1
         FFPI_Adjusted_Runoff = ifelse(Avg_SURQ_GENmm == 0, 0, Avg_SURQ_GENmm * FFPI))  # Якщо avg_runoff == 0, залишаємо 0

summary(ffpi_data_1$FFPI_Adjusted_Runoff)
hist(ffpi_data$FFPI_Adjusted_Runoff, col = "gray", main = "FFPI Adjusted Runoff Distribution")




merged_ffpi_1 <- ffpi_data_1 %>%
  mutate(FFPI_class = cut(FFPI_Adjusted_Runoff,
                          breaks = c(0, 3, 5, 7, 12),
                          labels = c("Low", "Moderate", "High", "Very High"),
                          include.lowest = TRUE))
summary(merged_ffpi_1)
# Генерація та збереження карт
for (scenario in scenarios) {
  for (period in periods) {

    # Фільтрація даних для поточного періоду та сценарію
    period_data <- merged_ffpi_1 %>%
      filter(Period == period, Scenario == scenario)

    if (nrow(period_data) > 0) {
      plot <- plot_ffpi_map(period_data, period, scenario)

      # Створення директорій для збереження графіків
      scenario_dir <- file.path("outputs/plots/FFPI_1_2", scenario)
      if (!dir.exists(scenario_dir)) dir.create(scenario_dir, recursive = TRUE)

      # Формуємо ім'я файлу
      file_name <- paste0("FFPI_", gsub("-", "_", period), ".png")

      ggsave(file.path(scenario_dir, file_name), plot, width = 14, height = 9, dpi = 300, bg = "white")

      cat("Файл збережено:", file.path(scenario_dir, file_name), "\n")
    } else {
      cat("⚠️ Немає даних для:", period, scenario, "\n")
    }
  }
}

