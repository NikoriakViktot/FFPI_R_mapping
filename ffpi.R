# Load required packages
library(tidyverse)
library(sf)
library(readxl)
library(ggplot2)
library(viridis)
library(RColorBrewer)


# Load geometries (adjust file paths as needed)
subbasins_SWAT <- st_read("data/gis/Subbasin_SWAT.gpkg")
rivers <- st_read("data/gis/rivers_ua.gpkg")
admin_border_ukraine <- st_read("data/gis/admin_border_Uk.gpkg")

subbasins_SWAT <- subbasins_SWAT %>% mutate(Subbasin = as.character(Subbasin))

# Load aggregated data with calculated FFPI
ffpi_data <- read_excel("data/aggregated_by_Sub_with_FFPI.xlsx")
# Convert "Sub" column to character to match "Subbasin"
ffpi_data <- ffpi_data %>% mutate(Sub = as.character(Sub))

# Merge spatial data with FFPI data using key columns:
# In shapefile – Subbasin, in Excel – Sub
merged_ffpi <- subbasins_SWAT %>%
  left_join(ffpi_data, by = c("Subbasin" = "Sub"))

# Check result of the merge (first rows)
print(head(merged_ffpi))
print(head(erosion_data_geo))

test_join <- left_join(erosion_data_geo, merged_ffpi, by = "Subbasin")
print(head(test_join))

# Join FFPI with erosion data and calculate adjusted runoff
ffpi_data <- erosion_data_geo %>%
  left_join(ffpi_data, by = c("Subbasin" = "Sub")) %>%
  mutate(
    FFPI = ifelse(is.na(FFPI), 1, FFPI),
    FFPI_Adjusted_Runoff = ifelse(weighted_runoff == 0, 0, weighted_runoff * FFPI)
  )

summary(ffpi_data$FFPI_Adjusted_Runoff)
hist(ffpi_data$FFPI_Adjusted_Runoff, col = "gray", main = "FFPI Adjusted Runoff Distribution")

# Classify FFPI-adjusted runoff into classes
merged_ffpi <- ffpi_data %>%
  mutate(FFPI_class = cut(FFPI_Adjusted_Runoff,
                          breaks = c(0, 0.3, 0.5, 0.7, 1.0),
                          labels = c("Low", "Moderate", "High", "Very High"),
                          include.lowest = TRUE))
summary(merged_ffpi_1)

# Create map with fill based on FFPI_class
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
    subtitle = "Aggregated Data by Subbasins (SWAT model)",
    caption = "Source: SWAT model output"
  )

# Load additional packages
library(RColorBrewer)

# Format climate scenario name
format_scenario <- function(scenario) {
  if (scenario == "rcp85") {
    return("RCP8.5")
  } else if (scenario == "rcp45") {
    return("RCP4.5")
  } else {
    return(toupper(scenario))
  }
}

# Function to plot FFPI map
plot_ffpi_map <- function(data, period, scenario) {
  formatted_scenario <- format_scenario(scenario)

  ggplot(data) +
    geom_sf(aes(fill = FFPI_class), color = NA, alpha = 0.3) +
    geom_sf(data = rivers, color = "blue", size = 0.1, alpha = 0.3) +
    geom_sf(data = admin_border_ukraine, color = "black", size = 0.7, alpha = 0.8) +
    geom_sf(data = admin_border_ukraine, color = "white", size = 0.3, linetype = "dotted", alpha = 0.7) +
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
      caption = paste("Data source – Period:", period, "| Scenario:", formatted_scenario)
    )
}

# Define periods and scenarios
periods <- c("1991-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100")
scenarios <- c("rcp45", "rcp85")

# Join FFPI and calculate adjusted runoff
ffpi_data_1 <- erosion_data_geo %>%
  left_join(ffpi_data, by = c("Subbasin" = "Sub")) %>%
  mutate(FFPI = ifelse(is.na(FFPI), 1, FFPI),
         FFPI_Adjusted_Runoff = ifelse(Avg_SURQ_GENmm == 0, 0, Avg_SURQ_GENmm * FFPI))

summary(ffpi_data_1$FFPI_Adjusted_Runoff)
hist(ffpi_data$FFPI_Adjusted_Runoff, col = "gray", main = "FFPI Adjusted Runoff Distribution")

# Classify by FFPI range
merged_ffpi_1 <- ffpi_data_1 %>%
  mutate(FFPI_class = cut(FFPI_Adjusted_Runoff,
                          breaks = c(0, 3, 5, 7, 12),
                          labels = c("Low", "Moderate", "High", "Very High"),
                          include.lowest = TRUE))

summary(merged_ffpi_1)

# Generate and save maps
for (scenario in scenarios) {
  for (period in periods) {

    # Filter data for the current period and scenario
    period_data <- merged_ffpi_1 %>%
      filter(Period == period, Scenario == scenario)

    if (nrow(period_data) > 0) {
      plot <- plot_ffpi_map(period_data, period, scenario)

      # Create directory to save output plots
      scenario_dir <- file.path("outputs/plots/FFPI_1_2", scenario)
      if (!dir.exists(scenario_dir)) dir.create(scenario_dir, recursive = TRUE)

      # Define filename
      file_name <- paste0("FFPI_", gsub("-", "_", period), ".png")

      ggsave(file.path(scenario_dir, file_name), plot, width = 14, height = 9, dpi = 300, bg = "white")

      cat("✔️ File saved:", file.path(scenario_dir, file_name), "\n")
    } else {
      cat("⚠️ No data for:", period, scenario, "\n")
    }
  }
}
