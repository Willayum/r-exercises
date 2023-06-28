install.packages(c("tidyr", "data.table", "ggplot2", "dplyr"))
library(tidyr)
library(data.table)
library(ggplot2)
library(dplyr)

poplar <- fread("C:\\Users\\wetzl\\Downloads\\YELLOW_POPLAR_PLOTS.csv")

# Part 1
plot_data <- poplar %>%
  group_by(CN) %>%
  summarise(
    trees_acre = sum(tpa_unadj),
    basal_area_acre = sum(BA * tpa_unadj),
    volume_acre = sum(volcfgrs * tpa_unadj, na.rm = TRUE)
  ) %>%
  mutate(
    qmd = sqrt((basal_area_acre / trees_acre) / 0.005454),
    trees_acre = trees_acre / n(),
    basal_area_acre = basal_area_acre / n(),
    volume_acre = volume_acre / n()
  )

overall_totals <- colSums(plot_data[, c("CN", "trees_acre", "basal_area_acre", "volume_acre", "qmd")], na.rm = TRUE)
plot_data <- rbind(plot_data, overall_totals)

n <- nrow(plot_data)
t_value <- qt(0.975, df = n - 1)
se <- apply(plot[, c("trees_acre", "basal_area_acre", "volume_acre", "qmd")], 2, sd) / sqrt(n)
lower_ci <- colMeans(plot_data[, c("trees_acre", "basal_area_acre", "volume_acre", "qmd")], na.rm = TRUE) - t_value * se
upper_ci <- colMeans(plot_data[, c("trees_acre", "basal_area_acre", "volume_acre", "qmd")], na.rm = TRUE) + t_value * se

# Part 2
class_width <- 2
poplar$dbh <- round(round(poplar$dia/class_width, 0) * class_width, 0)

stand_table <- poplar %>%
  mutate(DBH_Class = cut(dbh, breaks = seq(min(dbh), max(dbh) + class_width, by = class_width), include.lowest = TRUE)) %>%
  group_by(DBH_Class) %>%
  summarise(n = n(), mean_tpa = mean(tpa_unadj), mean_BA = mean(BA), mean_vol = mean(volcfgrs, na.rm = TRUE)) %>%
  add_row(summarise_all(., ~if(is.numeric(.)) sum(., na.rm = TRUE) else "Total"))

stock_table <- poplar %>%
  group_by(common_name) %>%
  summarise(n = n(), mean_tpa = mean(tpa_unadj), mean_BA = mean(BA), mean_vol = mean(volcfgrs, na.rm = TRUE)) %>%
  add_row(summarise_all(., ~if(is.numeric(.)) sum(., na.rm = TRUE) else "Total"))

#stand and stock table mean totals won't sum to the same number because there are more rows in the stock table being
#multiplied by the tpa multiplier inflating the number as the difference there is not accounted for when dividing by n
#I believe I would need more information on the tpa multiplier in order to align the totals across all data frames

# Part 3
species_percent <- poplar %>%
  group_by(common_name) %>%
  summarise(
    trees_acre = sum(tpa_unadj),
    basal_area_acre = sum(BA * tpa_unadj),
    volume_acre = sum(volcfgrs * tpa_unadj, na.rm = TRUE)
  ) %>%
  mutate(
     qmd = sqrt((basal_area_acre / trees_acre) / 0.005454),
    trees_acre = trees_acre / n(),
    basal_area_acre = basal_area_acre / n(),
    volume_acre = volume_acre / n(),
    percent_ta = trees_acre / sum(trees_acre) * 100,
    percent_baa = basal_area_acre / sum(basal_area_acre) * 100
  ) %>%
   arrange(desc(trees_acre)) %>%
  add_row(summarise_all(., ~if(is.numeric(.)) sum(., na.rm = TRUE) else "Total"))

# Post analysis
sweetgum <- fread("\\Users\\wetzl\\Downloads\\SWEETGUM_YELLOW_POPLAR_PLOTS.csv")
poplar$ecodivision <- as.integer(poplar$ecodivision)
gumpoplar <- bind_rows(poplar, sweetgum)