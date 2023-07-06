    trees_acre = trees_acre / n(),
    basal_area_acre = basal_area_acre / n(),
    volume_acre = volume_acre / n()



install.packages("tidyr")
install.packages("data.table")
install.packages("ggplot2")
install.packages("dplyr")
library(tidyr)
library(data.table)
library(ggplot2)
library(dplyr)
poplar <- fread("C:\\Users\\wetzl\\Downloads\\YELLOW_POPLAR_PLOTS.csv")
trees.acre <- poplar %>%
  group_by(ECOSUBCD) %>%
  summarise(count = n())
trees.acre
basal.acre <- poplar %>%
  group_by(ECOSUBCD) %>%
  summarise(mean_acre = mean(BA))
basal.acre
ggplot(poplar, aes(x = LON, y = LAT)) +
  geom_point(position = "jitter", alpha = 0.5) +
  theme_minimal()
# Part 1
?aggregate
summary_data <- aggregate(cbind(trees_acre = tpa_unadj, basal_area_acre = BA,volume_acre = volcfgrs) ~ CN, data = poplar, FUN = sum)
summary_data$qmd <- sqrt((summary_data$basal_area_acre / summary_data$trees_acre) / 0.005454)
overall_averages <- colMeans(summary_data[, c("trees_acre", "basal_area_acre", "volume_acre", "qmd")], na.rm = TRUE)
n <- nrow(summary_data)
###
summary_data #table for #1
###
t_value <- qt(0.975, df = n - 1)
se <- apply(summary_data[, c("trees_acre", "basal_area_acre", "volume_acre", "qmd")], 2, sd) / sqrt(n)
lower_ci <- overall_averages - t_value * se
upper_ci <- overall_averages + t_value * se
sum_trees_acre <- sum(summary_data$trees_acre)
sum_basal_area_acre <- sum(summary_data$basal_area_acre)
sum_volume_acre <- sum(summary_data$volume_acre)
sum_qmd <- sqrt((sum_basal_area_acre / sum_trees_acre) / 0.005454)
#part two, stand and stock tables
?cut
class_width <- 2
poplar$dbh <- round(round(poplar$dia/class_width, 0) * class_width, 0)
stand_table <- poplar %>%
  mutate(DBH_Class = cut(dbh, breaks = seq(min(dbh), max(dbh) + class_width, by = class_width), include.lowest = TRUE)) %>%
  group_by(DBH_Class) %>%
  summarise(n = n(), mean_tpa = mean(tpa_unadj), mean_BA = mean(BA)) %>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))
stock_table <- poplar %>%
  group_by(common_name) %>%
  summarise(n = n(), mean_tpa = mean(tpa_unadj), mean_BA = mean(BA)) %>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))
###
stock_table #for #2b
stand_table #for #2a
# Part three, sums and percentages
part3a <- aggregate(cbind(trees_acre = tpa_unadj, basal_area_acre = BA,volume_acre = volcfgrs) ~ common_name, data = poplar, FUN = sum)
part3a$percent_trees_acre <- with(part3a, (trees_acre / sum(trees_acre)) * 100)
part3a$percent_basal_area_acre <- with(part3a, (basal_area_acre / sum(basal_area_acre)) * 100)
summary_data$percent_trees_acre <- with(summary_data, (trees_acre / sum(trees_acre)) * 100)
summary_data$percent_basal_area_acre <- with(summary_data, (basal_area_acre / sum(basal_area_acre)) * 100)
###
part3a #for part 3a
part3b <- print(summary_data[, c("CN", "percent_trees_acre", "percent_basal_area_acre")]) #for part 3b
#Post analysis
sweetgum <- fread("\\Users\\wetzl\\Downloads\\SWEETGUM_YELLOW_POPLAR_PLOTS.csv")
unique(poplar$ecodivision)
is.na(poplar$ecodivision)
poplar$ecodivision <- as.integer(poplar$ecodivision)
gumpoplar <- bind_rows(poplar,sweetgum)
# write.csv(df, file =  "C:\\Users\\wetzl\\Downloads\\df.csv") 

#### this is the new new
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
    CN = as.character(CN)
  )

# Create plot_summary by binding the mean values at the bottom
plot_summary <- plot_data %>% ungroup() %>%
  summarise(across(trees_acre:qmd, mean, na.rm = TRUE)) %>%
  mutate(CN = "All", qmd = sqrt(basal_area_acre / trees_acre / 0.005454)) %>%
  bind_rows(plot_data, .) %>%
  as.data.frame()

overall_totals <- colSums(plot_summary[, c("trees_acre", "basal_area_acre", "volume_acre", "qmd")], na.rm = TRUE)

plot_summary <- rbind(plot_summary, overall_totals)

n <- nrow(plot_summary)
t_value <- qt(0.975, df = n - 1)

se <- apply(plot_summary[, c("trees_acre", "basal_area_acre", "volume_acre", "qmd")], 2, sd) / sqrt(n)
lower_ci <- colMeans(plot_summary[, c("trees_acre", "basal_area_acre", "volume_acre", "qmd")], na.rm = TRUE) - t_value * se
upper_ci <- colMeans(plot_summary[, c("trees_acre", "basal_area_acre", "volume_acre", "qmd")], na.rm = TRUE) + t_value * se

# Part 2
class_width <- 2
poplar$dbh <- round(round(poplar$dia / class_width, 0) * class_width, 0)

stand_table <- poplar %>%
  mutate(DBH_Class = cut(dbh, breaks = seq(min(dbh), max(dbh) + class_width, by = class_width), include.lowest = TRUE)) %>%
  group_by(DBH_Class) %>%
  summarise(
    trees_acre = sum(tpa_unadj) / n(),
    basal_area_acre = sum(BA * tpa_unadj) / n(),
    volume_acre = sum(volcfgrs * tpa_unadj, na.rm = TRUE) / n()
  ) %>%
  ungroup()

stand_table <- stand_table %>%
  summarise(
    trees_acre = sum(trees_acre),
    basal_area_acre = sum(basal_area_acre),
    volume_acre = sum(volume_acre)
  ) %>%
  mutate(DBH_Class = "All") %>%
  bind_rows(stand_table, .) %>%
  as.data.frame()

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