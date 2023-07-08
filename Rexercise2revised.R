install.packages(c("tidyr", "data.table", "ggplot2", "dplyr"))
library(tidyr)
library(data.table)
library(ggplot2)
library(dplyr)

poplar <- fread("C:\\Users\\wetzl\\Downloads\\YELLOW_POPLAR_PLOTS.csv")
# poplar <- fread("./YELLOW_POPLAR_PLOTS.csv")

# Part 1
plot_data <- poplar %>%
  group_by(CN) %>%
  summarise( observations = n(),
    trees_acre = sum(tpa_unadj),
    basal_area_acre = sum(BA * tpa_unadj),
    volume_acre = sum(volcfgrs * tpa_unadj, na.rm = TRUE)
  ) %>%
  mutate(
    qmd = sqrt((basal_area_acre / trees_acre) / 0.005454),
    CN = as.character(CN)
  )
overall_means <- colMeans(plot_data[, c("observations", "trees_acre", "basal_area_acre", "volume_acre", "qmd")], na.rm = TRUE)
overall_means_df <- data.frame(t(overall_means))
overall_means_df$CN <- "All"
overall_means_df <- overall_means_df[c("CN", "observations", "trees_acre", "basal_area_acre", "volume_acre", "qmd")]

plot_summary <- rbind(plot_data, overall_means_df)

n <- nrow(plot_data)

t_value <- qt(0.975, df = n - 1)
se <- apply(plot_data[plot_data$CN != "All", c("trees_acre", "basal_area_acre", "volume_acre", "qmd")], 2, sd) / sqrt(n)
lower_ci <- colMeans(plot_data[, c("trees_acre", "basal_area_acre", "volume_acre", "qmd")], na.rm = TRUE) - t_value * se
upper_ci <- colMeans(plot_data[, c("trees_acre", "basal_area_acre", "volume_acre", "qmd")], na.rm = TRUE) + t_value * se

# Part 2
class_width <- 2
poplar$dbh <- ceiling(poplar$dia/class_width) * class_width

stand_table <- poplar %>%
  mutate(DBH_Class = cut(dbh, breaks = seq(1, max(dbh) + class_width, by = class_width), include.lowest = TRUE)) %>%
  group_by(DBH_Class) %>%
  summarise(observations = n(),
            trees_acre = sum(tpa_unadj)/n,
            BA_acre = sum(BA*tpa_unadj)/n,
            vol_acre = sum(volcfgrs*tpa_unadj, na.rm = TRUE)/n,
  ) %>%
  ungroup()

stand_table <- stand_table %>%
  summarise(observations = sum(observations),
            trees_acre = sum(trees_acre),
            BA_acre = sum(BA_acre),
            vol_acre = sum(vol_acre),
  ) %>%
  mutate(DBH_Class = "All") %>%
  bind_rows(stand_table, .) %>% as.data.frame()

stock_table <- poplar %>%
   group_by(common_name) %>%
  summarise(observations = n(),
            trees_acre = sum(tpa_unadj)/n,
            BA_acre = sum(BA*tpa_unadj)/n,
            vol_acre = sum(volcfgrs*tpa_unadj, na.rm = TRUE)/n,
  )%>%
  ungroup()

stock_table <- stock_table %>%
  summarise(observations = sum(observations),
            trees_acre = sum(trees_acre),
            BA_acre = sum(BA_acre),
            vol_acre = sum(vol_acre),
  ) %>%
  mutate(common_name = "All") %>%
  bind_rows(stock_table, .) %>% as.data.frame()
  #group_by(common_name) %>%
  #summarise(, mean_tpa = mean(tpa_unadj), mean_BA = mean(BA), mean_vol = mean(volcfgrs, na.rm = TRUE)) %>%
  #add_row(summarise_all(., ~if(is.numeric(.)) sum(., na.rm = TRUE) else "Total"))

# Part 3
stock_percent <- poplar %>%
  group_by(common_name) %>%
  summarise(observations = n(),
            trees_acre = sum(tpa_unadj)/n,
            BA_acre = sum(BA*tpa_unadj)/n,
            vol_acre = sum(volcfgrs*tpa_unadj, na.rm = TRUE)/n,
            qmd = sqrt((BA_acre / trees_acre) / 0.005454)
  )%>%
  mutate(percent_ta = trees_acre / sum(trees_acre) * 100,
            percent_baa = BA_acre / sum(BA_acre) * 100,
            percent_vola = vol_acre / sum(vol_acre, na.rm = TRUE) * 100
  ) %>% ungroup()
stock_percent <- stock_percent %>%
  summarise(observations = sum(observations),
            trees_acre = sum(trees_acre),
            BA_acre = sum(BA_acre),
            vol_acre = sum(vol_acre),
            qmd = sum(qmd),
            percent_ta = sum(percent_ta),
            percent_baa = sum(percent_baa),
            percent_vola = sum(percent_vola)
  ) %>%
  mutate(common_name = "All") %>%
  bind_rows(stock_percent, .) %>% as.data.frame()
# Post analysis
sweetgum <- fread("\\Users\\wetzl\\Downloads\\SWEETGUM_YELLOW_POPLAR_PLOTS.csv")
poplar$ecodivision <- as.integer(poplar$ecodivision)
gumpoplar <- bind_rows(poplar, sweetgum)
species_abundance <- table(gumpoplar$common_name)
species_abundance <- sort(species_abundance)

species_table <- gumpoplar %>%
  group_by(common_name) %>%
  summarise(
    species_abundance = n(),
            mean_dbh = mean(dia),
            mean_vol = mean(volcfgrs, na.rm = TRUE),
            mean_BA = mean(BA)
  )
tree_ranks <- rank(species_abundance)
tree_colours <- colorRampPalette(c("green", "red"))(max(tree_ranks))
colour_mapping <- setNames(tree_colours, names(species_abundance))
small_constant <- 0.001
gumpoplar_filtered <- gumpoplar %>%
  filter(!is.na(volcfgrs) & !is.na(BA))
### Graphs
ggplot(gumpoplar, aes(x = reorder(common_name, dia), y = dia, colour = common_name)) +
  geom_boxplot() +
  scale_colour_manual(values = colour_mapping) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) +
  xlab("Species") +
  ylab("DBH (in)") +
  ggtitle("Species by DBH and Abundance") +
  labs(colour = "Species Abundance")

ggplot(species_table, aes(x = common_name, y = mean_dbh, colour = log
 (ifelse(species_abundance == 1, small_constant, species_abundance)))) +
  geom_point() + scale_color_gradient(low = "yellow", high = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) +
  xlab("Species") +
  ylab("Mean DBH (in)") +
  ggtitle("Species by Mean DBH") +
  labs(colour = "Species Abundance")

ggplot(gumpoplar_filtered, aes(x = volcfgrs, y = BA, size = dia, colour = dia)) +
  geom_point(shape = 1) +
  scale_color_continuous(low = "green", high = "red") +
  facet_wrap(~ common_name) +
   labs(x = expression(Volume~(ft^3)), y = expression(BA~(ac^2)), size = "DBH (in)", colour = "DBH (in)") +
  ggtitle("Volume by BA, Species, and DBH")

