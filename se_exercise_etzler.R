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
  summarise( count = n(),
    trees_acre = sum(tpa_unadj),
    basal_area_acre = sum(BA * tpa_unadj),
    volume_acre = sum(volcfgrs * tpa_unadj, na.rm = TRUE)
  ) %>%
  mutate(
    qmd = sqrt((basal_area_acre / trees_acre) / 0.005454),
    CN = as.character(CN)
  )
overall_means <- colMeans(plot_data[, c("count", "trees_acre", "basal_area_acre", "volume_acre", "qmd")], na.rm = TRUE)
overall_means_df <- data.frame(t(overall_means))
overall_means_df$CN <- "All"
overall_means_df <- overall_means_df[c("CN", "count", "trees_acre", "basal_area_acre", "volume_acre", "qmd")]

plot_summary <- rbind(plot_data, overall_means_df)

n <- nrow(plot_data)

t_value <- qt(0.975, df = n - 1)
se <- apply(plot_data[plot_data$CN != "All", c("trees_acre", "basal_area_acre", "volume_acre", "qmd")], 2, sd) / sqrt(n)
lower_ci <- colMeans(plot_data[, c("trees_acre", "basal_area_acre", "volume_acre", "qmd")], na.rm = TRUE) - t_value * se
upper_ci <- colMeans(plot_data[, c("trees_acre", "basal_area_acre", "volume_acre", "qmd")], na.rm = TRUE) + t_value * se

# Part 2
class_width <- 2
#poplar$dbh <- ceiling(poplar$dia/class_width) * class_width
dclass <- round(round(poplar$dia*class_width,0)/class_width,0)
poplar <- poplar %>%
  mutate(DBH_Class = cut(dclass, breaks = seq(1, max(dclass) + class_width, by = class_width), include.lowest = TRUE))
poplar$DBH_Class <- gsub("\\(|\\)|\\[|\\]", "", poplar$DBH_Class)  # Remove brackets
poplar$DBH_Class <- gsub("^(\\d+),(\\d+)$", "(\\1,\\2)", poplar$DBH_Class)
poplar$DBH_Class <- gsub("^(\\d+)-(\\d+)$", "(\\1,\\2)", poplar$DBH_Class)

s.t._groupby_se <- poplar %>%
  group_by(DBH_Class) %>%
  summarise(
    percent_se_tpa = (sd(tpa_unadj) / sqrt(n())) / (mean(tpa_unadj)) * 100,
    percent_se_vol_acre = (sd(volcfgrs * tpa_unadj) / sqrt(n())) / (mean(volcfgrs * tpa_unadj)) * 100
  )

# class1 <- poplar %>%
#   filter(poplar$DBH_Class == dfilter) %>%
#   summarise(
#     DBH_Class = DBH_Class,
#     tpa = tpa_unadj,
#     vol_acre = volcfgrs*tpa_unadj
#   )

# dfilter <- filter(dseq, dseq == x-1 | dseq == x+1)
# dseq <- seq(from = 2, to = 34, by = 2)
# standard_error <- function(x) {
#   n <- length(x)
#   se <- sd(x) / sqrt(n)
#   return(se)
# }

# class1_sum <- class1 %>%
#   summarise(
#     count = n(),
#     trees_acre = sum(tpa_unadj)/n(),
#     BA_acre = sum(BA*tpa_unadj)/n(),
#     vol_acre = sum(volcfgrs*tpa_unadj, na.rm = TRUE)/n()
#   )
# se_trees <- sd(class1$tpa) / mean(class1$tpa) * 100
# se_vol <- sd(class1$vol_acre) / mean(class1$vol_acre) * 100
# class1_sum <- class1_sum %>%
#   mutate(
#     se_pct_trees = se_trees,
#     se_pct_vol = se_vol
#   )

stand_table <- poplar %>%
  mutate(DBH_Class = cut(dclass, breaks = seq(1, max(dclass) + class_width, by = class_width), include.lowest = TRUE)) %>%
  group_by(DBH_Class) %>%
  summarise(count = n(),
            trees_acre = sum(tpa_unadj)/n,
            BA_acre = sum(BA*tpa_unadj)/n,
            vol_acre = sum(volcfgrs*tpa_unadj, na.rm = TRUE)/n,
  ) %>%
  ungroup()

# stand_test <- poplar %>%
#   mutate(DBH_class = cut(dclass, breaks = seq(1, max(dclass) + class_width, by = class_width), include.lowest = TRUE)) %>%
#   group_by(DBH_class) %>%
#   summarise(trees_acre = tpa_unadj, vol_acre = volcfgrs*tpa_unadj) %>%
#   filter(DBH_class == "(1,3)")
#
# stand_test_se <- apply(stand_test[stand_test$DBH_class !="All", c("trees_acre", "vol_acre")], 2, sd) / sqrt(n)
# stand_table_se <- stand_table %>%
#   rowwise() %>%
#   mutate(
#     se_trees = sd(trees_acre)/sqrt(count1),
#     se_vol = sd(vol_acre)/sqrt(count1)
#   )

stand_table_sum <- stand_table %>%
  summarise(count = sum(count),
            trees_acre = sum(trees_acre),
            BA_acre = sum(BA_acre),
            vol_acre = sum(vol_acre),
  ) %>%
  mutate(DBH_Class = "All") %>%
  bind_rows(stand_table, .) %>% as.data.frame()

stock_table <- poplar %>%
   group_by(common_name) %>%
  summarise(count = n(),
            trees_acre = sum(tpa_unadj)/n,
            BA_acre = sum(BA*tpa_unadj)/n,
            vol_acre = sum(volcfgrs*tpa_unadj, na.rm = TRUE)/n,
  )%>%
  ungroup()

stock_table <- stock_table %>%
  summarise(count = sum(count),
            trees_acre = sum(trees_acre),
            BA_acre = sum(BA_acre),
            vol_acre = sum(vol_acre),
  ) %>%
  mutate(common_name = "All") %>%
  bind_rows(stock_table, .) %>% as.data.frame()

# Part 3
stock_percent <- poplar %>%
  group_by(common_name) %>%
  summarise(count = n(),
            trees_acre = sum(tpa_unadj)/n,
            BA_acre = sum(BA*tpa_unadj)/n,
            vol_acre = sum(volcfgrs*tpa_unadj, na.rm = TRUE)/n,
            qmd = sqrt((BA_acre / trees_acre) / 0.005454)
  )%>%
  mutate(percent_ta = trees_acre / sum(trees_acre) * 100,
            percent_baa = BA_acre / sum(BA_acre) * 100,
            percent_vola = vol_acre / sum(vol_acre, na.rm = TRUE) * 100
  ) %>% ungroup()
stock_percent_sum <- stock_percent %>%
  summarise(count = sum(count),
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
### Graphs pt 1
ggplot(gumpoplar, aes(x = reorder(comamon_name, dia), y = dia, colour = common_name)) +
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
### Graphs pt 2
plot_data_long <- pivot_longer(plot_data, cols = c(trees_acre, qmd), names_to = "Variable", values_to = "Value")
ggplot(plot_data_long, aes(x = CN, fill = Variable)) +
  geom_col(aes(y = Value), position = "dodge", width = 0.7) +
  scale_y_continuous(name = "trees_acre", sec.axis = sec_axis(~./100, name = "qmd")) +
  labs(x = "CN") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"))

ggplot(stand_table, aes(trees_acre, vol_acre)) +
  geom_jitter(aes(color = DBH_Class)) +
  geom_smooth(method = lm) +
  labs(title = "Trees per Acre vs Volume per Acre")
ggplot(stand_table, aes(x = DBH_Class, y = trees_acre)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "DBH Class", y = "Trees per Acre") +
  ggtitle("Trees per Acre by DBH Class")

combined_stock <- stock_percent %>%
  mutate(common_name = if_else(percent_vola < 1, "Other", as.character(common_name))) %>%
  group_by(common_name) %>%
  summarise(percent_vola = sum(percent_vola))
combined_stock <- combined_stock[order(-combined_stock$percent_vola), ]
color_palette <- scales::hue_pal()(nrow(combined_stock))
ggplot(combined_stock, aes(x = "", y = percent_vola, fill = common_name)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(x = NULL, y = NULL, fill = "Species") +
  scale_fill_manual(values = color_palette, guide = guide_legend(title = NULL)) +
  geom_text(aes(label = if_else(percent_vola >= 1, paste(round(percent_vola, 1), "%"), "")),
            position = position_stack(vjust = .5), size = 4) +
  theme_minimal()


# sd_trees <- sd(poplar$tpa_unadj)
# mean_trees <- mean(poplar$tpa_unadj)
#
# sd_vol <- sd(poplar$volcfgrs*poplar$tpa_unadj, na.rm=TRUE)
# mean_vol <- mean(poplar$volcfgrs*poplar$tpa_unadj, na.rm=TRUE)
#
# stand_table <- poplar %>%
#   mutate(DBH_Class = cut(dclass, breaks = seq(1, max(dclass) + class_width, by = class_width), include.lowest = TRUE)) %>%
#   group_by(DBH_Class) %>%
#   summarise(
#     trees_acre = sum(tpa_unadj)/n(),
#     vol_acre = sum(volcfgrs*tpa_unadj, na.rm=TRUE)/n()
#   )
#
# stand_table <- stand_table %>%
#   mutate(
#     se_pct_trees = (sd_trees/mean_trees)*100,
#     se_pct_vol = (sd_vol/mean_vol)*100
#   )