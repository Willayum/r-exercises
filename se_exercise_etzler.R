# install.packages(c("tidyr", "data.table", "ggplot2", "dplyr"))
library(tidyr)
library(data.table)
library(ggplot2)
library(dplyr)
library(tibble)

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

n <- nrow(plot_data) #sum of all unique plots = 48

t_value <- qt(0.975, df = n - 1)
se <- apply(plot_data[plot_data$CN != "All", c("trees_acre", "basal_area_acre", "volume_acre", "qmd")], 2, sd) / sqrt(n)
lower_ci <- colMeans(plot_data[, c("trees_acre", "basal_area_acre", "volume_acre", "qmd")], na.rm = TRUE) - t_value * se
upper_ci <- colMeans(plot_data[, c("trees_acre", "basal_area_acre", "volume_acre", "qmd")], na.rm = TRUE) + t_value * se

# Part 2
class_width <- 2
# poplar$dbh <- ceiling(poplar$dia/class_width) * class_width
# dclass <- round(round(poplar$dbh*class_width,0)/class_width,0)

##new
dclass <- floor((poplar$dia + 1) / class_width) * class_width
dclass[dclass > 34] <- 34
poplar$DBH_Class <- paste0("[", dclass - 1, ",", dclass + 1, ")")

se <- apply(plot_data[plot_data$CN != "All", c("trees_acre", "basal_area_acre", "volume_acre", "qmd")], 2, sd) / sqrt(n)

# plot_sums <- poplar %>%
#   group_by(CN, poplar$DBH_CLass) %>%
#   summarise(trees_acre = sum(tpa_unadj), vol_acre = sum(volcfgrs*tpa_unadj, na.rm = TRUE))
#
# plot_se <- plot_sums %>%
#   group_by()
# unique_class <- data.frame(unique(poplar$DBH_Class)) %>%
#   arrange('DBH_Class') %>%
#   rownames_to_column(var = "Class_Order")

stand_table <- poplar %>%
  mutate(DBH_Class = cut(dclass, breaks = seq(1, max(dclass) + class_width, by = class_width), include.lowest = TRUE)) %>%
  group_by(DBH_Class) %>%
  summarise(count1 = n(),
            trees_acre = sum(tpa_unadj)/n,
            BA_acre = sum(BA*tpa_unadj)/n,
            vol_acre = sum(volcfgrs*tpa_unadj, na.rm = TRUE)/n

  ) %>%
  mutate(DBH_Class = str_replace(DBH_Class, "\\(", "["),
         DBH_Class = str_replace(DBH_Class, "\\]", "\\)")) %>%

  ungroup()

dbh_classes <- list(
  "[1,3)", "[3,5)", "[5,7)", "[7,9)", "[9,11)", "[11,13)", "[13,15)", "[15,17)",
  "[17,19)", "[19,21)", "[21,23)", "[23,25)", "[25,27)", "[27,29)", "[29,31)",
  "[31,33)", "[33,35)"
)

#something wrong, need to check every single part of the function call, especially the sd and se to make sure it is doing exactly what I want
plot_by_dbh <- function(x) {
  plot_data1 <- poplar %>%
    filter(DBH_Class %in% x) %>%
    group_by(CN) %>%
    summarise(
      count1 = n(),
      trees_acre = sum(tpa_unadj),
      basal_area_acre = sum(BA * tpa_unadj),
      volume_acre = sum(volcfgrs * tpa_unadj, na.rm = TRUE)
    ) %>%
    mutate(
      qmd = sqrt((basal_area_acre / trees_acre) / 0.005454),
      CN = as.character(CN),
      D_Order = x
    )
  sd_trees_acre <- sqrt(sum((plot_data1$trees_acre - (sum(plot_data1$trees_acre) / sum(plot_data1$count1)))^2) / nrow(plot_data1))
  se_trees_acre <- sd_trees_acre / sqrt(n)
  sd_volume_acre <- sd(plot_data1$volume_acre)
  se_volume_acre <- sd(plot_data1$volume_acre) / sqrt(nrow(plot_data1))
  se_df <- data.frame(DBH_Class = x, sd_trees_acre = sd_trees_acre, se_trees_acre = se_trees_acre,
                      sd_volume_acre = sd_volume_acre, se_volume_acre = se_volume_acre)
  return(se_df)
}

se_list <- lapply(dbh_classes, plot_by_dbh)
DBH_se <-do.call(rbind,se_list)
stand_table_se <- left_join(stand_table, DBH_se)


se_test <- poplar %>%
  group_by(CN, DBH_Class) %>%
  summarise(
    se_value = sd(tpa_unadj) / sqrt(n())
  )

# dseq <- seq(from = 2, to = 34, by = 2)
# dfilter <- filter(dseq, dseq == X-1 | dseq == X+1)
# class1 <- poplar %>%
#   filter(poplar$DBH_Class == dfilter) %>%
#   group_by(CN)
#   summarise(
#     DBH_Class = DBH_Class,
#     tpa = tpa_unadj,
#     vol_acre = volcfgrs*tpa_unadj
#   )

stand_table_se2 <- poplar %>%
  mutate(DBH_Class = cut(dclass, breaks = seq(1, max(dclass) + class_width, by = class_width), include.lowest = TRUE)) %>%
  group_by(DBH_Class) %>%
  summarise(count = n(),
            trees_acre = sum(tpa_unadj)/n,
            percent_se_tpa = (sd(tpa_unadj) / sqrt(n())) / (mean(tpa_unadj)) * 100, #the sums in the sd are wrong, by dbh not by plot, also should just be 48 for /
            BA_acre = sum(BA*tpa_unadj)/n,
            vol_acre = sum(volcfgrs*tpa_unadj, na.rm = TRUE)/n,
            percent_se_vol_acre = (sd(volcfgrs * tpa_unadj) / sqrt(n())) / (mean(volcfgrs * tpa_unadj)) * 100
  ) %>%
  ungroup()

stand_table_se_sum <- stand_table_se2 %>%
  summarise(count = sum(count),
            trees_acre = sum(trees_acre),
            percent_se_tpa = mean(percent_se_tpa, na.rm = TRUE),
            BA_acre = sum(BA_acre),
            vol_acre = sum(vol_acre),
            percent_se_vol_acre = mean(percent_se_vol_acre, na.rm = TRUE)
  ) %>%
  mutate(DBH_Class = "Sums, Means for SE's") %>%
  bind_rows(stand_table_se, .) %>% as.data.frame()

##start of lapply method appears to be issue with no_sum_data weird output
no_sum_data <- poplar %>%
  mutate(DBH_Class = cut(dclass, breaks = seq(1, max(dclass) + class_width, by = class_width), include.lowest = TRUE)) %>%
  group_by(CN) %>%
  summarise(observations = n(),
            trees_acre = (tpa_unadj),
            vol_acre = (volcfgrs*tpa_unadj),
            DBH_Class = DBH_Class
  ) %>%
  ungroup()

no_sum_data <- na.omit(no_sum_data)
dsplit <- split(no_sum_data, no_sum_data$DBH_Class)

percent_standard_error <- function(p1, column) {
  se <- sd(p1[[column]]) / sqrt(nrow(p1))
  percent_se <- (se / mean(p1[[column]])) * 100
  return(percent_se)
}

trees_se_function <- function(p2) {
  return(percent_standard_error(p2, 'trees_acre'))
}
tpa_se_class <- lapply(dsplit, trees_se_function)

vol_acre_se_class <- lapply(dsplit, function(p3) percent_standard_error(p3, 'vol_acre'))

se_table <- data.frame(
  tpa_se = unlist(tpa_se_class),
  vol_acre_se = unlist(vol_acre_se_class)
)

se_table <- rownames_to_column(se_table, var = "DBH_Class")
rownames(se_table) <- NULL
stand_table_lapply_se <- left_join(stand_table,se_table)

### End of SE Assignment ###

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