# Plots for "Public transport in the 15-minutes city" paper - November 2025
# Figur 2, 3, 4

rm(list=ls())

# Import libraries
library(ggplot2)
library(ggstance)
#install.packages('stargazer')
library(stargazer)
library(dplyr)
#install.packages('standardize')
library(standardize)
library(mosaic)


library(gridExtra)
library(cowplot)


library(ggridges)


# 1. Folder and data
setwd("g:/Saját meghajtó/Public transport in the 15-minutes city") # I work in a Google Drive folder shard with the group.

bp=read.table("./budapest/bp_socioecon_merged5.csv", header = T, sep=",")
#  helsinki=read.table("./helsinki/helsinki_socioecon_merged2.csv", header = T, sep=",")
madrid=read.table("./madrid/madrid_socioecon_merged2.csv", header = T, sep=",")

helsinki=read.table("./helsinki/helsinki_socioecon_merged4.csv", header = T, sep=",")

ls(bp)
ls(madrid)
ls(helsinki)


# 2. Check variables
ls(bp)
ls(madrid)
ls(helsinki)


# 3. Figure 2 boxplots

bp_sub <- bp %>%
  select(ellipticity, distance_betweenness) %>%
  mutate(city = "Budapest")

madrid_sub <- madrid %>%
  select(ellipticity, distance_betweenness) %>%
  mutate(city = "Madrid")

helsinki_sub <- helsinki %>%
  select(ellipticity, distance_betweenness) %>%
  mutate(city = "Helsinki")

all_data <- bind_rows(bp_sub, madrid_sub, helsinki_sub)

all_data <- all_data %>%
  group_by(city) %>%
  mutate(distance_betweenness_bin = ntile(log10(distance_betweenness), 2)) %>%  # 5 quantile bins
  ungroup()

ggplot(all_data, aes(x = as.factor(distance_betweenness_bin), y = ellipticity, fill = city)) +
  geom_boxplot(outlier.shape = 21, alpha = 0.7) +
  facet_wrap(~ city, scales = "free") +
  labs(
    x = "Distance Betweenness (log, median cut)",
    y = "Ellipticity",
    title = "Ellipticity by Distance Betweenness in Three Cities"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


# make the plot object
p <- ggplot(all_data, aes(x = as.factor(distance_betweenness_bin), y = ellipticity, fill = city)) +
  geom_boxplot(outlier.shape = 21, alpha = 0.7) +
  facet_wrap(~ city, scales = "free") +
  labs(
    x = "Distance Betweenness (log, median cut)",
    y = "Ellipticity",
    title = "Ellipticity by Distance Betweenness in Three Cities"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# save with Cairo
ggsave(
  filename = "g:/Saját meghajtó/Public transport in the 15-minutes city/plots_BL_20251119/ellipticity_by_distance_betweenness.pdf",
  plot = p,
  device = cairo_pdf,   # <- uses Cairo
  width = 10, height = 6, dpi = 300
)


correlations <- all_data %>%
  group_by(city) %>%
  summarise(
    correlation = cor(ellipticity, log10(distance_betweenness), use = "complete.obs", method = "pearson")
  )

print(correlations)


library(broom)

# For each city, test if ellipticity differs between low/high distance_betweenness groups
test_results <- all_data %>%
  group_by(city) %>%
  do(tidy(wilcox.test(ellipticity ~ distance_betweenness_bin, data = .))) %>%
  ungroup()
# U-test shws significant difference
print(test_results)


#install.packages('coin')
library(coin)
library(rstatix)

# Effects sizes are very small
effect_sizes <- all_data %>%
  group_by(city) %>%
  wilcox_effsize(ellipticity ~ distance_betweenness_bin)

print(effect_sizes)


# 4. Figure 3

ls(bp)

# Generate Access variables

bp <- bp %>%
  mutate(
    cultural_institutions_multimod_d = as.integer(cultural_institutions_multimodal > 0),
    drugstores_multimodal_d = as.integer(drugstores_multimodal > 0),
    groceries_multimodal_d = as.integer(groceries_multimodal > 0),
    healthcare_multimodal_d = as.integer(healthcare_multimodal > 0),
    parks_multimodal_d = as.integer(parks_multimodal > 0),
    religious_organ_multimod_d = as.integer(religious_organizations_multimodal > 0),
    restaurants_multimodal_d = as.integer(restaurants_multimodal > 0),
    schools_multimodal_d = as.integer(schools_multimodal > 0),
    services_multimodal_d = as.integer(services_multimodal > 0),
    
    cultural_institutions_walk15_d = as.integer(cultural_institutions_walk15 > 0),
    drugstores_walk15_d = as.integer(drugstores_walk15 > 0),
    groceries_walk15_d = as.integer(groceries_walk15 > 0),
    healthcare_walk15_d = as.integer(healthcare_walk15 > 0),
    parks_walk15_d = as.integer(parks_walk15 > 0),
    religious_organizations_walk15_d = as.integer(religious_organizations_walk15 > 0),
    restaurants_walk15_d = as.integer(restaurants_walk15 > 0),
    schools_walk15_d = as.integer(schools_walk15 > 0),
    services_walk15_d = as.integer(services_walk15 > 0),
    
    multimod_sum = cultural_institutions_multimod_d + drugstores_multimodal_d +
      groceries_multimodal_d + healthcare_multimodal_d +
      parks_multimodal_d + religious_organ_multimod_d +
      restaurants_multimodal_d + schools_multimodal_d +
      services_multimodal_d,
    
    walk_sum = cultural_institutions_walk15_d + drugstores_walk15_d +
      groceries_walk15_d + healthcare_walk15_d +
      parks_walk15_d + religious_organizations_walk15_d +
      restaurants_walk15_d + schools_walk15_d +
      services_walk15_d,
    

    multimod_all = cultural_institutions_multimodal + drugstores_multimodal +
      groceries_multimodal + healthcare_multimodal +
      parks_multimodal + religious_organizations_multimodal +
      restaurants_multimodal + schools_multimodal +
      services_multimodal,
    
    walk_all = cultural_institutions_walk15 + drugstores_walk15 +
      groceries_walk15 + healthcare_walk15 +
      parks_walk15 + religious_organizations_walk15 +
      restaurants_walk15 + schools_walk15 +
      services_walk15
    
        
  )

helsinki <- helsinki %>%
  mutate(
    cultural_institutions_multimod_d = as.integer(cultural_institutions_multimodal > 0),
    drugstores_multimodal_d = as.integer(drugstores_multimodal > 0),
    groceries_multimodal_d = as.integer(groceries_multimodal > 0),
    healthcare_multimodal_d = as.integer(healthcare_multimodal > 0),
    parks_multimodal_d = as.integer(parks_multimodal > 0),
    religious_organ_multimod_d = as.integer(religious_organizations_multimodal > 0),
    restaurants_multimodal_d = as.integer(restaurants_multimodal > 0),
    schools_multimodal_d = as.integer(schools_multimodal > 0),
    services_multimodal_d = as.integer(services_multimodal > 0),
    
    cultural_institutions_walk15_d = as.integer(cultural_institutions_walk15 > 0),
    drugstores_walk15_d = as.integer(drugstores_walk15 > 0),
    groceries_walk15_d = as.integer(groceries_walk15 > 0),
    healthcare_walk15_d = as.integer(healthcare_walk15 > 0),
    parks_walk15_d = as.integer(parks_walk15 > 0),
    religious_organizations_walk15_d = as.integer(religious_organizations_walk15 > 0),
    restaurants_walk15_d = as.integer(restaurants_walk15 > 0),
    schools_walk15_d = as.integer(schools_walk15 > 0),
    services_walk15_d = as.integer(services_walk15 > 0),
    
    multimod_sum = cultural_institutions_multimod_d + drugstores_multimodal_d +
      groceries_multimodal_d + healthcare_multimodal_d +
      parks_multimodal_d + religious_organ_multimod_d +
      restaurants_multimodal_d + schools_multimodal_d +
      services_multimodal_d,
    
    walk_sum = cultural_institutions_walk15_d + drugstores_walk15_d +
      groceries_walk15_d + healthcare_walk15_d +
      parks_walk15_d + religious_organizations_walk15_d +
      restaurants_walk15_d + schools_walk15_d +
      services_walk15_d,
    
    
    multimod_all = cultural_institutions_multimodal + drugstores_multimodal +
      groceries_multimodal + healthcare_multimodal +
      parks_multimodal + religious_organizations_multimodal +
      restaurants_multimodal + schools_multimodal +
      services_multimodal,
    
    walk_all = cultural_institutions_walk15 + drugstores_walk15 +
      groceries_walk15 + healthcare_walk15 +
      parks_walk15 + religious_organizations_walk15 +
      restaurants_walk15 + schools_walk15 +
      services_walk15
  )

madrid <- madrid %>%
  mutate(
    cultural_institutions_multimod_d = as.integer(cultural_institutions_multimodal > 0),
    drugstores_multimodal_d = as.integer(drugstores_multimodal > 0),
    groceries_multimodal_d = as.integer(groceries_multimodal > 0),
    healthcare_multimodal_d = as.integer(healthcare_multimodal > 0),
    parks_multimodal_d = as.integer(parks_multimodal > 0),
    religious_organ_multimod_d = as.integer(religious_organizations_multimodal > 0),
    restaurants_multimodal_d = as.integer(restaurants_multimodal > 0),
    schools_multimodal_d = as.integer(schools_multimodal > 0),
    services_multimodal_d = as.integer(services_multimodal > 0),
    
    cultural_institutions_walk15_d = as.integer(cultural_institutions_walk15 > 0),
    drugstores_walk15_d = as.integer(drugstores_walk15 > 0),
    groceries_walk15_d = as.integer(groceries_walk15 > 0),
    healthcare_walk15_d = as.integer(healthcare_walk15 > 0),
    parks_walk15_d = as.integer(parks_walk15 > 0),
    religious_organizations_walk15_d = as.integer(religious_organizations_walk15 > 0),
    restaurants_walk15_d = as.integer(restaurants_walk15 > 0),
    schools_walk15_d = as.integer(schools_walk15 > 0),
    services_walk15_d = as.integer(services_walk15 > 0),
    
    multimod_sum = cultural_institutions_multimod_d + drugstores_multimodal_d +
      groceries_multimodal_d + healthcare_multimodal_d +
      parks_multimodal_d + religious_organ_multimod_d +
      restaurants_multimodal_d + schools_multimodal_d +
      services_multimodal_d,
    
    walk_sum = cultural_institutions_walk15_d + drugstores_walk15_d +
      groceries_walk15_d + healthcare_walk15_d +
      parks_walk15_d + religious_organizations_walk15_d +
      restaurants_walk15_d + schools_walk15_d +
      services_walk15_d,
    
    
    multimod_all = cultural_institutions_multimodal + drugstores_multimodal +
      groceries_multimodal + healthcare_multimodal +
      parks_multimodal + religious_organizations_multimodal +
      restaurants_multimodal + schools_multimodal +
      services_multimodal,
    
    walk_all = cultural_institutions_walk15 + drugstores_walk15 +
      groceries_walk15 + healthcare_walk15 +
      parks_walk15 + religious_organizations_walk15 +
      restaurants_walk15 + schools_walk15 +
      services_walk15
  )


# Prepare data for plotting
bp_long <- bp %>%
  select(walk_all, multimod_all) %>%
  mutate(city = "Budapest")

madrid_long <- madrid %>%
  select(walk_all, multimod_all) %>%
  mutate(city = "Madrid")

helsinki_long <- helsinki %>%
  select(walk_all, multimod_all) %>%
  mutate(city = "Helsinki")

all_data <- bind_rows(bp_long, madrid_long, helsinki_long) %>%
  pivot_longer(
    cols = c(walk_all, multimod_all),
    names_to = "type",
    values_to = "value"
  ) %>%
  mutate(
    type = recode(
      type,
      walk_all = "Walk Access (Nr. of amenities)",
      multimod_all = "Multimodal Access (Nr. of amenities)"
    ),
    value_log = log10(value + 1)  # log-transform to match your x-axis label
  )

# --- Harmonized density ridge plot ---
ggplot(all_data, aes(x = value_log, y = city, fill = type)) +
  geom_density_ridges(
    alpha = 0.6,
    scale = 1.1,
    rel_min_height = 0.01,
    color = "white",
    size = 0.3
  ) +
  scale_fill_manual(
    values = c(
      "Walk Access (Nr. of amenities)" = "#e41a1c",
      "Multimodal Access (Nr. of amenities)" = "#377eb8"
    )
  ) +
  labs(
    x = expression(log[10]~"Access (Nr. of amenities)"),
    y = NULL,
    fill = NULL,
    title = "Distribution of 15-Minute Walk and Multimodal Access by City"
  ) +
  coord_cartesian(xlim = c(0, 3)) +
  theme_ridges(grid = TRUE, font_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


# Create the plot object
p <- ggplot(all_data, aes(x = value_log, y = city, fill = type)) +
  geom_density_ridges(
    alpha = 0.6,
    scale = 1.1,
    rel_min_height = 0.01,
    color = "white",
    size = 0.3
  ) +
  scale_fill_manual(
    values = c(
      "Walk Access (Nr. of amenities)" = "#e41a1c",
      "Multimodal Access (Nr. of amenities)" = "#377eb8"
    )
  ) +
  labs(
    x = expression(log[10]~"Access (Nr. of amenities)"),
    y = NULL,
    fill = NULL,
    title = "Distribution of 15-Minute Walk and Multimodal Access by City"
  ) +
  coord_cartesian(xlim = c(0, 3)) +
  theme_ridges(grid = TRUE, font_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Save to PDF with Cairo
ggsave(
  filename = "g:/Saját meghajtó/Public transport in the 15-minutes city/plots_BL_20251119/walk_multimodal_access_N_by_city.pdf",
  plot = p,
  device = cairo_pdf,     # ensures Cairo rendering
  width = 8,
  height = 10,
  dpi = 300
)

# 5. Figure 4



plot(bp$gini_walk15, bp$gini_multimodal)
plot(bp$gini_house_walk15, bp$gini_house_multimodal)
plot(madrid$weighted_gini_walk, madrid$weighted_gini_multi)
plot(helsinki$weighted_gini_walk, helsinki$weighted_gini_multi)



# --- 1. Build city-level datasets with both 'Experienced' and 'Residential' types ---

# --- Build city pieces (unchanged) ---
bp_exp <- bp %>%
  transmute(
    walk_gini  = gini_walk15,
    multi_gini = gini_multimodal,
    city = "Budapest",
    group = "Experienced Access"
  )

bp_res <- bp %>%
  transmute(
    walk_gini  = gini_house_walk15,
    multi_gini = gini_house_multimodal,
    city = "Budapest",
    group = "Residential Access"
  )

madrid_long <- madrid %>%
  transmute(
    walk_gini  = weighted_gini_walk,
    multi_gini = weighted_gini_multi,
    city = "Madrid",
    group = "Residential Access"
  )

helsinki_long <- helsinki %>%
  transmute(
    walk_gini  = weighted_gini_walk,
    multi_gini = weighted_gini_multi,
    city = "Helsinki",
    group = "Residential Access"
  )

# --- Combine, reshape; DO NOT log here ---
all_data <- bind_rows(bp_exp, bp_res, madrid_long, helsinki_long) %>%
  pivot_longer(
    cols = c(walk_gini, multi_gini),
    names_to = "type",
    values_to = "value_raw"
  ) %>%
  mutate(
    type  = recode(type,
                   walk_gini  = "Walk Access",
                   multi_gini = "Multimodal Access"),
    group = factor(group, levels = c("Experienced Access", "Residential Access"))
  )

# --- Single robust log transform ---
eps <- 1e-12
plot_data <- all_data %>%
  mutate(value_log = ifelse(value_raw > 0, log10(value_raw), NA_real_)) %>%
  filter(is.finite(value_log)) %>%
  group_by(city, type, group) %>%
  filter(n() >= 2) %>%          # ensure enough points for a density
  ungroup()

# (Optional) sanity check: should list all cities in each facet
# print(plot_data %>% count(group, city, type))

#PLOT
ggplot(plot_data, aes(x = value_log, y = city, fill = type)) +
  geom_density_ridges(
    alpha = 0.5,
    scale = 1.2,
    rel_min_height = 0.01,
    color = "white",
    size = 0.3,
    na.rm = TRUE
  ) +
  facet_wrap(~ group, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("Walk Access" = "red", "Multimodal Access" = "blue")) +
  labs(
    x = expression(log[10]~"Gini"),
    y = NULL,
    fill = NULL,
    title = "Distributions of log10 15-Minute Walk and Multimodal Access Ginis by City"
  ) +
  coord_cartesian(xlim = c(-1.2, 1.7)) +       # limit x-axis range
  theme_ridges(grid = TRUE, font_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


###

# --- Combine and compute raw change (multimodal - walk15) ---
change_data <- bind_rows(bp_exp, bp_res, madrid_long, helsinki_long) %>%
  mutate(
    gini_change = multi_gini - walk_gini
  ) %>%
  filter(is.finite(gini_change)) %>%
  group_by(city, group) %>%
  filter(n() >= 2) %>%  # ensure enough points for density
  ungroup()

eps <- 1e-12
plot_data <- all_data %>%
  mutate(value_log = ifelse(value_raw > 0, log10(value_raw), NA_real_)) %>%
  filter(is.finite(value_log)) %>%
  group_by(city, type, group) %>%
  filter(n() >= 2) %>%          # ensure enough points for a density
  ungroup()

# (Optional) quick check
# change_data %>% count(city, group)

# --- Density ridge plot of RAW change by city ---
ggplot(change_data, aes(x = gini_change, y = city, fill = group)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4, color = "grey40") +
  geom_density_ridges(
    alpha = 0.6,
    scale = 1.1,
    rel_min_height = 0.01,
    color = "white",
    size = 0.3,
    na.rm = TRUE
  ) +
  facet_wrap(~ group, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("Experienced Access" = "#8da0cb", "Residential Access" = "#fc8d62")) +
  labs(
    x = "Change in Gini (Multimodal − Walk15)",
    y = NULL,
    fill = NULL,
    title = "Distribution of Raw Change in Access Ginis by City"
  ) +
  coord_cartesian(xlim = c(-1, 1)) +   # <-- limit x-axis range
  theme_ridges(grid = TRUE, font_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


set.seed(1234)  # for reproducibility of random noise

# --- Compute change in Gini ---
change_data <- bind_rows(bp_exp, bp_res, madrid_long, helsinki_long) %>%
  mutate(
    gini_change = multi_gini - walk_gini
  ) %>%
  filter(is.finite(gini_change)) %>%
  group_by(city, group) %>%
  filter(n() >= 2) %>%  # ensure enough points for density
  ungroup() %>%
  # Add small random noise at zero and log-transform signed values
  mutate(
    gini_change_noisy = ifelse(
      gini_change == 0,
      runif(n(), -1e-5, 1e-5),  # small symmetric random noise
      gini_change
    ),
    gini_change_log = sign(gini_change_noisy) * log10(abs(gini_change_noisy) + 1e-6)
  )

# --- Density ridge plot with log-scaled x (signed log10) ---
ggplot(change_data, aes(x = gini_change_log, y = city, fill = group)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4, color = "grey40") +
  geom_density_ridges(
    alpha = 0.6,
    scale = 1.1,
    rel_min_height = 0.01,
    color = "white",
    size = 0.3,
    na.rm = TRUE
  ) +
  facet_wrap(~ group, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("Experienced Access" = "#8da0cb", "Residential Access" = "#fc8d62")) +
  labs(
    x = expression("Signed log"[10] * "(|Change in Gini| + 1e-6)"),
    y = NULL,
    fill = NULL,
    title = "Distribution of Log-Scaled Change in Access Ginis by City"
  ) +
  coord_cartesian(xlim = c(-5, 5)) +   # adjust depending on your data range
  theme_ridges(grid = TRUE, font_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

correlations <- all_data %>%
  group_by(city) %>%
  summarise(
    correlation = cor(walk_gini, multi_gini, use = "complete.obs", method = "pearson")
  )

print(correlations)

