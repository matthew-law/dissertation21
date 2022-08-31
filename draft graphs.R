#### graphs sandbox


library(knitr)
library(tidyverse)
library(kableExtra)
library(readr)


# tables ------------------------------------------------------------------

mpg_list <- split(mtcars$mpg, mtcars$cyl)
disp_list <- split(mtcars$disp, mtcars$cyl)
inline_plot <- data.frame(cyl = c(4, 6, 8), mpg_box = "", mpg_hist = "",
                          mpg_line1 = "", mpg_line2 = "",
                          mpg_points1 = "", mpg_points2 = "", mpg_poly = "")

inline_plot %>%
  kbl(booktabs = TRUE) %>%
  kable_paper(full_width = FALSE) %>%
  column_spec(2, image = spec_boxplot(mpg_list)) %>%
  column_spec(3, image = spec_hist(mpg_list)) %>%
  column_spec(4, image = spec_plot(mpg_list, same_lim = TRUE)) %>%
  column_spec(5, image = spec_plot(mpg_list, same_lim = FALSE)) %>%
  column_spec(6, image = spec_plot(mpg_list, type = "p")) %>%
  column_spec(7, image = spec_plot(mpg_list, disp_list, type = "p")) %>%
  column_spec(8, image = spec_plot(mpg_list, polymin = 5))

# I try w type data

type <- typology

area_list <- split(type$area_km2, type$Segmentation)

type %>%
  group_by(Segmentation) %>%
  dplyr::summarise(`Mean QCoD` = mean(QCoD2, na.rm=TRUE),
                   `Mean` = mean(area_km2, na.rm=TRUE),
                   `Number of units` = n()) %>% 
  add_column(Distribution = "") %>% 
  mutate_at('Mean QCoD', ~formatC(., format = "f", digits = 3)) %>% 
  mutate_at('Mean', ~formatC(., format = "f", digits = 2)) %>% 
  arrange(factor(Segmentation, levels = c("Morphological tessellation", "Enclosed tessellation", "ET transposed to block", 
                                          "ET transposed to H3", "H3 basic", "H3 with ET characters", "Spatially constrained MT", 
                                          "Existing neighbourhoods", "Existing districts", "idealista polygons"))) %>%
  relocate(Distribution, .after = Mean) %>% 
  kable(booktabs = TRUE, caption="Average typology values for each segmentation.") %>% 
  column_spec(3, image = spec_boxplot(area_list))

# table pre-kable
type %>%
  group_by(Segmentation) %>%
  dplyr::summarise(`Mean QCoD` = mean(QCoD2, na.rm=TRUE),
                   `Mean` = mean(area_km2, na.rm=TRUE),
                   `Number of units` = n()) %>% 
  add_column(Distribution = "") %>% 
  mutate_at('Mean QCoD', ~formatC(., format = "f", digits = 3)) %>% 
  mutate_at('Mean', ~formatC(., format = "f", digits = 2)) %>% 
  arrange(factor(Segmentation, levels = c("Morphological tessellation", "Enclosed tessellation", "ET transposed to block", 
                 "ET transposed to H3", "H3 basic", "H3 with ET characters", "Spatially constrained MT", 
                 "Existing neighbourhoods", "Existing districts", "idealista polygons"))) %>%
  relocate(Distribution, .after = Mean)

# typology ----------------------------------------------------------------

typology <- read_csv("figures/typology_metrics.csv")

typology %>% 
  mutate(n = replace_na(n, 0))

library(ggplot2)
library(ggrepel)
library(scales)

# python QCoD
typology %>% 
  ggplot(aes(area, QCoD)) +
  geom_point()

typology <- typology %>% 
  # new version of QCoD
  mutate(QCoD2 = (`Price Q3` - `Price Q1`) / (`Price Q3` + `Price Q1`)) %>% 
  # from m2 to km2
  mutate(area_km2 = area/1e+6) %>%
  # rename segmentations
  mutate_at('segmentation', ~dplyr::recode(segmentation, 
                                           "MT8cls5sw" = "Morphological tessellation", 
                                           "ET8cls5sw" = "Enclosed tessellation",
                                           "block_from_ET8cls5sw" = "ET transposed to block",
                                           "H3_from_ET8cls5sw" = "ET transposed to H3",
                                           "H3_basic_8cls1sw" = "H3 basic",
                                           "H3_charsfrom_ET8cls5sw" = "H3 with ET characters",
                                           "constrained_15cls" = "Spatially constrained MT",
                                           "barris" = "Existing neighbourhoods",
                                           "districtes" = "Existing districts",
                                           "polygons_BCN" = "idealista polygons"
  )) %>% 
  # rename some columns
  rename(Segmentation = segmentation,
         'Number of cadastral parcels' = n)

# typology <- typology %>% 
#   rename('Number of cadastral parcels' = 'Number of \ncadastral parcels')

# hmmm
# plot(typology$QCoD, typology$QCoD2)

# new font who dis
font_add(family = "TeX Gyre Pagella", 
         regular = "/Library/Fonts/texgyrepagella-regular.otf",
         bold = '/Library/Fonts/texgyrepagella-bold.otf',
         italic = '/Library/Fonts/texgyrepagella-italic.otf',
         bolditalic = '/Library/Fonts/texgyrepagella-bolditalic.otf')

# each typology separately
typology %>% 
  ggplot(aes(area_km2, QCoD2, colour = Segmentation, size = `Number of cadastral parcels`)) +
  geom_point() +
  xlim(0, 35) +
  scale_colour_brewer(palette = "Paired") +
  scale_size(range = c(0.5,5)) +
  labs(x = expression("Typology area (km"^2*")"), y = 'Typology QCoD') +
  guides(size = guide_legend(title="Number of corresponding\ncadastral parcels")) +
  theme_minimal() +
  theme(legend.position="right",
        text=element_text(family="TeX Gyre Pagella"))

# average values for each segmentation
typology %>% 
  group_by(Segmentation) %>% 
  dplyr::summarise(avg_QCoD = mean(QCoD2, na.rm=TRUE),
                   avg_area = mean(area_km2, na.rm=TRUE)) %>% 
  ggplot(aes(avg_area, avg_QCoD)) +
  geom_point() +
  geom_smooth(method = "lm", 
              se = F, 
              colour = 'grey',
              fill = 'chartreuse',
              alpha = 0.2) +
  coord_cartesian(clip = "off") +
  geom_text_repel(
    aes(label=Segmentation),
    xlim = c(-3, Inf), ylim = c(0, Inf),
    min.segment.length = 0,
    point.padding = 0,
    box.padding = 0.5,
    bg.color = "white",
    bg.r = 0.2,
    family = "TeX Gyre Pagella"
  ) +
  xlim(0, 23) +
  labs(x = expression("Mean typology area (km"^2*")"), y = 'Mean typology QCoD') +
  # scale_colour_brewer(palette = "Paired") +
  theme_minimal() +
  theme(legend.position="bottom",
        text=element_text(family="TeX Gyre Pagella"))

# polygons ----------------------------------------------------------------

polygon_OG <- read_csv("figures/polygon_metrics.csv")

polygon <- polygon_OG %>% 
  # new version of QCoD
  mutate(QCoD2 = (`Price Q3` - `Price Q1`) / (`Price Q3` + `Price Q1`)) %>% 
  # from m2 to km2
  mutate(area_km2 = area/1e+6) %>%
  # rename segmentations
  mutate_at('segmentation', ~dplyr::recode(segmentation, 
                                           "MT8cls5sw" = "Morphological tessellation", 
                                           "ET8cls5sw" = "Enclosed tessellation",
                                           "block_from_ET8cls5sw" = "ET transposed to block",
                                           "H3_from_ET8cls5sw" = "ET transposed to H3",
                                           "H3_basic_8cls1sw" = "H3 basic",
                                           "H3_charsfrom_ET8cls5sw" = "H3 with ET characters",
                                           "constrained_15cls" = "Spatially constrained MT",
                                           "barris" = "Existing neighbourhoods",
                                           "districtes" = "Existing districts",
                                           "polygons_BCN" = "idealista polygons"
  )) %>% 
  # rename some columns
  rename(Segmentation = segmentation)

polygon %>% 
  filter(n > 10)

polygon %>% 
  group_by(Segmentation) %>% 
  dplyr::summarise(avg_QCoD = mean(QCoD, na.rm=TRUE),
                   avg_area = mean(area_km2, na.rm=TRUE))

# each polygon separately
polygon %>% 
  # filter(n > 10) %>% 
  ggplot(aes(area_km2, QCoD, colour = Segmentation, 
             alpha = n^0.01,
             size = n)) +
  geom_point() +
  # xlim(0, 35) +
  scale_colour_brewer(palette = "Paired") +
  scale_size(range = c(0.3,5)) +
  scale_x_continuous(trans = 'log10', 
                     labels = function(x) sprintf("%g", x),
                     limits = c(0.00005,100),
                     breaks=breaks_log(6)) +
  labs(x = expression("Polygon area (km"^2*")"), y = 'Polygon QCoD') +
  guides(size = guide_legend(title="Number of corresponding\ncadastral parcels"),
         alpha = F) +
  theme_minimal() +
  theme(legend.position="right",
        text=element_text(family="TeX Gyre Pagella"))

# average values for each segmentation
polygon %>% 
  group_by(Segmentation) %>% 
  dplyr::summarise(avg_QCoD = mean(QCoD, na.rm=TRUE),
                   avg_area = mean(area_km2, na.rm=TRUE)) %>% 
  ggplot(aes(avg_area, avg_QCoD)) +
  geom_point() +
  geom_smooth(method = "lm", 
              se = F, 
              colour = 'grey',
              fill = 'chartreuse',
              alpha = 0.2) +
  coord_cartesian(clip = "off") +
  geom_text_repel(
    aes(label=Segmentation),
    xlim = c(-1.5, 9), ylim = c(0, Inf),
    min.segment.length = 0,
    point.padding = 0,
    box.padding = 0.5,
    bg.color = "white",
    bg.r = 0.2,
    family = "TeX Gyre Pagella"
  ) +
  scale_x_continuous(breaks=seq(0,8,2), limits = c(-1,9)) +
  scale_y_continuous(labels = function(x) sprintf("%g", x)) +
  labs(x = expression("Mean polygon area (km"^2*")"), y = 'Mean polygon QCoD') +
  # scale_colour_brewer(palette = "Paired") +
  theme_minimal() +
  theme(legend.position="bottom",
        text=element_text(family="TeX Gyre Pagella"))