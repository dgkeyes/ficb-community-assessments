
# Packages ----------------------------------------------------------------

library(tidyverse)
library(purrr)
library(hrbrthemes)
library(hrbrmisc)
library(readxl)
library(janitor)


# Get Data -----------------------------------------------------------------

path <- "data/CGM data for AGA IV Sius 4 19 2019.xlsx"

sheets <- excel_sheets(path)

cgm_data <- path %>% 
  excel_sheets() %>%
  set_names() %>% 
  map_df(~ read_excel(col_types = "text", path = path, sheet = .x), .id = "sheet") %>% 
  clean_names() %>% 
  rename("item" = "x1") %>% 
  drop_na(item) %>% 
  gather(key = "type_of_rating", value = "rating", -c(sheet, item)) %>% 
  filter(str_detect(sheet, "round", negate = TRUE)) %>% 
  mutate(rating = parse_number(rating)) %>% 
  mutate(rating = na_if(rating, "0")) %>% 
  mutate(rating = as.numeric(rating)) %>% 
  mutate(type_of_rating = case_when(
    type_of_rating == "ficb_assessment" ~ "FICB Assessment",
    type_of_rating == "self_assess_future" ~ "Community Goal",
    type_of_rating == "self_assess_past" ~ "2018 Community Baseline",
    type_of_rating == "self_assess_present" ~ "2019 Community Self Assessment",
  )) %>% 
  # mutate(item = str_wrap(item, 25)) %>%
  mutate(item = fct_inorder(item)) %>% 
  mutate(item = fct_rev(item)) %>% 
  mutate(type_of_rating = factor(type_of_rating, 
                                 levels = c("2018 Community Baseline",
                                            "FICB Assessment",
                                            "2019 Community Self Assessment",
                                            "Community Goal"))) %>% 
  # mutate(type_of_rating = fct_rev(type_of_rating)) %>% 
  mutate(category = rep(c("Connections", "Connections", "Connections", "Connections",
                          "Capacity", "Capacity", "Capacity", "Capacity",
                          "CL Action", "CL Action", "CL Action", "CL Action",
                          "CB Culture", "CB Culture", "CB Culture", "CB Culture"), 12)) %>% 
  mutate(category = factor(category, levels = c("Connections", "Capacity", "CL Action", "CB Culture")))



# Define things for plots -------------------------------------------------

x_axis_labels = c("1\nEmerging",
                  "2\nCommitted",
                  "3\nIntentional",
                  "4\nStrategic",
                  "5\nCatalytic",
                  "6\nTransformational")

tfff_dark_green <- "#265142"
tfff_light_green <- "#B5CC8E"
tfff_orange <- "#e65100"
tfff_yellow <- "#FBC02D"
tfff_blue <- "#283593"
tfff_red <- "#B71C1C"
tfff_dark_gray <- "#545454"
tfff_medium_gray <- "#a8a8a8"
tfff_light_gray <- "#eeeeee"

ficb_palette <- c(tfff_medium_gray,
                  tfff_yellow,
                  tfff_light_green,
                  tfff_dark_green)




# Dot Plot v2 -------------------------------------------------------------


dk_make_save_cgm_plot <- function(community) {

cgm_data_filtered <- cgm_data %>% 
  filter(str_detect(sheet, community))

ggplot(cgm_data_filtered, aes(item, rating,
                             color = type_of_rating,
                             shape = type_of_rating,
                             size = type_of_rating,
                             group = item,
                             alpha = type_of_rating)) +
  geom_line(size = 2.5,
            alpha = 0.5,
            color = tfff_medium_gray,
            show.legend = F) +
  geom_point(alpha = 0.7) +
  coord_flip() +
  scale_shape_manual(values = c(16, 124, 16, 16)) +
  scale_color_manual(values = ficb_palette) +
  scale_size_manual(values = c(7, 10, 10, 7)) +
  theme_ipsum(base_family = "Inter",
              base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_blank(),
        axis.text.x = element_text(size = 8),
        strip.text = element_text(face = "bold"),
        axis.title = element_blank()) +
  labs(color = "",
       title = "") +
  scale_y_continuous(limits = c(0, 6),
                     breaks = seq(0, 6, by = 1),
                     labels = seq(0, 6, by = 1)) +
  facet_wrap(~category,
             scale = "free",
             ncol = 1)

ggsave(paste0("plots/dot-plot-", str_to_lower(community), ".pdf"),
       device = cairo_pdf,
       width = 7.75,
       height = 9)
}


dk_make_save_cgm_plot("IV")
dk_make_save_cgm_plot("Siuslaw")
dk_make_save_cgm_plot("Applegate")

