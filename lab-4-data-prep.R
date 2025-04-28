# data-prep.R

library(readxl)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Load and clean your data
wgm_raw <- read_excel("wgm2018-dataset-crosstabs-all-countries_2.xlsx", skip = 2)
wgm_clean <- wgm_raw %>% fill(Question)

# Region mapping
region_map <- list(
  "Asia" = c("Afghanistan", "Bangladesh", "India", "Iran", "Nepal", "Pakistan", "Sri Lanka",
             "Cambodia", "Indonesia", "Laos", "Malaysia", "Myanmar", "Philippines", "Singapore",
             "Thailand", "Vietnam", "China", "Japan", "Mongolia", "South Korea", "Taiwan"),
  "Middle East and North Africa" = c("Algeria", "Egypt", "Libya", "Morocco", "Tunisia", "Iraq",
                                     "Israel", "Jordan", "Kuwait", "Lebanon", "Palestinian Territories", "Saudi Arabia",
                                     "Turkey", "United Arab Emirates", "Yemen"),
  "Sub-Saharan Africa" = c("Burundi", "Comoros", "Ethiopia", "Kenya", "Madagascar", "Malawi",
                           "Mauritius", "Mozambique", "Rwanda", "Tanzania", "Uganda", "Zambia", "Zimbabwe",
                           "Benin", "Burkina Faso", "Ghana", "Guinea", "Ivory Coast", "Liberia", "Mali",
                           "Mauritania", "Niger", "Nigeria", "Senegal", "Sierra Leone", "The Gambia", "Togo",
                           "Botswana", "Namibia", "South Africa", "Eswatini", "Cameroon", "Chad",
                           "Republic of the Congo", "Gabon"),
  "Americas" = c("Costa Rica", "Dominican Republic", "El Salvador", "Guatemala", "Haiti",
                 "Honduras", "Mexico", "Nicaragua", "Panama", "Argentina", "Bolivia", "Brazil",
                 "Chile", "Colombia", "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela",
                 "Canada", "United States"),
  "Europe" = c("Denmark", "Estonia", "Finland", "Iceland", "Ireland", "Latvia", "Lithuania",
               "Norway", "Sweden", "United Kingdom", "Albania", "Bosnia and Herzegovina",
               "Croatia", "Cyprus", "Greece", "Italy", "Malta", "North Macedonia", "Montenegro",
               "Portugal", "Serbia", "Slovenia", "Spain", "Austria", "Belgium", "France",
               "Germany", "Luxembourg", "Netherlands", "Switzerland"),
  "Former Soviet Union" = c("Armenia", "Azerbaijan", "Georgia", "Kazakhstan", "Kyrgyzstan",
                            "Tajikistan", "Turkmenistan", "Uzbekistan", "Belarus", "Bulgaria",
                            "Czech Republic", "Hungary", "Moldova", "Poland", "Romania", "Russia",
                            "Slovakia", "Ukraine")
)

# Belief that Vaccines are Safe
wgm_agree <- wgm_clean %>%
  filter(
    Question == "Q25 Do you strongly or somewhat agree, strongly or somewhat disagree or neither agree nor disagree with the following statement? Vaccines are safe.",
    Response %in% c("Strongly agree", "Somewhat agree")
  ) %>%
  group_by(Country) %>%
  summarise(percent_agree = sum(`Column N %...4`, na.rm = TRUE)) %>%
  arrange(desc(percent_agree))

country_to_region <- unlist(lapply(names(region_map), function(region) {
  setNames(rep(region, length(region_map[[region]])), region_map[[region]])
}))

wgm_agree <- wgm_agree %>%
  mutate(Region = country_to_region[Country],
         Region = ifelse(is.na(Region), "Other/Unclassified", Region))

# Table Data
q19_overall <- wgm_clean %>%
  filter(Question == "Q19 Overall, do you think that science and technology will increase or decrease the number of jobs in your local area in the next five years?") %>%
  select(Country, Response, OverallPercent = `Column N %...4`) %>%
  distinct() %>%
  pivot_wider(names_from = Response, values_from = OverallPercent)

world <- ne_countries(scale = "medium", returnclass = "sf") %>% st_centroid()
centroids <- world %>% select(Country = name_long, geometry) %>% st_coordinates() %>% as.data.frame() %>% bind_cols(Country = world$name_long)

q19_final <- q19_overall %>%
  select(Country, Increase, Decrease) %>%
  mutate(Total = Increase - Decrease)

q19_geo <- q19_final %>%
  left_join(centroids, by = "Country") %>%
  filter(!is.na(X) & !is.na(Y))

datatable_with_region <- q19_geo %>%
  mutate(
    Region = country_to_region[Country],
    Region = ifelse(is.na(Region), "Other/Unclassified", Region),
    Net = Increase - Decrease
  ) %>%
  filter(!is.na(Increase), !is.na(Decrease), !is.na(Net)) %>%
  select(Country, Region, Increase, Decrease, Net)

# Save files
write_csv(wgm_agree, "wgm_agree_clean.csv")
write_csv(datatable_with_region, "datatable_with_region_clean.csv")
