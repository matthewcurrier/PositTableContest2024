library(readxl)
library(here)
library(stringr)
library(tidyr)
library(dplyr)
library(janitor)
library(gt)
library(gtExtras)
library(countrycode)

cc <- countrycode::codelist_panel |> # To help get a short code for each country in dataset
  select(
    country_name = country.name.en,
    iso2c
  ) |>
  unique()

pss <- read_excel(
  here("data", "pss.xlsx"),
  sheet = "data"
) |>
  clean_names() |>
  mutate(country = if_else(country == "UK", "GB", country),
         month = format(date, '%m'),
         month_abbr = tolower(format(date, '%b'))) |>
  left_join(cc, by = c("country" = "iso2c"))


pss2 <- pss |>
  select(
    month,
    month_abbr,
    publisher,
    country,
    country_name,
    branding,
    device,
    impr,
    pub_cost,
    clicks
  )

pss3 <- pss2 |>
  group_by(
    month,
    month_abbr,
    publisher,
    country,
    country_name,
    branding,
    device,
  ) |>
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) |>
  mutate(cpc = sum(pub_cost, na.rm = TRUE) / sum(clicks, na.rm = TRUE)) |>
  ungroup()


pss4 <- pss3 |>
  select(
    -impr,
    -pub_cost,
    -clicks,
    -month
  ) |>
  pivot_wider(
    names_from = branding,
    values_from = cpc
  ) |>
  filter(is.na(MB)) |>
  select(
    -MB
  ) |>
  clean_names()


pss5 <- pss4 |>
  pivot_wider(
    names_from = month_abbr,
    values_from = c(b, nb)
  ) |>
  mutate(
    channel_icon = case_when(
      publisher == "Google" ~ "google",
      publisher == "Bing" ~ "microsoft",
      TRUE ~ "question"
    ),
    device_icon = case_when(
      device == "DSKT" ~ "desktop",
      device == "MB" ~ "mobile",
    )
  ) |>
  select(
    country,
    country_name,
    publisher,
    channel_icon,
    device,
    device_icon,
    everything()
  ) |>
  clean_names()

write.csv(pss5, here('data', 'paid_search_cost_per_click.csv'), row.names = FALSE)


