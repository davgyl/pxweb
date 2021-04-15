library(tidyverse)

Sys.setlocale(locale="UTF-8")

# Read data ---------------------------------------------------------------

cens_csv <- read_csv(
  "005_11re_2019.csv", # Add path
  skip = 1 # skip first row
)

# Origin: 
# https://pxnet2.stat.fi/PXWeb/pxweb/en/StatFin/StatFin__vrm__vaerak/statfin_vaerak_pxt_11re.px/

# Tidy data ---------------------------------------------------------------

# Rename

cens_m <- 
  cens_csv %>% 
  select(1:2, starts_with("Males"))

cens_f <- 
  cens_csv %>% 
  select(1:2, starts_with("Females"))

cens_t <- 
  cens_csv %>% 
  select(1:2, starts_with("Total"))

colnames(cens_m) <- 
  c("Area", "Age", 2017:2019)

colnames(cens_f) <- 
  c("Area", "Age", 2017:2019)

colnames(cens_t) <- 
  c("Area", "Age", 2017:2019)

# Make long data and bind rows

cens_tidy <- 
  bind_rows(
    cens_m %>% 
      pivot_longer(3:5, "year") %>% 
      mutate(gender = "Males"),
    cens_f %>% 
      pivot_longer(3:5, "year") %>% 
      mutate(gender = "Females"),
    cens_t %>% 
      pivot_longer(3:5, "year") %>% 
      mutate(gender = "Total")
  )

# Summarise by age group and other stratifiers

cens_sum <- 
  cens_tidy %>% 
  rename(area = Area) %>% 
  mutate(
    year = as.numeric(year), 
    age_group = case_when(
      Age < 12 ~ "0-12", 
      Age >= 12 & Age <= 17 ~ "13-17"
    )
  ) %>% 
  group_by(area, year, gender, age_group) %>% 
  summarise(n_at_risk = sum(value)) %>% 
  ungroup()

cens_sum

# Proportion of whole country

cens_sum <- 
  cens_sum %>% 
  left_join(
    cens_sum %>% 
      filter(area == "WHOLE COUNTRY") %>% 
      select(-area) %>% 
      rename(whole_n_at_risk = n_at_risk)
  ) %>% 
  mutate(
    prop_whole = n_at_risk / whole_n_at_risk 
  )
