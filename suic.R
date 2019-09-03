# David Gyllenberg

# Packages ----------------------------------------------------------------

# Install pacman for easy loading of packages
suppressMessages(
  if(!require(pacman)) install.packages("pacman")
)

# Load packages
p_load(
  tidyverse, 
  pxweb
)

Sys.setlocale(locale="UTF-8")


# URLs --------------------------------------------------------------------

# Specific table of suicides
url_s <- 
  "http://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/ter/ksyyt/statfin_ksyyt_pxt_11by.px"

# Population structure
url_p <- 
  "http://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/vrm/vaerak/statfin_vaerak_pxt_11rc.px"

# Causes of death (not needed)
# url_d <- 
  # "http://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/ter/ksyyt/statfin_ksyyt_pxt_11bs.px"

# Download data -----------------------------------------------------------

s <- 
  pxweb_get_data(
    url = url_s,
    query = 
      list(Ik� = c('*'),
           Vuosi = c('*'),
           Sukupuoli = c('SSS'),
           Tiedot = c('ksyylkm6')
      )
  ) %>% 
  as.tibble()

p <- 
  pxweb_get_data(
    url = url_p,
    query = 
      list(
        Vuosi = c('*'),
        Sukupuoli = c('SSS'),
        Ik� = c('*'),
        Tiedot = c('*')
      )
  ) %>% 
  as.tibble()


# Wrangle data ------------------------------------------------------------

s <- s %>% 
  transmute(
    Age = Age %>% as.character(),
    Year = Year %>% as.character() %>% as.numeric(), 
    Suicides
  ) %>% 
  filter(
    Year >= 1981, 
    !Age == "Total"
  )


p <- p %>% 
  transmute(
    Age = Age %>% as.character(),
    Year = 
      Year %>% 
      as.character() %>% 
      as.numeric() + 1, # Add one to show population at risk the following year
    Population = `Population 31 Dec`
  ) %>% 
  filter(
    Year >= 1981, 
    Year <= 2017,
    !Age == "Total"
  ) %>% 
  group_by(Year) %>% 
  mutate(
    Age = ifelse(Age %in% c("- 4", "5 - 9", "10 - 14"), "- 14", Age)
  ) %>% 
  group_by(
    Age, Year
  ) %>% 
  mutate(
    Population = sum(Population)
  ) %>% 
  ungroup() %>% 
  distinct()


# Plot --------------------------------------------------------------------

r <- 
  s %>% 
  left_join(p) %>% 
  mutate(
    Rate = Suicides / Population * 10000
  ) 

r %>% 
  ggplot(aes(
    x = Age, 
    y = Rate, 
    fill = Rate
  )) +
  geom_bar(stat = "identity") +
  facet_grid(Year ~ .) +
  scale_fill_gradient(
    low = "lightblue1",
    high = "navy"
  ) + 
  theme_minimal()
