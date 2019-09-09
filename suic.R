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

s_orig <- 
  pxweb_get_data(
    url = url_s,
    query = 
      list(Ikä = c('*'),
           Vuosi = c('*'),
           Sukupuoli = c('*'),
           Tiedot = c('ksyylkm6')
      )
  ) %>% 
  as.tibble()

p_orig <- 
  pxweb_get_data(
    url = url_p,
    query = 
      list(
        Vuosi = c('*'),
        Sukupuoli = c('*'),
        Ikä = c('*'),
        Tiedot = c('*')
      )
  ) %>% 
  as.tibble()


# Wrangle data ------------------------------------------------------------

s <- s_orig %>% 
  transmute(
    Age = Age %>% as.character(),
    Year = Year %>% as.character() %>% as.numeric(), 
    Year_cat = cut_width(
      Year, 5, boundary = 1920, closed = "left"
      ) %>% as.character(), 
    Sex = Gender %>% as.character(),
    Suicides
  ) %>% 
  filter(
    !Age == "Total"
  ) %>% 
  group_by(
    Age, Year_cat, Sex
  ) %>% 
  mutate(
    Suicides_cat = sum(Suicides)
  ) %>% 
  ungroup() %>% 
  distinct()


p <- p_orig %>% 
  transmute(
    Age = Age %>% as.character(),
    Year = 
      Year %>% 
      as.character() %>% 
      as.numeric() + 1, # Add one to show population at risk the following year
    Sex = Sex %>% as.character(),
    Population = `Population 31 Dec`
  ) %>% 
  filter(
    Year >= 1921,
    Year <= 2017,
    !Age == "Total"
  ) %>% 
  mutate(
    Year_cat = cut_width(
      Year, 5, boundary = 1920, closed = "left"
      ) %>% as.character()
  ) %>% 
  group_by(Year) %>% 
  mutate(
    Age = ifelse(Age %in% c("- 4", "5 - 9", "10 - 14"), "- 14", Age)
  ) %>% 
  group_by(
    Age, Year, Sex
  ) %>% 
  mutate(
    Population = sum(Population)
  ) %>% 
  ungroup() %>% 
  group_by(
    Age, Year_cat, Sex
  ) %>% 
  mutate(
    Population_cat = sum(Population)
  ) %>% 
  ungroup() %>% 
  distinct()


# Plot --------------------------------------------------------------------

r <- 
  s %>% filter(Year >= 1921) %>% 
  left_join(p %>% filter(Year >= 1921)) %>% 
  mutate(
    Rate = Suicides / Population * 10000
  ) 

r %>% filter(Year > 1981, Sex = "Total") %>% 
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

r %>% filter(Year > 1921, Sex == "Total") %>% 
  ggplot(aes(
    x = Year, 
    y = Rate, 
    fill = Rate
  )) +
  geom_bar(stat = "identity") +
  facet_grid(Age ~ .) +
  scale_fill_gradient(
    low = "lightblue1",
    high = "navy"
  ) + 
  theme_minimal()

r_cat <- 
  s %>% filter(Year >= 1925, Year < 2015) %>% 
  left_join(p %>% filter(Year >= 1925, Year <= 2015)) %>% 
  transmute(
    Age, 
    Year = case_when(
      Year_cat == "[1920,1925)" ~ "1920 - 1924",    
      Year_cat == "[1925,1930)" ~ "1925 - 1929",    
      Year_cat == "[1930,1935)" ~ "1930 - 1934",    
      Year_cat == "[1935,1940)" ~ "1935 - 1939",    
      Year_cat == "[1940,1945)" ~ "1940 - 1944",    
      Year_cat == "[1945,1950)" ~ "1945 - 1949",    
      Year_cat == "[1950,1955)" ~ "1950 - 1954",    
      Year_cat == "[1955,1960)" ~ "1955 - 1959",    
      Year_cat == "[1960,1965)" ~ "1960 - 1964",    
      Year_cat == "[1965,1970)" ~ "1965 - 1969",   
      Year_cat == "[1970,1975)" ~ "1970 - 1974",   
      Year_cat == "[1975,1980)" ~ "1975 - 1979",   
      Year_cat == "[1980,1985)" ~ "1980 - 1984",   
      Year_cat == "[1985,1990)" ~ "1985 - 1989",   
      Year_cat == "[1990,1995)" ~ "1990 - 1994",   
      Year_cat == "[1995,2000)" ~ "1995 - 1999",   
      Year_cat == "[2000,2005)" ~ "2000 - 2004",   
      Year_cat == "[2005,2010)" ~ "2005 - 2009",   
      Year_cat == "[2010,2015)" ~ "2010 - 2014",   
      Year_cat == "[2015,2020]" ~ "2015 - 2019",
      TRUE ~ as.character(Year_cat)
    ),
    Year_cat, 
    Sex, 
    Suicides_cat, 
    Population_cat, 
    Rate = Suicides_cat / Population_cat * 10000
  ) %>% 
  distinct()

# Main figure

r_cat %>% filter(Sex == "Total") %>% 
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
  scale_y_continuous(
    name="Suicide rate (1/10000)", 
    breaks = c(0, 5), 
    limits = c(0, 6)) + 
  labs(title = "Suicide rates in Finland by age groups and calendar year.", 
       caption = "Data: http://pxnet2.stat.fi/PXWeb/pxweb/en/\nCode: https://github.com/davgyl/pxweb/blob/master/suic.R") + 
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0), 
        axis.text.x = element_text(angle=45, hjust=1))

r_cat %>% filter(Sex == "Total", Age != "- 14") %>% 
  ggplot(aes(
    x = Year_cat, 
    y = Rate, 
    fill = Rate
  )) +
  geom_bar(stat = "identity") +
  facet_grid(Age ~ .) +
  scale_fill_gradient(
    low = "lightblue1",
    high = "navy"
  ) + 
  theme_minimal()

# Other figures

r_cat %>% filter(Sex == "Males", Age != "- 14") %>% 
  ggplot(aes(
    x = Year_cat, 
    y = Rate, 
    fill = Rate
  )) +
  geom_bar(stat = "identity") +
  facet_grid(Age ~ .) +
  scale_fill_gradient(
    low = "yellow",
    high = "darkgreen"
  ) + 
  theme_minimal()
