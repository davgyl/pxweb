# David Gyllenberg

# Packages ----------------------------------------------------------------

# Install pacman for easy loading of packages
suppressMessages(
  if(!require(pacman)) {install.packages("pacman"); library(pacman)}
)

# Load packages
p_load(
  tidyverse, 
  pxweb, 
  openxslx
)

Sys.setlocale(locale="UTF-8")

# URLs --------------------------------------------------------------------

# Number of born alive

# url_b <- 
  # "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/synt/statfin_synt_pxt_003.px"

url_b <- 
  "https://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/synt/statfin_synt_pxt_12dl.px"

# Population according to age (1-year) and sex by area, 1972-2019

url_census <- 
  "https://pxnet2.stat.fi:443/PXWeb/api/v1/en/StatFin/vrm/vaerak/statfin_vaerak_pxt_11re.px"

# Population projection 2019: Population according to age and sex by area, 2019-2040

url_proj <- 
  "https://pxnet2.stat.fi:443/PXWeb/api/v1/en/StatFin/vrm/vaenn/statfin_vaenn_pxt_128v.px"


# Query -------------------------------------------------------------------

cens_query <- file.path(system.file(package = "pxweb"), 
                        "extdata", "examples", 
                        "/Users/David/Documents/git/pxweb/cens_query.json")
  
# Download data -----------------------------------------------------------

b_orig <- 
  pxweb_get_data(
    url = url_b,
    query = 
      list(
        Vuosi = c('*'),
        Tapahtumakuukausi = c('*'),
        Tiedot = c('*')
      )
  ) %>% 
  as_tibble()

cens_orig <- 
  pxweb_get_data(
    url = url_census,
    query = 
      list(
        Tiedot = c('*'),
        Alue = c('*'),
        Ikä = c('*'),
        Sukupuoli = c('*'),
        Vuosi = c('*')
      )
  ) %>% 
  as_tibble()

proj_orig <- 
  pxweb_get_data(
    url = url_proj,
    query = 
      list(
        Tiedot = c('*'),
        Alue = c('*'),
        Ikä = c('*'),
        Sukupuoli = c('*'),
        Vuosi = c('*')
      )
  ) %>% 
  as_tibble()

# Tidy data ---------------------------------------------------------------

b <- 
  b_orig %>% 
  filter(
    Tapahtumakuukausi == "Kuukaudet yhteensä"
  ) %>% 
  transmute(
    Year = Vuosi %>% as.character() %>% as.numeric(), 
    N = `Elävänä syntyneet`
  ) 

b %>% 
  filter(Year >= 1980, Year <= 2025) %>% 
  mutate(
  cum_N = cumsum(N)) %>% 
  tail()

b_est_2021_2025 <- 
  bind_rows(
  b %>% 
    filter(Year >= 1980, Year <= 2020), 
  tibble(
    Year = 2021:2025, 
    N = b %>% filter(Year == 2020) %>% pull(N) %>% rep(5)
  )
)

b_est_2021_2025 %>% 
  arrange(Year) %>% 
  mutate(
    cum_N = cumsum(N)
  ) %>% tail()

# Plot --------------------------------------------------------------------

b %>% 
  filter(Year >= 1987, Year <= 2003) %>% 
  arrange(Year) %>% 
  mutate(
    cum_N = cumsum(N)
  ) %>% 
  ggplot(aes(x = Year, y = cum_N)) +
  geom_bar(stat = "identity") + 
  labs(
    # title = "...", 
    caption = "Data: http://pxnet2.stat.fi/PXWeb/pxweb/en/\nCode: https://github.com/davgyl/pxweb/blob/master/population.R", 
    x = "Birth year", 
    y = "Cumulative number of subjects") + 
  theme_minimal()

b_est_2021_2025 %>% 
  arrange(Year) %>% 
  mutate(
    cum_N = cumsum(N)
  ) %>% 
  ggplot(aes(x = Year, y = cum_N)) +
  geom_bar(stat = "identity") + 
  labs(
    # title = "...", 
    caption = "Data: http://pxnet2.stat.fi/PXWeb/pxweb/en/\nCode: https://github.com/davgyl/pxweb/blob/master/population.R", 
    x = "Birth year", 
    y = "Cumulative number of subjects") + 
  theme_minimal()


# Table -------------------------------------------------------------------

openxlsx::write.xlsx(
  b %>% 
    filter(Year >= 1987) %>% 
    mutate(cum = cumsum(N)), 
  "born_alive.xlsx"
)


# Save data ---------------------------------------------------------------

saveRDS(cens_orig, file = "cens_orig.rds")

# Test local download -----------------------------------------------------

cens_orig_local <- read_rds("/Users/David/Documents/git/pxweb_data/cens_orig.rds")

cens_orig %>% count(Area)
cens_orig_local %>% count(Area) # Correct UTF-8

