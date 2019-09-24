# David Gyllenberg

# Packages ----------------------------------------------------------------

# Install pacman for easy loading of packages
suppressMessages(
  if(!require(pacman)) {install.packages("pacman"); library(pacman)}
)

# Load packages
p_load(
  tidyverse, 
  pxweb
)

Sys.setlocale(locale="UTF-8")


# URLs --------------------------------------------------------------------

# Population structure
url_p <- 
  "http://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/vrm/vaerak/statfin_vaerak_pxt_11rc.px"

url_b <- 
  "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/synt/statfin_synt_pxt_003.px"


# Download data -----------------------------------------------------------

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
  as.tibble()

b <- 
  b_orig %>% 
  filter(
    Tapahtumakuukausi == "Kuukaudet yhteensä"
  ) %>% 
  transmute(
    Year = Vuosi %>% as.character() %>% as.numeric(), 
    N = `Elävänä syntyneet`
  ) 


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
    caption = "Data: http://pxnet2.stat.fi/PXWeb/pxweb/en/\nCode: https://github.com/davgyl/pxweb", 
    x = "Birth year", 
    y = "Cumulative number of subjects") + 
  theme_minimal()
