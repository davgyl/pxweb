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
  "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/synt/statfin_synt_pxt_12dl.px"

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
  as.tibble()


# Tidy data ---------------------------------------------------------------

b <- 
  b_orig %>% 
  filter(
    Tapahtumakuukausi == "Kuukaudet yhteens�"
  ) %>% 
  transmute(
    Year = Vuosi %>% as.character() %>% as.numeric(), 
    N = `El�v�n� syntyneet`
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
