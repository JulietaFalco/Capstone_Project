library(readxl)
library(dplyr)
library(ggplot2)

# URL actualizada (2025)
url <- "https://thedocs.worldbank.org/en/doc/18675f1d1639c7a34d463f59263ba0a2-0050012025/related/CMO-Historical-Data-Monthly.xlsx"

destfile <- "pink_sheet.xlsx"
download.file(url, destfile, mode = "wb")

# Leer y limpiar
trigo_raw <- read_excel(destfile, 
                        sheet = "Monthly Prices",
                        skip  = 4)

trigo_fob <- trigo_raw %>%
  select(date = 1, trigo = "Wheat, US HRW") %>%
  filter(!is.na(date), !is.na(trigo)) %>%
  mutate(date  = as.Date(paste0(date, "-01"), format = "%YM%m-%d"),
         trigo = as.numeric(trigo)) %>%
  filter(date >= as.Date("2012-01-01"))

ggplot(trigo_fob, aes(x = date, y = trigo)) +
  geom_line(color = "goldenrod3", linewidth = 1) +
  geom_hline(yintercept = mean(trigo_fob$trigo, na.rm = TRUE),
             linetype = "dashed", color = "gray50") +
  labs(
    title    = "Precio FOB del Trigo (US HRW - Golfo de México)",
    subtitle = "Fuente: World Bank Pink Sheet",
    x        = "Fecha",
    y        = "USD por tonelada métrica"
  ) +
  theme_minimal()

write.csv(trigo_fob, "precio_fob_trigo_2012.csv", row.names = FALSE)

install.packages("tidyquant")
library(tidyquant)


# Instalar si no están
if (!requireNamespace("readxl",    quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("dplyr",     quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tidyquant", quietly = TRUE)) install.packages("tidyquant")
if (!requireNamespace("ggplot2",   quietly = TRUE)) install.packages("ggplot2")

library(readxl)
library(dplyr)
library(ggplot2)


library(tidyquant)

audusd <- tq_get("AUDUSD=X",
                 from = "2012-01-01",
                 to   = Sys.Date(),
                 get  = "stock.prices")

head(audusd)
write.csv(audusd, "audusd.csv", row.names = FALSE)

getwd()
