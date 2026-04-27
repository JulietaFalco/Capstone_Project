getwd()
write.csv(data.frame(test = 1), "test_permisos.csv", row.names = FALSE)
cat("✓ Permisos OK - podés escribir en", getwd(), "\n")

library(tidyquant)
library(readxl)
library(dplyr)

# ── Trigo (Pink Sheet) ───────────────────────
url_pink  <- "https://thedocs.worldbank.org/en/doc/18675f1d1639c7a34d463f59263ba0a2-0050012025/related/CMO-Historical-Data-Monthly.xlsx"
dest_pink <- tempfile(fileext = ".xlsx")
download.file(url_pink, dest_pink, mode = "wb")

trigo_raw <- read_excel(dest_pink, sheet = "Monthly Prices", skip = 4)

trigo_fob <- trigo_raw %>%
  select(date = 1, trigo = "Wheat, US HRW") %>%
  filter(!is.na(date), !is.na(trigo)) %>%
  mutate(date  = as.Date(paste0(date, "-01"), format = "%YM%m-%d"),
         trigo = as.numeric(trigo)) %>%
  filter(date >= as.Date("2012-01-01"))

write.csv(trigo_fob, "precio_trigo_usd.csv", row.names = FALSE)
cat("✓ Trigo guardado\n")

# ── AUD/USD ──────────────────────────────────
audusd <- tq_get("AUDUSD=X",
                 from = "2012-01-01",
                 to   = Sys.Date(),
                 get  = "stock.prices") %>%
  select(date, audusd = close)

write.csv(audusd, "audusd.csv", row.names = FALSE)
cat("✓ AUD/USD guardado\n")