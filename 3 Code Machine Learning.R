# ---- SCF 2022: dipendente + 20 controlli, export Excel ----
# Pacchetti
pkgs <- c("readr", "dplyr", "openxlsx")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
library(readr); library(dplyr); library(openxlsx)

# 1) Scarica lo SCF Extract (CSV) dal sito della Fed
zip_url <- "https://www.federalreserve.gov/econres/files/scfp2022excel.zip"  # CSV extract (ZIP)
zip_path <- tempfile(fileext = ".zip")
download.file(zip_url, destfile = zip_path, mode = "wb", quiet = TRUE)

# 2) Estrai e individua il CSV contenuto nello ZIP
unz_dir <- tempdir()
files <- unzip(zip_path, exdir = unz_dir)
csv_path <- files[grepl("\\.csv$", files, ignore.case = TRUE)]
stopifnot(length(csv_path) == 1)

# 3) Leggi il CSV completo
df <- readr::read_csv(csv_path, show_col_types = FALSE)

# 4) Definisci variabili
#    Dipendente: 1 se detiene qualunque attivo finanziario in equity (azioni, fondi azionari ecc.)
#    Controlli (20): demografia, lavoro, struttura familiare, casa, veicoli, liquidità, pensioni, ecc.
controls <- c(
  "AGE","HHSEX","EDCL","MARRIED","RACECL4","LF","OCCAT1","INDCAT","FAMSTRUCT","HOUSECL",
  "HLIQ","HDEBT","OWN","NOWN","NVEHIC","LIQ","CHECKING","SAVING","HOMEEQ","ANYPEN"
)
needed <- unique(c("EQUITY", controls))
missing <- setdiff(needed, names(df))
if (length(missing)) stop("Mancano queste variabili nel file SCF: ", paste(missing, collapse = ", "))

# 5) Crea Y e tieni solo Y + 20 controlli
out <- df %>%
  mutate(STOCK_PARTICIPATION = as.integer(EQUITY > 0)) %>%
  select(STOCK_PARTICIPATION, dplyr::all_of(controls))

# 6) Esporta
openxlsx::write.xlsx(out, "SCF_2022_stocks_20controls.xlsx", overwrite = TRUE)
readr::write_csv(out, "SCF_2022_stocks_20controls.csv")

cat("Pronto:\n - SCF_2022_stocks_20controls.xlsx\n - SCF_2022_stocks_20controls.csv\n")
