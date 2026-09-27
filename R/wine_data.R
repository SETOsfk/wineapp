# Load and clean the 21st-century Bordeaux dataset — R twin of python/wine/data.py.
# Only base R + readr/dplyr/stringr; every rule lives in data/reference/*.csv so both
# languages read the same tables.

if (!isTRUE(l10n_info()[["UTF-8"]])) invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))  # accents in names
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

project_root <- function() {
  here <- normalizePath(".")
  for (i in 1:4) {
    if (file.exists(file.path(here, "data", "reference", "appellations.csv"))) return(here)
    here <- dirname(here)
  }
  stop("Run from the repository (or a sub-folder of it).")
}

ROOT <- project_root()
REF <- file.path(ROOT, "data", "reference")
RAW_DEFAULT <- file.path(ROOT, "data", "raw", "BordeauxWines.csv")
GENERIC <- c("château", "chateau", "clos", "domaine", "vieux", "le", "la", "les", "l'", "de", "du",
             "des", "d'", "cru", "grand", "petit", "enclos", "mas", "vignobles", "cuvée")

fix_encoding <- function(x, table = read_csv(file.path(REF, "encoding_fixes.csv"),
                                             col_types = "cc", na = character())) {
  for (i in seq_len(nrow(table))) x <- str_replace_all(x, fixed(table$broken[i]), table$fixed[i])
  str_squish(x)
}

appellation_regex <- function(app) {
  pats <- unique(app$pattern)
  pats <- pats[order(-nchar(pats))]
  esc <- str_replace_all(pats, "([.\\-\\\\^$|?*+()\\[\\]{}])", "\\\\\\1")
  esc[pats == "Cadillac"] <- paste0(esc[pats == "Cadillac"], "(?! Côtes)")
  paste0("(?<![\\w-])(", paste(esc, collapse = "|"), ")(?![\\w-])")
}

parse_name <- function(name, app, rx) {
  lookup <- app[!duplicated(app$pattern), ]
  loc <- str_locate_all(name, rx)[[1]]
  for (k in seq_len(nrow(loc))) {
    prefix <- str_trim(substr(name, 1, loc[k, 1] - 1))
    tokens <- tolower(str_split(prefix, "\\s+")[[1]])
    tokens <- tokens[tokens != ""]
    if (length(tokens) == 0 || all(tokens %in% GENERIC)) next
    pat <- substr(name, loc[k, 1], loc[k, 2])
    row <- lookup[lookup$pattern == pat, ]
    rest <- str_trim(substr(name, loc[k, 2] + 1, nchar(name)))
    style <- row$style
    grapes <- row$main_grapes
    if (startsWith(rest, "White")) {
      style <- "white"
      rest <- str_trim(substring(rest, 6))
    }
    if (str_detect(name, "\\b(Rosé|Clairet)\\b")) style <- "rosé"
    if (style == "white") grapes <- "Sauvignon Blanc, Sémillon, Muscadelle"
    return(list(producer = prefix, appellation = row$appellation, bank = row$bank,
                style = style, cuvee = rest, main_grapes = grapes))
  }
  list(producer = name, appellation = NA, bank = NA, style = NA, cuvee = "", main_grapes = NA)
}

parse_price <- function(x) suppressWarnings(as.numeric(str_extract(x, "\\d+(\\.\\d+)?")))

load_wines <- function(raw_path = RAW_DEFAULT) {
  raw <- read_csv(raw_path, show_col_types = FALSE, locale = locale(encoding = "UTF-8"),
                  name_repair = "minimal")
  desc <- read_csv(file.path(REF, "descriptors.csv"), show_col_types = FALSE, na = character())
  attr_cols <- names(raw)[-(1:4)]
  stopifnot(identical(attr_cols, desc$attribute))

  app <- read_csv(file.path(REF, "appellations.csv"), show_col_types = FALSE)
  rx <- appellation_regex(app)
  names_fixed <- fix_encoding(raw$Wine)
  uniq <- unique(names_fixed)
  parsed <- lapply(uniq, parse_name, app = app, rx = rx)
  parsed <- bind_rows(lapply(parsed, as_tibble))
  parsed$name <- uniq

  meta <- tibble(wine_id = seq_len(nrow(raw)) - 1L, name = names_fixed, year = as.integer(raw$Year),
                 score = as.integer(raw$Score), price_usd = parse_price(raw$Price)) |>
    left_join(parsed, by = "name") |>
    mutate(group = match(name, unique(name)) - 1L, y = as.integer(score >= 90))

  X <- as.matrix(raw[, attr_cols])
  storage.mode(X) <- "integer"
  list(meta = meta, X = X, descriptors = desc, raw_names = raw$Wine)
}

feature_sets <- function(desc, X) {
  used <- colSums(X) > 0
  list(all = which(used),
       sensory = which(used & desc$family != "descriptive"),
       descriptive = which(used & desc$family == "descriptive"))
}
