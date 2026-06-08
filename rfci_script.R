if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

if (!require("dplyr", quietly = TRUE))
  install.packages("dplyr")

if (!require("graph", quietly = TRUE))
  BiocManager::install("graph")

if (!require("RBGL", quietly = TRUE))
  BiocManager::install("RBGL")

if (!require("Rgraphviz", quietly = TRUE))
  BiocManager::install("Rgraphviz")

if (!require("pcalg", quietly = TRUE))
  install.packages("pcalg")

library(dplyr)
library(graph)
library(RBGL)
library(Rgraphviz)
library(pcalg)


# ── 1. Load & clean data ──────────────────────────────────────────────────────
data_complete <- read.csv("../566 Project Data/crashes2012to24.csv")

data_complete <- data_complete %>%
  select(-X, -ST_CASE, -VEH_NO, -PER_NO, -CrashVehicle, -M_HARM, -YEAR) %>%
  mutate(across(where(is.double), ~ round(.x, 0)))

# data_complete <- data_complete[sample(nrow(data_complete), 100000), ]  # for testing

# ── 2. Encode as integers ─────────────────────────────────────────────────────
data_complete <- as.data.frame(lapply(data_complete, as.factor))
data_int      <- as.data.frame(lapply(data_complete, as.integer))

# ── 3. Diagnostics ────────────────────────────────────────────────────────────
nlev_check <- sapply(data_int, function(x) length(unique(x)))
sort(nlev_check, decreasing = TRUE)
prod(nlev_check)

# ── 4. Build suffStat ─────────────────────────────────────────────────────────
dm       <- data.matrix(data_int) - 1L
nlev     <- sapply(data_complete, nlevels)
suffStat <- list(dm = dm, nlev = nlev, adaptDF = FALSE)

# ── 5. Run RFCI ───────────────────────────────────────────────────────────────
set.seed(42)
rfci_fit <- rfci(
  suffStat    = suffStat,
  indepTest   = disCItest,
  labels      = colnames(data_int),
  alpha       = 0.05,
  verbose     = TRUE,
  m.max       = 3L,
  skel.method = "stable.fast",
  numCores    = parallel::detectCores() - 2L
)

saveRDS(rfci_fit, file = "rfci_fit_1mil_2012_24.rds")