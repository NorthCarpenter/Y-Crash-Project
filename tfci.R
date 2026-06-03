library(causalDisco)

data_complete <- read.csv("data_complete.csv")
data_complete <- as.data.frame(lapply(data_complete, as.factor))  # add this
data_int <- as.data.frame(lapply(data_complete, as.integer))


# ── 1. Build knowledge object with tier structure ─────────────────────────────
know <- knowledge(
  data_complete,
  tier(
    1 ~ AGE + DR_AGE + DR_SEX,
    2 ~ DAY_WEEK + ROUTE + RUR_URB + RELJCT1 +
      LGT_COND + WEATHER + VPAVETYP + VSURCOND + MECISSUE,
    3 ~ DR_DRINK + PREV_ACC + P_CRASH1,
    4 ~ VE_FORMS + M_HARM + VIS,
    5 ~ PED_DEATH
  )
)

# ── 2. Run temporal FCI via disco ─────────────────────────────────────────────

# Flag ALL variables as binary/categorical to avoid spline fitting
binary_vars <- rep(TRUE, ncol(data_int))
names(binary_vars) <- colnames(data_int)

result <- tfci_run(
  data      = data_int,
  knowledge = know,
  alpha     = 0.05,
  test      = reg_test,
  suff_stat = list(data = data_int, binary = binary_vars)
)

# ── 3. Plot the PAG ───────────────────────────────────────────────────────────
pdf("tfci_result.pdf")
plot(result)
dev.off()

saveRDS(result, "tfci_result.rds")