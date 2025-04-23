library(tidyverse)
library(VGAM)

#1a and b
# one‐sided 5% level
alpha = 0.05
t20.crit <- qt(1-alpha, df = 20 - 1)   # df = 19
t30.crit <- qt(1-alpha, df = 30 - 1)   # df = 29
  
n.sim = 10000
#1c

sim.tbl <- tibble(
  early_reject = logical(n.sim),
  overall      = logical(n.sim)
)


for (i in n.sim) {
  x <- rlaplace(30, location = 0, scale = 4)
  
  # interim look
  t20 <- mean(x[1:20]) / (sd(x[1:20]) / sqrt(20))
  sim.tbl$early_reject[i] <- (t20 > t20.crit)
  
  if (sim.tbl$early_reject[i]) {
    # if rejected early, overall is also TRUE
    sim_tbl$overall[i] <- TRUE
  } else {
    # final look
    t30 <- mean(x) / (sd(x) / sqrt(30))
    sim.tbl$overall[i] <- (t30 > t30.crit)
  }
}


sim.tbl <- sim.tbl |>
  mutate(late_reject = !early_reject & overall)

sim.stats.tibble <- sim.tbl |>
mutate(late_reject <- as.numeric(late_reject),
       early_reject <- as.numeric(early_reject),
       overall <- as.numeric(overall))
  summarize(
    early_rate   = mean(early_reject),
    late_rate    = mean(late_reject),
    overall_rate = mean(overall)
  )
