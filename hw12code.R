library(tidyverse)
library(VGAM)
library(xtable)
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


for (i in 1:n.sim) {
  x <- rlaplace(30, location = 0, scale = 4)
  
  # interim look
  t20 <- mean(x[1:20]) / (sd(x[1:20]) / sqrt(20))
  sim.tbl$early_reject[i] <- (t20 > t20.crit)
  
  if (sim.tbl$early_reject[i]) {
    # if rejected early, overall is also TRUE
    sim.tbl$overall[i] <- TRUE
  } else {
    # final look
    t30 <- mean(x) / (sd(x) / sqrt(30))
    sim.tbl$overall[i] <- (t30 > t30.crit)
  }
}


sim.tbl.rates <- sim.tbl |>
  mutate(late_reject = !early_reject & overall)|>
  summarize(
    early_rate   = mean(early_reject),
    late_rate    = mean(late_reject),
    overall_rate = mean(overall)
  )

####Question 2######
n_sim      <- 10000
n          <- 15
df         <- n - 1

tcrit_left <- qt(alpha, df=df)

tcrit_right <- qt(1 - alpha, df = df)  
# two‐sided critical value
tcrit_2s   <- qt(1 - alpha/2, df = df)

params <- list(
  Beta_10_2  = list(a=10, b=2,  mu=10/12),
  Beta_2_10  = list(a=2,  b=10, mu=2/12),
  Beta_10_10 = list(a=10, b=10,mu=0.5)
)

####Rejection rate when using a left tailed test
type1_rates_left <- sapply(params, function(p) {
  rejections <- replicate(n_sim, {
    x     <- rbeta(n, p$a, p$b)
    tstat <- (mean(x) - p$mu) / (sd(x) / sqrt(n))
    tstat < tcrit_left
  })
  mean(rejections)
})

type1_rates_right <- sapply(params, function(p) {
  rejections <- replicate(n_sim, {
    x     <- rbeta(n, p$a, p$b)
    tstat <- (mean(x) - p$mu) / (sd(x) / sqrt(n))
    # reject if t-statistic is too large
    tstat > tcrit_right
  })
  mean(rejections)
})

type1_rates_two <- sapply(params, function(p) {
  mean(replicate(n_sim, {
    x     <- rbeta(n, p$a, p$b)
    tstat <- (mean(x) - p$mu) / (sd(x) / sqrt(n))
    (tstat < -tcrit_2s) | (tstat > +tcrit_2s)
  }))
})

#2d

distribution_type <- c("Beta(10,2)", "Beta(2,10)", "Beta(10,10)")

summary_df <- data.frame(
  Distribution = distribution_type,
  Left_Tailed  = type1_rates_left,
  Right_Tailed = type1_rates_right,
  Two_Tailed   = type1_rates_two,
  check.names  = FALSE
)

xt <- xtable(
  summary_df,
  caption = "Empirical Type I Error Rates by Test Type and Distribution",
  label   = "tab:type1"
)

print(
  xt,
  type             = "latex",
  include.rownames = FALSE,
  digits           = c(0, 0, 3, 3, 3)  # no decimals for col 1; three for the rates
)

##The relationship between the skewness of each beta distribution is that the error for each distribution is the smallest for the test which holds the same direction as the skewness of the distribution, with the proportions of type 1 errors increasing as tests begin to deviate from the same direction as the beta distribution's particular skewness.
