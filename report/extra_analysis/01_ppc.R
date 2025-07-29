library(brms)
fit <-  brm(count ~ zAge + zBase * Trt
            + (1|patient) + (1|obs),
            data = epilepsy, family = poisson())


pp_check(fit)  # shows dens_overlay plot by default
pp_check(fit, type = "error_hist", ndraws = 11)
pp_check(fit, type = "scatter_avg", ndraws = 100)
pp_check(fit, type = "stat_2d")
pp_check(fit, type = "rootogram")
pp_check(fit, type = "loo_pit")

## get an overview of all valid types
pp_check(fit, type = "xyz")