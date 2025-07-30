library(brms)
fit <-  brm(count ~ zAge + zBase * Trt
            + (1|patient) + (1|obs),
            data = epilepsy, family = poisson())


pp_check(fit)  # shows dens_overlay plot by default
