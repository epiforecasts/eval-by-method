library(rstanarm)
library(shinystan)

# fit
SingleLevelModel <- stan_glm(score ~ 0 + method_f + target_f,
                             data=m.data)
# default priors
prior_summary(SingleLevelModel)

# adapting priors and sampler settings
SingleLevelModelMod <- stan_glm(valence ~ arousal,
                                data=dat,
                                prior = normal(0,1,autoscale=FALSE),
                                prior_intercept=normal(50,100, autoscale=FALSE),
                                iter=4000,
                                adapt_delta=0.99)

# summary
summarySingleLevelModel <- summary(SingleLevelModel)
print(summarySingleLevelModel)

# trace plot
plot(SingleLevelModel,"trace", pars="target_f2")

# explore posterior
launch_shinystan(SingleLevelModel)

# pp check
pp_check(SingleLevelModel,nreps=100)


# heirarchical -----------------
m.fit.mlm <- stan_glmer(score ~ 0 + method_f + target_f +
                        observed + trend_f + horizon + (1 | model),
                        QR = TRU,
                        algorithm = "meanfield",
                      data=m.data)

detach(rstanarm)
