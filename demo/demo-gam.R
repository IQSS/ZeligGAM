HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- lavaan(HS.model, data=HolzingerSwineford1939,
              auto.var=TRUE, auto.fix.first=TRUE,
              auto.cov.lv.x=TRUE)
summary(fit, fit.measures=TRUE)

semPaths(fit)


HS.model2 <- ' visual  =~ x1 + x2
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit2 <- lavaan(HS.model2, data=HolzingerSwineford1939,
              auto.var=TRUE, auto.fix.first=TRUE,
              auto.cov.lv.x=TRUE)
summary(fit2, fit.measures=TRUE)
semPaths(fit2)

anova(fit, fit2)

z5 <- zsem$new()
z5
z5$zelig(formula = HS.model, data = HolzingerSwineford1939,
         auto.var = TRUE, auto.fix.first = TRUE,
         auto.cov.lv.x = TRUE)


summary(z5$zelig.out$z.out[[1]])
# z5$path()

semPaths(z5$zelig.out$z.out[[1]], "std", curvePivot = TRUE)
z.out <- z5$zelig.out$z.out[[1]]
z5$num <- 10 # Number of simulations
z5$param(z5$zelig.out$z.out[[1]])

z5$set()

