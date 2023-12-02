
library(lme4)

dataset <- read.csv("prochazka2022/dataset.csv")

fit1 <- t.test(task_intense ~ condition, data = dataset, var.equal = TRUE)
fit2 <- lm(cooperation ~ condition, data = dataset)
fit3 <- lmer(cooperation ~ (1 | groupnr),
             data = dataset, REML = FALSE)
fit4 <- lmer(cooperation ~ condition + (1 | groupnr),
             data = dataset, REML = FALSE)

