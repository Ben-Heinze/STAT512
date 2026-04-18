## --------------------------------------------------------------------------------
#| message: false
#| warning: false
options(show.signif.stars = FALSE)
library(Sleuth3)
library(tidyverse)
# rename dataframe 
donner_df <- case2001
glimpse(donner_df) 

# create new variables to match text
df_donner <- donner_df |> 
  mutate(fem = factor(ifelse(Sex == "Female", 1, 0)), 
         surv = factor(ifelse(Status == "Survived", 1, 0)))
# check new vars
glimpse(df_donner)

# fit binary logistic regression model 
fit_donner <- glm(surv ~ Age + fem, 
                  data = df_donner, family = "binomial") 
summary(fit_donner)


## --------------------------------------------------------------------------------
#| eval: false
# # library effects package
# library(effects)
# # "effects" plot on the tranformed scale
# plot(allEffects(fit_donner))
# 
# # on the original scale
# library(ggeffects)
# plot(ggpredict(fit_donner, terms = c("Age", "fem")),
#      show_data = TRUE)
# # using the effects package
# plot(allEffects(fit_donner), type = "response")


## --------------------------------------------------------------------------------
# fit the interaction (full) model
fit_dint <- glm(surv ~ fem*Age, data = df_donner, 
                family = "binomial")
# Wald-based tests are provided in summary output for 
# testing beta_j = 0 for just one beta. Which beta is assoc with the interaction? 
summary(fit_dint)
qnorm(p = 0.975)


## --------------------------------------------------------------------------------
# LRT is
anova(fit_donner, fit_dint)
confint(fit_dint)
exp(confint(fit_dint))

