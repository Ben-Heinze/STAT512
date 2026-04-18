## ----q1, message = FALSE, warning = FALSE-----------------------------------------------------------------
options(show.signif.stars = FALSE) 
library(tidyverse)
library(Sleuth3)
sat <- case1201
# ? case1201 and see text for more descriptions of variables
glimpse(sat)



## ----q2, message = FALSE, warning = FALSE-----------------------------------------------------------------
library(GGally)
library(Sleuth3)
sat <- case1201
ggpairs(sat[,c(8:2)])


## ----q3, fig.width=8, fig.height=3.5, out.width="0.85\\linewidth", warning = FALSE, message = FALSE-------
# note you may need to install patchwork 
# install.packages("patchwork")
library(patchwork)
df_sat <- sat |> 
  mutate(ltakers = log(Takers))

# create scatterplot for SAT vs. ltakers 
p1 <- ggplot(df_sat, aes(x = ltakers, y = SAT)) + 
  geom_point() +
  theme_bw()

# create scatterplot for expenditures with labels as state names
p2 <- ggplot(df_sat, 
             aes(x = Expend, y = SAT, label = State)) +
  geom_smooth(method = "lm") + 
  geom_label(alpha = 0.5)  +
  theme_bw()

# does the relationship between log(takers) and SAT appear more linear? 
# does Alaska have high leverage? large residual? 
# Do you think it will have high Cook's d? 
p1 + p2


## ----q4, fig.width=10, fig.height=7, out.width="0.75\\linewidth"------------------------------------------
# compare residual plots with AK
sat_all <- lm(SAT ~ log(Takers) + Rank + Income +
            Years + Public + Expend , 
          data = sat)
# without AK
sat_red <- lm(SAT ~ log(Takers) + Rank + Income +
            Years + Public + Expend, 
          data = sat[-29, ])
# plot all in one window
par(mfrow = c(2,4))
plot(sat_all)
plot(sat_red)


## ----q5---------------------------------------------------------------------------------------------------
# forward selection approach, want to consider all but 
# two vars we want in model
m1 <- lm(SAT ~ log(Takers) + Rank, 
           data = sat[-29,])
# Mcand1
mcand1 <- lm(SAT ~ log(Takers) + Rank + Expend, 
           data = sat[-29,])
anova(m1, mcand1)

# Mcand2
mcand2 <- lm(SAT ~ log(Takers) + Rank + Income, 
           data = sat[-29,])

anova(m1, mcand2)


mcand3 <- lm(SAT ~ log(Takers) + Rank + Years, 
             data = sat[-29,])
anova(m1, mcand3)

mcand4 <- lm(SAT ~ log(Takers) + Rank + Public, 
             data = sat[-29,])
anova(m1, mcand4)


# Mcand5
mcand5 <- lm(SAT ~ log(Takers) + Rank + Expend + Public, 
             data = sat[-29,])
anova(mcand1, mcand5)

# Mcand6
mcand6 <- lm(SAT ~ log(Takers) + Rank + Expend + Years, 
             data = sat[-29,])
anova(mcand1, mcand6)

## ----q6, eval = FALSE-------------------------------------------------------------------------------------
# fit_forward <- step(lm(SAT ~ log(Takers) + Rank,
#                        data = sat[-29,]),
#                     scope = SAT ~ log(Takers) +
#                       Income + Years + Public +
#                       Expend + Rank,
#                     direction = "forward")
# summary(fit_forward)


# ----q7a, eval = FALSE------------------------------------------------------------------------------------
 fit_backward <- step(lm(SAT ~ log(Takers) + Income + Years +
                                  Public + Expend + Rank,
                         data = sat), direction = "backward")
 
 summary(fit_backward)

## ----q7b, eval = FALSE------------------------------------------------------------------------------------
 fit_step <- step(lm(SAT ~ log(Takers) + Rank,
                     data = sat),
                  scope = SAT ~ (log(Takers) +
                                   Income + Years +
                                   Public + Expend + Rank),
                  direction = "both")
 summary(fit_step)


## ----all-subsets------------------------------------------------------------------------------------------
# make dataset that removes alaska 
sat2 <- df_sat[-29, ]
full_model <- lm(SAT ~ ltakers + Income + Years + 
                   Public + Expend + Rank, 
                 data = sat2)
summary(full_model)


## ----mumin, cache=TRUE------------------------------------------------------------------------------------
# load MuMIn
library(MuMIn) 
# ensure all models use the same data 
# need to run to use dredge function
options(na.action = "na.fail") 

# defaults to AICc for criterion, set to BIC
allSubsets_sat <- dredge(full_model, rank = "BIC") 

# number of rows is = # models in model set
dim(allSubsets_sat) 

# look at top set of models with diff in BIC < 6 from "top" model
subset(allSubsets_sat, delta <6)


## ----mumin-plot, fig.width = 10, fig.height=10, out.width = "0.59\\linewidth"-----------------------------
# visualize same summary as above
par(mar = c(3,5,6,4))
plot(subset(allSubsets_sat,delta < 6), 
     labAsExpr = TRUE)

