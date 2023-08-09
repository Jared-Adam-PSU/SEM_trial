

# 8/9/2023

#following along with the video "Statistical Methods Series: Structural Equation Modeling"

# structural equation modeling 

# how does resident diversity influence ecosystem function? 
# resources > diversity > function 
# can fit model, but potentially indirect pathway between resources and diversity that is 
  # mediated by diversity 
# can we disentangle the direct and indirect effects? 
# what about things that are intractable to recreate? e.g., abiotic 

# structural : 
  # there is some underlying cause and effect in nature. WE are just observers
  # we think we know how they work, so we can create hypotheses for this

# equation :
  # can sep different paths into different equations and fit to data to support or refute the structure 

# modeling : 
  # can be modeled 

#SEM: series of equations united in a single causal network 
  # causality? ? ? ? 
  # we are implicitly assuming that there are no simple associations 
  # if we tweak resources, there would be an impact on diversity 

# correlation DOES imply causation 
  # a simply correlation implies an unresolved causal structure, since we cannot know which is cause, which is effect,
    # of even if both are common effects of some third, unmeasurable variable

# current SEM with piecewiseSEM package
# = graph theory 
# local estimation of each equation 
  # this increases flexibility of the model
  # each model can be vastly different 
# increased flexibility to accommodate messy ecology data

# Begin ####

# Starting the coding along here

# need devtools to get this package from github 

install.packages("devtools")
library(devtools)

#loading this package 
devtools::install_github("jslefche/piecewiseSEM@devel") # version 2.3.0
library(piecewiseSEM)

# load in df
data(keeley)

head(keeley)

# fit the individual structural models with tools in base R
abiotic <- lm(abiotic ~ distance, keeley)
hetero <- lm(hetero ~ distance, keeley)
richness <- lm(rich ~ abiotic + hetero, keeley)

# now, add them all to a single SEM with psem function 
model <- psem(abiotic, hetero, richness)

# looking at the model now
  # output will show us the structural equations that we just made and a snippet of the data 
model

# psem can ALSO accept the models within  
  # for example 
model <- psem(
  lm(abiotic ~ distance, keeley),
  lm(hetero ~ distance, keeley),
  lm(rich ~ abiotic + hetero, keeley)
  #data = keeley
  #psem sometimes needs the specific df 
)
model


# NOW evaluate the fit 
  # missing two paths
    # abiotic and hetero 
    # distance and richness

# fit independence claims

# need to add co-variates because seeking independence based on the relationship
# we are only including immediate predecessor co-variate
dsep1 <- lm(abiotic ~ hetero + distance, keeley)
dsep2 <- lm(rich ~ distance + abiotic + hetero, keeley)

# summary time 
summary(dsep1)
# no sig here with abiotic
  # p-value of 0.187 (not stasticially different from 0)

summary(dsep2)
# given known influences, there is sig relationship between distance and richness
  # p-value of 9.56e-05

# let's check the model quality with fisher C test 
fisherC(model)
# here, a low p-value means our model is not explaining well
# BUT we know where issue potentially is based on our d-sep tests (dsep2)

# what now? Model restructure time
model2 <- psem(
  lm(abiotic ~ distance, keeley),
  lm(hetero ~ distance, keeley),
  lm(rich ~ abiotic + hetero + distance, keeley),
  data = keeley
)
fisherC(model2)
#p-value now 0.187
  # model not sig diff from the relationships implied by the data

# let's now make inferences 
coefs(model2)
# elinear stimates here are the betas 
  # por ejemplo, for every one increase in distance we expect 0.3998 in the index oc abiotic env
# standardized estimate: takes raw coefficient and scales it to the variates 
  # row one would be distance and the abiotic index 
  # this is a unitless (in standard deviations of the mean)!
    # means we can compare magnitude of standardized estimates of things that may be in vastly different units 
      # can multiply these values as we work through flow of the framework 

#rudimentary plot! 
# IRL, PowerPoint is probably better
plot(model2)

# let's now do all of those steps at once
summary(model2)
# has chi-squared and fisher's c for goodness of fit
# fisher p-value = the direct sep bc only one test of independence 
# also get r squared for each variable



