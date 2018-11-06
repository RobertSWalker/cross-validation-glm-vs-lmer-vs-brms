library(lme4)
library(brms)
library(rcompanion)
library(tidyverse)

# center x variables to make intercepts interpretable
vars = c("disp","hp","drat","wt","qsec")
mtcars <- mtcars %>% mutate_at(vars, scale, scale = FALSE)

#convert certain columns to factors
cols <- c("cyl", "vs", "am", "gear", "carb")
mtcars <- mtcars %>% mutate_at(cols, funs(factor(.))) # or use ordered

# check normality
plotNormalHistogram(mtcars$mpg)
shapiro.test(mtcars$mpg)

# k fold cv function for GLM
kcv_functionGLM = function(form,df,y){
  for(i in 1:k){
    valIndices <- which(folds==i,arr.ind=TRUE)
    train <- df[-valIndices, ]
    model = glm(form, family=gaussian(link="identity"), data=train)
      validation <- df[valIndices, ]
    pred[[i]] = predict(model, validation, allow.new.levels = TRUE)
  }
  return(cor(unlist(pred),y)^2)
}

# k fold cv function for lmer
kcv_functionLMER = function(form,df,y){
  for(i in 1:k){
    valIndices <- which(folds==i,arr.ind=TRUE)
    train <- df[-valIndices, ]
    model = lmer(form, data=train)
      validation <- df[valIndices, ]
    pred[[i]] = predict(model, validation, allow.new.levels = TRUE)
  }
  return(cor(unlist(pred),y)^2)
}

# k fold cv function for brms
kcv_functionBR = function(form,df,y){
  for(i in 1:k){
    valIndices <- which(folds==i,arr.ind=TRUE)
    train <- df[-valIndices, ]
    model = brm(form, data=train, cores=4, iter = 1e3, 
          chains = 2, control = list(adapt_delta = 0.9))
      validation <- df[valIndices, ]
    pred[[i]] = predict(model, validation, allow.new.levels = TRUE)[,1]
  }
  return(cor(unlist(pred),y)^2)
}

# write functional form for GLM 
form1 = as.formula("mpg ~ wt + cyl")

# write functional form for lmer and brms
form2 = as.formula("mpg ~ wt + cyl + (wt | cyl)")

k = 10 # set number of cross validation folds

listLMER = list()
listGLM = list()

for(j in 1:5){ # number of repeated cross validations
 pred = list()
 mtcars <- mtcars[sample(nrow(mtcars)),]
 folds <- cut(seq(1,nrow(mtcars)),breaks=k,labels=FALSE)
  listGLM[[j]] = kcv_functionGLM(form1,mtcars,mtcars$mpg)
  listLMER[[j]] = kcv_functionLMER(form2,mtcars,mtcars$mpg)
}

cat("Median cross-validated R-sq for linear model =",median(unlist(listGLM)))
cat("Median cross-validated R-sq for lmer model =",median(unlist(listLMER)))

# Bayesian regression takes time and so not repeated, priors left at defaults
kcv_functionBR(form2,mtcars,mtcars$mpg) # cross-validated rsq for brms

#END
