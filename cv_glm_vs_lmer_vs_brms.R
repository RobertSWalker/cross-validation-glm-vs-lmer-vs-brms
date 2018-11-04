library(lme4)
library(brms)
library(rcompanion)

# function to center x variables to make intercepts interpretable
center_colmeans <- function(x) { xcenter = colMeans(x, na.rm=TRUE)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))}
varsToCenter = c("disp","hp","drat","wt","qsec")
mtcars[varsToCenter] = center_colmeans(mtcars[varsToCenter])

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
form1 = as.formula("mpg ~ wt + factor(gear) + factor(cyl)")

# write functional form for lmer and brms
form2 = as.formula("mpg ~ wt + (1 | cyl) + (1 | gear)")

k = 10 # set number of cross validation folds
set.seed(3)
listLMER = list()
listGLM = list()

for(j in 1:100){ # number of repeated cross validations
 pred = list()
 mtcars <- mtcars[sample(nrow(mtcars)),]
 folds <- cut(seq(1,nrow(mtcars)),breaks=k,labels=FALSE)
  listGLM[[j]] = kcv_functionGLM(form1,mtcars,mtcars$outcome_boxcox)
  listLMER[[j]] = kcv_functionLMER(form2,mtcars,mtcars$outcome_boxcox)
}

cat("Average cross-validated R-sq for linear model =",mean(unlist(listGLM)))
cat("Average cross-validated R-sq for lmer model =",mean(unlist(listLMER)))

# Bayesian regression takes time and so not repeated, priors left at defaults
kcv_functionBR(form2,mtcars,mtcars$outcome_boxcox) # cross-validated rsq for brms

