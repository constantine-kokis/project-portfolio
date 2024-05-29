### Data Initialization and Categorical conversion:
Proj<- heart1
Proj$cp<-as.factor(Proj$cp)
Proj$restecg<-as.factor(Proj$restecg)
Proj$slope<-as.factor(Proj$slope)
Proj$thal<-as.factor(Proj$thal)
Proj<-subset(Proj, thal!="0" & ca!="4")


####Data exploration
summary(Proj)


#####Logit functions
### Model with all of the predictors:

heartslmFull = glm(target ~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope +ca +thal, family=binomial(link=logit ),data=Proj)
summary(heartslmFull)

##Removed some stuff
heartslmTest = glm(target ~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + slope +ca, family=binomial(link=logit ),data=Proj)
summary(heartslmTest)

#### Logit Model below removes all of the predictors that are not statistically significant at the 5% level:

## Create a binary variable denoting if the patient has reversible thal, since it's the only one out of the thal factors that's significant

Proj$ThalRev <- 0
for (i in 1:nrow(Proj)){
  if(Proj$thal[i] == 3){
    Proj$ThalRev[i] <- 1
  } else{
    Proj$ThalRev[i] <- 0
  }
}

## Create the reduced model
heartslmReduced = glm(target~sex+cp + trestbps+ chol+ thalach+exang+oldpeak+ca+ThalRev, family =binomial(link=logit),data=Proj)

summary(heartslmReduced)

## Test to see which is better
install.packages("lmtest")
library(lmtest)
lrtest(heartslmFull, heartslmReduced)

## Create Probit Model

heartslmFullProb = glm(target ~ age + sex + cp + trestbps +
                         chol + fbs + restecg + thalach + exang + 
                         oldpeak + slope +ca +thal, family=binomial(link=probit ),data=Proj)
summary(heartslmFullProb)

## Create Model with interactions

heartslmIntProb <- glm(target ~ age*thalach + sex + cp + 
                         trestbps + chol + fbs + restecg + exang + oldpeak + 
                         slope +ca + thal, family=binomial(link=probit ),data=Proj)
summary(heartslmIntProb)

## sex, cp (all), trestbps, chol, restecg1, thalach, exang, oldpeak, ca, and thal3 were all statistically significant to some extent.
## our reduced model will use these variables while removing the others.

# Create a binary variable that is 1 when the value of restecg is 1:

Proj$restecgOne <- 0
for (i in 1:nrow(Proj)){
  if(Proj$restecg[i] == 1){
    Proj$restecgOne[i] <- 1
  } else{
    Proj$restecgOne[i] <- 0
  }
}

## Run the reduced Probit Model

heartslmReducedProb = glm(target~ sex + cp + trestbps + chol + restecgOne + thalach + exang + oldpeak + ca + ThalRev, family =binomial(link=probit), data = Proj)
summary(heartslmReducedProbit)

## RestecgOne is still significant but reducing the model didn't increase it's significance level all that much.
## AIC is 684.17 in the reduced model vs 676.03 in the more robust model

## Use LRT to see which model we prefer:

lrtest(heartslmFullProb, heartslmReducedProb)

# As with the probit, we seem to prefer the 
logLik(heartslmReduced)
str(proj.omit)
str(Proj)
view(Proj.omit)
as.data.frame(proj.omit)


###Decision Trees
treefull.heart<-tree(target~sex+age+cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope+ca+thal,data=proj_omit)
summary(treefull.heart)

plot(treefull.heart)
text(treefull.heart, cex=0.6)

#5-fold Cross validation to determine optimal tree size (|T|)
cv.heart=cv.tree(treefull.heart, K=10)
plot(cv.heart$size, cv.heart$dev, type="b")


#pruned decision tree
prunetree.heart=prune.tree(treefull.heart, best=4)
plot(prunetree.heart)
text(prunetree.heart, cex=0.6)

bestsubset.fullh = regsubsets(target~., Proj_omit, nvmax=13)
bestsubseth.summary=summary(bestsubset.fullh)
bestsubseth.summary
?regsubsets

######Independent test
set.seed(1)
train = sample(1:nrow(Proj), nrow(Proj)/2)

tree.hearts = tree(Proj$target ~ .,Proj, subset = train)
summary(tree.hearts)

plot(tree.hearts)
text(tree.hearts, pretty = 0)

######Classification error from training set
yhat_hearts = predict(tree.hearts, newdata = Proj[-train, ],type = "class")
hearts.test = Proj[-train, "target"]
plot(yhat_hearts, hearts.test)
nrow(Proj)
classs<-rep(0,500)
for (i in 1:500) {
  
if (yhat_hearts[i]==hearts.test[i]) {
  classs[i]= 1
}
}
}

classMSE<-mean(classs)
library(randomForest)

set.seed(1)

bag.hearts = randomForest(target ~., data = Proj, subset = train, mtry = 12, importance = TRUE)
bag.hearts


# Estimate a test error

yhat.bag = predict(bag.boston, newdata = Boston[-train, ])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag - boston.test)^2)



#####Logit functions
heartslmFull = glm(target ~ age + sex + cp + trestbps + chol
                    + fbs + restecg + thalach + exang + oldpeak + slope +ca +thal, family=binomial(link=logit ),Proj)
summary(heartslmFull)
str(Proj)


heartslmFull = glm(target ~ age + sex + cp + trestbps + chol
                   + fbs + restecg + thalach + exang + oldpeak + slope +ca +thal, family=binomial(link=logit),proj.omit)
summary(heartslmFull)
heartslmFull = glm(target ~thal, family=binomial(link=logit),proj.omit)
logLik(heartslmFull)
heartslmReduced = glm(target~sex+fbs+thalach+exang+oldpeak+ca+thal, family =binomial(link=logit),proj.omit)
summary(heartslmReduced)
logLik(heartslmReduced)
str(proj.omit)
str(Proj)
view(Proj.omit)
as.data.frame(proj.omit)
