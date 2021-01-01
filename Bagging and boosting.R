### Bagging and boosting ##############

library(rpart)

#prepare the data

risk <-read.csv("E:/Ensembles.txt",stringsAsFactors = FALSE,header = TRUE,
                sep = ",")
#View (df)
choose=runif(dim(risk)[1],0,1)
train=risk[which(choose<=.75),]
test=risk[which(choose >.75),]
#View(test)


#original CART Model for predicting risk
#######################################################

cart.o = rpart(formula=risk~marital_status+mortgage+loans+income+age,data=train,method="class")

p.o = predict(cart.o, newdata=test)

pred1 = ifelse(p.o[,1]>p.o[,2],"Pred: bad loss","Pred: good risk")

o.t=table(pred1,test$risk)


#Bagging Model (5 base models)
#####################################################

s1=train[sample(dim(train)[1],replace = TRUE),]

#Repeat the above for s2 through s5

cart1=rpart(formula=risk~marital_status+mortgage+loans+income+age,data=s1,method="class")

p1=predict(cart1,newdata = test)

pred1=ifelse(p1[,1]>p1[,2],"Pred: bad loss","Pred: good risk")

###########################################################
s2 <- train[sample(dim(train)[1], replace = TRUE),]

cart2 = rpart(risk ~ marital_status+mortgage+loans+income+age,
              data = s2, method = "class")
p2 = predict(cart2, newdata = test)
pred2= ifelse(p2[,1] > p1[,2], "Pred: bad loss", "Pred: good risk")

##############################################################
s3 <- train[sample(dim(train)[1], replace = TRUE),]


cart3 = rpart(risk ~ marital_status+mortgage+loans+income+age,
              data = s3, method = "class")
p3 = predict(cart3, newdata = test)
pred3= ifelse(p3[,1] > p1[,2], "Pred: bad loss", "Pred: good risk")


#################################################################''
#################################################################'
s4 <- train[sample(dim(train)[1], replace = TRUE),]


cart4 = rpart(risk ~ marital_status+mortgage+loans+income+age,
              data = s4, method = "class")
p4 = predict(cart4, newdata = test)
pred4= ifelse(p4[,1] > p1[,2], "Pred: bad loss", "Pred: good risk")

######################################################################

s5 <- train[sample(dim(train)[1], replace = TRUE),]
cart5 = rpart(risk ~ marital_status+mortgage+loans+income+age,
              data = s5, method = "class")
p5 = predict(cart3, newdata = test)
pred5= ifelse(p5[,1] > p1[,2], "Pred: bad loss", "Pred: good risk")


#######################################################################

#predict

preds=c(pred1,pred2,pred3,pred4,pred5)
recs=as.integer(names(preds));fin.pred = rep(0,dim(test)[1])
for(i in 1:dim(test)[1]){
  t=table(preds[which(recs==as.integer(rownames(test))[i])])
  fin.pred[i]=names(t)[t==max(t)]
}

bag.t=table(fin.pred,test$risk)

#########################################################################
#### Boosting Model

# Boosting model (5 iterations)
cart6 = rpart(risk ~ marital_status+mortgage+loans+income+age,
              data = train, method = "class")
p6 = predict(cart6, newdata = train)
pred6 = ifelse(p6[,1] > p6[,2], "bad loss", "good risk")
moreweight = train$risk != pred6
new.weights = ifelse(moreweight==TRUE, 2, 1)
cart7 = rpart(risk ~ marital_status+mortgage+loans+income+age,
              weights = new.weights, data = train, method = "class")

p7 = predict(cart7, newdata = train)
pred7 = ifelse(p7[,1] > p7[,2], "bad loss", "good risk")
moreweight = train$risk != pred7
new.weights = ifelse(moreweight==TRUE, 2, 1)

cart8 = rpart(risk ~ marital_status+mortgage+loans+income+age,
              weights = new.weights, data = train, method = "class")

p8 = predict(cart8, newdata = train)
pred8 = ifelse(p8[,1] > p8[,2], "bad loss", "good risk")
moreweight = train$risk != pred8
new.weights = ifelse(moreweight==TRUE, 2, 1)

cart9 = rpart(risk ~ marital_status+mortgage+loans+income+age,
              weights = new.weights, data = train, method = "class")

p9 = predict(cart9, newdata = train)
pred9 = ifelse(p9[,1] > p9[,2], "bad loss", "good risk")
moreweight = train$risk != pred9
new.weights = ifelse(moreweight==TRUE, 2, 1)


# Repeat the above for cart8 and cart9
cart10 = rpart(risk ~ marital_status+mortgage+loans+income+age,
               weights = new.weights, data = train, method = "class")
p10 = predict(cart10, newdata = test)
pred10 = ifelse(p10[,1] > p10[,2], "Pred: bad loss", "Pred: good risk")
boost.t = table(pred10, test$risk) # Contingency table

#########################################################################################
#Compare models
#compare contigencey tables
o.t
bag.t
boost.t

#compare errors
####################################################################################
o.t[2]+o.t[3]/sum(o.t)
(bag.t[2]+bag.t[3])/sum(bag.t)
(boost.t[2]+boost.t[3]/sum(boost.t))

  
