set.seed(4897)
cl <- makePSOCKcluster(6)
registerDoParallel(cl)
#############################################################################
inTrain_alc <- createDataPartition(alcohol$solici_atend, p = .75, list = FALSE)
train_alc <- alcohol[inTrain_alc, ]
test_alc <- alcohol[-inTrain_alc, ]

#Modelo 1 Regresión Logistica: Alcohol
tic()
modfit_logit_alc <- train(solici_atend ~ ., data = train_alc, method="glm")
toc()
logit_alc_pred <- predict.train(modfit_logit_alc, newdata = test_alc)
confusionMatrix(data = logit_alc_pred, reference = test_alc$solici_atend)

#Modelo 1 SVM: Alcohol
tic()
modfit_svm_alc <- train(solici_atend ~ ., data = train_alc, method="svmLinear")
toc()
svm_alc_pred <- predict.train(modfit_svm_alc, newdata = test_alc)
confusionMatrix(data = svm_alc_pred, reference = test_alc$solici_atend)

#Modelo 1 SVM: CART
tic()
modfit_cart_alc <- train(solici_atend ~ ., data = train_alc, method="rpart")
toc()
cart_alc_pred <- predict.train(modfit_cart_alc, newdata = test_alc)
confusionMatrix(data = cart_alc_pred, reference = test_alc$solici_atend)

#Modelo 1 Treebag: Alcohol
tic()
modfit_treebag_alc <- train(solici_atend ~ ., data = train_alc, method="treebag")
toc()
treebag_alc_pred <- predict.train(modfit_treebag_alc, newdata = test_alc)
confusionMatrix(data = treebag_alc_pred, reference = test_alc$solici_atend)

#Modelo 1 Naive Bayes: Alcohol
tic()
modfit_nb_alc <- train(solici_atend ~ ., data = train_alc, method="naive_bayes")
toc()
nb_alc_pred <- predict.train(modfit_nb_alc, newdata = test_alc)
confusionMatrix(data = nb_alc_pred, reference = test_alc$solici_atend)

#Modelo 1 Boosting Regresion Logistica: Alcohol
tic()
modfit_logitboost_alc <- train(solici_atend ~ ., data = train_alc, method="LogitBoost")
toc()
logitboost_alc_pred <- predict.train(modfit_logitboost_alc, newdata = test_alc)
confusionMatrix(data = logitboost_alc_pred, reference = test_alc$solici_atend)





#Tabaco
tabaco2 <- tabaco[rowSums(is.na(tabaco)) == 0, ] 

inTrain_tab <- createDataPartition(tabaco2$solici_atend, p = .75, list = FALSE)
train_tab <- tabaco2[inTrain_tab, ]
test_tab <- tabaco2[-inTrain_tab, ]

#Modelo 1 REgresión Logística: Tabaco
tic()
modfit_logit_tab <- train(solici_atend ~ ., data = train_tab, method="glm")
toc()
logit_tab_pred <- predict.train(modfit_logit_tab, newdata = test_tab)
confusionMatrix(data = logit_tab_pred, reference = test_tab$solici_atend)

#Modelo 1 SVM: Tabaco
tic()
modfit_svm_tab <- train(solici_atend ~ ., data = train_tab, method="svmLinear")
toc()
svm_tab_pred <- predict.train(modfit_svm_tab, newdata = test_tab)
confusionMatrix(data = svm_tab_pred, reference = test_tab$solici_atend)

#Modelo 1 CART: Tabaco
tic()
modfit_cart_tab <- train(solici_atend ~ ., data = train_tab, method="rpart")
toc()
cart_tab_pred <- predict.train(modfit_cart_tab, newdata = test_tab)
confusionMatrix(data = cart_tab_pred, reference = test_tab$solici_atend)

#Modelo 1 Treebag: Tabaco
tic()
modfit_treebag_tab <- train(solici_atend ~ ., data = train_tab, method="treebag")
toc()
treebag_tab_pred <- predict.train(modfit_treebag_tab, newdata = test_tab)
confusionMatrix(data = treebag_tab_pred, reference = test_tab$solici_atend)

#Modelo 1 Naive Bayes: Tabaco
tic()
modfit_nb_tab <- train(solici_atend ~ ., data = train_tab, method="naive_bayes")
toc()
nb_tab_pred <- predict.train(modfit_nb_tab, newdata = test_tab)
confusionMatrix(data = nb_tab_pred, reference = test_tab$solici_atend)

#Modelo 1 Boosting Regresion Logistica: Alcohol
tic()
modfit_logitboost_tab <- train(solici_atend ~ ., data = train_tab, method="LogitBoost")
toc()
logitboost_tab_pred <- predict.train(modfit_logitboost_tab, newdata = test_tab)
confusionMatrix(data = logitboost_tab_pred, reference = test_tab$solici_atend)






#Ansiedad
inTrain_ans <- createDataPartition(ansi$solici_atend, p = .75, list = FALSE)
train_ans <- ansi[inTrain_ans, ]
test_ans<- ansi[-inTrain_ans, ]

#Modelo 1 REgresion Logistica: Ansiedad
tic()
modfit_logit_ans <- train(solici_atend ~ ., data = train_ans, method = "glm")
toc()
logit_ans_pred <- predict(modfit_logit_ans, newdata = test_ans)
confusionMatrix(data = logit_ans_pred, reference = test_ans$solici_atend)

#Modelo 1 SVM: Ansiedad
tic()
modfit_svm_ans <- train(solici_atend ~ ., data = train_ans, method="svmLinear")
toc()
svm_ans_pred <- predict.train(modfit_svm_ans, newdata = test_ans)
confusionMatrix(data = svm_ans_pred, reference = test_ans$solici_atend)

#Modelo 1 SVM: Ansiedad
tic()
modfit_cart_ans <- train(solici_atend ~ ., data = train_ans, method="rpart")
toc()
cart_ans_pred <- predict.train(modfit_cart_ans, newdata = test_ans)
confusionMatrix(data = cart_ans_pred, reference = test_ans$solici_atend)

#Modelo 1 Treebag: Ansiedad
tic()
modfit_treebag_ans <- train(solici_atend ~ ., data = train_ans, method="treebag")
toc()
treebag_ans_pred <- predict.train(modfit_treebag_ans, newdata = test_ans)
confusionMatrix(data = treebag_ans_pred, reference = test_ans$solici_atend)

#Modelo 1 Naive Bayes: Ansiedad
tic()
modfit_nb_ans <- train(solici_atend ~ ., data = train_ans, method="naive_bayes")
toc()
nb_ans_pred <- predict.train(modfit_nb_ans, newdata = test_ans)
confusionMatrix(data = nb_ans_pred, reference = test_ans$solici_atend)

#Modelo 1 Logistic REgression Boost: Ansiedad
tic()
modfit_logitboost_ans <- train(solici_atend ~ ., data = train_ans, method="naive_bayes")
toc()
logitboost_ans_pred <- predict.train(modfit_logitboost_ans, newdata = test_ans)
confusionMatrix(data = logitboost_ans_pred, reference = test_ans$solici_atend)







#Depresión
depre2 <- depre[rowSums(is.na(depre)) == 0, ] 

inTrain_dep <- createDataPartition(depre2$solici_atend, p = .75, list = FALSE)
train_dep <- depre2[inTrain_dep, ]
test_dep <- depre2[-inTrain_dep, ]

#Modelo 1 REgresión Logística: Depresión
tic()
modfit_logit_dep <- train(solici_atend ~ ., data = train_dep, method="glm")
toc()
logit_dep_pred <- predict.train(modfit_logit_dep, newdata = test_dep)
confusionMatrix(data = logit_dep_pred, reference = test_dep$solici_atend)

#Modelo 1 SVM: Depresión

tic()
modfit_svm_dep <- train(solici_atend ~ ., data = train_dep, method="svmLinear")
toc()
stopCluster(cl)
svm_dep_pred <- predict.train(modfit_svm_dep, newdata = test_dep)
confusionMatrix(data = svm_dep_pred, reference = test_dep$solici_atend)

#Modelo 1 CART: Tabaco
tic()
modfit_cart_dep <- train(solici_atend ~ ., data = train_dep, method="rpart")
toc()
cart_dep_pred <- predict.train(modfit_cart_dep, newdata = test_dep)
confusionMatrix(data = cart_dep_pred, reference = test_dep$solici_atend)

#Modelo 1 Treebag: Tabaco
tic()
modfit_treebag_tab <- train(solici_atend ~ ., data = train_tab, method="treebag")
toc()
treebag_tab_pred <- predict.train(modfit_treebag_tab, newdata = test_tab)
confusionMatrix(data = treebag_tab_pred, reference = test_tab$solici_atend)

#Modelo 1 Naive Bayes: Tabaco
tic()
modfit_nb_tab <- train(solici_atend ~ ., data = train_tab, method="naive_bayes")
toc()
nb_tab_pred <- predict.train(modfit_nb_tab, newdata = test_tab)
confusionMatrix(data = nb_tab_pred, reference = test_tab$solici_atend)

#Modelo 1 Boosting Regresion Logistica: Alcohol
tic()
modfit_logitboost_tab <- train(solici_atend ~ ., data = train_tab, method="LogitBoost")
toc()
logitboost_tab_pred <- predict.train(modfit_logitboost_tab, newdata = test_tab)
confusionMatrix(data = logitboost_tab_pred, reference = test_tab$solici_atend)



#recall = tp / (tp+ fp)
recall_mod1 <- 2047/2074+101
#Precision = tp / tp + tn
precision_mod1 <- 2047/2047
#F1 
f1_mod1 <-2*((precision_mod1*recall_mod1)/(precision_mod1+recall_mod1))


tic()
set.seed(4897)
modelfit3 <- train(solici_atend ~ ., data = train, method="rpart")
toc()

mod3_pred <- predict.train(modelfit3, newdata = test)
confusionMatrix(data = mod3_pred, reference = test$solici_atend)

tic()
set.seed(4897)
modelfit5 <- train(solici_atend ~ ., data = train, method="LogitBoost")
toc()

mod5_pred <- predict.train(modelfit5, newdata = test)
confusionMatrix(data = mod5_pred, reference = test$solici_atend)

tic()
set.seed(4897)
modelfit6 <- train(solici_atend ~ ., data = train, method="rf")
toc()

mod6_pred <- predict.train(modelfit6, newdata = test)
confusionMatrix(data = mod6_pred, reference = test$solici_atend)