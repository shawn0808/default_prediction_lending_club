setwd("~/Desktop/哥大/BA ")
loan=read.csv2("loan_cleaned_new.csv",sep=",",header = TRUE)
loan=loan[,-4]


#deal with the data
loan$loan_amnt=as.numeric(loan$loan_amnt)
loan$installment=as.numeric(loan$installment)
loan$grade=as.factor(loan$grade)
loan$int_rate=as.numeric(as.vector(loan$int_rate))
loan$emp_length=as.factor(loan$emp_length)
loan$annual_inc=as.numeric(loan$annual_inc)
loan$dti=as.numeric(loan$dti)
loan$delinq_2yrs=as.numeric(loan$delinq_2yrs)
loan$inq_last_6mths=as.numeric(loan$inq_last_6mths)
loan$open_acc=as.numeric(loan$open_acc) 
loan$pub_rec=as.numeric(loan$pub_rec) 
loan$revol_bal=as.numeric(loan$revol_bal) 
loan$revol_util=as.numeric(loan$revol_util) 
loan$total_acc=as.numeric(loan$total_acc) 
loan$collections_12_mths_ex_med=as.numeric(loan$collections_12_mths_ex_med)
loan$acc_now_delinq=as.numeric(loan$acc_now_delinq)
loan$tot_coll_amt=as.numeric(loan$tot_coll_amt)
loan$tot_cur_bal=as.numeric(loan$tot_cur_bal)
loan$total_rev_hi_lim=as.numeric(loan$total_rev_hi_lim)
loan$home_ownership_OWN=as.factor(loan$home_ownership_OWN)
loan$home_ownership_RENT=as.factor(loan$home_ownership_RENT)
loan$verification_status_Source.Verified=as.factor(loan$verification_status_Source.Verified)
loan$verification_status_Verified=as.factor(loan$verification_status_Verified)
loan$home=as.factor(loan$home)
loan$major_purchase=as.factor(loan$major_purchase)
loan$refinance=as.factor(loan$refinance)
loan$term_.60.months=as.factor(loan$term_.60.months)


set.seed(1337)
train.row = sample(1:nrow(loan),0.75*nrow(loan))
train<-loan[train.row, ]
test = loan[-train.row,]



#LDA
#train data and prediction
library(MASS)
lda.fit = lda(loan_status~.,data = loan,subset=train.row) 
lda.fit
lda.pred=predict(lda.fit,train)
names(lda.pred)
lda.class = lda.pred$class 
table(lda.class,train$loan_status) 
mean(lda.class==train$loan_status)
#model using
lda.pred=predict(lda.fit,test) 
names(lda.pred)
lda.class = lda.pred$class 
table(lda.class,test$loan_status) 
mean(lda.class!=test$loan_status)
correct1=1-mean(lda.class!=test$loan_status)
correct1

#Decision Tree
library(tree)
library(MASS) 
loan$loan_status=as.factor(loan$loan_status)
tree.loan=tree(loan_status~.,data=loan,subset=train.row,mincut=2) 
summary(tree.loan)
plot(tree.loan)
text(tree.loan,pretty=0)
yhat=predict(tree.loan,newdata=test[,-6]) 
loan.tree.test=test$loan_status
shreldshore<-seq(0.1,0.8,0.01)
correct<-rep(0,71)
for(i in 1:71){
  result.tree=rep(0,52580)
  result.tree[yhat > shreldshore[i]]=1 
  table(result.tree,loan.tree.test)
  correct[i]=sum(result.tree==loan.tree.test)/length(result.tree)
}
plot(correct,t="l")
correct


#KNN
library(ggvis) 
library(class)
# myselect<-c("loan_amnt","installment","int_rate ","annual_inc","dti","delinq_2yrs","inq_last_6mths","open_acc","pub_rec","revol_bal","revol_util","total_acc","collections_12_mths_ex_med","acc_now_delinq","tot_coll_amt","tot_cur_bal","total_rev_hi_lim") 
standardized.loan=scale(loan[,c("loan_amnt","installment","int_rate ","annual_inc","dti","delinq_2yrs","inq_last_6mths","open_acc","pub_rec","revol_bal","revol_util","total_acc","collections_12_mths_ex_med","acc_now_delinq","tot_coll_amt","tot_cur_bal","total_rev_hi_lim") ]) 
standardized.loan=cbind(loan[,c(4,6,20:27)],standardized.loan) 
train.X=standardized.loan[train.row,]
train.Y=train.X$loan_status
train.X=train.X[,-1]
test.X=standardized.loan[-train.row,] 
test.Y=test.X$loan_status
test.X=test.X[,-1] 
correct3<-rep(0,5)
k=seq(1,11,2)
for(i in 1:6){
  knn.pred=knn(train.X,test.X,train.Y,k[i])
  print(knn.pred)
  table(knn.pred,test.Y)
  correct3[i]=sum(knn.pred==test.Y)/length(test.Y)
  }

correct3

hc.average=hclust(dist(standardized.loan),method="average")

