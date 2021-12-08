library(car)
data<-read.table('/Users/fbsh/Desktop/LINTHALL.txt',header = TRUE)
dim(data)
str(data)
summary(data)
plot(data)
lm.fit1<-lm(BIO ~ H2S+SAL+Eh7+pH+BUF+P+K+Ca+Mg+Na+Mg+Zn+Cu+NH4,data = data)
summary(lm.fit1)
vif(lm.fit1)
kappa(cor(data[,4:18]))
eigen(cor(data[,4:18]))
data_1<-data[,4:18]
cor(data_1)
plot(data_1)

library(leaps)
regsubsets<-regsubsets(BIO~.,data = data_1)
sumreg<-summary(regsubsets)
sumreg
data.frame(adj2=sumreg$adjr2,cp=sumreg$cp,bic=sumreg$bic)
lm.fit2<-lm(BIO ~ H2S+pH+BUF+P+Ca+Mg+Na+Mg,data = data_1)
summary(lm.fit2)

#part 2


#### 
data_1.pr<-princomp(~H2S+SAL+Eh7+pH+BUF+P+K+Ca+Mg+Na+Mg+Zn+Cu+NH4, data=data_1, cor=T)
summary(data_1.pr, loadings=TRUE)
screeplot(data_1.pr,type="lines")

pre<-predict(data_1.pr)
data_1$z1<-pre[,1]
data_1$z2<-pre[,2]
data_1$z3<-pre[,3]
data_1$z4<-pre[,4]
data_1$z5<-pre[,5]
data_1$z6<-pre[,6]
data_1$z7<-pre[,7]
lm.sol<-lm(BIO~z1+z2+z3, data=data_1)
summary(lm.sol)

beta<-coef(lm.fit1)
A<-loadings(data_1.pr)
x.bar<-data_1.pr$center
x.sd<-data_1.pr$scale
coef<-(beta[2]*A[,1]+ beta[3]*A[,2])/x.sd
beta0 <- beta[1]- sum(x.bar * coef)
c(beta0, coef)


#Part 3
mydata<-read.table('/Users/fbsh/Desktop/LINTH-5.txt',header = TRUE)
mydata_1<-mydata[,4:9]

#逐步回归
shuju.reg <- lm(BIO~.,data=mydata_1)
shuju.regbackward2 <- step(shuju.reg,direction="backward",nvmax=3)
lm_both<- step(shuju.reg,direction="both")
mydata_1.lm1 <- lm(BIO~SAL + pH + K + Zn,data=mydata_1)
summary(mydata_1.lm1)

regsubsets<-regsubsets(BIO~.,data = mydata_1)
sumreg<-summary(regsubsets)
sumreg
data.frame(adj2=sumreg$adjr2,cp=sumreg$cp,bic=sumreg$bic)

md.lm1<-lm(BIO~SAL + pH ,data=mydata_1)
md.lm2<-lm(BIO~SAL + K,data=mydata_1)
md.lm3<-lm(BIO~SAL + Na,data=mydata_1)
md.lm4<-lm(BIO~SAL + Zn,data=mydata_1)
md.lm5<-lm(BIO~pH + K ,data=mydata_1)
md.lm6<-lm(BIO~pH + Na,data=mydata_1)
md.lm7<-lm(BIO~pH + Zn,data=mydata_1)
md.lm8<-lm(BIO~K + Na,data=mydata_1)
md.lm9<-lm(BIO~K + Zn,data=mydata_1)
md.lm10<-lm(BIO~Na + Zn,data=mydata_1)
AIC(md.lm1)
BIC(md.lm1)
AIC(md.lm2)
BIC(md.lm2)
AIC(md.lm3)
BIC(md.lm3)
AIC(md.lm4)
BIC(md.lm4)
AIC(md.lm5)
BIC(md.lm5)
AIC(md.lm6)
BIC(md.lm6)
AIC(md.lm7)
BIC(md.lm7)
AIC(md.lm8)
BIC(md.lm8)
AIC(md.lm9)
BIC(md.lm9)
AIC(md.lm10)
BIC(md.lm10)

best.lm <- lm(BIO~pH + Na,data=mydata_1)
summary(best.lm)

