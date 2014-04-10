rm(list=ls(all=TRUE))
library(xtable)
library(matrixcalc)
setwd("C:/Users/EliotP/Documents/GitHub/STA_237_sp14/Homework/") #laptop
pop = as.matrix(read.table("population.csv"))
n = nrow(pop)

### plot the data
time = as.vector(seq(1790,2000,10))
pdf("hw1_dataplot.pdf")
plot(time,t(pop),type="o",xlab="Year", ylab="Population",main="U.S. Population in 10-Year Increments")
dev.off()

### fit a 2nd degree polynomial
F = vandermonde.matrix(1:n,3)
mhat_ls = F%*%solve(t(F)%*%F)%*%t(F)%*%pop 

print(xtable(mhat_ls),include.rownames=FALSE)

### residuals

resid = pop-mhat_ls
pdf("hw1_resid.pdf")
plot(x=time, resid,type="o",xlab="Time",ylab="Residual",main="Plot of Residuals against Time")
abline(h=0)
dev.off()

### predict using direct method
A=solve(t(F)%*%F)%*%t(F)%*%pop
predict_val = as.matrix(c(A[1]*1+A[2]*23+A[3]*23^2,A[1]*1+A[2]*24+A[3]*24^2))
print(xtable(t(predict_val)),include.rownames=FALSE)


##############alternative method##########3
###predict using lm function
new = c(23,24)
new_df = data.frame(one = 1, two=new, three = new^2)
B = predict.lm(model,new_df)

## alternate method to comuting least-squares fit
dat = as.data.frame(cbind(pop,F))
names(dat) = c("y","one","two","three")
model = lm(y~0+.,data=dat)
model$fit
#plot(model$fit,xlim=c(0,25),ylim=c(0,max(B)))
#points(pop,type="o")
#points(new,B)
