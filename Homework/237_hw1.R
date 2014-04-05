library(matrixcalc)

pop <- read.table("~/School/UC Davis/sta 237 - s14/population.csv", quote="\"")
y = as.vector(pop)

a = as.vector(seq(1790,2000,10))
n = nrow(pop)

plot(1:n,y=t(pop))

F = vandermonde.matrix(1:n,3)
mhat_ls = solve(t(F)%*%F)%*%t(F)%*%pop 

dat = as.data.frame(cbind(y,F))
names(dat) = c("y","one","two","three")
model = lm(y~.,data=dat)
model$fit

plot(1:n, model$fit,type="l")
points(pop)



new = c(23,24)
new_df = data.frame(one = 1, two=new, three = new^2)
predict.lm(model,new_df)
