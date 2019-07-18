summary(cars)
head(cars)
plot(cars)
model_lm <- lm(dist~speed, data=cars)

summary(model_lm)

simple_linear<-function(x,y,dane){

N<-nrow(dane) #sample size
avgx<- sum(x)/N
avgy<- sum(y)/N

sdx<-sqrt(sum((x-avgx)^2)/(N-1))
sdy<-sqrt(sum((y-avgy)^2)/(N-1))

wariancja_x<-sum((x-avgx)^2)/(N-1)

kowariancja_xy<-sum((x-avgx)*(y-avgy))/(N-1)
korelacja_p<-kowariancja_xy/(sdx*sdy)

rsq<-korelacja_p^2

b1c<- kowariancja_xy/wariancja_x

b0<-avgy-b1c*avgx

y_hat<-b0+b1c*x #estimated function form
resi<-y-y_hat #residual calculation
avgresi<-sum(resi)/length(resi)
wariancja_resi<-sum((resi-avgresi)^2)/(N-2)
wariancja_b1<-wariancja_resi/sum((x-avgx)^2)

#b1 statistics
stex<- sqrt(wariancja_b1)
tvalue_b1<-b1c/stex
pvalue_b1<-2*pt(-abs(tvalue_b1), df=N-2)

#b0 statistics
stey<-stex*sqrt(sum((x^2)/N))
tvalue_b0<-b0/stey
pvalue_b0<-2*pt(-abs(tvalue_b0), df=N-2)

#results print
cat ("
    b0: ",b0,"std. error b0: ", stey, "t-value: ", tvalue_b0, "p-value: ", pvalue_b0, "
    b1: ", b1c, "std. error b1: ", stex, "t-value: ", tvalue_b1, "p-value: ", pvalue_b1, "
    R-squared: ", rsq, "
    Mean Sq: ", wariancja_resi, "RSE: ", sqrt(wariancja_resi))

plot(cars)
lines(x, y_hat, col="blue")
}

simple_linear(cars$speed,cars$dist,cars)
