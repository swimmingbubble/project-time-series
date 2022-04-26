ola jj 
dat<-as.data.frame(admissions20)
plot(admissions20[12], ylim=c(0,500))
x<-admissions20[12]
meanf<-function(t){
  
  temp_b<-10*log(1+t/50)+12*(1-exp(-30*t))*(1+sin(2*pi*0.00277778*t))+2*pi*12*0.00277778*t
  return(temp_b)
}
curve(meanf,0,3000, xlab = "t", ylab = "m(t)")