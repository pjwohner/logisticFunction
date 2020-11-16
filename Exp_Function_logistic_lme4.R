##################################################################
###  Function for exponentiating logistic regresion output    ####
##################################################################
PJW_exp<-function(x,scaler){
  if (x < 0){
    result = 1/exp(x*scaler)
  }
  else (result = exp(x*scaler))  
return(result)
}
PJW_exp(-0.31,10)
PJW_exp(0.13,10)

#######################################################################
###   R Primer  #############################################

v2 = c(234, 17, 42.5, 64, 38)
 v2 <  200 

v2[v2 > 200]
v2[v2 >= 17]
v2[v2!=64]
v2[v2 ==17|v2==42.5]
v2[v2!17]
v=NULL
v[8]=254
v[2]=10
v[12]=13.4
v
M<-matrix(0,nrow=3,ncol=7)
M[1,]=c(1995,10.0,5,6,7,6,7)
M[2,]=c(2000,7.5,4,3,4,1,3)
M[3,]=c(2005,13.5,4,3,4,4,3)
M[1,]
M[,1]
max(M)
mean(M[,2])
M[M[,2]<=10,2]

M[M[,2]> mean(M[,2]),2]

##exponential growth function
#value of growth rate
lambda = 1.2
#vector for holding state variable
N = numeric(10)
#initial value of N
N[1] = 10
#loop to calculate N over time Use t to index vector
for (t in 2:10){
  #store the values of N in a vector
  N[t] = lambda * N[t-1]
}#end of loop
#if plot has only 1 arguement it assumes it is a time series
plot(N)
rm(list = ls())

#gompertz model for plant growth
mu0 = 1
k =0.3
#B is numeric with size 30
B = numeric(30)
R = numeric(30)
r = numeric(30)
#the first element of B to the value of 10
#B= biomass
B[1] = 10


for (t in 2:length(B)) {
  B[t] = B[t-1] + (mu0*B[t-1])*(1-(k/mu0*log(B[t-1]/B[1])))
  R[t] = (mu0*B[t-1])*(1-(k/mu0*log(B[t-1]/B[1])))
  r[t] = R[t]/B[t]
 }
par(mfrow=c(2,2))
plot(B)
plot(R[2:length(B)])
plot(r[2:length(B)])

#######################################################################################
#3gompertz model for plant growth with matrix
mu0 = 1
k =0.3
#B is numeric with size 30
B = numeric(30)
t = numeric(30)
mymatrix<-matrix(0,nrow=30,ncol=2)
#the first element of B to the value of 10
#B= biomass
B[1] = 10
t[1] = 1
for (t in 2:length(B)) {
  B[t] = B[t-1] + (mu0*B[t-1])*(1-(k/mu0*log(B[t-1]/B[1])))
}

for(i in 1:30){
  mymatrix[i,2] <- B[i]
  mymatrix[i,1] <- i
}

plot(x = mymatrix[,1],y = mymatrix[,2])
mymatrix


a = matrix(0,nrow=5,ncol=5)
for (i in 1:5){
    for (j in 1:5) {
    a[j,i]= j*i 
    }#end of j
}#end of i
a
####################################################
##  exponential growth function with  nested loops
#####################################################
#value of growth rate
lambda = seq(1,1.6,0.1)
N = matrix(0,nrow=10,ncol=length(lambda))
N[1,]=10

#loop to calculate N over time Use t to index vector
      for (j in 1:length(lambda)){
        for (t in 2:10){      
          #store the values of N in a vector
          N[t,j] = lambda[j] * N[t-1,j]
        } #end of j loop
        
} #end of t loop
#if plot has only 1 arguement it assumes it is a time series


rm(list = ls())
v=c(5,6,7,45,123,324)

pwnormalize <- function(x){
  normvector = x/(sum(x))
  return (normvector)
}
pwnormalize(v)







  

