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



  

