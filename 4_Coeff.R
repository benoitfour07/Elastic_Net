


################### FUNCTION AROUND COEFFICIENT CONSISTANCY ########################

#Get coeff for 1 threeshold: input = cut_predict_elnet/cap_predict_elnet output
#                           output = dataframe of coefficient
# First execute cut_predict_elnet/cap_predict_elnet function and then use the output for this function

get_coeff <- function(predictE,nb_var){
  
  temp = nb_var + 1
  result = as.data.frame(attr(predictE$coeff,"Dimnames")[[1]][2:temp])
  for (i in 0:9){
    indice1 = 2 + (nb_var*i)
    indice2 = temp + (nb_var*i)
    result = cbind(result,as.data.frame(predictE$coeff[indice1:indice2]))
  }
  result[result==0.000000e+00] <- NA
  return(result)
}

#test
P = cap_predict_elnet(PD,1,2500000)
P1 = get_coeff(P,72)
P1
P1 =format(P1,digits=4)
write.csv2(P1,file = "PD_coeffcient_amount_2500000_alpha1")

#Get number of non-null coefficients for 1 alpha value
#                           input = cut_predict_elnet/cap_predict_elnet output
#                           output = average of coffecients on 10-fold

get_number_coeff <- function(predictE){
  df = get_coeff(predictE)
  result = 0
  for (i in 2:11){
    result = result + (71 - as.numeric(summary(df[,i])["NA's"]))
  }
  result = result/10
  return(result)
}


#Get number of non-null coefficients: Loop on alpha value
get_number_coeff_alpha <- function(PD,threshold){
  result = list()
  for (alpha in seq(0.01,0.1,by=0.01)){
    df = cut_predict_elnet(PD,alpha,threshold)
    result = rbind(result,get_number_coeff(df))
  }
  return(result)
}

################################ GINI ################################# 

Gini <- function(observed, predicted){
  n = length(observed)
  i = 1:n
  y = observed[order(predicted)]
  
  G = 1 / (n - 1) * (n + 1 - 2 * ( sum( (n + 1 - i) * y )  ) / sum(y) )
  return(G)
}


normalized_gini <- function(observed, predicted){
  Gini(observed, predicted) / Gini(observed,observed)
}  

