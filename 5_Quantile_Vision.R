####################### PD ##################################

qtile_indice <- ntile(PD$INC,10)
Q_PD <- data.frame(PD,qtile_indice)
Q_PD$INC
list_mean_qtile_obs_value = list()
for (i in 1:10){
  list_mean_qtile_obs_value = rbind(list_mean_qtile_obs_value,mean(filter(Q_PD,Q_PD$qtile_indice==i)$INC))
}

PD_predict = cap_predict_elnet(PD,1,2000000)
PD_predict$inc

qtile_indice_pred <- ntile(PD_predict$inc,10)
PD_predict <- data.frame(as.data.frame(PD_predict$inc),qtile_indice_pred)
list_mean_qtile_pred_value = list()
for (i in 1:10){
  list_mean_qtile_pred_value = rbind(list_mean_qtile_pred_value,mean(filter(PD_predict,PD_predict$qtile_indice_pred==i)$PD_predict.inc))
}

PD_predict_test = cap_predict_elnet(PD,1,2000000) #using lambda of with intercept in no intercept
PD_predict_test$inc

qtile_indice_pred2 <- ntile(PD_predict_test$inc,10)
PD_predict_test <- data.frame(as.data.frame(PD_predict_test$inc),qtile_indice_pred2)
list_mean_qtile_pred_value2 = list()
for (i in 1:10){
  list_mean_qtile_pred_value2 = rbind(list_mean_qtile_pred_value2,mean(filter(PD_predict_test,PD_predict_test$qtile_indice_pred2==i)$PD_predict_test.inc))
}

PD_predict_test2 = cap_predict_elnet(PD,0.001,2000000) #using lambda of with intercept in no intercept + small alpha value
PD_predict_test2$inc
qtile_indice_pred3 <- ntile(PD_predict_test2$inc,10)
PD_predict_test2 <- data.frame(as.data.frame(PD_predict_test2$inc),qtile_indice_pred3)
list_mean_qtile_pred_value3 = list()
for (i in 1:10){
  list_mean_qtile_pred_value3 = rbind(list_mean_qtile_pred_value3,mean(filter(PD_predict_test2,PD_predict_test2$qtile_indice_pred3==i)$PD_predict_test2.inc))
}

line = seq(1:10)

nfile = paste("PD_Quantile_obs_pred", ".png", sep="")
png(nfile, 1000, 800)

plot(line, list_mean_qtile_obs_value, xlab = "PD Quantile number", ylab = "PD Mean INC value / Quantile", pch = 3, lwd = 2, xaxt = "n", col = "black")

legend("topright", legend=c("Mean INC average by quantile"), col = c("black"), pch = 3, lwd = 2, bty = "n")
lines(line, list_mean_qtile_pred_value2, col = c("red"))
lines(line,list_mean_qtile_obs_value,col = c("black"))
axis(1, at=line, labels=paste(line, "", sep = ""))

dev.off()


PD_predict_test3 = cap_predict_elnet(PD,0,2000000)
PD_predict_test2 <- PD_predict_test3




####################### PD ##################################
qtile_indice_ODdrive <- ntile(ODdrive$INC,10)
Q_ODdrive <- data.frame(ODdrive,qtile_indice_ODdrive)
Q_ODdrive$INC
list_mean_qtile_obs_value_ODdrive = list()
for (i in 1:10){
  list_mean_qtile_obs_value_ODdrive = rbind(list_mean_qtile_obs_value_ODdrive,mean(filter(Q_ODdrive,Q_ODdrive$qtile_indice_ODdrive==i)$INC))
}

ODdrive_predict = cap_predict_elnet(ODdrive,1,2000000)
ODdrive_predict$inc

qtile_indice_pred_ODdrive <- ntile(ODdrive_predict$inc,10)
ODdrive_predict <- data.frame(as.data.frame(ODdrive_predict$inc),qtile_indice_pred_ODdrive)
list_mean_qtile_pred_value_ODdrive = list()
for (i in 1:10){
  list_mean_qtile_pred_value_ODdrive = rbind(list_mean_qtile_pred_value_ODdrive,mean(filter(ODdrive_predict,ODdrive_predict$qtile_indice_pred_ODdrive==i)$ODdrive_predict.inc))
}


nfile = paste("ODdrive_Quantile_obs_pred", ".png", sep="")
png(nfile, 1000, 800)

line = seq(1:10)
plot(line, list_mean_qtile_obs_value_ODdrive, xlab = "Threshold *Millions", ylab = "Normalized GINI", pch = 3, lwd = 2, xaxt = "n", col = "black")

legend("topright", legend=c("Mean INC average by quantile"), col = c("black"), pch = 3, lwd = 2, bty = "n")
lines(line, list_mean_qtile_pred_value_ODdrive, col = c("red"))
lines(line,list_mean_qtile_obs_value_ODdrive,col = c("black"))
axis(1, at=line, labels=paste(line, "", sep = ""))

dev.off()






























