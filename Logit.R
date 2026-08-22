rm(list=ls())

library(ChoiceModelR)

wd   = "/Users/yu-cheng/Desktop/Yucheng/ML_CBC-PROBIT/"
data = read.csv(paste(wd, "cam_input.csv", sep = ""), header = T)
choi = data[,ncol(data)]

nchoice = 7
choi    = matrix(choi, nrow(data)/nchoice, nchoice, byrow=T)

CHOI = NULL
for(c in 1:nrow(choi))
{
	task = c(which(choi[c,]==1), rep(0,nchoice-1-1)) # nchoice -1 (-1) because of none
	CHOI = c(CHOI, task)
}

# to deal with 'none'
label = 1:nrow(data)
data = data[-which(label %% nchoice ==0),]

data[,ncol(data)] = CHOI

xcoding = c(0,0,0,0,0,0,0,0)
mcmc = list(R = 40000, use = 20000)

options = list(none=TRUE, save=TRUE, keep=5, wgt=1)

attlevels = c(3, 3, 6, 2, 3, 2, 4, 4)

out = choicemodelr(data, xcoding, mcmc = mcmc, options = options, directory = wd)
beta = apply(out$betadraw, 1:2, mean)

None = beta[,ncol(beta)]

estbetas = cbind(
	beta[,1:2],  0-apply(beta[,1:2],1,sum),
	beta[,3:4],  0-apply(beta[,3:4],1,sum),
	beta[,5:9],  0-apply(beta[,5:9],1,sum),
	beta[,10],   0-beta[,10],
	beta[,11:12],0-apply(beta[,11:12],1,sum),
	beta[,13],   0-beta[,13],
	beta[,14:16],0-apply(beta[,14:16],1,sum),
	beta[,17:19],0-apply(beta[,17:19],1,sum),
	None
)

colnames(estbetas) = c("Body_L","Body_M","Body_H","Chng_N","Chng_M","Chng_A",
"Anno_0","Anno_1","Anno_2","Anno_3","Anno_4","Anno_5","Optn_N","Optn_Y","Zoom_0","Zoom_2","Zoom_4",
"View_R","View_L","Set_N","Set_L","Set_V","Set_B","Prce_1","Prce_2","Prce_3","Prce_4","None")

as.matrix(apply(estbetas, 2, mean))

write.csv(estbetas, paste(wd, "logit_result.csv" , sep=""), row.names=F)

save.image(paste(wd, "logit_result.RData", sep=""))
