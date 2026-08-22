rm(list=ls())

DummyKminus1 = function(Level)
{
	B     = Level
	N     = nrow(B)
	n_att = ncol(B)

	end    = 0
	Design = rep(0, N)
	for(i in 1:n_att)
	{
		att = B[,i]
		Design1 = matrix(0, N, length(unique(att))-1)
	
		end = end + length(unique(att))

		for(k in 1:max(unique(att)))
		{
			for(t in 1:N)
			{
				if(B[t,i] == k)
				{
					Design1[t,k] = 1
				}
			}
		}

		Design = cbind(Design, Design1[,-ncol(Design1)])
	}

	Design = Design[,-1]
	return(Design)
}


result_RLH = NULL
rank_RLH   = NULL

logitd = "/Users/yu-cheng/Desktop/Yucheng/ML_CBC-PROBIT/"
datad  = "/Users/yu-cheng/Desktop/Yucheng/ML_CBC-PROBIT/"

load(paste(logitd, "logit_result.RData", sep=""))
data = read.csv(paste(datad, "cam_input.csv", sep = ""), header = T)
choi = data[,ncol(data)]

betadraw = out$betadraw
beta     = apply(betadraw, 1:2, mean)

N      = dim(betadraw)[1]
n.iter = dim(betadraw)[3]
Ncard  = 10
Nm     = nchoice

X    = data[,-c(1:3,ncol(data))]
XDUM = DummyKminus1(X)
none = rep(c(rep(0,Nm-1),1), N*Ncard)
XDUM = cbind(XDUM, none)

RLH = NULL

for(i in 1:n.iter)
{
	RLH.i = NULL

	for(j in 1:N)
	{
		choi.ind = choi[((j-1)*(Ncard*Nm)+1):((j-1)*(Ncard*Nm)+Ncard*Nm)]
		choi.ind = matrix(choi.ind, Nm, Ncard, byrow=F)
		task.set = XDUM[((j-1)*(Ncard*Nm)+1):((j-1)*(Ncard*Nm)+Ncard*Nm),]
		
		raw.u = exp(matrix(as.vector(task.set %*% betadraw[j,,i]), Nm, Ncard, byrow=F))
		sum.u = apply(raw.u, 2, sum)

		p.cho = apply( t(t(raw.u)/sum.u) * choi.ind, 2, sum)

		RLH.i = c(RLH.i, exp(mean(log(p.cho))))
	}

	RLH  = rbind(RLH, RLH.i)
}

RLH.order  = apply(apply(RLH, 1, rank), 1, mean)
RLH.rank   = cbind(rank_RLH, rank(RLH.order))
	
RLH.mean   = apply(RLH, 2, mean)
result_RLH = cbind(result_RLH, RLH.mean)

write.csv(result_RLH, paste(logitd, "RLH.csv",      sep=""), row.names=F)
write.csv(RLH.rank,   paste(logitd, "RLH_rank.csv", sep=""), row.names=F)




