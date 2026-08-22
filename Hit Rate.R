#rm(list=ls())

## NOTICE !! ##
## _hetero means Non-Identity with leading diagonal = 1
## _hetero_full means fully unrestricted Non-Identity 

lev  = "h"
meas = "RLH_rank"  # RLH_rank, UD_rank or LARD_rank


dd = "/Users/yu-cheng/Desktop/Yucheng/ML_CBC-PROBIT/"
ld = "/Users/yu-cheng/Desktop/Yucheng/ML_CBC-PROBIT/"
wd = "/Users/yu-cheng/Desktop/Yucheng/ML_CBC-PROBIT/"

CUT   = c(2.5, 5, 10, 20, 50, 75, 90 ,100)


out.hit = NULL
for(cc in 1:length(CUT))
{
	cut   = CUT[cc]
	nn    = 302
	n_att = 8
	level = c(3,3,6,2,3,2,4,4)+1	


	##############
	# Logit Part #
	##############
	UTIL = read.csv(paste(ld, "logit_result.csv", sep = ""), header = T)

	if(meas == "RLH_rank")
	{
	  rlh.rank = read.csv(paste(ld, "RLH_rank.csv", sep = ""), header=T)
	  rlh.rank = cbind(c(1:dim(as.matrix(rlh.rank))[1]), rlh.rank)
	  
	  if(lev=="h")
	  {
	    Group = rlh.rank[which(rlh.rank[,2] <= quantile(rlh.rank[,2], 0.01*cut)),1]
	  }
	  
	  if(lev=="l")
	  {
	    Group = rlh.rank[which(rlh.rank[,2] >= quantile(rlh.rank[,2], 1-0.01*cut)),1]
	  }
	}
	
	if(meas == "UD_rank")
	{
		dev.rank = cbind(c(1:nn), read.csv(paste(wd, "UD_rank.csv",sep = ""), header=T))	# Bradlow

		if(lev=="h")
		{
			Group = dev.rank[which(dev.rank[,2] >= quantile(dev.rank[,2], 1-0.01*cut)),1]
		}

		if(lev=="l")
		{
			Group = dev.rank[which(dev.rank[,2] <= quantile(dev.rank[,2], 0.01*cut)),1]
		}
	}

	if(meas == "LARD_rank")
	{
	  dev.rank = cbind(c(1:nn), read.csv(paste(wd, "LARD_rank.csv",sep = ""), header=T))	# Bradlow
	  
	  if(lev=="h")
	  {
	    Group = dev.rank[which(dev.rank[,2] >= quantile(dev.rank[,2], 1-0.01*cut)),1]
	  }
	  
	  if(lev=="l")
	  {
	    Group = dev.rank[which(dev.rank[,2] <= quantile(dev.rank[,2], 0.01*cut)),1]
	  }
	}

	### change names and the number of attirbutes
	design = as.matrix(read.csv(paste(dd, "holdout_design.csv", sep = ""), header = T)[,4:(4+n_att-1)])
	true   = read.csv(paste(dd, "holdout_choice.csv", sep = ""), header = T)

	util   = UTIL[Group,]
	true   = true[Group,]

	design = design[-which(design[,1]==0),]
	true1  = true[,1]
	true2  = true[,2]
	true3  = true[,3]
	true4  = true[,4]

	NONE = util[ ,ncol(util)]
	util = util[,-ncol(util)]


	### dummy, change N and n_att
	H      = nrow(util)
	h.task = 4
	N.m    = 7
	N      = (N.m-1) * h.task

	to.keep = NULL
	for(jj in 1:length(Group))
	{
		to.keep = c(to.keep, c(((Group[jj]-1)*N+1):((Group[jj]-1)*N+N)))
	}

	design = design[to.keep,]

	Pred = NULL
	for(h in 1:H)
	{
		B = design[((h-1)*N + 1):((h-1)*N + N),]
		end   = 0
		Design = rep(0, N)
		for(i in 1:n_att)
		{
			Design1 = matrix(0, N, level[i]-1)
	
			end = end + level[i]

			for(k in 1:level[i])
			{
				for(t in 1:N)
				{
					if(B[t,i] == k)
					{
						Design1[t,k] = 1
					}
				}
			}

			Design = cbind(Design, Design1)
		}

		Truth = Design[,-1]
		utili = as.vector(as.matrix(util[h,]))


		pred.raw = c(as.vector(Truth %*% utili), NONE[h])

		pred = rep(0, h.task)

		pred[1] = which.max(pred.raw[c(1:6,   (N+1))])
		pred[2] = which.max(pred.raw[c(7:12,  (N+1))])
		pred[3] = which.max(pred.raw[c(13:18, (N+1))])
		pred[4] = which.max(pred.raw[c(19:24, (N+1))])

		Pred = rbind(Pred,pred)
	}

	hit1 = cbind(true1, Pred[,1])
	Hit1 = round(mean(hit1[,1] == hit1[,2]), 4)

	hit2 = cbind(true2, Pred[,2])
	Hit2 = round(mean(hit2[,1] == hit2[,2]), 4)

	hit3 = cbind(true3, Pred[,3])
	Hit3 = round(mean(hit3[,1] == hit3[,2]), 4)

	hit4 = cbind(true4, Pred[,4])
	Hit4 = round(mean(hit4[,1] == hit4[,2]), 4)

	hit = c(Hit1, Hit2, Hit3, Hit4)
	HIT = round(100*mean(hit),1)

	out.hit = c(out.hit, HIT)
}

if(lev == "h")
{
	out.hit = as.matrix(out.hit)
	rownames(out.hit) = c("H2.5", "H5", "H10", "H20", "H50", "H75", "H90", "Entire")
	colnames(out.hit) = "Hite Rate"
	print(out.hit)
}




