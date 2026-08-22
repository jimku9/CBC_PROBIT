The code and data are for the implementation of the work "Is what you choose what you want?
¡Xoutlier detection in choice-based conjoint analysis" (doi: 10.1007/s11002-015-9389-3) by 
Ku et al. (2015, Marketing Letters).

The camera CBC data can be downloaded at Professor Rossi's website:
http://www.perossi.org/home/bsm-1. 

The csv files in this package are of a reshaped version.

The cam_all.csv contains the entire data set which contains 14 choice tasks.

The cam_input contains only the first 10 choice tasks for model fitting and training purpose.

The holdout_choice.csv and holdout_design.csv contain the remaining 4 choice tasks for holdout
validation purposes.

###############
# The R files #
###############

ID Convert.R: used to create pseudo respondent ID number.

Dummy k-1-none.R: used to convert the original level-coded data into dummy-coded data 
matrix. This conversion only applies to CBC that includes the "No-buy" choice.

Main-HB_CAM.R: the main file which read in the data, call the MCMC procedure, and then
return the results. The LARD and UD metrics are generated here.

HB_ALL.R: to implement the MCMC estimation for the hierarchical Bayes multinomial probit
model considered in this paper.

ZGen1.R: to implement the "Gibbes thru" sampling for the latent utilities, where a unit
variance is assumed in the prior.

Logit.R: used to generate the logic utility estimates and save the logic MCMC draws.

Get_RLH.R: to generate the RLH metrics based on logic draws

Hit Rate.R: Obtain the hit rates using the three metrics: UD, LARD, and RLH, based on logit utilities

###################
# Run Instruction #
###################

1. Run Main-HB_CAM.R
2. Run Logit.R
3. Run Get_RLH.R
4. Run Hit Rate.R (select the metrics among UD, LARD, and RLH)

If you have any questions, please write to jimyku@gmail.com

Copyright (c) 2015. Yu-Cheng Ku. All Rights Reserved.