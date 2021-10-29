#+++++++°+++++++++°+++++++++°+++++++++°+++++++++°+++++++++°+++++++++°+++++++++°
# list of models
#(1 - a)^n a^m
# A3 <- "2.079*(x^0.515)*((1-x)^0.806)"
 co_A2 <- 2.079
 m_A2 <- 0.515 #0.515
 n_A2 <- 0.806
# A3 <- "3.192*(x^0.693)*((1-x)^0.748)"
 co_A3 <- 3.102
 m_A3 <- 0.693
 n_A3 <- 0.748
# D2 <- "0.973*(x^(-1.008*))*((1-x)^0.425)"
 co_D2 <- 0.973
 m_D2 <- -1.008
 n_D2 <- 0.425
# D3 <- "4.431*(x^(-1.004))*((1-x)^0.951)"
 co_D3 <- 4.431
 m_D3 <- -1.004
 n_D3 <- 0.951
# r2sb <- "(1-x)^(1/2)"
# r3sb <- "(1-x)^(2/3)"

# time.s=0
# Ts=0
# TE=500
# AA=10^6
# E=100000
# qq=c(1,2,5,10,20,50)
# coref=co_A3
# mref=m_A3
# nref=n_A3
# np=10^3
# pr=10^-2 
# refMod="A3"
# mystart1=list(co = 1,  m = 0.5 , n=0.5)
# mystart2=list(m = 0.5 , n=0.5)
# amin=0.05
# amax=0.95
# my_step=0.05
# myptol=10^-32
# myftol=10^-32
# myset="A3"

mystart2=list(m = 0.5 , n=0.5)

A2set1 <- simmy(time.s=0, Ts=0, TE=500,
				  AA=10^6, E=100000, qq=c(1,2,5,10,20,50), 
				  coref=co_A2, mref=m_A2, nref=n_A2, 
				  np=10^3, pr=10^-2, 
				  refMod="A2",
				  mystart=list(co = 1,  m = 0.5 , n=0.5),
				  amin=0.05,amax=0.95,my_step=0.05,
			      myptol=10^-64,
				  myftol=10^-64,myset="A2set1",mytitle="A2set12")

  

A2set12 <- simmy(time.s=0, Ts=0, TE=500,
				  AA=10^6, E=100000, qq=c(1,2,5,10,20,50), 
				  coref=co_A2, mref=m_A2, nref=n_A2, 
				  np=10^5, pr=10^-12, 
				  refMod="A2",
				  mystart=list(co = 1,  m = 0.5 , n=0.5),
				  amin=0.05,amax=0.95,my_step=0.05,
			      myptol=10^-64,
				  myftol=10^-64,myset="A2set12",mytitle="A2set12")



A3set1 <- simmy(time.s=0, Ts=0, TE=500,
				  AA=10^6, E=100000, qq=c(1,2,5,10,20,50), 
				  coref=co_A3, mref=m_A3, nref=n_A3, 
				  np=10^3, pr=10^-3, 
				  refMod="A3",
				  mystart=list(co = 1,  m = 0.5 , n=0.5),
				  amin=0.05,amax=0.95,my_step=0.05,
			      myptol=10^-64,
				  myftol=10^-64,myset="A3set1",mytitle="A3set1")

	  
				  

A3set12 <- simmy(time.s=0, Ts=0, TE=500,
				  AA=10^6, E=100000, qq=c(1,2,5,10,20,50), 
				  coref=co_A3, mref=m_A3, nref=n_A3, 
				  np=10^5, pr=10^-12, 
				  refMod="A3",
				  mystart=list(co = 1,  m = 0.5 , n=0.5),
				  amin=0.05,amax=0.95,my_step=0.05,
			      myptol=10^-64,
				  myftol=10^-64,myset="A3set12",mytitle="A3set1")


D2setU <- simmy(time.s=0, Ts=0, TE=3000,
				  AA=10^6, E=100000, qq=c(1,2,5,10,20,50,100,200,500,1000), 
				  coref=co_D2, mref=m_D2, nref=n_D2, 
				  np=10^3, pr=10^-3, 
				  refMod="D2",
				  mystart=list(co = 1,  m = -0.5 , n=0.5),
				  amin=0.05,amax=0.95,my_step=0.05,
			      myptol=10^-64,
				  myftol=10^-64,myset="D2setU",mytitle="D2setU")



D3setU <- simmy(time.s=0, Ts=0, TE=3000,
				  AA=10^6, E=100000, qq=c(1,2,5,10,20,50,100,200,500,1000), 
				  coref=co_D3, mref=m_D3, nref=n_D3, 
				  np=10^3, pr=10^-3, 
				  refMod="D3",
				  mystart=list(co = 1,  m = -0.5 , n=0.5),
				  amin=0.05,amax=0.95,my_step=0.05,
			      myptol=10^-64,
				  myftol=10^-64,myset="D3setU",mytitle="D3setU")



deg <- seq(amin,amax,my_step)


#++°+++++++++°+++++++++°+++++++++°+++++++++°+++++++++°+++++++++°+++++++++°
# XLS SAVE OF EA
#++°+++++++++°+++++++++°+++++++++°+++++++++°+++++++++°+++++++++°+++++++++°
A3<- createWorkbook("A3")
addWorksheet(A3,"EA")
xlsEA(A3,"EA",out=A3set1,setlab="A3set1",space=5,startpos=1)
xlsEA(A3,"EA",out=A3set1f,setlab="A3set1_fixed",space=5,startpos=25)
saveWorkbook(A3, "A3.xlsx", overwrite = TRUE)
#++°+++++++++°+++++++++°+++++++++°+++++++++°+++++++++°+++++++++°+++++++++

#++°+++++++++°+++++++++°+++++++++°+++++++++°+++++++++°+++++++++°+++++++++°
# XLS SAVE OF DATASET
#++°+++++++++°+++++++++°+++++++++°+++++++++°+++++++++°+++++++++°+++++++++°
A3set1_alldat <- list(A3set1$dat_fit,A3set1f$dat_fit_fixed,A3set1$dat_ictac,A3set1$dat_gt)
names(A3set1_alldat) <- c("dat_free","dat_fixed","dat_ictac","dat_gt")
A3_dat<- createWorkbook("A3_dat")
xlsDumpDAT(A3_dat,A3set1_alldat,nameset="A3set1")
#xlsDumpDAT(A3_dat,A3set2_alldat,nameset="A3set2")
saveWorkbook(A3_dat, "A3.xlsx", overwrite = TRUE)
#++°+++++++++°+++++++++°+++++++++°+++++++++°+++++++++°+++++++++°+++++++++°

