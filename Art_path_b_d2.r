# A2 <- "2.079*(x^0.515)*((1-x)^0.806)"
co_A2 <- 2.079
m_A2 <- 0.515 
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

#++++++++++++++++++++++++++++
time.s=0
Ts=0
TE=500
AA=10^6
E=100000
qq=c(1,2,5,10,20,50)
coref=co_A3
mref=m_A3
nref=n_A3
np=10^3
pr=10^-2 
refMod="D2"
mystart1=list(co = 1,  m = 0.5 , n=0.5)
mystart2=list(m = 0.5 , n=0.5)
amin=0.05
amax=0.95
my_step=0.05
myptol=10^-64
myftol=10^-64
myset="A2"
deg <- seq(0.05,0.95,0.005)

coeffA2  <- c(co_A2,m_A2,n_A2)
coeffD2  <- c(co_D2,m_D2,n_D2)
coeffD2  <- c(co_D2,m_D2,n_D2)
coeffD3  <- c(co_D3,m_D3,n_D3)


time_start=0
Temp_start=0
Temp_End=800
A=10^6
Ea=100000
rate=c(10)
alfa_degree=seq(0.05,0.95,0.01)
alfa_min=0.05
alfa_max=0.95
co_reference=co_D2
m_reference=m_D2
n_reference=n_D2
fitted_co=D2c106$co
fitted_m=D2c106$m
fitted_n=D2c106$n

D2setP3S3 <- create_set_gt(time.s=time_start,Ts=Temp_start,TE=Temp_End,AA=A,E=Ea,qq=rate, np=10^3,pr=10^-3,refMod="D2",deg=alfa_degree,amin=alfa_min,amax=alfa_max)
D2setP3S4 <- create_set_gt(time.s=time_start,Ts=Temp_start,TE=Temp_End,AA=A,E=Ea,qq=rate, np=10^3,pr=10^-4,refMod="D2",deg=alfa_degree,amin=alfa_min,amax=alfa_max)
D2setP3S6 <- create_set_gt(time.s=time_start,Ts=Temp_start,TE=Temp_End,AA=A,E=Ea,qq=rate, np=10^3,pr=10^-6,refMod="D2",deg=alfa_degree,amin=alfa_min,amax=alfa_max)
D2setP3S8 <- create_set_gt(time.s=time_start,Ts=Temp_start,TE=Temp_End,AA=A,E=Ea,qq=rate, np=10^3,pr=10^-8,refMod="D2",deg=alfa_degree,amin=alfa_min,amax=alfa_max)
D2setP3S12 <- create_set_gt(time.s=time_start,Ts=Temp_start,TE=Temp_End,AA=A,E=Ea,qq=rate,np=10^3,pr=10^-12,refMod="D2",deg=alfa_degree,amin=alfa_min,amax=alfa_max)

D2setP4S3 <- create_set_gt(time.s=time_start,Ts=Temp_start,TE=Temp_End,AA=A,E=Ea,qq=rate, np=10^4,pr=10^-3,refMod="D2",deg=alfa_degree,amin=alfa_min,amax=alfa_max)
D2setP4S4 <- create_set_gt(time.s=time_start,Ts=Temp_start,TE=Temp_End,AA=A,E=Ea,qq=rate, np=10^4,pr=10^-4,refMod="D2",deg=alfa_degree,amin=alfa_min,amax=alfa_max)
D2setP4S6 <- create_set_gt(time.s=time_start,Ts=Temp_start,TE=Temp_End,AA=A,E=Ea,qq=rate, np=10^4,pr=10^-6,refMod="D2",deg=alfa_degree,amin=alfa_min,amax=alfa_max)
D2setP4S8 <- create_set_gt(time.s=time_start,Ts=Temp_start,TE=Temp_End,AA=A,E=Ea,qq=rate, np=10^4,pr=10^-8,refMod="D2",deg=alfa_degree,amin=alfa_min,amax=alfa_max)
D2setP4S12 <- create_set_gt(time.s=time_start,Ts=Temp_start,TE=Temp_End,AA=A,E=Ea,qq=rate,np=10^4,pr=10^-12,refMod="D2",deg=alfa_degree,amin=alfa_min,amax=alfa_max)

D2setP5S3 <- create_set_gt(time.s=time_start,Ts=Temp_start,TE=Temp_End,AA=A,E=Ea,qq=rate, np=10^5,pr=10^-3,refMod="D2",deg=alfa_degree,amin=alfa_min,amax=alfa_max)
D2setP5S4 <- create_set_gt(time.s=time_start,Ts=Temp_start,TE=Temp_End,AA=A,E=Ea,qq=rate, np=10^5,pr=10^-4,refMod="D2",deg=alfa_degree,amin=alfa_min,amax=alfa_max)
D2setP5S6 <- create_set_gt(time.s=time_start,Ts=Temp_start,TE=Temp_End,AA=A,E=Ea,qq=rate, np=10^5,pr=10^-6,refMod="D2",deg=alfa_degree,amin=alfa_min,amax=alfa_max)
D2setP5S8 <- create_set_gt(time.s=time_start,Ts=Temp_start,TE=Temp_End,AA=A,E=Ea,qq=rate, np=10^5,pr=10^-8,refMod="D2",deg=alfa_degree,amin=alfa_min,amax=alfa_max)
D2setP5S12 <- create_set_gt(time.s=time_start,Ts=Temp_start,TE=Temp_End,AA=A,E=Ea,qq=rate,np=10^5,pr=10^-12,refMod="D2",deg=alfa_degree,amin=alfa_min,amax=alfa_max)

# D2setP6S3 <- create_set_gt(time.s=time_start,Ts=Temp_start,TE=Temp_End,AA=A,E=Ea,qq=rate, np=10^6,pr=10^-3,refMod="D2",deg=alfa_degree,amin=alfa_min,amax=alfa_max)
# D2setP6S4 <- create_set_gt(time.s=time_start,Ts=Temp_start,TE=Temp_End,AA=A,E=Ea,qq=rate, np=10^6,pr=10^-4,refMod="D2",deg=alfa_degree,amin=alfa_min,amax=alfa_max)
# D2setP6S6 <- create_set_gt(time.s=time_start,Ts=Temp_start,TE=Temp_End,AA=A,E=Ea,qq=rate, np=10^6,pr=10^-6,refMod="D2",deg=alfa_degree,amin=alfa_min,amax=alfa_max)
# D2setP6S8 <- create_set_gt(time.s=time_start,Ts=Temp_start,TE=Temp_End,AA=A,E=Ea,qq=rate, np=10^6,pr=10^-8,refMod="D2",deg=alfa_degree,amin=alfa_min,amax=alfa_max)
# D2setP6S12 <- create_set_gt(time.s=time_start,Ts=Temp_start,TE=Temp_End,AA=A,E=Ea,qq=rate,np=10^6,pr=10^-12,refMod="D2",deg=alfa_degree,amin=alfa_min,amax=alfa_max)


D2setP3S3$dat_gt$pset <- "p3"
D2setP3S4$dat_gt$pset <- "p3"
D2setP3S6$dat_gt$pset <- "p3"
D2setP3S8$dat_gt$pset <- "p3"
D2setP3S12$dat_gt$pset <- "p3"

D2setP4S3$dat_gt$pset <- "p4" 
D2setP4S4$dat_gt$pset <- "p4" 
D2setP4S6$dat_gt$pset <- "p4" 
D2setP4S8$dat_gt$pset <- "p4" 
D2setP4S12$dat_gt$pset <- "p4"

D2setP5S3$dat_gt$pset <- "p5" 
D2setP5S4$dat_gt$pset <- "p5" 
D2setP5S6$dat_gt$pset <- "p5" 
D2setP5S8$dat_gt$pset <- "p5" 
D2setP5S12$dat_gt$pset <- "p5"

# D2setP6S3$dat_gt$pset <- "p6" 
# D2setP6S4$dat_gt$pset <- "p6" 
# D2setP6S6$dat_gt$pset <- "p6" 
# D2setP6S8$dat_gt$pset <- "p6" 
# D2setP6S12$dat_gt$pset <- "p6"

D2setP3S3$dat_gt$sset <- "s3"
D2setP3S4$dat_gt$sset <- "s4"
D2setP3S6$dat_gt$sset <- "s6"
D2setP3S8$dat_gt$sset <- "s8"
D2setP3S12$dat_gt$sset <- "s12"

D2setP4S3$dat_gt$sset <- "s3" 
D2setP4S4$dat_gt$sset <- "s4" 
D2setP4S6$dat_gt$sset <- "s6" 
D2setP4S8$dat_gt$sset <- "s8" 
D2setP4S12$dat_gt$sset <- "s12"

D2setP5S3$dat_gt$sset <- "s3" 
D2setP5S4$dat_gt$sset <- "s4" 
D2setP5S6$dat_gt$sset <- "s6" 
D2setP5S8$dat_gt$sset <- "s8" 
D2setP5S12$dat_gt$sset <- "s12"

# D2setP6S3$dat_gt$sset <- "s3" 
# D2setP6S4$dat_gt$sset <- "s4" 
# D2setP6S6$dat_gt$sset <- "s6" 
# D2setP6S8$dat_gt$sset <- "s8" 
# D2setP6S12$dat_gt$sset <- "s12"


alldatD2 <- rbind(D2setP3S3$dat_gt,
				  D2setP3S4$dat_gt,
				  D2setP3S6$dat_gt,
				  D2setP3S8$dat_gt,
				  D2setP3S12$dat_gt,
				  D2setP4S3$dat_gt,
				  D2setP4S4$dat_gt, 
				  D2setP4S6$dat_gt, 
				  D2setP4S8$dat_gt,
				  D2setP4S12$dat_gt, 
				  D2setP5S3$dat_gt,
				  D2setP5S4$dat_gt,
				  D2setP5S6$dat_gt,
				  D2setP5S8$dat_gt,
				  D2setP5S12$dat_gt )


p_T_vs_dadt <- ggplot(alldatD2)  +   geom_line(aes(temperature.s, dadt, col=sset))  +   facet_wrap(~ pset, scales="free",ncol = 6)
p1 <- p_T_vs_dadt
p2  <- p1  +  plot_annotation(title = 'D2 points 10^3')
p3 <- p2 +  xlim(200,600) + theme(legend.position="none")
patchwork1 <- p2 / p3 
patchwork1+ plot_annotation(title = 'D2 p=points s=precision ground truth') 
ggsave("D2_T_dadt.png", width =32, height = 24, units = "cm",dpi=1200) 

p_T_vs_fa <- ggplot(alldatD2)    +   geom_line(aes(temperature.s, fa, col=sset))  +   facet_wrap(~ pset, scales="free",ncol = 6)
p5 <- p_T_vs_fa
p6 <- p5 +  xlim(0,100) + theme(legend.position="none")
patchwork2 <- p5 / p6 
patchwork2 + plot_annotation(title = 'D2 p=points s=precision ground truth') 
ggsave("D2_T_vs_fa.png", width =32, height = 24, units = "cm",dpi=1200) 

p_T_vs_fan <- ggplot(alldatD2)    +   geom_line(aes(temperature.s, fan, col=sset))  +   facet_wrap(~ pset, scales="free",ncol = 6)
p8 <- p_T_vs_fa
p9 <- p8 +  xlim(0,50) + theme(legend.position="none")
patchwork3 <- p8 / p9 
patchwork3 + plot_annotation(title = 'D2 p=points s=precision ground truth') 
ggsave("D2_T_vs_fan.png", width =32, height = 24, units = "cm",dpi=1200) 


mystart1=list(co=1,m=-0.5,n=1)


D2setP3S4fit <- sfitSB(nameset="D2setP3S4",D2setP3S4$dat_gt,time.s=time_start, Ts=Temp_start, TE=Temp_End, AA=A, E=Ea, qq=rate, np=10^3, pr=10^-4, refMod="D2", mystart=mystart1,amin=0.05,amax=0.95,my_step,coref=0)

D2setP4S3fit <- sfitSB(nameset="D2setP4S3",D2setP4S3$dat_gt,time.s=time_start, Ts=Temp_start, TE=Temp_End, AA=A, E=Ea, qq=rate, np=10^4, pr=10^-3, refMod="D2", mystart=mystart1,amin=0.05,amax=0.95,my_step,coref=0)
D2setP4S4fit <- sfitSB(nameset="D2setP4S4",D2setP4S4$dat_gt,time.s=time_start, Ts=Temp_start, TE=Temp_End, AA=A, E=Ea, qq=rate, np=10^4, pr=10^-4, refMod="D2", mystart=mystart1,amin=0.05,amax=0.95,my_step,coref=0)

D2setP5S3fit <- sfitSB(nameset="D2setP5S3",D2setP5S3$dat_gt,time.s=time_start, Ts=Temp_start, TE=Temp_End, AA=A, E=Ea, qq=rate, np=10^5, pr=10^-3, refMod="D2", mystart=mystart1,amin=0.05,amax=0.95,my_step,coref=0)
D2setP5S4fit <- sfitSB(nameset="D2setP5S4",D2setP5S4$dat_gt,time.s=time_start, Ts=Temp_start, TE=Temp_End, AA=A, E=Ea, qq=rate, np=10^5, pr=10^-4, refMod="D2", mystart=mystart1,amin=0.05,amax=0.95,my_step,coref=0)

# D2setP6S3fit <- sfitSB(nameset="D2setP6S3",D2setP6S3$dat_gt,time.s=time_start, Ts=Temp_start, TE=Temp_End, AA=A, E=Ea, qq=rate, np=10^5, pr=10^-3, refMod="D2", mystart=mystart1,amin=0.05,amax=0.95,my_step,coref=0)
# D2setP6S4fit <- sfitSB(nameset="D2setP6S4",D2setP6S4$dat_gt,time.s=time_start, Ts=Temp_start, TE=Temp_End, AA=A, E=Ea, qq=rate, np=10^5, pr=10^-4, refMod="D2", mystart=mystart1,amin=0.05,amax=0.95,my_step,coref=0)
# D2setP6S6fit <- sfitSB(nameset="D2setP6S6",D2setP6S6$dat_gt,time.s=time_start, Ts=Temp_start, TE=Temp_End, AA=A, E=Ea, qq=rate, np=10^5, pr=10^-6, refMod="D2", mystart=mystart1,amin=0.05,amax=0.95,my_step,coref=0)
# D2setP6S8fit <- sfitSB(nameset="D2setP6S8",D2setP6S8$dat_gt,time.s=time_start, Ts=Temp_start, TE=Temp_End, AA=A, E=Ea, qq=rate, np=10^5, pr=10^-8, refMod="D2", mystart=mystart1,amin=0.05,amax=0.95,my_step,coref=0)
# D2setP6S12fit <- sfitSB(nameset="D2setP6S12",D2setP6S12$dat_gt,time.s=time_start, Ts=Temp_start, TE=Temp_End, AA=A, E=Ea, qq=rate, np=10^5, pr=10^-12, refMod="D2", mystart=mystart1,amin=0.05,amax=0.95,my_step,coref=0)



D2setP3S4fit$dat_fit$pset <- "p3"

D2setP4S3fit$dat_fit$pset <- "p4" 
D2setP4S4fit$dat_fit$pset <- "p4" 

D2setP5S3fit$dat_fit$pset <- "p5" 
D2setP5S4fit$dat_fit$pset <- "p5" 

# D2setP6S3fit$dat_fit$pset <- "p6" 
# D2setP6S4fit$dat_fit$pset <- "p6" 
# D2setP6S6fit$dat_fit$pset <- "p6" 
# D2setP6S8fit$dat_fit$pset <- "p6" 
# D2setP6S12fit$dat_fit$pset <- "p6"


D2setP3S4fit$dat_fit$sset <- "s4"

D2setP4S3fit$dat_fit$sset <- "s3" 
D2setP4S4fit$dat_fit$sset <- "s4" 

D2setP5S3fit$dat_fit$sset <- "s3" 
D2setP5S4fit$dat_fit$sset <- "s4" 

# D2setP6S3fit$dat_fit$sset <- "s3" 
# D2setP6S4fit$dat_fit$sset <- "s4" 
# D2setP6S6fit$dat_fit$sset <- "s6" 
# D2setP6S8fit$dat_fit$sset <- "s8" 
# D2setP6S12fit$dat_fit$sset <- "s12"



alldatD2reg <- rbind(
D2setP3S4fit$dat_fit,
D2setP4S3fit$dat_fit,
D2setP4S4fit$dat_fit,
D2setP5S3fit$dat_fit,
D2setP5S4fit$dat_fit)

# D2setP4S12$dat_fit)

#p_T_vs_dadt <- ggplot(alldatD2reg)  +   geom_line(aes(temperature.s, dadt, col=sset))  +   facet_wrap(~ pset, scales="free",ncol = 6)
#p_T_vs_fa <- ggplot(alldatD2reg)    +   geom_line(aes(temperature.s, fa, col=sset))  +   facet_wrap(~ pset, scales="free",ncol = 6)
#p_T_vs_fan <- ggplot(alldatD2reg)    +   geom_line(aes(temperature.s, fan, col=sset))  +   facet_wrap(~ pset, scales="free",ncol = 6)

preg_T_vs_dadt <- ggplot(alldatD2reg)  +   geom_line(aes(temperature.s, dadt, col=sset))  +   facet_wrap(~ pset, scales="free",ncol = 6)
p51 <- preg_T_vs_dadt
p51+ plot_annotation(title = 'D2  p=points s=precision fit') 
ggsave("D2_fit_T_dadt.png", width =32, height = 24, units = "cm",dpi=1200) 

p_T_vs_fa <- ggplot(alldatD2reg)    +   geom_line(aes(temperature.s, fa, col=sset))  +   facet_wrap(~ pset, scales="free",ncol = 6)
p55 <- p_T_vs_fa
p55 + plot_annotation(title = 'D2  p=points s=precision fit') 
ggsave("D2_fit_T_vs_fa.png", width =32, height = 24, units = "cm",dpi=1200) 

p_T_vs_fan <- ggplot(alldatD2reg)    +   geom_line(aes(temperature.s, fan, col=sset))  +   facet_wrap(~ pset, scales="free",ncol = 6)
p58 <- p_T_vs_fa

p58 + plot_annotation(title = 'D2 p=points s=precision ground truth') 
ggsave("D2_fit_T_vs_fan.png", width =32, height = 24, units = "cm",dpi=1200) 

alldatD2reg$fitset <- "fit"
alldatD2reg <- alldatD2reg[,c("id_cycle","id"):=NULL]
alldatD2$fitset <- "gt"

D2cset <- rbind(alldatD2,alldatD2reg)

t1 <- ggplot(D2cset)  +   geom_line(aes(temperature.s, dadt, col=fitset))  +   facet_wrap(~ sset + pset, scales="free",ncol = 3)
t1 <-  t1  +  plot_annotation(title = 'D2 p=points s=precision')
ggsave("D2_fit_T_dadt_p1.png", width =32, height = 48, units = "cm",dpi=1200) 

