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

funA2classic  <-  function(npoints=10^2){
alpha <- seq(0,1,length=npoints)
A2=2*(1-alpha)*((-log(1-alpha))^(1/2))
alpha <- alpha[1:(npoints)-1]
A2 <- A2[1:(npoints)-1]
dfxy <- data.frame(alpha,A2)
mystart=list(co = 1,  m = 0.5 , n=0.5)
free_fun <- function(alpha,co,m,n)(co*(alpha^m*(1-alpha)^n))
mod_fit <- nlsLM(A2~free_fun(alpha,co,m,n),start=mystart,data=dfxy)
fit_co <- summary(mod_fit)$coeff[1]
fit_m  <- summary(mod_fit)$coeff[2]
fit_n  <- summary(mod_fit)$coeff[3]
mylist=list("co"=fit_co,"m"=fit_m,"n"=fit_n,"mod_fit"=mod_fit,"dfxy"=dfxy)
return(mylist)
}

#A2c102 = funA2classic(npoints=10^2)
A2c103 = funA2classic(npoints=10^3)
A2c104 = funA2classic(npoints=10^4)
A2c105 = funA2classic(npoints=10^5)
A2c106 = funA2classic(npoints=10^6)
#A2c107 = funA2classic(npoints=10^7)
#A2c108 = funA2classic(npoints=10^8)
#A2c109 = funA2classic(npoints=10^9)

A2coc <- data.frame(A2c103$co,A2c104$co,A2c105$co,A2c106$co)
colnames(A2coc) <- c("set3","set4","set5","set6")
A2mc <- data.frame(A2c103$m,A2c104$m,A2c105$m,A2c106$m)
colnames(A2mc) <- c("set3","set4","set5","set6")
A2nc <- data.frame(A2c103$n,A2c104$n,A2c105$n,A2c106$n)
colnames(A2nc) <- c("set3","set4","set5","set6")

A2cdt <- rbind(A2coc,A2mc,A2nc)
A2ictac <- c(co_A2,m_A2,n_A2)
A2cdt <- cbind(A2cdt,A2ictac)
A2cdt$parameter = c("co","m","n")
colnames(A2cdt) <- c("set3","set4","set5","set6","ictac","parameter")

A2co <- A2coc-co_A2
A2m <- A2mc-m_A2
A2n <- A2nc-n_A2

A2co$parameter <- "c"
A2m$parameter  <- "m"
A2n$parameter  <- "n"

A2diff <- rbind(A2co,A2m,A2n)
A2diff_melt <- melt(A2diff)

A2mat_melt <- melt(A2cdt)


funA3classic  <-  function(npoints=10^2){
alpha <- seq(0,1,length=npoints)
A3=3*(1-alpha)*((-log(1-alpha))^(2/3))
alpha <- alpha[1:(npoints)-1]
A3 <- A3[1:(npoints)-1]
dfxy <- data.frame(alpha,A3)
mystart=list(co = 1,  m = 0.5 , n=0.5)
free_fun <- function(alpha,co,m,n)(co*(alpha^m*(1-alpha)^n))
mod_fit <- nlsLM(A3~free_fun(alpha,co,m,n),start=mystart,data=dfxy)
fit_co <- summary(mod_fit)$coeff[1]
fit_m  <- summary(mod_fit)$coeff[2]
fit_n  <- summary(mod_fit)$coeff[3]
mylist=list("co"=fit_co,"m"=fit_m,"n"=fit_n,"mod_fit"=mod_fit,"dfxy"=dfxy)
return(mylist)
}

A3c103 = funA3classic(npoints=10^3)
A3c104 = funA3classic(npoints=10^4)
A3c105 = funA3classic(npoints=10^5)
A3c106 = funA3classic(npoints=10^6)


A3coc <- data.frame(A3c103$co,A3c104$co,A3c105$co,A3c106$co)
colnames(A3coc) <- c("set3","set4","set5","set6")
A3mc <- data.frame(A3c103$m,A3c104$m,A3c105$m,A3c106$m)
colnames(A3mc) <- c("set3","set4","set5","set6")
A3nc <- data.frame(A3c103$n,A3c104$n,A3c105$n,A3c106$n)
colnames(A3nc) <- c("set3","set4","set5","set6")

A3cdt <- rbind(A3coc,A3mc,A3nc)
A3ictac <- c(co_A3,m_A3,n_A3)
A3cdt <- cbind(A3cdt,A3ictac)
A3cdt$parameter = c("co","m","n")
colnames(A3cdt) <- c("set3","set4","set5","set6","ictac","parameter")

A3co <- A3coc-co_A3
A3m <- A3mc-m_A3
A3n <- A3nc-n_A3

A3co$parameter <- "c"
A3m$parameter  <- "m"
A3n$parameter  <- "n"

A3diff <- rbind(A3co,A3m,A3n)
A3diff_melt <- melt(A3diff)

A3mat_melt <- melt(A3cdt)


funD2classic  <-  function(npoints=10^2){
alpha <- seq(0,1,length=npoints)
D2=-1/(log(1-alpha))
alpha <- alpha[2:(npoints)]
D2 <- D2[2:(npoints)]
dfxy <- data.frame(alpha,D2)
mystart=list(co = 1,  m = -0.5 , n=0.5)
free_fun <- function(alpha,co,m,n)(co*(alpha^m*(1-alpha)^n))
mod_fit <- nlsLM(D2~free_fun(alpha,co,m,n),start=mystart,data=dfxy)
fit_co <- summary(mod_fit)$coeff[1]
fit_m  <- summary(mod_fit)$coeff[2]
fit_n  <- summary(mod_fit)$coeff[3]
mylist=list("co"=fit_co,"m"=fit_m,"n"=fit_n,"mod_fit"=mod_fit,"dfxy"=dfxy)
return(mylist)
}


D2c103 = funD2classic(npoints=10^3)
D2c104 = funD2classic(npoints=10^4)
D2c105 = funD2classic(npoints=10^5)
D2c106 = funD2classic(npoints=10^6)

D2coc <- data.frame(D2c103$co,D2c104$co,D2c105$co,D2c106$co)
colnames(D2coc) <- c("set3","set4","set5","set6")
D2mc <- data.frame(D2c103$m,D2c104$m,D2c105$m,D2c106$m)
colnames(D2mc) <- c("set3","set4","set5","set6")
D2nc <- data.frame(D2c103$n,D2c104$n,D2c105$n,D2c106$n)
colnames(D2nc) <- c("set3","set4","set5","set6")

D2cdt <- rbind(D2coc,D2mc,D2nc)
D2ictac <- c(co_D2,m_D2,n_D2)
D2cdt <- cbind(D2cdt,D2ictac)
D2cdt$parameter = c("co","m","n")
colnames(D2cdt) <- c("set3","set4","set5","set6","ictac","parameter")

D2co <- D2coc-co_D2
D2m <- D2mc-m_D2
D2n <- D2nc-n_D2

D2co$parameter <- "c"
D2m$parameter  <- "m"
D2n$parameter  <- "n"

D2diff <- rbind(D2co,D2m,D2n)
D2diff_melt <- melt(D2diff)

D2mat_melt <- melt(D2cdt)


funD3classic  <-  function(npoints=10^2){
alpha <- seq(0,1,length=npoints)
D3=((3/2)*(1-alpha)^(2/3))*(1-(1-alpha)^(1/3))^-1
alpha <- alpha[2:(npoints)]
D3 <- D3[2:(npoints)]
dfxy <- data.frame(alpha,D3)
mystart=list(co = 1,  m = -0.5 , n=0.5)
free_fun <- function(alpha,co,m,n)(co*(alpha^m*(1-alpha)^n))
mod_fit <- nlsLM(D3~free_fun(alpha,co,m,n),start=mystart,data=dfxy)
fit_co <- summary(mod_fit)$coeff[1]
fit_m  <- summary(mod_fit)$coeff[2]
fit_n  <- summary(mod_fit)$coeff[3]
mylist=list("co"=fit_co,"m"=fit_m,"n"=fit_n,"mod_fit"=mod_fit,"dfxy"=dfxy)
return(mylist)
}

D3c103 = funD3classic(npoints=10^3)
D3c104 = funD3classic(npoints=10^4)
D3c105 = funD3classic(npoints=10^5)
D3c106 = funD3classic(npoints=10^6)


D3coc <- data.frame(D3c103$co,D3c104$co,D3c105$co,D3c106$co)
colnames(D3coc) <- c("set3","set4","set5","set6")
D3mc <- data.frame(D3c103$m,D3c104$m,D3c105$m,D3c106$m)
colnames(D3mc) <- c("set3","set4","set5","set6")
D3nc <- data.frame(D3c103$n,D3c104$n,D3c105$n,D3c106$n)
colnames(D3nc) <- c("set3","set4","set5","set6")

D3cdt <- rbind(D3coc,D3mc,D3nc)
D3ictac <- c(co_D3,m_D3,n_D3)
D3cdt <- cbind(D3cdt,D3ictac)
D3cdt$parameter = c("co","m","n")
colnames(D3cdt) <- c("set3","set4","set5","set6","ictac","parameter")

D3co <- D3coc-co_D3
D3m <- D3mc-m_D3
D3n <- D3nc-n_D3

D3co$parameter <- "c"
D3m$parameter  <- "m"
D3n$parameter  <- "n"

D3diff <- rbind(D3co,D3m,D3n)
D3diff_melt <- melt(D3diff)

D3mat_melt <- melt(D3cdt)


#++++++++++++++++++++++++++++
# plot parameters
#++++++++++++++++++++++++++++

p1_A2 <- ggplot(A2diff_melt) +   geom_point(aes(parameter, value, shape=variable ,col=variable))  +   facet_wrap(~ parameter, scales="free",ncol = 3)
p2_A2 <- ggplot(A2mat_melt)  +   geom_point(aes(parameter, value, shape=variable ,col=variable))  +   facet_wrap(~ parameter, scales="free",ncol = 3)
A2pparam <- p2_A2/p1_A2


ggsave("A2.png", width =12, height = 12, units = "cm", dpi=1200)

p1_A3 <- ggplot(A3diff_melt) +   geom_point(aes(parameter, value, shape=variable ,col=variable))  +   facet_wrap(~ parameter, scales="free",ncol = 3)
p2_A3 <- ggplot(A3mat_melt)  +   geom_point(aes(parameter, value, shape=variable ,col=variable))  +   facet_wrap(~ parameter, scales="free",ncol = 3)
A3pparam <- p2_A3/p1_A3

ggsave("A3.png", width =12, height = 12, units = "cm",dpi=1200)

p1_D2 <- ggplot(D2diff_melt) +   geom_point(aes(parameter, value, shape=variable ,col=variable))  +   facet_wrap(~ parameter, scales="free",ncol = 3)
p2_D2 <- ggplot(D2mat_melt)  +   geom_point(aes(parameter, value, shape=variable ,col=variable))  +   facet_wrap(~ parameter, scales="free",ncol = 3)
D2pparam <- p2_D2/p1_D2


ggsave("D2.png", width =12, height = 12, units = "cm",dpi=1200)

p1_D3 <- ggplot(D3diff_melt) +   geom_point(aes(parameter, value, shape=variable ,col=variable))  +   facet_wrap(~ parameter, scales="free",ncol = 3)
p2_D3 <- ggplot(D3mat_melt)  +   geom_point(aes(parameter, value, shape=variable ,col=variable))  +   facet_wrap(~ parameter, scales="free",ncol = 3)
D3pparam <- p2_D3/p1_D3

ggsave("D3.png", width =12, height = 12, units = "cm",dpi=1200)

coeffA2  <- c(A2c106$co,A2c106$m,A2c106$n)
coeffA3  <- c(A3c106$co,A3c106$m,A3c106$n)
coeffD2  <- c(D2c106$co,D2c106$m,D2c106$n)
coeffD3  <- c(D3c106$co,D3c106$m,D3c106$n)

#++++++++++++++++++++++++++++

# outreg <- sreg(nameset="outreg",time_start=0,
					   # Temp_start=0,
					   # Temp_End=500,
					   # A=10^6,
					   # Ea=100000,
					   # rate=c(1,2,5,10,20,50),
					   # number_of_points=10^3,
					   # precision_start=10^-2,
					   # referenceMod="A2",
					   # alfa_degree=seq(0.05,0.95,0.01),
					   # alfa_min=0.05,
					   # alfa_max=0.95,
					   # co_reference=co_A2,
					   # m_reference=m_A2,
					   # n_reference=n_A2,
					   # fitted_co=A2c106$co,
					   # fitted_m=A2c106$m,
					   # fitted_n=A2c106$n)
					   

A2set2 <- sreg(nameset="A2set2",time_start=0, Temp_start=0,Temp_End=500, A=10^6, Ea=100000, rate=c(1,2,5,10,20,50), number_of_points=10^4, precision_start=10^-2, referenceMod="A2",
					   alfa_degree=seq(0.05,0.95,0.01), alfa_min=0.05, alfa_max=0.95,  co_reference=co_A2,  m_reference=m_A2, n_reference=n_A2, fitted_co=A2c106$co, fitted_m=A2c106$m,   fitted_n=A2c106$n)

A2s2 <- pepper(A2set2)
library(patchwork)
p1 <- A2s2$pfa | A2s2$pheat
p1 + plot_annotation(title = 'A2 precision 10^2')
ggsave("A2s2_p1.png", width =36, height = 12, units = "cm",dpi=1200) 
p2 <- A2s2$pE
p2 + plot_annotation(title = 'A2 precision 10^-2')
ggsave("A2s2_p2.png", width =16, height = 16, units = "cm",dpi=1200) 
p2m <- A2s2$pmetrics
p2m + plot_annotation(title = 'A2 precision 10^-2')
ggsave("A2s2_p3.png", width =16, height = 16, units = "cm",dpi=1200) 

A2set8 <- sreg(nameset="A2set2",time_start=0, Temp_start=0,Temp_End=500, A=10^6, Ea=100000, rate=c(1,2,5,10,20,50), number_of_points=10^4, precision_start=10^-8, referenceMod="A2",
					   alfa_degree=seq(0.05,0.95,0.01), alfa_min=0.05, alfa_max=0.95,  co_reference=co_A2,  m_reference=m_A2, n_reference=n_A2, fitted_co=A2c106$co, fitted_m=A2c106$m,   fitted_n=A2c106$n)


A2s8 <- pepper(A2set8)
library(patchwork)
p3 <- A2s8$pfa | A2s8$pheat
p3 + plot_annotation(title = 'A2 precision 10^-8')
ggsave("A2s2_p1.png", width =36, height = 12, units = "cm",dpi=1200) 
p4 <- A2s2$pE
p5 + plot_annotation(title = 'A2 precision 10^-8')
ggsave("A2s8_p2.png", width =16, height = 16, units = "cm",dpi=1200) 
p5m <- A2s8$pmetrics
p5m + plot_annotation(title = 'A2 precision 10^-8')
ggsave("A2s8_p3.png", width =16, height = 16, units = "cm",dpi=1200) 



A3set2 <- sreg(nameset="A3set2",time_start=0, Temp_start=0,Temp_End=500, A=10^6, Ea=100000, rate=c(1,2,5,10,20,50), number_of_points=10^4, precision_start=10^-2, referenceMod="A3",
					   alfa_degree=seq(0.05,0.95,0.01), alfa_min=0.05, alfa_max=0.95,  co_reference=co_A3,  m_reference=m_A3, n_reference=n_A3, fitted_co=A3c106$co, fitted_m=A3c106$m,   fitted_n=A3c106$n)

A3s2 <- pepper(A3set2)
library(patchwork)
p6 <- A3s2$pfa | A3s2$pheat
p6 + plot_annotation(title = 'A3 precision 10^2')
ggsave("A3s2_p1.png", width =36, height = 12, units = "cm",dpi=1200) 
p7 <- A3s2$pE
p7 + plot_annotation(title = 'A3 precision 10^-2')
ggsave("A3s2_p2.png", width =16, height = 16, units = "cm",dpi=1200) 
p7m <- A3s2$pmetrics
p7m + plot_annotation(title = 'A3 precision 10^-2')
ggsave("A3s2_p3.png", width =16, height = 16, units = "cm",dpi=1200) 
			   

A3set8 <- sreg(nameset="A3set2",time_start=0, Temp_start=0,Temp_End=500, A=10^6, Ea=100000, rate=c(1,2,5,10,20,50), number_of_points=10^4, precision_start=10^-8, referenceMod="A3",
					   alfa_degree=seq(0.05,0.95,0.01), alfa_min=0.05, alfa_max=0.95,  co_reference=co_A3,  m_reference=m_A3, n_reference=n_A3, fitted_co=A3c106$co, fitted_m=A3c106$m,   fitted_n=A3c106$n)
					   
A3s8 <- pepper(A3set8)
library(patchwork)
p8 <- A3s8$pfa | A3s8$pheat
p8 + plot_annotation(title = 'A3 precision 10^-8')
ggsave("A3s2_p1.png", width =36, height = 12, units = "cm",dpi=1200) 
p9 <- A3s2$pE
p9 + plot_annotation(title = 'A3 precision 10^-8')
ggsave("A3s8_p2.png", width =16, height = 16, units = "cm",dpi=1200) 
p9m <- A3s8$pmetrics
p9m + plot_annotation(title = 'A3 precision 10^-8')
ggsave("A3s8_p3.png", width =16, height = 16, units = "cm",dpi=1200) 



					   	   

D2set2 <- sreg(nameset="D2set2",time_start=0, Temp_start=0,Temp_End=500, A=10^6, Ea=100000, rate=c(1,2,5,10,20,50), number_of_points=10^4, precision_start=10^-2, referenceMod="D2",
					   alfa_degree=seq(0.05,0.95,0.01), alfa_min=0.05, alfa_max=0.95,  co_reference=co_D2,  m_reference=m_D2, n_reference=n_D2, fitted_co=D2c106$co, fitted_m=D2c106$m,   fitted_n=D2c106$n)
					   
A3set2 <- sreg(nameset="A3set2",time_start=0, Temp_start=0,Temp_End=500, A=10^6, Ea=100000, rate=c(1,2,5,10,20,50), number_of_points=10^4, precision_start=10^-2, referenceMod="A3",
					   alfa_degree=seq(0.05,0.95,0.01), alfa_min=0.05, alfa_max=0.95,  co_reference=co_A3,  m_reference=m_A3, n_reference=n_A3, fitted_co=A3c106$co, fitted_m=A3c106$m,   fitted_n=A3c106$n)

D2s2 <- pepper(D2set2)
library(patchwork)
p10 <- D2s2$pfa | D2s2$pheat
p10 + plot_annotation(title = 'D2 precision 10^2')
ggsave("D2s2_p1.png", width =36, height = 12, units = "cm",dpi=1200) 
p11 <- D2s2$pE
p11 + plot_annotation(title = 'D2 precision 10^-2')
ggsave("D2s2_p1.png", width =16, height = 16, units = "cm",dpi=1200) 
p11m <- D2s2$pmetrics
p11m + plot_annotation(title = 'D2 precision 10^-2')
ggsave("D2s2_p3.png", width =16, height = 16, units = "cm",dpi=1200) 






D2set8 <- sreg(nameset="D2set2",time_start=0, Temp_start=0,Temp_End=500, A=10^6, Ea=100000, rate=c(1,2,5,10,20,50), number_of_points=10^4, precision_start=10^-8, referenceMod="D2",
					   alfa_degree=seq(0.05,0.95,0.01), alfa_min=0.05, alfa_max=0.95,  co_reference=co_D2,  m_reference=m_D2, n_reference=n_D2, fitted_co=D2c106$co, fitted_m=D2c106$m,   fitted_n=D2c106$n)

D2s8 <- pepper(D2set8)
library(patchwork)
p12 <- D2s8$pfa | D2s8$pheat
p12 + plot_annotation(title = 'D2 precision 10^-8')
ggsave("D2s2_p1.png", width =36, height = 12, units = "cm",dpi=1200) 
p13 <- D2s2$pE
p13 + plot_annotation(title = 'D2 precision 10^-8')
ggsave("D2s8_p1.png", width =16, height = 16, units = "cm",dpi=1200) 
p13m <- D2s8$pmetrics
p13m + plot_annotation(title = 'D2 precision 10^-8')
ggsave("D2s8_p3.png", width =16, height = 16, units = "cm",dpi=1200) 




D3set2 <- sreg(nameset="D3set2",time_start=0, Temp_start=0,Temp_End=500, A=10^6, Ea=100000, rate=c(1,2,5,10,20,50), number_of_points=10^4, precision_start=10^-2, referenceMod="D3",
					   alfa_degree=seq(0.05,0.95,0.01), alfa_min=0.05, alfa_max=0.95,  co_reference=co_D3,  m_reference=m_D3, n_reference=n_D3, fitted_co=D3c106$co, fitted_m=D3c106$m,   fitted_n=D3c106$n)
		
D3s2 <- pepper(D3set2)
library(patchwork)
p14 <- D3s2$pfa | D3s2$pheat
p14 + plot_annotation(title = 'D3 precision 10^2')
ggsave("D3s2_p1.png", width =36, height = 12, units = "cm",dpi=1200) 
p15 <- D3s2$pE
p15 + plot_annotation(title = 'D3 precision 10^-2')
ggsave("D3s2_p2.png", width =16, height = 16, units = "cm",dpi=1200) 
p15m <- D3s2$pmetrics
p15m + plot_annotation(title = 'D3 precision 10^-2')
ggsave("D3s2_p3.png", width =16, height = 16, units = "cm",dpi=1200) 

D3set8 <- sreg(nameset="D3set2",time_start=0, Temp_start=0,Temp_End=500, A=10^6, Ea=100000, rate=c(1,2,5,10,20,50), number_of_points=10^4, precision_start=10^-8, referenceMod="D3",
					   alfa_degree=seq(0.05,0.95,0.01), alfa_min=0.05, alfa_max=0.95,  co_reference=co_D3,  m_reference=m_D3, n_reference=n_D3, fitted_co=D3c106$co, fitted_m=D3c106$m,   fitted_n=D3c106$n)

D3s8 <- pepper(D3set8)
library(patchwork)
p16 <- D3s8$pfa | D3s8$pheat
p16 + plot_annotation(title = 'D3 precision 10^-8')
ggsave("D3s2_p1.png", width =36, height = 12, units = "cm",dpi=1200) 
p17 <- D3s2$pE
p17 + plot_annotation(title = 'D3 precision 10^-8')
ggsave("D3s8_p2.png", width =16, height = 16, units = "cm",dpi=1200) 
p17m <- D3s8$pmetrics
p17m + plot_annotation(title = 'D3 precision 10^-8')
ggsave("D3s8_p3.png", width =16, height = 16, units = "cm",dpi=1200) 

		