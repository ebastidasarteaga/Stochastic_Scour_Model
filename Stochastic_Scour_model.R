rm(list=ls())
library(readxl)
library(xlsx)
library(rsq)
library(SciViews)
library(MVN)

# set working directory in which you want to open and save your extractions

setwd("")

#######################################
# Reading (opening) the data file and choosing specific data from it "river discharge values"

File<- read_excel(".xlsx")
RCPs2.6.1 <- read_excel(".xlsx",range = cell_cols("B") )
RCPs2.6.2 <- read_excel(".xlsx",range = cell_cols("C") )
RCPs2.6.3 <- read_excel(".xlsx",range = cell_cols("D") )

RCPs4.5.1 <- read_excel(".xlsx",range = cell_cols("E") )
RCPs4.5.2 <- read_excel(".xlsx",range = cell_cols("F") )
RCPs4.5.3 <- read_excel(".xlsx",range = cell_cols("G") )

RCPs8.5.1 <- read_excel(".xlsx",range = cell_cols("H") )
RCPs8.5.2 <- read_excel(".xlsx",range = cell_cols("I") )
RCPs8.5.3 <- read_excel(".xlsx",range = cell_cols("J") )



data= array((RCPs4.5.3$`4.5.4`),c(length(RCPs2.6.1$`2.6.1`)))
#########################################################
#sd=scour depth
#fd=Flow depth directly upstream of the pier(Water level)
#a=pier width
#L: pier length
#fr=froude number
#K1:pier shape coefficients
#K2:angle of attack coefficients (flow skewness)
#K3:streambed conditions coefficients
#K4:river bed material size coefficients
#g: gravity acceleration
#V:Mean velocity of flow directly upstream of the pier
#Q: is the flow (m3/s)                                    
#B: is the river width (m)  
#n: is the Manning's coefficient                  
#S: is the longitudinal slope of the channel(slope of bed stream). 
n=0.035
a=2
B=25
S=0.00035
dataarray=as.numeric(length(RCPs2.6.1$`2.6.1`))
for (i in 1:72) {
  dataarray[i]=data[i]
}
## flow depth

fd=as.numeric(length(RCPs2.6.1$`2.6.1`))
for (i in 1:72) {
 fd[i]= (n*dataarray[i]/(B*S^(0.5)))^(3/5)
 print(fd[i])
}
fd
plot(fd)
fd.df=as.data.frame(fd)
###Mean flow velocity mfv (m/sec)
V=as.numeric(length(RCPs2.6.1$`2.6.1`))
for (i in 1:72) {
  V[i]=dataarray[i]/(B*fd[i])
  print(V[i])
}
V
plot(V)
mfv=as.data.frame(V)#m/sec


###Hydraulic radius (m)
HR=as.numeric(length(RCPs2.6.1$`2.6.1`))
for (i in 1:72) {
  HR[i]=(B*fd[i])/((2*fd[i])+B)
  print(HR[i])
}
HR
plot(HR)
HRdf=as.data.frame(HR)

###Local velocity (m/sec)
Kp=1.5# Round nose acceleration factor
VL=as.numeric(length(RCPs2.6.1$`2.6.1`))
for (i in 1:72) {
  VL[i]=((1/n)*(HR[i]^(2/3))*(S^(0.5)))*Kp
  print(VL[i])
}
VL
plot(VL)
VLdf=as.data.frame(VL)#m/sec

###Reynolds number
Dw=998.21# Density of water in river
Dv=0.0010016# Dynamic viscosity

Re=as.numeric(length(RCPs2.6.1$`2.6.1`))
for (i in 1:72) {
  Re[i]=(Dw*V[i]*HR[i])/Dv
  print(Re[i])
}
Re
plot(Re)
Redf=as.data.frame(Re)#m/sec

###Froude number
fr=as.numeric(length(RCPs2.6.1$`2.6.1`))
for (i in 1:72) {
  fr[i]=V[i]/(9.81*fd[i])^(0.5)
  print(fr[i])
}
fr
plot(fr)
frn=as.data.frame(fr)
###Bed Condition
Ku=6.19
Sable=0.002
Vc=as.numeric(length(RCPs2.6.1$`2.6.1`))
for (i in 1:72) {
  Vc[i]=Ku*(fd[i]^(1/6))*(Sable^(1/3))
  print(Vc[i])
}
Vc
plot(Vc)
Vcdf=as.data.frame(Vc)

###Scour distribution

ScourD=as.numeric(length(RCPs2.6.1$`2.6.1`))
for (i in 1:72) {
  ScourD[i]=fd[i]/a
  print(ScourD[i])
}
ScourD
plot(ScourD)
ScourDdf=as.data.frame(ScourD)

###Bed Shear Velocity (m/sec)

BSV=as.numeric(length(RCPs2.6.1$`2.6.1`))
for (i in 1:72) {
  BSV[i]=V[i]*(n/1)*(9.81*(HR[i]^(-1/3)))^(0.5)
  print(BSV[i])
}
BSV
plot(BSV)
BSVdf=as.data.frame(BSV)
###ChÈzy Coefficient (m^0.5/sec)
min(Re)# The equation of this value changes based on the Re value. It this case the Re exceeds 2000 in all cases presenting a Turbulent flow which is the case in rivers, therefoer a single equation is used
Kv=1.0034E-06#Kinematic viscosity
alpha=1# alpha coefficient
CC=as.numeric(length(RCPs2.6.1$`2.6.1`))
for (i in 1:72) {
  CC[i]=5.75*(9.81^(0.5))*(log10((12*fd[i])/(alpha*Sable)))
  print(CC[i])
}
CC
plot(CC)
CCdf=as.data.frame(CC)

###Bed shear stress N/m^2

BSS=as.numeric(length(RCPs2.6.1$`2.6.1`))
for (i in 1:72) {
BSS[i]= Dw*9.81*(((V[i])^2)/(CC[i]^(2)))
print(BSS[i])
}
BSS
plot(BSS)
BSSdf=as.data.frame(BSS)

###Shields parameter
DS=1500#Density of river sand
SP=as.numeric(length(RCPs2.6.1$`2.6.1`))
for (i in 1:72) {
  SP[i]= BSS[i]/((DS-Dw)*9.81*Sable)
  print(SP[i])
}
SP
plot(SP)
BSSdf=as.data.frame(BSS)

###Fall velocity
# The same concept for Re which is always superior of 2000; presents only an equation to be coded
delta=(DS-Dw)/Dw
FV=as.numeric(length(RCPs2.6.1$`2.6.1`))
for (i in 1:72) {
  FV[i]= 1.077*((delta*9.81*Sable)^(0.5))
  print(FV[i])
}
FV
plot(FV)
FVdf=as.data.frame(FV)

###Suspension Number
VKC=0.4#Von Karmann coefficient
SN=as.numeric(length(RCPs2.6.1$`2.6.1`))
for (i in 1:72) {
  SN[i]= FV[i]/(BSV[i]*VKC)
  print(SN[i])
}
SN
plot(SN)
SNdf=as.data.frame(SN)

###Local shear stress (N/m^2)
ku=1
LSS=as.numeric(length(RCPs2.6.1$`2.6.1`))
for (i in 1:72) {
  LSS[i]= ((n*VL[i]/ku)^(2))*((Dw*10)/(fd[i]^(1/3)))
  print(LSS[i])
}
LSS
plot(LSS)
LSSdf=as.data.frame(LSS)

###Critical shear stress (N/m^2)
Ks=0.047
CSS=as.numeric(length(RCPs2.6.1$`2.6.1`))
for (i in 1:72) {
  CSS[i]= Ks*((DS*10)-(Dw*10))*9.81*Sable
  print(CSS[i])
}
CSS
plot(CSS)
CSSdf=as.data.frame(CSS)

###Initiation of scour conditions

#Bed Condition
DMBD=function(V,Vc) {
  ifelse(V>Vc,0.9,1)
}

DMBD=DMBD(V,Vc)
#Initiation Motion and Suspension
DMIMS=function(BSV,FV,LSS,CSS) {
  ifelse(BSV>FV & LSS>CSS,1,0)
}

DMIMS=DMIMS(BSV,FV,LSS,CSS)



###Correction factors

#1-Shape factor
#The correction factor K1 for pier nose shape should be determined using Table 
#for angles of attack up to 5 degrees if it is more than 5 degrees k1=1

k1=1

#2-angle of attack(skewness) factor

#L:length of the pier
#a: width of the pier
#(Theta=) anlge of attack
L= 12
a= 2
theta=0

k2=(cos(theta)+(L/a)*(sin(theta)))^0.65
k2
#3-Bed condition"Dunes" 
k3=1.1

#4- Bed material
#K4 varies between 0.4 and 1.0 
#0.4 for fine and 1 for coarse bed materials
#The correction factor K = 1.0 if D(50) <2 mm or D(95) < 20 mm for the bed material. 
#If D >(50) 2 mm and D(95) > 20 mm, then K decreases and is calculated as 
#(Mueller and Jones, 1999)
k4=0.4
Correction.factors=data.frame(k1,k2,k3,k4)
colnames(Correction.factors) <- c("k1","k2","k3","k4")
Correction.factors
#########################################################
### Scour equation

sd=as.numeric(length(RCPs2.6.1$`2.6.1`))
for (i in 1:72) {
  sd[i]= 2*fd[i]*k1*k2*k3*k4*DMBD[i]*DMIMS[i]*((a/fd[i])^(0.65))*(fr[i]^(0.43))
  print(sd)
}
sd
plot(sd)
sddf=as.data.frame(sd)

time_index <- seq_along(sd)
exceeding_values <- sd[sd >= 1]
exceeding_times <- time_index[sd >= 1]
before_57 <- exceeding_times[exceeding_times < 57]
after_57 <- exceeding_times[exceeding_times >= 57]
count_before_57 <- length(before_57)
count_after_57 <- length(after_57)
before_values <- exceeding_values[exceeding_times < 57]
after_values <- exceeding_values[exceeding_times >= 57]
results <- data.frame(Time = exceeding_times, Scour = exceeding_values)
print(results)

write.xlsx(
  sddf ,
  file="Scourdepth2024V2.xlsx",
  sheetName = "RCPS-4.5.3",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
write.xlsx(
  results ,
  file="scourfailure2024VS.xlsx",
  sheetName = "RCPS-4.5.3",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
# Those results will be used to determine the statistical charactersitcs and the rate of occurrence in the stocahtic approach
#########################################################
### Top width of scour holes( Scour width size)
#The top width could range from 1.0 to 2.8 sd
#Phi angle of the scour depth is used to determine it in equation
#anglephi: inclination angle inside the scour depth ( 30 to 44 degrees)
#wb: bottom width of scour hole = 0 : narrow scour and = sd or a : wide scour
#wd: half the top scour hole width
#wdtotal: section of the top width of the scour hole
#anglephi=30
#wb=as.numeric(length(RCPs2.6.1))# narrow scour wb=0 else if wide scour wb= a
#wb=0
#wd=as.numeric(length(RCPs2.6.1))
#wdtotal=as.numeric(length(RCPs2.6.1))
#for (i in 1:85) {
#  wd[i]=abs(sd[i]*(wb+(1/tan(anglephi))))
#  wdtotal[i]=2*wd[i]
#  print(wd[i])
#}
#plot(wd)
#wd.df=as.data.frame(wd)
#plot(wdtotal)
#wdtotal.df=as.data.frame(wdtotal)
#Topscourholeswidth=data.frame(wd.df,wdtotal.df)
#colnames(Topscourholeswidth) <- c("Topwidthpartition","TopwidthTotal")
#Topscourholeswidth
#########################################################


# POISSON PROCESS
#Poisson generator
PPgen=function(lambda){
  X=0
  Sum=0
  flag=0
  while (flag==0){
    E=-log(runif(1))
    Sum=Sum+E
    if (Sum < lambda) { X=X+1} else { flag=1}
  }
  return(X)
}
#Poisson process
PP=function(lambda, N, T){
  h=T/N
  t=(0:T)/N
  X=rep(0, N+1)
  I=rep(0,N)
  X[1]=0
  for(i in 1:N) {
    I[i]=PPgen(h*lambda)
    X[i+1]=X[i] + I[i]}
  return(X)
}
# Statistical characteristcis of the resulting scour depth
#RCPs 2024-2079
Years.1=57
LambdaRCPs2.6.1="values from the determinsitc approach"
LambdaRCPs4.5.1="values from the determinsitc approach"
LambdaRCPs8.5.1="values from the determinsitc approach"

RCPS2.6.1shockmean="values from the determinsitc approach"
RCPS2.6.1shockstd="values from the determinsitc approach"


RCPS4.5.1shockmean="values from the determinsitc approach"
RCPS4.5.1shockstd="values from the determinsitc approach"


RCPS8.5.1shockmean="values from the determinsitc approach"
RCPS8.5.1shockstd="values from the determinsitc approach"

#2080-2095

Years.2=15
LambdaRCPs2.6.2="values from the determinsitc approach" 
LambdaRCPs4.5.2="values from the determinsitc approach"
LambdaRCPs8.5.2="values from the determinsitc approach"


RCPS2.6.2shockmean="values from the determinsitc approach"
RCPS2.6.2shockstd="values from the determinsitc approach"

RCPS4.5.2shockmean="values from the determinsitc approach"
RCPS4.5.2shockstd="values from the determinsitc approach"

RCPS8.5.2shockmean="values from the determinsitc approach"
RCPS8.5.2shockstd="values from the determinsitc approach"


#################################################################
#Compound Poisson Process no accumulation:
CPP=function(lambda, N, T, mu_xi, sigma_xi) {
  h = T / N              
  t = (0:T) / N          
  X = rep(0, N+1)        
  I = rep(0, N)          
  X[1] = 0               
  for (i in 1:N) {
    I[i] = PPgen(h * lambda)  r
      if (I[i] == 1) {
      F[i] = rnorm(n = N, mean = mu_xi, sd = sigma_xi) 
      X[i] = F[i]  
    } else {
      X[i+1] = X[i]  
    }
  }
  return(X)
}

# Results 2024-2079

#Simulations
#RCPs 2.6

Results2.6.1=as.numeric(0)
for (i in 1:1000) {
  CPP2.6.1=CPP(lambda = LambdaRCPs2.6.1,N = Years.1,T = Years.1-1,mu_xi = RCPS2.6.1shockmean,sigma_xi = RCPS2.6.1shockstd)
  Results2.6.1[i]=data.frame(CPP2.6.1)
}
Results2.6.1

#RCPs 4.5
Results4.5.1=as.numeric(0)
for (i in 1:1000) {
  CPP4.5.1=CPP(lambda = LambdaRCPs4.5.1,N = Years.1,T = Years.1-1,mu_xi = RCPS4.5.1shockmean,sigma_xi = RCPS4.5.1shockstd)
  Results4.5.1[i]=data.frame(CPP4.5.1)
}
Results4.5.1

#RCPs 8.5
Results8.5.1=as.numeric(0)
for (i in 1:1000) {
  CPP8.5.1=CPP(lambda = LambdaRCPs8.5.1,N = Years.1,T = Years.1-1,mu_xi = RCPS8.5.1shockmean,sigma_xi = RCPS8.5.1shockstd)
  Results8.5.1[i]=data.frame(CPP8.5.1)
}
Results8.5.1

write.xlsx(
  Results2.6.1,
  file="CPP2024.2079.xlsx",
  sheetName = "RCPs2.6.1",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE)
write.xlsx(
  Results4.5.1,
  file="CPP2024.2079.xlsx",
  sheetName = "RCPs4.5.1",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
write.xlsx(
  Results8.5.1,
  file="CPP2024.2079.xlsx",
  sheetName = "RCPs8.5.1",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)


# Results 2080-2095

#RCPs 2.6
Results2.6.2=as.numeric(0)
for (i in 1:1000) {
  CPP2.6.2=CPP(lambda = LambdaRCPs2.6.2,N = Years.2,T = Years.2-1,mu_xi = RCPS2.6.2shockmean,sigma_xi = RCPS2.6.2shockstd)
  Results2.6.2[i]=data.frame(CPP2.6.2)
}
Results2.6.2

#RCPs 4.5
Results4.5.2=as.numeric(0)
for (i in 1:1000) {
  CPP4.5.2=CPP(lambda = LambdaRCPs4.5.2,N = Years.2,T = Years.2-1,mu_xi = RCPS4.5.2shockmean,sigma_xi = RCPS4.5.2shockstd)
  Results4.5.2[i]=data.frame(CPP4.5.2)
}
Results4.5.2

#RCPs 8.5
Results8.5.2=as.numeric(0)
for (i in 1:1000) {
  CPP8.5.2=CPP(lambda = LambdaRCPs8.5.2,N = Years.2,T = Years.2-1,mu_xi = RCPS8.5.2shockmean,sigma_xi = RCPS8.5.2shockstd)
  Results8.5.2[i]=data.frame(CPP8.5.2)
}
Results8.5.2

write.xlsx(
  Results2.6.2,
  file="CPP2080.2095.xlsx",
  sheetName = "RCPs2.6.2",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE)
write.xlsx(
  Results4.5.2,
  file="CPP2080.2095.xlsx",
  sheetName = "RCPs4.5.2",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
write.xlsx(
  Results8.5.2,
  file="CPP2080.2095.xlsx",
  sheetName = "RCPs8.5.2",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)

#######################################################

# CPP degradation model of shocks
##compute the probability that a system reaches threshold k before t= j years with shock size mean value
#Failure occurs when number of events n>:

#########################
#########################
########################

##2024-2095 based on the lifetime
# "values from the lifetime analysis where the rate =1/Lifetime"
YTF2.6.1.U=0.013888889
YTF2.6.1.L=0.045454545
YTF2.6.1.M=0.023809524

YTF4.5.1.U=0.023809524
YTF4.5.1.L=0.083333333
YTF4.5.1.M=0.038461538


YTF8.5.1.U=0.034482759
YTF8.5.1.L=0.125
YTF8.5.1.M=0.052631579



# In the case of fix shock sizes, the failure probability at life time t=j: 
# i is the number >n
#P(V(t) ??? k) = P(N(t) > n)

#2024-2095

n=(V0-k)/u;n# This indicates the number of shocks till failure ( in this case we already know it based on the analysis of the failure rate)

#############RCPs 2.6

n2.6UL=YTF2.6.1.U
n2.6LL=YTF2.6.1.L
n2.6ML=YTF2.6.1.M

#Upper 
V2.6=0
V2.6=as.numeric(100)
probabilityoffailure2.6UL=as.numeric(100)
n=1
lambda2.6=n2.6UL
j=1

for(j in 1:100) {
  for (i in 1:n) {
    x2.6=((lambda2.6*j)^0)*exp(-lambda2.6*j)/factorial(0)
    V2.6[i]=((lambda2.6*j)^i)*exp(-lambda2.6*j)/factorial(i)
    probabilityoffailure2.6UL[j]=1-(x2.6+sum(V2.6))
    print(V2.6[i])
    i=i+1
  }
}
V2.6
plot(V2.6)
probabilityoffailure2.6UL#probability of the system to reach the threshold before each year
plot(probabilityoffailure2.6UL)

#Lower 
V2.6=0
V2.6=as.numeric(100)
probabilityoffailure2.6LL=as.numeric(100)


lambda2.6=n2.6LL
j=1

for(j in 1:100) {
  i=1
  while(i <=n) {
    x2.6=((lambda2.6*j)^0)*exp(-lambda2.6*j)/factorial(0)
    V2.6[i]=((lambda2.6*j)^i)*exp(-lambda2.6*j)/factorial(i)
    probabilityoffailure2.6LL[j]=1-(x2.6+sum(V2.6))
    print(V2.6[i])
    i=i+1
  }
  
}
V2.6
plot(V2.6)
probabilityoffailure2.6LL#probability of the system to reach the threshold before each year
plot(probabilityoffailure2.6UL)
lines(probabilityoffailure2.6LL)


#Average 
V2.6=0
V2.6=as.numeric(100)
probabilityoffailure2.6ML=as.numeric(100)


lambda2.6=n2.6ML
j=1

for(j in 1:100) {
  i=1
  while(i <=n) {
    x2.6=((lambda2.6*j)^0)*exp(-lambda2.6*j)/factorial(0)
    V2.6[i]=((lambda2.6*j)^i)*exp(-lambda2.6*j)/factorial(i)
    probabilityoffailure2.6ML[j]=1-(x2.6+sum(V2.6))
    print(V2.6[i])
    i=i+1
  }
  
}
V2.6
plot(V2.6)
probabilityoffailure2.6ML#probability of the system to reach the threshold before each year
plot(probabilityoffailure2.6UL)
lines(probabilityoffailure2.6LL)
lines(probabilityoffailure2.6ML)

POF2.6.1=data.frame(probabilityoffailure2.6LL,probabilityoffailure2.6UL,probabilityoffailure2.6ML)
write.xlsx(
  POF2.6.1,
  file="Probability of failurell.xlsx",
  sheetName = "RCPs2.6",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE)


############# RCPs 4.5


n4.5UL=YTF4.5.1.U
n4.5LL=YTF4.5.1.L
n4.5ML=YTF4.5.1.M

#Upper 
V4.5=0
V4.5=as.numeric(100)
probabilityoffailure4.5UL=as.numeric(100)
n=1
lambda4.5=n4.5UL
j=1

for(j in 1:100) {
  for (i in 1:n) {
    x4.5=((lambda4.5*j)^0)*exp(-lambda4.5*j)/factorial(0)
    V4.5[i]=((lambda4.5*j)^i)*exp(-lambda4.5*j)/factorial(i)
    probabilityoffailure4.5UL[j]=1-(x4.5+sum(V4.5))
    print(V4.5[i])
    i=i+1
  }
}

plot(probabilityoffailure4.5UL)

#Lower 
V4.5=0
V4.5=as.numeric(100)
probabilityoffailure4.5LL=as.numeric(100)
n=1
lambda4.5=n4.5LL
j=1

for(j in 1:100) {
  for (i in 1:n) {
    x4.5=((lambda4.5*j)^0)*exp(-lambda4.5*j)/factorial(0)
    V4.5[i]=((lambda4.5*j)^i)*exp(-lambda4.5*j)/factorial(i)
    probabilityoffailure4.5LL[j]=1-(x4.5+sum(V4.5))
    print(V4.5[i])
    i=i+1
  }
}

plot(probabilityoffailure4.5LL)

########### Average

V4.5=0
V4.5=as.numeric(100)
probabilityoffailure4.5ML=as.numeric(100)
n=1
lambda4.5=n4.5ML
j=1

for(j in 1:100) {
  for (i in 1:n) {
    x4.5=((lambda4.5*j)^0)*exp(-lambda4.5*j)/factorial(0)
    V4.5[i]=((lambda4.5*j)^i)*exp(-lambda4.5*j)/factorial(i)
    probabilityoffailure4.5ML[j]=1-(x4.5+sum(V4.5))
    print(V4.5[i])
    i=i+1
  }
}

plot(probabilityoffailure4.5ML)





plot(probabilityoffailure4.5UL)
lines(probabilityoffailure4.5LL)
lines(probabilityoffailure4.5ML)

POF4.5=data.frame(probabilityoffailure4.5LL,probabilityoffailure4.5UL,probabilityoffailure4.5ML)
write.xlsx(
  POF4.5,
  file="Probability of failurell.xlsx",
  sheetName = "RCPs4.5",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE)

####################

#######RCPs 8.5


n8.5UL=YTF8.5.1.U
n8.5LL=YTF8.5.1.L
n8.5ML=YTF8.5.1.M

#Upper 
V8.5=0
V8.5=as.numeric(100)
probabilityoffailure8.5UL=as.numeric(100)
n=1
lambda8.5=n8.5UL
j=1

for(j in 1:100) {
  for (i in 1:n) {
    x8.5=((lambda8.5*j)^0)*exp(-lambda8.5*j)/factorial(0)
    V8.5[i]=((lambda8.5*j)^i)*exp(-lambda8.5*j)/factorial(i)
    probabilityoffailure8.5UL[j]=1-(x8.5+sum(V8.5))
    print(V8.5[i])
    i=i+1
  }
}

plot(probabilityoffailure8.5UL)

#Lower 
V8.5=0
V8.5=as.numeric(100)
probabilityoffailure8.5LL=as.numeric(100)
n=1
lambda8.5=n8.5LL
j=1

for(j in 1:100) {
  for (i in 1:n) {
    x8.5=((lambda8.5*j)^0)*exp(-lambda8.5*j)/factorial(0)
    V8.5[i]=((lambda8.5*j)^i)*exp(-lambda8.5*j)/factorial(i)
    probabilityoffailure8.5LL[j]=1-(x8.5+sum(V8.5))
    print(V8.5[i])
    i=i+1
  }
}

plot(probabilityoffailure8.5LL)
lines(probabilityoffailure8.5UL)

#Average 
V8.5=0
V8.5=as.numeric(100)
probabilityoffailure8.5ML=as.numeric(100)
n=1
lambda8.5=n8.5ML
j=1

for(j in 1:100) {
  for (i in 1:n) {
    x8.5=((lambda8.5*j)^0)*exp(-lambda8.5*j)/factorial(0)
    V8.5[i]=((lambda8.5*j)^i)*exp(-lambda8.5*j)/factorial(i)
    probabilityoffailure8.5ML[j]=1-(x8.5+sum(V8.5))
    print(V8.5[i])
    i=i+1
  }
}

plot(probabilityoffailure8.5ML)
lines(probabilityoffailure8.5UL)
lines(probabilityoffailure8.5LL)


POF8.5=data.frame(probabilityoffailure8.5LL,probabilityoffailure8.5UL,probabilityoffailure8.5ML)
write.xlsx(
  POF8.5,
  file="Probability of failurell.xlsx",
  sheetName = "RCPs8.5",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE)
##############
