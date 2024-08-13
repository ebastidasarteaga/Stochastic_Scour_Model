rm(list=ls())
library(readxl)
library(xlsx)
library(rsq)
library(SciViews)
library(MVN)

# set working directory in which you want to open and save your extractions

setwd("C:/Users/")

#######################################
# Reading (opening) the data file of river discharge values under a changinging climate

File<- read_excel("C:/Users/.xlsx")

#Determining the RCPs from the opened file
#Optimistic climate change scenarios from 3 regional climate moddels
RCPs2.6.1 <- read_excel("C:/Users/.xlsx",range = cell_cols("B") )
RCPs2.6.2 <- read_excel("C:/Users/.xlsx",range = cell_cols("C") )
RCPs2.6.3 <- read_excel("C:/Users/.xlsx",range = cell_cols("D") )
#Medium climate change scenarios from 3 regional climate moddels
RCPs4.5.1 <- read_excel("C:/Users/.xlsx",range = cell_cols("E") )
RCPs4.5.2 <- read_excel("C:/Users/.xlsx",range = cell_cols("F") )
RCPs4.5.3 <- read_excel("C:/Users/.xlsx",range = cell_cols("G") )

#Pessimistic climate change scenarios from 3 regional climate moddels
RCPs8.5.1 <- read_excel("C:/Users/.xlsx",range = cell_cols("H") )
RCPs8.5.2 <- read_excel("C:/Users/.xlsx",range = cell_cols("I") )
RCPs8.5.3 <- read_excel("C:/Users/.xlsx",range = cell_cols("J") )

#########################################



#Determine which RCPs we will consider for the run definied as data (line 38).  After launching the simulations this values should be changed to another RCPs to measure the local scour 

data= array((RCPs2.6.1$`2.6.1`),c(length(RCPs2.6.1$`2.6.1`)))
data= array((RCPs2.6.2$`2.6.2`),c(length(RCPs2.6.2$`2.6.2`)))
data= array((RCPs2.6.3$`2.6.4`),c(length(RCPs2.6.3$`2.6.4`)))
data= array((RCPs4.5.1$`4.5.1`),c(length(RCPs4.5.1$`4.5.1`)))
data= array((RCPs4.5.2$`4.5.2`),c(length(RCPs4.5.2$`4.5.2`)))
data= array((RCPs4.5.3$`4.5.4`),c(length(RCPs4.5.3$`4.5.4`)))
data= array((RCPs8.5.1$`8.5.1`),c(length(RCPs8.5.1$`8.5.1`)))
data= array((RCPs8.5.2$`8.5.2`),c(length(RCPs8.5.2$`8.5.2`)))
data= array((RCPs8.5.3$`8.5.4`),c(length(RCPs8.5.3$`8.5.4`)))




#########################################################


#HEC-18 guidline

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
dataarray=as.numeric(length(RCPs2.6.1))
for (i in 1:85) {
  dataarray[i]=data[i]
}
## flow depth

fd=as.numeric(length(RCPs2.6.1))
for (i in 1:85) {
 fd[i]= (n*dataarray[i]/(B*S^(0.5)))^(3/5)
 print(fd[i])
}
fd
plot(fd)
fd.df=as.data.frame(fd)
###Mean flow velocity mfv (m/sec)
V=as.numeric(length(RCPs2.6.1))
for (i in 1:85) {
  V[i]=dataarray[i]/(B*fd[i])
  print(V[i])
}
V
plot(V)
mfv=as.data.frame(V)#m/sec


###Hydraulic radius (m)
HR=as.numeric(length(RCPs2.6.1))
for (i in 1:85) {
  HR[i]=(B*fd[i])/((2*fd[i])+B)
  print(HR[i])
}
HR
plot(HR)
HRdf=as.data.frame(HR)

###Local velocity (m/sec)
Kp=1.5# Round nose acceleration factor
VL=as.numeric(length(RCPs2.6.1))
for (i in 1:85) {
  VL[i]=((1/n)*(HR[i]^(2/3))*(S^(0.5)))*Kp
  print(VL[i])
}
VL
plot(VL)
VLdf=as.data.frame(VL)#m/sec

###Reynolds number
Dw=998.21# Density of water in river
Dv=0.0010016# Dynamic viscosity

Re=as.numeric(length(RCPs2.6.1))
for (i in 1:85) {
  Re[i]=(Dw*V[i]*HR[i])/Dv
  print(Re[i])
}
Re
plot(Re)
Redf=as.data.frame(Re)#m/sec

###Froude number
fr=as.numeric(length(RCPs2.6.1))
for (i in 1:85) {
  fr[i]=V[i]/(9.81*fd[i])^(0.5)
  print(fr[i])
}
fr
plot(fr)
frn=as.data.frame(fr)
###Bed Condition
Ku=6.19
Sable=0.002
Vc=as.numeric(length(RCPs2.6.1))
for (i in 1:85) {
  Vc[i]=Ku*(fd[i]^(1/6))*(Sable^(1/3))
  print(Vc[i])
}
Vc
plot(Vc)
Vcdf=as.data.frame(Vc)

###Scour distribution: (narrow pier, wide pier, transitional pier)

ScourD=as.numeric(length(RCPs2.6.1))
for (i in 1:85) {
  ScourD[i]=fd[i]/a
  print(ScourD[i])
}
ScourD
plot(ScourD)
ScourDdf=as.data.frame(ScourD)

###Bed Shear Velocity (m/sec)

BSV=as.numeric(length(RCPs2.6.1))
for (i in 1:85) {
  BSV[i]=V[i]*(n/1)*(9.81*(HR[i]^(-1/3)))^(0.5)
  print(BSV[i])
}
BSV
plot(BSV)
BSVdf=as.data.frame(BSV)
###Chézy Coefficient (m^0.5/sec)
min(Re)# The equation of this value changes based on the Re value. It this case the Re exceeds 2000 in all cases presenting a Turbulent flow which is the case in rivers, therefoer a single equation is used
Kv=1.0034E-06#Kinematic viscosity
alpha=1# alpha coefficient
CC=as.numeric(length(RCPs2.6.1))
for (i in 1:85) {
  CC[i]=5.75*(9.81^(0.5))*(log10((12*fd[i])/(alpha*Sable)))
  print(CC[i])
}
CC
plot(CC)
CCdf=as.data.frame(CC)

###Bed shear stress N/m^2

BSS=as.numeric(length(RCPs2.6.1))
for (i in 1:85) {
BSS[i]= Dw*9.81*(((V[i])^2)/(CC[i]^(2)))
print(BSS[i])
}
BSS
plot(BSS)
BSSdf=as.data.frame(BSS)

###Shields parameter
DS=1500#Density of river sand
SP=as.numeric(length(RCPs2.6.1))
for (i in 1:85) {
  SP[i]= BSS[i]/((DS-Dw)*9.81*Sable)
  print(SP[i])
}
SP
plot(SP)
BSSdf=as.data.frame(BSS)

###Fall velocity
# The same concept for Re which is always superior of 2000; presents only an equation to be coded
delta=(DS-Dw)/Dw
FV=as.numeric(length(RCPs2.6.1))
for (i in 1:85) {
  FV[i]= 1.077*((delta*9.81*Sable)^(0.5))
  print(FV[i])
}
FV


###Suspension Number
VKC=0.4#Von Karmann coefficient
SN=as.numeric(length(RCPs2.6.1))
for (i in 1:85) {
  SN[i]= FV[i]/(BSV[i]*VKC)
  print(SN[i])
}
SN
plot(SN)
SNdf=as.data.frame(SN)

###Local shear stress (N/m^2)
ku=1
LSS=as.numeric(length(RCPs2.6.1))
for (i in 1:85) {
  LSS[i]= ((n*VL[i]/ku)^(2))*((Dw*10)/(fd[i]^(1/3)))
  print(LSS[i])
}
LSS
plot(LSS)
LSSdf=as.data.frame(LSS)

###Critical shear stress (N/m^2)
Ks=0.047
CSS=as.numeric(length(RCPs2.6.1))
for (i in 1:85) {
  CSS[i]= Ks*((DS*10)-(Dw*10))*9.81*Sable
  print(CSS[i])
}
CSS


###Initiation of scour conditions

#Coefficient of back-filling
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

SD=as.numeric(length(RCPs2.6.2$`2.6.2`))
for (i in 1:length(RCPs2.6.2$`2.6.2`)) {
  SD[i]= 2*fd[i]*k1*k2*k3*k4*DMBD[i]*DMIMS[i]*((a/fd[i])^(0.65))*(fr[i]^(0.43))
}
SD
plot(SD)
sddf=as.data.frame(SD)

#Foundation depth
FD=1

length(SD)
########################## Accumulation 
########################################

scourdepthprocessaccumulation <- function(scour_depths, foundation_depth) {
  results <- numeric(length(scour_depths))  # Pre-allocate the results vector
  accumulated_depth <- 0
  
  for (i in seq_along(scour_depths)) {
    depth <- scour_depths[i]
    
    if (depth == 0) {
      results[i] <- 0
    } else if (depth < foundation_depth) {
      accumulated_depth <- accumulated_depth + depth
      if (accumulated_depth >= 1.0) {
        results[i] <- accumulated_depth
        accumulated_depth <- 0
      } else {
        results[i] <- 0
      }
    } else {
      if (accumulated_depth > 0) {
        accumulated_depth <- accumulated_depth + depth
        results[i] <- accumulated_depth
        accumulated_depth <- 0
      } else {
        results[i] <- depth
      }
    }
  }
  
  # Handle any remaining accumulated depth if it hasn't reached 1 meter
  if (accumulated_depth > 0 && accumulated_depth < 1.0) {
    results[length(scour_depths)] <- 0
  } else if (accumulated_depth >= 1.0) {
    results[length(scour_depths)] <- accumulated_depth
  }
  return(results)
}

SD
result=scourdepthprocessaccumulation(SD,FD);result
plot(SD)
lines(result)
length(SD)
length(result)
SDx=array(SD)
resultx=array(result)
Xr=data.frame(SDx,resultx);Xr
TotalNumberoffailure2.6.1=sum(result > 1)# Number of failures from 2011 to 2095
Numberoffailure2079.2.6.1=sum(result[1:69] > 1)# Number of failures from 2011 to 2095
Numberoffailure2095.2.6.1=sum(result[70:85] > 1)# Number of failures from 2011 to 2095


#Shocks and values for CPP
filterpositive <- function(x) {
  # Filter only positive values (greater than 0)
  positivevalues <- x[x > 0]
  
  # Return the filtered vector
  return(positivevalues)
}
SDfilter=filterpositive(SD)
####### No loop function is done so every rune of the previous lines, the use has to safe the results considering the RCPs index


SDfilter2079=filterpositive(SD[1:69])
SDfilter2095=filterpositive(SD[70:85])

#########RCPs 2.6

SDmean.2.6.1.1=mean(SDfilter2079)
SDstd.2.6.1.1=sd(SDfilter2079)
SDmean.2.6.1.2=mean(SDfilter2095)
SDstd.2.6.1.2=sd(SDfilter2095)


Numberofshocks2.6.1=sum(SD > 0)# Number of failures from 2011 to 2095
Numberofshocks2.6.1.2079=sum(SD[1:69] > 0)# Number of failures from 2011 to 2095
Numberofshocks2.6.1.2095=sum(SD[70:85] > 0)# Number of failures from 2011 to 2095
#
SDmean.2.6.2.1=mean(SDfilter2079)
SDstd.2.6.2.1=sd(SDfilter2079)
SDmean.2.6.2.2=mean(SDfilter2095)
SDstd.2.6.2.2=sd(SDfilter2095)
Numberofshocks2.6.2=sum(SD > 0)# Number of failures from 2011 to 2095
Numberofshocks2.6.2.2079=sum(SD[1:69] > 0)# Number of failures from 2011 to 2095
Numberofshocks2.6.2.2095=sum(SD[70:85] > 0)# Number of failures from 2011 to 2095
#
SDmean.2.6.3.1=mean(SDfilter2079)
SDstd.2.6.3.1=sd(SDfilter2079)
SDmean.2.6.3.2=mean(SDfilter2095)
SDstd.2.6.3.2=sd(SDfilter2095)
Numberofshocks2.6.3=sum(SD > 0)# Number of failures from 2011 to 2095
Numberofshocks2.6.3.2079=sum(SD[1:69] > 0)# Number of failures from 2011 to 2095
Numberofshocks2.6.3.2095=sum(SD[70:85] > 0)# Number of failures from 2011 to 2095

#########RCPs 4.5
SDmean.4.5.1.1=mean(SDfilter2079)
SDstd.4.5.1.1=sd(SDfilter2079)
SDmean.4.5.1.2=mean(SDfilter2095)
SDstd.4.5.1.2=sd(SDfilter2095)
Numberofshocks4.5.1=sum(SD > 0)# Number of failures from 2011 to 2095
Numberofshocks4.5.1.2079=sum(SD[1:69] > 0)# Number of failures from 2011 to 2095
Numberofshocks4.5.1.2095=sum(SD[70:85] > 0)# Number of failures from 2011 to 2095
#
SDmean.4.5.2.1=mean(SDfilter2079)
SDstd.4.5.2.1=sd(SDfilter2079)
SDmean.4.5.2.2=mean(SDfilter2095)
SDstd.4.5.2.2=sd(SDfilter2095)
Numberofshocks4.5.2=sum(SD > 0)# Number of failures from 2011 to 2095
Numberofshocks4.5.2.2079=sum(SD[1:69] > 0)# Number of failures from 2011 to 2095
Numberofshocks4.5.2.2095=sum(SD[70:85] > 0)# Number of failures from 2011 to 2095
#
SDmean.4.5.3.1=mean(SDfilter2079)
SDstd.4.5.3.1=sd(SDfilter2079)
SDmean.4.5.3.2=mean(SDfilter2095)
SDstd.4.5.3.2=sd(SDfilter2095)
Numberofshocks4.5.3=sum(SD > 0)# Number of failures from 2011 to 2095
Numberofshocks4.5.3.2079=sum(SD[1:69] > 0)# Number of failures from 2011 to 2095
Numberofshocks4.5.3.2095=sum(SD[70:85] > 0)# Number of failures from 2011 to 2095

#########RCPs 8.5
SDmean.8.5.1.1=mean(SDfilter2079)
SDstd.8.5.1.1=sd(SDfilter2079)
SDmean.8.5.1.2=mean(SDfilter2095)
SDstd.8.5.1.2=sd(SDfilter2095)
Numberofshocks8.5.1=sum(SD > 0)# Number of failures from 2011 to 2095
Numberofshocks8.5.1.2079=sum(SD[1:69] > 0)# Number of failures from 2011 to 2095
Numberofshocks8.5.1.2095=sum(SD[70:85] > 0)# Number of failures from 2011 to 2095
#
SDmean.8.5.2.1=mean(SDfilter2079)
SDstd.8.5.2.1=sd(SDfilter2079)
SDmean.8.5.2.2=mean(SDfilter2095)
SDstd.8.5.2.2=sd(SDfilter2095)
Numberofshocks8.5.2=sum(SD > 0)# Number of failures from 2011 to 2095
Numberofshocks8.5.2.2079=sum(SD[1:69] > 0)# Number of failures from 2011 to 2095
Numberofshocks8.5.2.2095=sum(SD[70:85] > 0)# Number of failures from 2011 to 2095
#
SDmean.8.5.3.1=mean(SDfilter2079)
SDstd.8.5.3.1=sd(SDfilter2079)
SDmean.8.5.3.2=mean(SDfilter2095)
SDstd.8.5.3.2=sd(SDfilter2095)
Numberofshocks8.5.3=sum(SD > 0)# Number of failures from 2011 to 2095
Numberofshocks8.5.3.2079=sum(SD[1:69] > 0)# Number of failures from 2011 to 2095
Numberofshocks8.5.3.2095=sum(SD[70:85] > 0)# Number of failures from 2011 to 2095

###############
#2011-2079
Years.1=(2079-2011)+1
Failures2.6.1=mean(Numberofshocks2.6.1.2079,Numberofshocks2.6.2.2079,Numberofshocks2.6.3.2079)
Failures4.5.1=mean(Numberofshocks4.5.1.2079,Numberofshocks4.5.2.2079,Numberofshocks4.5.3.2079)
Failures8.5.1=mean(Numberofshocks8.5.1.2079,Numberofshocks8.5.2.2079,Numberofshocks8.5.3.2079)


LambdaRCPs2.6.1=Failures2.6.1/Years.1
LambdaRCPs4.5.1=Failures4.5.1/Years.1
LambdaRCPs8.5.1=Failures8.5.1/Years.1

RCPS2.6.1shockmean=mean(SDmean.2.6.1.1,SDmean.2.6.2.1,SDmean.2.6.3.1)
RCPS2.6.1shockstd=mean(SDstd.2.6.1.1,SDstd.2.6.2.1,SDstd.2.6.3.1)
RCPS4.5.1shockmean=mean(SDmean.4.5.1.1,SDmean.4.5.2.1,SDmean.4.5.3.1)
RCPS4.5.1shockstd=mean(SDstd.4.5.1.1,SDstd.4.5.2.1,SDstd.4.5.3.1)
RCPS8.5.1shockmean=mean(SDmean.8.5.1.1,SDmean.8.5.2.1,SDmean.8.5.3.1)
RCPS8.5.1shockstd=mean(SDstd.8.5.1.1,SDstd.8.5.2.1,SDstd.8.5.3.1)
#2080-2095
Years.2=(2095-2080)+1
Failures2.6.2=mean(Numberofshocks2.6.1.2095,Numberofshocks2.6.2.2095,Numberofshocks2.6.3.2095)
Failures4.5.2=mean(Numberofshocks4.5.1.2095,Numberofshocks4.5.2.2095,Numberofshocks4.5.3.2095)
Failures8.5.2=mean(Numberofshocks8.5.1.2095,Numberofshocks8.5.2.2095,Numberofshocks8.5.3.2095)


LambdaRCPs2.6.2=Failures2.6.2/Years.2
LambdaRCPs4.5.2=Failures4.5.2/Years.2
LambdaRCPs8.5.2=Failures8.5.2/Years.2

RCPS2.6.2shockmean=mean(SDmean.2.6.1.2,SDmean.2.6.2.2,SDmean.2.6.3.2)
RCPS2.6.2shockstd=mean(SDstd.2.6.1.2,SDstd.2.6.2.2,SDstd.2.6.3.2)
RCPS4.5.2shockmean=mean(SDmean.4.5.1.2,SDmean.4.5.2.2,SDmean.4.5.3.2)
RCPS4.5.2shockstd=mean(SDstd.4.5.1.2,SDstd.4.5.2.2,SDstd.4.5.3.2)
RCPS8.5.2shockmean=mean(SDmean.8.5.1.2,SDmean.8.5.2.2,SDmean.8.5.3.2)
RCPS8.5.2shockstd=mean(SDstd.8.5.1.2,SDstd.8.5.2.2,SDstd.8.5.3.2)
#########################################################
### Top width of scour holes( Scour width size)
#The top width could range from 1.0 to 2.8 sd
#Phi angle of the scour depth is used to determine it in equation
#anglephi: inclination angle inside the scour depth ( 30 to 44 degrees)
#wb: bottom width of scour hole = 0 : narrow scour and = sd or a : wide scour
#wd: half the top scour hole width
#wdtotal: section of the top width of the scour hole


#This is not used in the analysis 
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
Poissongen=function(lambda){
  X=0
  acc=0
  f=0
  while (f==0){
    y=-log(runif(1))
    acc=acc+y
    if (acc < lambda) {X=X+1} else {f=1}
  }
  return(X)
}
#Poisson process
PP=function(lambda, N, T){
  s=T/N
  t=(0:T)/N
  X=rep(0, N+1)
  I=rep(0,N)
  X[1]=0
  for(i in 1:N) {
    I[i]=Poissongen(s*lambda)
    X[i+1]=X[i] + I[i]}
  return(X)
}
#################################################################
#Compound Poisson Process:
CPP=function(lambda, N, T,mu,sigm) {
  s=T/N
  t=(0:T)/N
  X=rep(0, N+1)
  F=rep(0, N+1)
  I=rep(0,N)
  X[1]=0
  for(i in 1:N) {
    I[i]=Poissongen(s*lambda)
    if (I[i]==0){F[i]=0} else {F[i]=rnorm(n = N,mean =mu,sd = sigm)}
    X[i+1]=X[i]+F[i]}
  return(X)
}
# Results 2011-2079
#Input
Years.1
LambdaRCPs2.6.1
LambdaRCPs4.5.1
LambdaRCPs8.5.1

RCPS2.6.1shockmean
RCPS2.6.1shockstd
RCPS4.5.1shockmean
RCPS4.5.1shockstd
RCPS8.5.1shockmean
RCPS8.5.1shockstd

#Simulations
#RCPs 2.6
Results2.6.1=as.numeric(0)
for (i in 1:1000) {
  CPP2.6.1=CPP(lambda = LambdaRCPs2.6.1,N = Years.1,T = Years.1-1,mu = RCPS2.6.1shockmean,sigm = RCPS2.6.1shockstd)
  Results2.6.1[i]=data.frame(CPP2.6.1)
}
Results2.6.1
#RCPs 4.5
Results4.5.1=as.numeric(0)
for (i in 1:1000) {
  CPP4.5.1=CPP(lambda = LambdaRCPs4.5.1,N = Years.1,T = Years.1-1,mu = RCPS4.5.1shockmean,sigm = RCPS4.5.1shockstd)
  Results4.5.1[i]=data.frame(CPP4.5.1)
}
Results4.5.1
#RCPs 8.5
Results8.5.1=as.numeric(0)
for (i in 1:1000) {
  CPP8.5.1=CPP(lambda = LambdaRCPs8.5.1,N = Years.1,T = Years.1-1,mu = RCPS8.5.1shockmean,sigm = RCPS8.5.1shockstd)
  Results8.5.1[i]=data.frame(CPP8.5.1)
}
Results8.5.1

write.xlsx(
  Results2.6.1,
  file="CPP2011.2079.xlsx",
  sheetName = "RCPs2.6.1",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE)
write.xlsx(
  Results4.5.1,
  file="CPP2011.2079.xlsx",
  sheetName = "RCPs4.5.1",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
write.xlsx(
  Results8.5.1,
  file="CPP2011.2079.xlsx",
  sheetName = "RCPs8.5.1",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)


# Results 2080-2095
#Input

Years.2

LambdaRCPs2.6.2
LambdaRCPs4.5.2
LambdaRCPs8.5.2


RCPS2.6.2shockmean
RCPS2.6.2shockstd
RCPS4.5.2shockmean
RCPS4.5.2shockstd
RCPS8.5.2shockmean
RCPS8.5.2shockstd

#RCPs 2.6
Results2.6.2=as.numeric(0)
for (i in 1:1000) {
  CPP2.6.2=CPP(lambda = LambdaRCPs2.6.2,N = Years.2,T = Years.2-1,mu = RCPS2.6.2shockmean,sigm = RCPS2.6.2shockstd)
  Results2.6.2[i]=data.frame(CPP2.6.2)
}
Results2.6.2
#RCPs 4.5
Results4.5.2=as.numeric(0)
for (i in 1:1000) {
  CPP4.5.2=CPP(lambda = LambdaRCPs4.5.2,N = Years.2,T = Years.2-1,mu = RCPS4.5.2shockmean,sigm = RCPS4.5.2shockstd)
  Results4.5.2[i]=data.frame(CPP4.5.2)
}
Results4.5.2
#RCPs 8.5
Results8.5.2=as.numeric(0)
for (i in 1:1000) {
  CPP8.5.2=CPP(lambda = LambdaRCPs8.5.2,N = Years.2,T = Years.2-1,mu = RCPS8.5.2shockmean,sigm = RCPS8.5.2shockstd)
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

# Presenting the UL and LL of the simulations 
# The code is presented for RCPs2.6 from the time interval between 2080-2095 and must be redone for other RCPs and for the another time interval from 2011-2079
# I suggest exporting the simulations and perform the following procedure done below in excel sheet (The owner of this code treated the data in excel using the Quartile method and histograms to present te distribution). However, the code is written below and can be used just take into consideration updating the fields of the RCPs and time intervals
# The procedure is as follows:
#1- calcualte the mean of the simulations for with [i] based on the length of time interval
Results2.6.2
R=data.frame(Results2.6.2)
dim(R)
R2.6.2=rowMeans(data.frame(R[2:16,]))
RCPS2.6.2shockmean
# Present the value of the shocks based on the predetermined shock mean value
R2.6.2Filtered=as.numeric(1)
for (i in 1:length(R2.6.2)) {
  R2.6.2Filtered[i]=ifelse(R2.6.2[i]<=RCPS2.6.2shockmean,0,R2.6.2[i])
}
R2.6.2Filtered1=as.numeric(1)

for (i in 1:length(R2.6.2)) {
  R2.6.2Filtered1[i]=ifelse(R2.6.2[i]<=RCPS2.6.2shockmean*2,R2.6.2Filtered[i],0)
}
R2.6.2Filtered
R2.6.2Filtered1
R2.6.2F=array(data = c('array values of shocks that should be inserted with comma separation'))# THis should be updated for each simulation

#2- Compute the 95% CI of the distribution UL and LL, in addition to considering the mean shock size to represent the accumulation if exist
zscore=array(1.96,dim = length(R2.6.2))
RR=ts(R[2:16])
StdR2.6.2=as.numeric(1)
for (i in 1:length(R2.6.2)) {
  StdR2.6.2[i]=sd(RR[i,])
}
StdR2.6.2
SQRT=array(data=sqrt(1000),dim = 15)
RCPs2.6RUL=R2.6.2F+(zscore*(StdR2.6.2/SQRT))

RCPs2.6RULFiltered=as.numeric(1)
for (i in 1:length(R2.6.2)) {
  RCPs2.6RULFiltered[i]=ifelse(RCPs2.6RUL[i]<=RCPS2.6.2shockmean,0,RCPs2.6RUL[i])
}
RCPs2.6RULFiltered

RCPs2.6RULFiltered1=as.numeric(1)
for (i in 1:length(R2.6.2)) {
  RCPs2.6RULFiltered1[i]=ifelse(RCPs2.6RUL[i]>RCPS2.6.2shockmean*2,0,1)
}
RCPs2.6RULFiltered1# Decision if there is a second shock
R2.6.2FUL=array(data = c('array values of shocks that should be inserted with comma separation'))# THis should be updated for each simulation

RCPs2.6RLL=R2.6.2F-(zscore*(StdR2.6.2/SQRT))

RCPs2.6RLLFiltered=as.numeric(1)
for (i in 1:length(R2.6.2)) {
  RCPs2.6RLLFiltered[i]=ifelse(RCPs2.6RLL[i]<=RCPS2.6.2shockmean,0,RCPs2.6RLL[i])
}
RCPs2.6RLLFiltered

RCPs2.6RLLFiltered1=as.numeric(1)
for (i in 1:length(R2.6.2)) {
  RCPs2.6RLLFiltered1[i]=ifelse(RCPs2.6RLL[i]>RCPS2.6.2shockmean*2,1,0)
}
RCPs2.6RLLFiltered1# zero values resemble que there is no second shock
R2.6.2FLL=array(data = c('array values of shocks that should be inserted with comma separation'))# THis should be updated for each simulation


# CPP degradation model of shocks
##compute the probability that a system reaches threshold k before t= j years with shock size mean value
#Failure occurs when number of events n>:





# The probability of failure is based on the rate of failure computed on the expected lifespan of the structure which is affected by the shocks (computed before) exceeding the foundation depth

#2080-2095
# The expected lifespan is treated in excel following the distribution of the simulations as found in the article using the quartile method to provide the UL and LL limit
YTF2.6.2.U='Values of expected Lifespan: Read line 780 for information'
YTF2.6.2.L='Values of expected Lifespan: Read line 780 for information'

YTF4.5.2.U='Values of expected Lifespan: Read line 780 for information'
YTF4.5.2.L='Values of expected Lifespan: Read line 780 for information'

YTF8.5.2.U='Values of expected Lifespan: Read line 780 for information'
YTF8.5.2.L='Values of expected Lifespan: Read line 780 for information'
#2011-2095
YTF2.6.U='Values of expected Lifespan: Read line 780 for information'
YTF2.6.L='Values of expected Lifespan: Read line 780 for information'
YTF4.5.U='Values of expected Lifespan: Read line 780 for information'
YTF4.5.L='Values of expected Lifespan: Read line 780 for information'
YTF8.5.U='Values of expected Lifespan: Read line 780 for information'
YTF8.5.L='Values of expected Lifespan: Read line 780 for information'

##2011-2079
YTF2.6.1.U='Values of expected Lifespan: Read line 780 for information'
YTF2.6.1.L='Values of expected Lifespan: Read line 780 for information'

YTF4.5.1.U='Values of expected Lifespan: Read line 780 for information'
YTF4.5.1.L='Values of expected Lifespan: Read line 780 for information'

YTF8.5.1.U='Values of expected Lifespan: Read line 780 for information'
YTF8.5.1.L='Values of expected Lifespan: Read line 780 for information'

# In the case of fix shock sizes, the failure probability at life time t=j: 
# i is the number >n
#P(V(t) ??? k) = P(N(t) > n)



#2011-2079

n=(V0-k)/u;n# This indicates the number of shocks till failure ( in this case we already know it based on the analysis of the 95% CI of the CPP simulations)
n2.6UL=YTF2.6.1.U
n2.6LL=YTF2.6.1.L
#Upper 
V2.6=0
V2.6=as.numeric(100)
probabilityoffailure2.6UL=as.numeric(100)
n=2
lambda2.6=n/n2.6UL
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
lambda2.6=n/n2.6LL
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
POF2.6.1=data.frame(probabilityoffailure2.6LL,probabilityoffailure2.6UL)
##########################################


n4.5LL=YTF4.5.1.L
n4.5UL=YTF4.5.1.U

# In the case of fix shock sizes, the failure probability at life time t=j: 
# i is the number >n
#P(V(t) ??? k) = P(N(t) > n)
#Lower
V4.5=0
V4.5=as.numeric(100)
probabilityoffailure4.5LL=as.numeric(100)
j=1
lambda4.5=n/n4.5LL
for(j in 1:100) {
  i=1
  while(i <=n) {
    x4.5=((lambda4.5*j)^0)*exp(-lambda4.5*j)/factorial(0)
    V4.5[i]=((lambda4.5*j)^i)*exp(-lambda4.5*j)/factorial(i)
    probabilityoffailure4.5LL[j]=1-(x4.5+sum(V4.5))
    print(V4.5[i])
    i=i+1
  }
  
}
plot(V4.5)
probabilityoffailure4.5LL#probability of the system to reach the threshold before each year
plot(probabilityoffailure4.5LL)

#Upper
V4.5=0
V4.5=as.numeric(100)
probabilityoffailure4.5UL=as.numeric(100)
lambda4.5=n/n4.5UL
j=1
for(j in 1:100) {
  i=1
  while(i <=n) {
    x4.5=((lambda4.5*j)^0)*exp(-lambda4.5*j)/factorial(0)
    V4.5[i]=((lambda4.5*j)^i)*exp(-lambda4.5*j)/factorial(i)
    probabilityoffailure4.5UL[j]=1-(x4.5+sum(V4.5))
    print(V4.5[i])
    i=i+1
  }
  
}
plot(V4.5)
probabilityoffailure4.5UL#probability of the system to reach the threshold before each year
plot(probabilityoffailure4.5UL)
lines(probabilityoffailure4.5LL)
POF4.5.1=data.frame(probabilityoffailure4.5LL,probabilityoffailure4.5UL)
##########################################

n8.5LL=YTF8.5.1.L
n8.5UL=YTF8.5.1.U

V8.5=0
V8.5=as.numeric(100)
probabilityoffailure8.5LL=as.numeric(100)
lambda8.5=n/n8.5LL

j=1
for(j in 1:100) {
  i=1
  while(i <=n) {
    x8.5=((lambda8.5*j)^0)*exp(-lambda8.5*j)/factorial(0)
    V8.5[i]=((lambda8.5*j)^i)*exp(-lambda8.5*j)/factorial(i)
    probabilityoffailure8.5LL[j]=1-(x8.5+sum(V8.5))
    print(V8.5[i])
    i=i+1
  }
  
}
plot(V8.5)
probabilityoffailure8.5LL
plot(probabilityoffailure8.5LL)
# In the case of fix shock sizes, the failure probability at life time t=j: 
# i is the number >n
#P(V(t) ??? k) = P(N(t) > n)

V8.5=0
V8.5=as.numeric(100)
probabilityoffailure8.5UL=as.numeric(100)
lambda8.5=n/n8.5UL

j=1
for(j in 1:100) {
  i=1
  while(i <=n) {
    x8.5=((lambda8.5*j)^0)*exp(-lambda8.5*j)/factorial(0)
    V8.5[i]=((lambda8.5*j)^i)*exp(-lambda8.5*j)/factorial(i)
    probabilityoffailure8.5UL[j]=1-(x8.5+sum(V8.5))
    print(V8.5[i])
    i=i+1
  }
  
}
plot(V8.5)
probabilityoffailure8.5UL#probability of the system to reach the threshold before each year
plot(probabilityoffailure8.5UL)
lines(probabilityoffailure8.5LL)
POF8.5.1=data.frame(probabilityoffailure8.5LL,probabilityoffailure8.5UL)
POF=data.frame(POF2.6.1,POF4.5.1,POF8.5.1)
write.xlsx(
  POF,
  file="Probability of failure with limits from 2011-2079.xlsx",
  sheetName = "POF",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE)




#2080-2095
YTF2.6.2.U
YTF2.6.2.L

n2.6UL=YTF2.6.2.U
n2.6LL=YTF2.6.2.L
#Upper 
V2.6=0
V2.6=as.numeric(100)
probabilityoffailure2.6UL=as.numeric(100)
lambda2.6=n/n2.6UL

j=1
for(j in 1:50) {
  i=1
  while(i <=n) {
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
lambda2.6=n/n2.6LL
j=1
for(j in 1:50) {
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
plot(probabilityoffailure2.6LL)
lines(probabilityoffailure2.6UL)
probabilityoffailure2.6LL=array(data=0,dim = 50)
POF2.6.2=data.frame(probabilityoffailure2.6LL,probabilityoffailure2.6UL)
##########################################
YTF4.5.2.U
YTF4.5.2.L

n4.5LL=YTF4.5.2.L
n4.5UL=YTF4.5.2.U

probabilityoffailure4.5LL=array(data=0,dim=50) # This is since there is no failure
#Upper
V4.5=0
V4.5=as.numeric(100)
probabilityoffailure4.5UL=as.numeric(100)
lambda4.5=n/n4.5UL
j=1
for(j in 1:50) {
  i=1
  while(i <=n) {
    x4.5=((lambda4.5*j)^0)*exp(-lambda4.5*j)/factorial(0)
    V4.5[i]=((lambda4.5*j)^i)*exp(-lambda4.5*j)/factorial(i)
    probabilityoffailure4.5UL[j]=1-(x4.5+sum(V4.5))
    print(V4.5[i])
    i=i+1
  }
  
}
plot(V4.5)
probabilityoffailure4.5UL#probability of the system to reach the threshold before each year
plot(probabilityoffailure4.5UL)
lines(probabilityoffailure4.5LL)
POF4.5.2=data.frame(probabilityoffailure4.5LL,probabilityoffailure4.5UL)
##########################################
YTF8.5.2.L
YTF8.5.2.U
n8.5LL=YTF8.5.2.L
n8.5UL=YTF8.5.2.U

V8.5=0
V8.5=as.numeric(100)
probabilityoffailure8.5LL=as.numeric(100)
lambda8.5=n/n8.5LL
j=1
for(j in 1:50) {
  i=1
  while(i <=n) {
    x8.5=((lambda8.5*j)^0)*exp(-lambda8.5*j)/factorial(0)
    V8.5[i]=((lambda8.5*j)^i)*exp(-lambda8.5*j)/factorial(i)
    probabilityoffailure8.5LL[j]=1-(x8.5+sum(V8.5))
    print(V8.5[i])
    i=i+1
  }
  
}
plot(V8.5)
probabilityoffailure8.5LL
# In the case of fix shock sizes, the failure probability at life time t=j: 
# i is the number >n
#P(V(t) ??? k) = P(N(t) > n)

V8.5=0
V8.5=as.numeric(100)
probabilityoffailure8.5UL=as.numeric(100)
lambda8.5=n/n8.5UL
j=1
for(j in 1:50) {
  i=1
  while(i <=n) {
    x8.5=((lambda8.5*j)^0)*exp(-lambda8.5*j)/factorial(0)
    V8.5[i]=((lambda8.5*j)^i)*exp(-lambda8.5*j)/factorial(i)
    probabilityoffailure8.5UL[j]=1-(x8.5+sum(V8.5))
    print(V8.5[i])
    i=i+1
  }
  
}
plot(V8.5)
probabilityoffailure8.5UL#probability of the system to reach the threshold before each year
plot(probabilityoffailure8.5UL)
lines(probabilityoffailure8.5LL)
POF8.5.2=data.frame(probabilityoffailure8.5LL,probabilityoffailure8.5UL)
POFCI=data.frame(POF2.6.2,POF4.5.2,POF8.5.2)

write.xlsx(
  POFCI,
  file="Probability of failure with limits from 2080-2095.xlsx",
  sheetName = "POF",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE)
###########
# 2011-2095
YTF2.6.U
YTF2.6.L

n=(V0-k)/u;n
n2.6UL=YTF2.6.U
n2.6LL=YTF2.6.L
#Upper 
V2.6=0
V2.6=as.numeric(100)
probabilityoffailure2.6UL=as.numeric(100)
lambda2.6=n/n2.6UL

j=1
for(j in 1:125) {
  i=1
  while(i <=n) {
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
lambda2.6=n/n2.6LL
j=1
for(j in 1:125) {
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
POF2.6.3=data.frame(probabilityoffailure2.6LL,probabilityoffailure2.6UL)
##########################################
YTF4.5.U
YTF4.5.L

n4.5LL=YTF4.5.L
n4.5UL=YTF4.5.U
# In the case of fix shock sizes, the failure probability at life time t=j: 
# i is the number >n
#P(V(t) ??? k) = P(N(t) > n)
#Lower
V4.5=0
V4.5=as.numeric(100)
probabilityoffailure4.5LL=as.numeric(100)
lambda4.5=n/n4.5LL
j=1
for(j in 1:125) {
  i=1
  while(i <=n) {
    x4.5=((lambda4.5*j)^0)*exp(-lambda4.5*j)/factorial(0)
    V4.5[i]=((lambda4.5*j)^i)*exp(-lambda4.5*j)/factorial(i)
    probabilityoffailure4.5LL[j]=1-(x4.5+sum(V4.5))
    print(V4.5[i])
    i=i+1
  }
  
}
plot(V4.5)
probabilityoffailure4.5LL#probability of the system to reach the threshold before each year
plot(probabilityoffailure4.5LL)

#Upper
V4.5=0
V4.5=as.numeric(100)
probabilityoffailure4.5UL=as.numeric(100)
lambda4.5=n/n4.5UL

j=1
for(j in 1:125) {
  i=1
  while(i <=n) {
    x4.5=((lambda4.5*j)^0)*exp(-lambda4.5*j)/factorial(0)
    V4.5[i]=((lambda4.5*j)^i)*exp(-lambda4.5*j)/factorial(i)
    probabilityoffailure4.5UL[j]=1-(x4.5+sum(V4.5))
    print(V4.5[i])
    i=i+1
  }
  
}
plot(V4.5)
probabilityoffailure4.5UL#probability of the system to reach the threshold before each year
plot(probabilityoffailure4.5UL)
lines(probabilityoffailure4.5LL)
POF4.5.3=data.frame(probabilityoffailure4.5LL,probabilityoffailure4.5UL)
##########################################

YTF8.5.U
YTF8.5.L
n8.5LL=YTF8.5.L
n8.5UL=YTF8.5.U

V8.5=0
V8.5=as.numeric(100)
probabilityoffailure8.5LL=as.numeric(100)
lambda8.5=n/n8.5LL
j=1
for(j in 1:125) {
  i=1
  while(i <=n) {
    x8.5=((lambda8.5*j)^0)*exp(-lambda8.5*j)/factorial(0)
    V8.5[i]=((lambda8.5*j)^i)*exp(-lambda8.5*j)/factorial(i)
    probabilityoffailure8.5LL[j]=1-(x8.5+sum(V8.5))
    print(V8.5[i])
    i=i+1
  }
  
}
plot(V8.5)
probabilityoffailure8.5LL
# In the case of fix shock sizes, the failure probability at life time t=j: 
# i is the number >n
#P(V(t) ??? k) = P(N(t) > n)

V8.5=0
V8.5=as.numeric(100)
probabilityoffailure8.5UL=as.numeric(100)
lambda8.5=n/n8.5UL

j=1
for(j in 1:125) {
  i=1
  while(i <=n) {
    x8.5=((lambda8.5*j)^0)*exp(-lambda8.5*j)/factorial(0)
    V8.5[i]=((lambda8.5*j)^i)*exp(-lambda8.5*j)/factorial(i)
    probabilityoffailure8.5UL[j]=1-(x8.5+sum(V8.5))
    print(V8.5[i])
    i=i+1
  }
  
}
plot(V8.5)
probabilityoffailure8.5UL#probability of the system to reach the threshold before each year
plot(probabilityoffailure8.5UL)
lines(probabilityoffailure8.5LL)
POF8.5.3=data.frame(probabilityoffailure8.5LL,probabilityoffailure8.5UL)
POFTotal=data.frame(POF2.6.3,POF4.5.3,POF8.5.3)

write.xlsx(
  POFTotal,
  file="Probability of failure with limits from 2011-2095.xlsx",
  sheetName = "POF",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE)
#########################
#########################
########################










