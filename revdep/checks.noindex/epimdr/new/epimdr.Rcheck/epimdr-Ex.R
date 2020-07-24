pkgname <- "epimdr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('epimdr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("BarabasiAlbert")
### * BarabasiAlbert

flush(stderr()); flush(stdout())

### Name: BarabasiAlbert
### Title: Function to generate a Barabasi-Albert network
### Aliases: BarabasiAlbert

### ** Examples

cm3=BarabasiAlbert(200, 4)



cleanEx()
nameEx("May.app")
### * May.app

flush(stderr()); flush(stdout())

### Name: May.app
### Title: Launch a shiny-app simulating May's Parasitoid-host Model model
### Aliases: May.app
### Keywords: datasets

### ** Examples

## Not run: May.app



cleanEx()
nameEx("NB")
### * NB

flush(stderr()); flush(stdout())

### Name: NB
### Title: The Nicholson-Bailey model
### Aliases: NB

### ** Examples

sim= NB(R=1.1,a=0.1)



cleanEx()
nameEx("NetworkSIR")
### * NetworkSIR

flush(stderr()); flush(stdout())

### Name: NetworkSIR
### Title: Function to simulate an epidemic on a network
### Aliases: NetworkSIR netSIR

### ** Examples

cm1=BarabasiAlbert(N=200,K=2)
sim1=NetworkSIR(cm1,.3,0.1)
summary(sim1)
## Not run: plot(sim1)



cleanEx()
nameEx("SEIR.app")
### * SEIR.app

flush(stderr()); flush(stdout())

### Name: SEIR.app
### Title: Launch a shiny-app simulating the seasonal SEIR model
### Aliases: SEIR.app
### Keywords: datasets

### ** Examples

## Not run: SEIR.app



cleanEx()
nameEx("SEIRS.app")
### * SEIRS.app

flush(stderr()); flush(stdout())

### Name: SEIRS.app
### Title: Launch a shiny-app simulating the SEIRS model
### Aliases: SEIRS.app
### Keywords: datasets

### ** Examples

## Not run: SEIRS.app



cleanEx()
nameEx("SIR.app")
### * SIR.app

flush(stderr()); flush(stdout())

### Name: SIR.app
### Title: Launch a shiny-app simulating the SIR model
### Aliases: SIR.app
### Keywords: datasets

### ** Examples

## Not run: SIR.app



cleanEx()
nameEx("SimTsir")
### * SimTsir

flush(stderr()); flush(stdout())

### Name: SimTsir
### Title: Function to simulate the stochastic TSIR
### Aliases: SimTsir

### ** Examples

out = SimTsir()



cleanEx()
nameEx("SimTsir2")
### * SimTsir2

flush(stderr()); flush(stdout())

### Name: SimTsir2
### Title: Function to simulate the seasonally-forced TSIR
### Aliases: SimTsir2

### ** Examples

## Not run: see chapter 8 in book



cleanEx()
nameEx("TSIR.app")
### * TSIR.app

flush(stderr()); flush(stdout())

### Name: TSIR.app
### Title: Launch a shiny-app simulating TSIR model
### Aliases: TSIR.app
### Keywords: datasets

### ** Examples

## Not run: TSIR.app



cleanEx()
nameEx("TSIRllyap")
### * TSIRllyap

flush(stderr()); flush(stdout())

### Name: TSIRllyap
### Title: Function to calculate the local Lyapunov exponents for the TSIR
### Aliases: TSIRllyap

### ** Examples

## Not run: see chapter 10 in book



cleanEx()
nameEx("TSIRlyap")
### * TSIRlyap

flush(stderr()); flush(stdout())

### Name: TSIRlyap
### Title: Function to do Lyapunov exponent calculations from a TSIR
###   simulation
### Aliases: TSIRlyap

### ** Examples

## Not run: see chapter 10 in book



cleanEx()
nameEx("WattsStrogatz")
### * WattsStrogatz

flush(stderr()); flush(stdout())

### Name: WattsStrogatz
### Title: Function to generate a Watts-Strogats network
### Aliases: WattsStrogatz

### ** Examples

cm2=WattsStrogatz(N=20, K=4, Prw=.3)



cleanEx()
nameEx("chainSIR")
### * chainSIR

flush(stderr()); flush(stdout())

### Name: chainSIR
### Title: Gradient-function for the chain-SIR model
### Aliases: chainSIR

### ** Examples

require(deSolve)
times  = seq(0, 10, by=1/52)
paras2  = c(mu = 1/75, N = 1, beta =  625, gamma = 365/14, u=5)
xstart2 = log(c(S=.06, I=c(0.001, rep(0.0001, paras2["u"]-1)), R = 0.0001))
out = as.data.frame(ode(xstart2, times, chainSIR, paras2))



cleanEx()
nameEx("coyne")
### * coyne

flush(stderr()); flush(stdout())

### Name: coyne
### Title: Gradient-function for Coyne et al's rabies model
### Aliases: coyne

### ** Examples

require(deSolve)
times  = seq(0, 50, by=1/520)
paras  = c(gamma = 0.0397, b = 0.836, a = 1.34, sigma = 7.5, 
alpha = 66.36, beta = 33.25, c = 0, rho = 0.8)
start = log(c(X=12.69/2, H1=0.1, H2=0.1, Y = 0.1, I = 0.1))
out = as.data.frame(ode(start, times, coyne, paras))



cleanEx()
nameEx("flowField")
### * flowField

flush(stderr()); flush(stdout())

### Name: flowField
### Title: Flowfield
### Aliases: flowField

### ** Examples

#See archived phaseR package for examples



cleanEx()
nameEx("gillespie")
### * gillespie

flush(stderr()); flush(stdout())

### Name: gillespie
### Title: Gillespie exact algorithm
### Aliases: gillespie

### ** Examples

rlist=c(quote(mu * (S+I+R)), quote(mu * S), quote(beta * S * I /(S+I+R)), 
 quote(mu * I), quote(gamma * I), quote(mu*R))
emat=matrix(c(1,0,0,-1,0,0,-1,1,0,0,-1,0,0,-1,1,0,0,-1),ncol=3, byrow=TRUE)
paras  = c(mu = 1, beta =  1000, gamma = 365/20)
inits = c(S=100, I=2, R=0)
sim=gillespie(rlist, emat, paras, inits, 100)



cleanEx()
nameEx("llik.cb")
### * llik.cb

flush(stderr()); flush(stdout())

### Name: llik.cb
### Title: Negative log-likelihood function for the chain-binomial model
### Aliases: llik.cb

### ** Examples

twoweek=rep(1:15, each=2)
niamey_cases1=sapply(split(niamey$cases_1[1:30], twoweek), sum)
llik.cb(S0=6500, beta=23, I=niamey_cases1)



cleanEx()
nameEx("llik.pc")
### * llik.pc

flush(stderr()); flush(stdout())

### Name: llik.pc
### Title: Function to estimate parameters for the picewise-constant
###   catalytic model
### Aliases: llik.pc

### ** Examples

x=c(1,4,8,12,18,24)
para=rep(.1,length(x))
## Not run: optim(par=log(para),fn=loglikpc, age=rabbit$a, num=rabbit$inf, denom=rabbit$n, up=x)



cleanEx()
nameEx("orv.app")
### * orv.app

flush(stderr()); flush(stdout())

### Name: orv.app
### Title: Launch a shiny-app to study outbreak-response vaccination
###   campaigns
### Aliases: orv.app
### Keywords: datasets

### ** Examples

## Not run: orv.app



cleanEx()
nameEx("plot.cm")
### * plot.cm

flush(stderr()); flush(stdout())

### Name: plot.cm
### Title: Function to plot an object of class CM
### Aliases: plot.cm

### ** Examples

cm=ringlattice(N=20,K=4)
## Not run: plot(cm)



cleanEx()
nameEx("r0fun")
### * r0fun

flush(stderr()); flush(stdout())

### Name: r0fun
### Title: Function to calculate R0 from a contact matrix
### Aliases: r0fun

### ** Examples

cm1=BarabasiAlbert(N=200,K=2)
r0fun(cm1, 0.3, 0.1)



cleanEx()
nameEx("retrospec")
### * retrospec

flush(stderr()); flush(stdout())

### Name: retrospec
### Title: Function to predict efficacy of outbreak-response vaccination
###   campaign
### Aliases: retrospec

### ** Examples

red1=retrospec(R=1.8, 161, vaccine_efficacy=0.85, target_vaccination=0.5, 
 intervention_length=10, mtime=250, LP=8, IP=5, N=16000)
1-red1$redn



cleanEx()
nameEx("ringlattice")
### * ringlattice

flush(stderr()); flush(stdout())

### Name: ringlattice
### Title: Function to generate a ring lattice
### Aliases: ringlattice

### ** Examples

cm=ringlattice(N=20,K=4)



cleanEx()
nameEx("seirmod")
### * seirmod

flush(stderr()); flush(stdout())

### Name: seirmod
### Title: Gradient-function for the SEIR model
### Aliases: seirmod

### ** Examples

require(deSolve)
times  = seq(0, 10, by=1/120)
paras  = c(mu = 1/50, N = 1, beta =  1000, sigma = 365/8, gamma = 365/5)
start = c(S=0.06, E=0, I=0.001, R = 0.939)
out=ode(y=start, times=times, func=seirmod, parms=paras)



cleanEx()
nameEx("seirmod2")
### * seirmod2

flush(stderr()); flush(stdout())

### Name: seirmod2
### Title: Gradient-function for the forced SEIR model
### Aliases: seirmod2

### ** Examples

require(deSolve)
times  = seq(0, 10, by=1/120)
paras  = c(mu = 1/50, N = 1, beta0 = 1000, beta1 = 0.2, sigma = 365/8, gamma = 365/5)
start = c(S=0.06, E=0, I=0.001, R = 0.939)
out=ode(y=start, times=times, func=seirmod2, parms=paras)



cleanEx()
nameEx("sim.cb")
### * sim.cb

flush(stderr()); flush(stdout())

### Name: sim.cb
### Title: Function to simulate the chain-binomial model
### Aliases: sim.cb

### ** Examples

sim=sim.cb(S0=6500, beta=23)



cleanEx()
nameEx("siragemod")
### * siragemod

flush(stderr()); flush(stdout())

### Name: siragemod
### Title: Gradient-function for the age-structured SIR model with possibly
###   heterogeneous mixing
### Aliases: siragemod

### ** Examples

a=rep(1,4)
n=length(a)
betaM=matrix(1, ncol=4, nrow=4)
pars =list(N=1, gamma=365/14, mu=0.02, sigma=0.2, beta=500, betaM=betaM,p=rep(0,4), a=a)
xstart<-log(c(S=rep(0.099/n,n), I=rep(0.001/n,n), R=rep(0.9/n,n)))
times=seq(0,10,by=14/365)
out=as.data.frame(ode(xstart, times=times, func=siragemod, parms=pars))



cleanEx()
nameEx("sirmod")
### * sirmod

flush(stderr()); flush(stdout())

### Name: sirmod
### Title: Gradient-function for the SIR model
### Aliases: sirmod

### ** Examples

require(deSolve)
times  = seq(0, 26, by=1/10)
paras  = c(mu = 0, N = 1, beta =  2, gamma = 1/2)
start = c(S=0.999, I=0.001, R = 0)
out=ode(y=start, times=times, func=sirmod, parms=paras)



cleanEx()
nameEx("sirwmod")
### * sirwmod

flush(stderr()); flush(stdout())

### Name: sirwmod
### Title: Gradient-function for the SIRWS model
### Aliases: sirwmod

### ** Examples

require(deSolve)
times  = seq(0, 26, by=1/10)
paras  = c(mu = 1/70, p=0.2, N = 1, beta = 200, omega = 1/10, gamma = 17, kappa=30)
start = log(c(S=0.06, I=0.01, R=0.92, W = 0.01))
out = as.data.frame(ode(start, times, sirwmod, paras))



cleanEx()
nameEx("summary.cm")
### * summary.cm

flush(stderr()); flush(stdout())

### Name: summary.cm
### Title: Function to calculate the degree distribution for an object of
###   class CM
### Aliases: summary.cm

### ** Examples

cm=WattsStrogatz(N=20, K=4, Prw=.3)
summary(cm)



cleanEx()
nameEx("tau")
### * tau

flush(stderr()); flush(stdout())

### Name: tau
### Title: Gillespie tau-leap algorithm
### Aliases: tau

### ** Examples

rlist2=c(quote(mu * (S+E+I+R)), quote(mu * S), quote(beta * S * I/(S+E+I+R)), 
 quote(mu*E), quote(sigma * E), quote(mu * I), quote(gamma * I), quote(mu*R))
emat2=matrix(c(1,0,0,0,-1,0,0,0,-1,1,0,0,0,-1,0,0,0,-1,1,0,0,0,-1,0,0,0,-1,1,0,0,0,-1),
ncol=4, byrow=TRUE)
paras  = c(mu = 1, beta =  1000, sigma = 365/8, gamma = 365/5)
inits = c(S=999, E=0, I=1, R = 0)
sim2=tau(rlist2, emat2, paras, inits, 1/365, 1)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
