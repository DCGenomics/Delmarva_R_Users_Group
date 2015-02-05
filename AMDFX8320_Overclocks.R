# R code chunks pulled from .RNW formatted to compile with knitr & pdflatex
####
#predictR custom function
#you can use this custom function to generate well-formatted linear model forumlas, and convert between unknown and known values in a linear model
#this handy function was written by Dr. Philip Anderson of Salisbury University
GetLMFormula<-function(LM_List,Digits,Whitespace) {
  if ( !isTRUE(is.list(LM_List)) )  {
    stop('ERROR: You must use a list for this function. Exiting...\n\n')
  }
  if ( !isTRUE(is.logical(Whitespace)) )  {
    stop('ERROR: You must indicate TRUE/FALSE if you want whitespace in the formula. Exiting...\n\n')
  }
  if ( isTRUE(Whitespace) )  {
    SEP<-' '
  } else {
    SEP<-''
  }
  if (as.numeric(LM_List[[1]][1]) >= 0 ) {
    return(paste('y',
                 '=',
                 paste(signif(LM_List[[1]][2],digits=Digits),'x',sep=''),
                 '+',
                 signif(LM_List[[1]][1],digits=Digits),sep=SEP) 
    )
  } else {
    return(paste('y',
                 '=',
                 paste(signif(LM_List[[1]][2],digits=Digits),'x',sep=''),
                 '-',
                 signif(abs(LM_List[[1]][1]),digits=Digits),sep=SEP)
    )
  }
}

SolveUnkConc<-function(LM_List,KnownAbs,Digits) {
  if ( !isTRUE(is.list(LM)) )  {
    stop('ERROR: You must use a list for this function. Exiting...\n\n')
  }
  else if ( !isTRUE(is.numeric(KnownAbs)|is.integer(KnownAbs)) ) {
    stop('ERROR: Your known absorbance(s) must be numeric. Exiting...\n\n')
  }
  if (as.numeric(LM_List[[1]][1]) >= 0 ) {
    tmp<-signif(((KnownAbs-LM_List[[1]][1])/LM_List[[1]][2]),digits=Digits)
  } else {
    tmp<-signif(((KnownAbs+abs(LM_List[[1]][1]))/LM_List[[1]][2]),digits=Digits)
  }
  names(tmp)<-'Concentration'
  return(tmp)
}

SolveUnkAbs<-function(LM_List,KnownConc,Digits) {
  if ( !isTRUE(is.list(LM)) )  {
    stop('ERROR: You must use a list for this function. Exiting...\n\n')
  }
  else if ( !isTRUE(is.numeric(KnownConc)|is.integer(KnownConc)) ) {
    stop('ERROR: Your known concentration(s) must be numeric. Exiting...\n\n')
  }
  if (as.numeric(LM_List[[1]][1]) >= 0 ) {
    tmp<-signif(((KnownConc*LM_List[[1]][2])+LM_List[[1]][1]),digits=Digits)
  } else {
    tmp<-signif(((KnownConc*LM_List[[1]][2])-abs(LM_List[[1]][1])),digits=Digits)
  }
  names(tmp)<-'Absorbance'
  return(tmp)
}
####
# raw data entry
speed<-c(rep(3.5,times=10),
         rep(3.6,times=3),
         rep(3.7,times=3),
         rep(3.8,times=3),
         rep(3.9,times=6),
         rep(4.0,times=4),
         rep(4.1,times=3),
         rep(4.2,times=6)
)
multiplier<-c(rep(17.5,times=10),
              rep(18,times=3),
              rep(18.5,times=3),
              rep(19,times=3),
              rep(19.5,times=6),
              rep(20,times=4),
              rep(20.5,times=3),
              rep(21,times=6)
)
load<-c(0,
        rep(100,times=6),
        NA,NA,
        100,
        NA,
        100,100,
        NA,
        100,100,
        NA,
        100,100,
        NA,
        rep(100,times=18)
)
stability<-c(rep("S",times=6),#S: stable, U: unstable
             rep("U",times=3),
             "S",
             "U",
             "S","S",
             "U","U",
             "S",
             "U","U",
             "S",
             "U","U",
             "U","U",
             "S","S",
             "U","U",
             "S","S",
             "U","S","S","U","U","U","U","S","S")
voltage<-c(1.1875,1.1875,
           1.1750,1.1625,1.1500,1.1375,1.1250,1.1125,
           1.1375,1.1500,1.1500,1.1625,1.1625,1.1625,1.1750,1.1875,1.1875,1.2000,
           1.2125,1.2125,1.2250,1.2375,1.2375,1.2500,1.2500,1.2500,1.2625,1.2750,1.2750,
           1.2750,1.2875,1.2875,1.2875,1.3000,1.3125,1.3125,1.3250,1.3250)
CPUtemps<-c(11,25,26,25,25,24,24,NA,NA,26,NA,27,28,NA,29,30,NA,29,30,NA,31,31,32,32,31,32,34,34,33,
            NA,32,32,NA,33,35,34,35,34)
CPUTINtemp<-c(NA,NA,45,45,NA,43,43,NA,NA,45,NA,46,47,NA,47,49,NA,48,49,NA,49,49,51,51,51,50,53,53,52,
              NA,50,51,NA,51,52,53,53,53)
watts<-c(99,183,179,178,176,174,174,NA,NA,174,NA,179,181,NA,NA,190,NA,195,196,NA,205,206,202,210,205,210,213,218,216,
         NA,215,216,NA,NA,224,228,228,228)

Data<-data.frame(stability=stability,
                 speed=speed,
                 multiplier=multiplier,
                 voltage=voltage,
                 watts=watts,
                 CPUTINtemp=CPUTINtemp,
                 CPUtemps=CPUtemps,
                 load=load)
#####
# make some xtables
suppressPackageStartupMessages(require(xtable))

print(xtable(Data,NA.string = "NA",caption = "All recorded data. Stability; S = Stable, U = Unstable. Temperature = degress Celsius. Load = percentage of processor usage."))
#JUST VALUES FROM STABLE SETTINGS
Data_stable<-subset(Data,subset=(stability=="S"))

#JUST THE MINIMUM VOLTAGES NEEDED TO REACH STABILITY
StableMinmumVoltages<-Data_stable[Data_stable$voltage == ave(x=Data_stable$voltage,
                                                             Data_stable$speed,
                                                             FUN=min), ]

#LINEAR MODEL OF STABLE VOLTAGES AGAINST SPEEDS
LM_SpeedVolts<-lm(voltage~speed,data=StableMinmumVoltages)
formula_speedvolts<-GetLMFormula(LM_List = LM_SpeedVolts,Digits = 4,Whitespace = T)


LM_stable<-lm(voltage~speed,data=Data_stable)
formula_stable<-GetLMFormula(LM_List = LM_stable,Digits = 4,Whitespace = T)
#ALL THE VALUES AT FULL LOAD (STABLE & UNSTABLE)
Data_fullload<-subset(Data,subset=(load=="100"))

#LINEAR MODEL OF ALL VOLTAGES (stable & unstable) AGAINST CPU TEMPERATURES at full load
LM_tempvolts<-lm(CPUtemps~voltage,data=Data_fullload)
signif(summary(LM_tempvolts)$r.squared,digits=4)
formula_tempvolts<-GetLMFormula(LM_List = LM_tempvolts,Digits = 4,Whitespace = T)
#####
#plots
plot(voltage~speed,
     data=StableMinmumVoltages,
     main="Stable CPU Speed vs. Minimum Voltage",
     ylab="Volts",
     xlab="Speed (GHz)")
abline(LM_SpeedVolts,col="red")
legend("bottomright",
       legend=formula_speedvolts,
       title=as.expression(bquote(R^2==.(signif(summary(LM_SpeedVolts)$r.squared,digits=4)))),
       box.lty=0)

plot(voltage~speed,
     data=Data_stable,
     main="Stable CPU Speed vs. Voltage",
     ylab="Volts",
     xlab="Speed (GHz)")
abline(LM_stable,col="red")
legend("bottomright",
       legend=formula_stable,
       title=as.expression(bquote(R^2==.(signif(summary(LM_stable)$r.squared,digits=4)))),
       box.lty=0)

plot(CPUtemps~voltage,
     data=Data_fullload,
     main="CPU Temperatures vs. Voltage",
     ylab="Degrees C",
     xlab="Volts")
abline(LM_tempvolts,col="red")
legend("bottomright",
       legend=formula_tempvolts,
       title=as.expression(bquote(R^2==.(signif(summary(LM_tempvolts)$r.squared,digits=4)))),
       box.lty=0)
#######
### save PDF copies of each
pdf(file='/home/kellys/AMD8320_clocks/Volts_Speed.pdf',width=8,height=8)
plot(voltage~speed,
     data=StableMinmumVoltages,
     main="Stable CPU Speed vs. Minimum Voltage",
     ylab="Volts",
     xlab="Speed (GHz)")
abline(LM_SpeedVolts,col="red")
legend("bottomright",
       legend=formula_speedvolts,
       title=as.expression(bquote(R^2==.(signif(summary(LM_SpeedVolts)$r.squared,digits=4)))),
       box.lty=0)
dev.off()

pdf(file='/home/kellys/AMD8320_clocks/CPUtemps_Volts.pdf',width=8,height=8)
plot(CPUtemps~voltage,
     data=Data_fullload,
     main="CPU Temperatures vs. Voltage",
     ylab="Degrees C",
     xlab="Volts")
abline(LM_tempvolts,col="red")
legend("bottomright",
       legend=formula_tempvolts,
       title=as.expression(bquote(R^2==.(signif(summary(LM_tempvolts)$r.squared,digits=4)))),
       box.lty=0)
dev.off()

suppressPackageStartupMessages(require(corrgram))

corrgram(Data,
         upper.panel=panel.conf,
         lower.panel=panel.pts)

corrgram(StableMinmumVoltages,
         upper.panel=panel.conf,
         lower.panel=panel.pts)

#######
suppressPackageStartupMessages(require(xtable))

#print xtable for TeX output
print(xtable(StableMinmumVoltages,caption = "System data for each stable voltage and speed combination"))
print(xtable(StableMinmumVoltages,caption = "System data for each stable voltage and speed combination"),
      file = "/home/kellys/AMD8320_clocks/StableMinimumVoltage.Tex") #save a copy of the table to a file
