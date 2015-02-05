# https://stat.ethz.ch/pipermail/r-help/2005-September/079189.html
with(OrchardSprays, {
  #
  print(OrchardSprays)
  #
  stripchart(decrease ~ treatment,
             main = "stripchart(Orchardsprays)", ylab = "decrease",
             vertical = TRUE)
  m <- tapply(decrease, treatment, mean)
  #
  print(m)
  print(paste("nlevels(treatment):",nlevels(treatment),sep=":::"))
  print(paste("1:nlevels(treatment)-0.25",1:nlevels(treatment)-0.25,sep="="))
  #
  segments(1:nlevels(treatment)-0.25, m, 1:nlevels(treatment)+0.25, m)
})
#

