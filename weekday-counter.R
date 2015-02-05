weekdaycounts<-data.frame( day = c(rep("Monday",times=10),
                                   rep("Tuesday",times=10),
                                   rep("Wednesday",times=10),
                                   rep("Thursday",times=10),
                                   rep("Friday",times=10),
                                   rep("Saturday",times=10),
                                   rep("Sunday",times=10)),
                           value = c(8,35,25,7,99,64,2,1,3,12, #10 manually-chosen random numbers
                                     sample.int(60)) #60 more automatically generated random numbers
                           )


#how many times did the number 10 appear?
print(nrow(subset(weekdaycounts,subset=(value==10))))

#how many times did the number 1 appear?
print(nrow(subset(weekdaycounts,subset=(value==1))))
