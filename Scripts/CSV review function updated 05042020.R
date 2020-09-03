#Cameron Ho February 24 2020
#Sarah Basing Quantitative Ecology Lab
#Function code to find differences in 2 analysis of the same camera trap data

#RUN FIRST
library("dplyr")
folder <- list.files(pattern="*.csv")
odd <- seq(1, length(folder),2)
even <- seq(2,length(folder),2)
sets.even <- folder[even]
sets.odd <- folder[odd]
even.abr <- substr(sets.even,1,6)
odd.abr <- substr(sets.odd,1,6)
full.abr <- even.abr[even.abr %in% odd.abr]
tst.odd <- sets.odd[odd.abr %in% even.abr]
tst.even <- sets.even[even.abr %in% odd.abr]

qw <- function(x) unlist(strsplit(x, "[[:space:]]+"))

#THEN SET THE FUNCTION
func <- function(set1, set2, output, stat){
  data1 <- read.csv(set1, header=T)
  data2 <- read.csv(set2, header=T)
  for(i in c(12:16,28)){
    data1[,i] <- toupper(data1[,i])
    data2[,i] <- toupper(data2[,i])
  }
  # join datasets based on RelativePath and File but keep in mind relative path has to be the same between files 
  fulltable <- inner_join(data1, data2, by=c("RelativePath", "File")) 
  
  secoptable <- fulltable[fulltable$SecondOpinion.x=="TRUE",]
  secoptable <- mutate(secoptable, Difftype="secondopinion")
  
  secoptable2 <- fulltable[fulltable$SecondOpinion.y=="TRUE",]
  secoptable2 <- mutate(secoptable2, Difftype="secondopinion")
  
  servicetable <- fulltable[fulltable$Service.x != fulltable$Service.y,]
  servicetable <- mutate(servicetable, Difftype="service")
  
  emptytable <- fulltable[fulltable$Empty.x != fulltable$Empty.y,]
  emptytable <- mutate(emptytable, Difftype="empty")
  
  animaltable <- fulltable[fulltable$Animal.x != fulltable$Animal.y,]
  animaltable <- mutate(animaltable, Difftype="animal")
  
  humantable <- fulltable[fulltable$Human.x != fulltable$Human.y,]
  humantable <- mutate(humantable, Difftype="human")
  
  cartable <- fulltable[fulltable$Vehicle.x != fulltable$Vehicle.y,]
  cartable <- mutate(cartable, Difftype="car")
  
  speciestable <- fulltable[as.character(fulltable$Species.x) != as.character(fulltable$Species.y),]
  speciestable <- mutate(speciestable, Difftype="species")
  
  HumActable <- fulltable[as.character(fulltable$Human.x) != as.character(fulltable$Service.y),]
  HumActable <- mutate(HumActable, Difftype="human_activity")
  
  
  counttable <- fulltable[fulltable$Count.x != fulltable$Count.y,]
  counttable <- mutate(counttable, Difftype="count")
  
  AdFemtable <- fulltable[fulltable$AdultFemale.x != fulltable$AdultFemale.y,]
  AdFemtable <- mutate(AdFemtable, Difftype="adult_female")
  
  AdMaltable <- fulltable[fulltable$AdultMale.x != fulltable$AdultMale.y,]
  AdMaltable <- mutate(AdMaltable, Difftype="adult_male")
  
  AdUnktable <- fulltable[fulltable$AdultUnknown.x != fulltable$AdultUnknown.y,]
  AdUnktable <- mutate(AdUnktable, Difftype="adult_unknown")
  
  ofsptable <- fulltable[fulltable$Offspring.x != fulltable$Offspring.y,]
  ofsptable <- mutate(ofsptable, Difftype="offspring")
  
  UNKtable <- fulltable[fulltable$UNK.x>0,]
  UNKtable <- mutate(UNKtable, Difftype="UNK")
  
  UNKtable2 <- fulltable[fulltable$UNK.x>0,]
  UNKtable2 <- mutate(UNKtable2, Difftype="UNK")
  
  collartable <- fulltable[fulltable$Collars.x != fulltable$Collars.y,]
  collartable <- mutate(collartable, Difftype="collar")
  
  tagdif <- ifelse(as.character(fulltable$Tags.x)==as.character(fulltable$Tags.y),NA,1) 
  tagtable1.1 <- fulltable[as.logical(tagdif),, drop=F]
  tagtable1.2 <- tagtable1.1[!is.na(tagtable1.1$Tags.x),]
  tagtable1.2 <- mutate(tagtable1.2, Difftype="tag")
  
  markdif <- ifelse(as.character(fulltable$NaturalMarks.x)==as.character(fulltable$NaturalMarks.y),NA,1) 
  marktable1.1 <- fulltable[as.logical(markdif),, drop=F]
  marktable1.2 <- marktable1.1[!is.na(marktable1.1$Naturalmarks.x),]
  marktable1.2 <- mutate(marktable1.2, Difftype="mark")
  
  UNKspectable <- fulltable[fulltable$Species.x=="Unknown",]
  UNKspectable <- mutate(UNKspectable, Difftype="UNKspecies")
  
  UNKspectable2 <- fulltable[fulltable$Species.y=="Unknown",]
  UNKspectable2 <- mutate(UNKspectable2, Difftype="UNKspecies")
  
  UNKdeertable <- fulltable[fulltable$Species.x=="Unknown Deer",]
  UNKdeertable <- mutate(UNKdeertable, Difftype="UNKdeer")
  
  UNKdeertable2 <- fulltable[fulltable$Species.y=="Unknown Deer",]
  UNKdeertable2 <- mutate(UNKdeertable2, Difftype="UNKdeer")
  
  UNKcanidtable <- fulltable[fulltable$Species.x=="Unknown Canid",]
  UNKcanidtable <- mutate(UNKcanidtable, Difftype="UNKcanid")
  
  UNKcanidtable2 <- fulltable[fulltable$Species.y=="Unknown Canid",]
  UNKcanidtable2 <- mutate(UNKcanidtable2, Difftype="UNKcanid")
  
  UNKfelidtable <- fulltable[fulltable$Species.x=="Unknown Felid",]
  UNKfelidtable <- mutate(UNKfelidtable, Difftype="UNKfelid")
  
  UNKfelidtable2 <- fulltable[fulltable$Species.y=="Unknown Felid",]
  UNKfelidtable2 <- mutate(UNKfelidtable2, Difftype="UNKfelid")
  
  UNKungulatetable <- fulltable[fulltable$Species.x=="Unknown Ungulate",]
  UNKungulatetable <- mutate(UNKungulatetable, Difftype="UNKungulate")
  
  UNKungulatetable2 <- fulltable[fulltable$Species.y=="Unknown Ungulate",]
  UNKungulatetable2 <- mutate(UNKungulatetable2, Difftype="UNKungulate")
  
  
  uniontable <- union(union(union(union(union(union(union(union(union(union(union(union(union(union(union(union(union(union(union(union(union(union(union(union(union(union(union(union(
    servicetable, emptytable),animaltable),
    humantable),cartable),speciestable),HumActable),
    counttable),AdFemtable),AdMaltable), AdUnktable),
    ofsptable),UNKtable),UNKtable2),collartable),tagtable1.2),
    UNKspectable),UNKspectable2),UNKdeertable),UNKdeertable2), 
    marktable1.2),secoptable),secoptable2),UNKcanidtable),UNKcanidtable2),
    UNKfelidtable),UNKfelidtable2),UNKungulatetable),UNKungulatetable2)
  col.order <- c("File", "Date.x","Difftype", 
                 "Service.x", "Service.y", 
                 "Empty.x", "Empty.y", 
                 "Animal.x", "Animal.y",
                 "Human.x", "Human.y",
                 "Vehicle.x", "Vehicle.y",
                 "Species.x", "Species.y",
                 "HumanActivity.x", "HumanActivity.y",
                 "Count.x", "Count.y",
                 "AdultFemale.x", "AdultFemale.y",
                 "AdultMale.x", "AdultMale.y",
                 "AdultUnknown.x", "AdultUnknown.y",
                 "Offspring.x", "Offspring.y",
                 "UNK.x", "UNK.y",
                 "Collars.x", "Collars.y",
                 "Tags.x", "Tags.y",
                 "NaturalMarks.x", "NaturalMarks.y",
                 "SecondOpinion.x", "SecondOpinion.y")
  finaltable <- uniontable[,col.order]
  stats <- as.matrix(table(finaltable$Difftype)/nrow(finaltable))
  write.table(finaltable, file=output, sep=",",row.names = F)
  write.table(stats, file=stat,sep=",", row.names=T)
}


#FINAL, RUN THIS TO REVIEW
for(i in 1:(length(tst.even))){ #set.even.final
  a <- tst.even[i] #set.even.final
  b <- tst.odd[i] #set.odd.final
  c <- paste0("review/",full.abr[i],"_diff.csv") #set.abr
  d <- paste0("review/",full.abr[i],".txt") #set.abr
  func(a,b,c,d)
}

# FYI, this warning means the number of rows don't match up between the two cvs files
# "Column `File` joining factors with different levels, coercing to character vector"
# "Relative Path" warning means the two relative paths don't match- will need to fix this
