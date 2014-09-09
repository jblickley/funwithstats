
################ Wikipedia article metrics analysis ######################
#This script imports and analyzes Wikipedia citation data from articles on 19th century authors


#Remove all variables
  rm(list = ls())

# Set working directory
#work computer
  setwd("/Users/blickley/Dropbox/Wikipedia/Analysis")
#home computer
  #setwd("/Users/Blix/Dropbox/wikipedia/Analysis")

#Load libraries
  library(psych)
  library(lme4)
  library(bbmle)
  library(lawstat)
  library(car)


#Import all files in the current directory, saves as object with new name
  FileNames <- dir()

  wiki_data <- data.frame()

  ratingConv <-

  for(i in 1:length(FileNames)){
    assign (paste(substr(FileNames[i], 1,7),"all", sep ="_"),read.table(FileNames[i], header = TRUE, sep =",", quote=""))
    x <- assign (paste(substr(FileNames[i], 1,7),"all", sep ="_"),read.table(FileNames[i], header = TRUE, sep =",", quote=""))
    x$rating <- substr(FileNames[i],6,6)
    wiki_data <-rbind(wiki_data, x)
    remove(x)
  }
attach(wiki_data)
#Create new variable that compares the ratio of source types (books+journals to web+news)
  ratioBJtoWN <- (ntd_cite_book+ntd_cite_journal+1)/(ntd_cite_web+ntd_cite_news+1)

for (k in 1:nrow(wiki_data)){
  if (wiki_data$rating[k] == "b")
    wiki_data$rating2[k] <- "c"
  if (wiki_data$rating[k] == "F")
    wiki_data$rating2[k] <- "a"
  if (wiki_data$rating[k] == "G")
    wiki_data$rating2[k] <- "b"
  if (wiki_data$rating[k] == "s")
    wiki_data$rating2[k] <- "d"
  if (wiki_data$rating[k] == "u")
    wiki_data$rating2[k] <- "e"
}	

#Create a new variable that calculates the range of years for citations
for(i in 1:length(wiki_data$custom_years_list)){
  x <- substr(toString(wiki_data$custom_years_list[i]), 2, nchar(toString(wiki_data$custom_years_list[i]))-1)
  y<- as.integer(unlist(strsplit(x, split=", ")))
  ifelse (length(y)>=1, wiki_data$cite_year_max[i] <- max(y), 0)
  ifelse (length(y)>=1, wiki_data$cite_year_min[i] <- min(y), 0) 
  remove(x) 
  remove(y)
}


ifelse(custom_years_max=="None", 0, custom_years_max)
ifelse(custom_years_min=="None", 0, custom_years_min)
wiki_data$years_range = (as.integer(wiki_data$custom_years_max)- as.integer(wiki_data$custom_years_min))

aggregate(custom_years_max~rating2,  data = subset(wiki_data, custom_years_max>0), mean)             
wiki


wiki_data2 <- read.table("Wiki_data_small.csv", header = TRUE, sep =",", quote="")

refcount_model<-lm(ntd_ref_count~rating2,  data = wiki_data2)
refcount_model2<-aov(ntd_ref_count~rating2,  data = wiki_data2)
ref_posthoc <- TukeyHSD(x=refcount_model2, 'rating2', conf.level=0.95)
anova(refcount_model)
ref_posthoc


wordcount_model<-lm(ntd_general_wordcount~rating2,  data = wiki_data2)
wordcount_model2<-aov(ntd_general_wordcount~rating2,  data = wiki_data2)
wordcount_posthoc <- TukeyHSD(x=wordcount_model2, 'rating2', conf.level=0.95)
anova(wordcount_model)
wordcount_posthoc



yearsmax_model<-lm(as.integer(custom_years_max)~rating2,  data = subset(wiki_data2, custom_years_max!="None"))
yearsmax_model2<-aov(as.integer(custom_years_max)~rating2,  data = subset(wiki_data2, custom_years_max!="None"))
yearsmax_posthoc <- TukeyHSD(x=yearsmax_model2, 'rating2', conf.level=0.95)
anova(yearsmax_model)
yearsmax_posthoc


yearsrange_model<-lm(as.integer(years_range)~rating2,  data = wiki_data2)
yearsrange_model2<-aov(as.integer(years_range)~rating2,  data = wiki_data2)
yearsrange_posthoc <- TukeyHSD(x=yearsrange_model2, 'rating2', conf.level=0.95)
anova(yearsrange_model)
yearsrange_posthoc


ratioBJtoWN_model<-lm(ratioBJtoWN~rating2,  data = wiki_data2)
ratioBJtoWN_model2<-aov(ratioBJtoWN~rating2,  data = wiki_data2)
ratioBJtoWN_posthoc <- TukeyHSD(x=ratioBJtoWN_model2, 'rating2', conf.level=0.95)
anova(ratioBJtoWN_model)
ratioBJtoWN_posthoc

wordperref_model<-lm(Word.per.ref~rating2,  data = wiki_data2)
wordperref_model2<-aov(Word.per.ref~rating2,  data = wiki_data2)
wordperref_posthoc <- TukeyHSD(x=wordperref_model2, 'rating2', conf.level=0.95)
anova(wordperref_model)
wordperref_posthoc


test<-lm(ntd_ref_count~rating2,  data = wiki_data2)
test2<-aov(ntd_ref_count~rating2,  data = wiki_data2)
ref_posthoc <- TukeyHSD(x=test2, 'rating2', conf.level=0.95)

#teststring <- substr(toString(Wiki_data$custom_years_list[2]), 2, nchar(toString(Wiki_data$custom_years_list[2]))-1)
#as.integer(unlist(strsplit(teststring, split=", ")))
#ifelse(length(Wiki_data$custom_years_list[i])>= 2,Wiki_data$year_range1 <-max(Wiki_data$custom_years_list[i])- min(Wiki_data$custom_years_list[i]),0  )
#Use these to replace NaN and Inf with 0
  #Wiki_data$ratioBJtoWN[is.nan(Wiki_data$ratioBJtoWN)] <- 0
  #Wiki_data$ratioBJtoWN[Wiki_data$ratioBJtoWN==Inf] <- 0


####################Variables of interest###
# Wiki_data$ratioBJtoWN - ratio of book+journal cites to web+news cites
# Wiki_data$ntd_ref_count - number of references counted
# Wiki_data$custom_yearsdetected - number of different years that were detected
# Wiki_data$year_range <- range of years detected

xyplot(ntd_general_wordcount~ntd_ref_count, groups = rating2, data = wiki_data)



