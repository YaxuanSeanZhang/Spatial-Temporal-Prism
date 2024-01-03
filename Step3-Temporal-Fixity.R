#get temporal summary
mylist_temporal = temporal_list(activity_person,person_fixity_summary)
person_fixity_summary = mylist_temporal[[1]]
person_temporal = mylist_temporal[[2]]

#calculate Repetitive Ratio
person_fixity_summary$Repetitive_Ratio = 
  person_fixity_summary$activity_days/person_fixity_summary$ValidDay

#classify repetitive vs non-repetitive activity based on RR
RR = data.frame(
  UrserId = person_fixity_summary$UserId[which(person_fixity_summary$spatial_location>0)], 
  RR = person_fixity_summary$Repetitive_Ratio[which(person_fixity_summary$spatial_location>0)])

gmm_optimal_RR <- Mclust(RR$RR)
plot(gmm_optimal_RR, what = 'BIC')  #cluster number: 4
gmm_RR <- Mclust(RR$RR,4)
RR = cbind(RR,group = as.character(gmm_RR$classification))
table(gmm$group)
RR$group = as.character(RR$group)
tapply(RR$RR, RR$group, summary)