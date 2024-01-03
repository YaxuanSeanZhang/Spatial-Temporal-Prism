mylist_spacetime = space_time_list(activity_person,person_fixity_summary)
person_fixity_summary = mylist_spacetime[[1]]
#get summary data per person per day per location
person_spacetime = mylist_spacetime[[2]]  

#get activity sequence at each location
person_summary_byloc = get_sequence_count_byloc(person_spacetime,person_fixity_summary)

#get repetitive interval
person_summary_byloc = get_repetitive_interval_byloc(person_summary_byloc,
                                                     0.8,0.4,window = NA)

#show repetitive interval result
show_intersection_byloc(person_summary_byloc,person_spacetime,
                        percent=0.8,threshold = 0.4,
                        i=21,type='WORK',window=NA)  #change i to check other persons
