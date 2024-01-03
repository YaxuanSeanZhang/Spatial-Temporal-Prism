#get the number of anchor(s) for each person
person_fixity_summary = anchor_num(person_fixity_summary,
                                   person_summary_byloc,
                                   RR = 0.4)
table(person_fixity_summary$anchor_num) #number of people having anchor(s)
