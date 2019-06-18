
### TEST CODE. ALL VARIABLES SHOULD BE DEFINED WITHIN THE LARGER LOOP.
#metric <- "den_030" #THIS SHOULD BE DEFINED BY THE FILENAME AS READ BY THE LOOP
#metriclab <- paste0(metric,"_lab")
#newmetricname<-paste0("jvm_",metric,"_lab")

#metricpos <- grep(newmetricname, colnames(cchs)) # FIND METRIC LOCATION
#cchs[metricpos]
###  END TEST CODE


### THIS IS THE COLLECTED GROUPING - PLEASE FILL OUT
cchs[,newmetricname] <- dplyr::recode(cchs[,newmetricname], ### FILL OUT BELOW THIS LINE WITH << "CCHS VALUE"="NEW VALUE", >> NO COMMA ON LAST LINE
                                     "More than once a year for check-ups or treatment"="Once a year or more",
                                     "About once a year (for check-ups or treatment)"="Once a year or more",
                                     "Less than once a year (for check-ups or treatment)"="Less than once a year or never",
                                     "Only for emergency care"="Less than once a year or never",
                                     "Never"="Less than once a year or never"
)


### THIS IS THE CUSTOM AGE GROUP - COULD ALSO SPECIFY ONE OF THE PREVIOUSLY GENERATED LISTS
cchs$jv_age_custom <- cases("12-17"=(cchs$dhh_age>=12 & cchs$dhh_age<=17),
                            "18-34"=(cchs$dhh_age>=18 & cchs$dhh_age<=34),
                            "35-49"=(cchs$dhh_age>=35 & cchs$dhh_age<=49),
                            "50-64"=(cchs$dhh_age>=50 & cchs$dhh_age<=64),
                            "65+"=(cchs$dhh_age>=65),
                            "NA"=TRUE #Should only take action once all other rules have been processed
)




print(summary(cchs[,metriclab]))
print(summary(cchs[,newmetricname]))

