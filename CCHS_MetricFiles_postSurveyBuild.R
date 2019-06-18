########---########---########---########---########---########---########---########---########---########---########---########---
# #1 OPEN FILE AND LOAD DATA, LIBRARIES
########---########---########---########---########---########---########---########---########---########---########---########---
Sys.time()

library(tidyverse) #Data management and reading excel files
library(survey) # For generating survey object and cv/ci calculations
library(haven) # For reading Stata files?
library(memisc) # for cases
#library(xlsx) # To write excel, but needs java
library(writexl) # No formatting


#metriclist <- c("den_010","den_020","oht_015")
### USE ABOVE OR BELOW TO GENERATE METRICLIST
getwd()
setwd("C:/Desk/RStudio/CCHSData/Dental")
cchs <- readRDS("C:/Desk/RStudio/CCHSData/cchs_wbootstrap.rds")
metriclist <- list.files(path="./metricfiles", pattern = "*.R", ignore.case = TRUE, full.names = FALSE)

########---########---########---########---########---########---########---########---########---########---########---########---
# #2 CREATE GEOGRAPHIC GROUPS - G, W, W*, D, GWD - AND YEAR GROUPS (NOT RELEVANT YET WITH ONLY 3 YEARS) - GEODVCSD SELECTION OPTION TOO
########---########---########---########---########---########---########---########---########---########---########---########---

###slices:
#wdgname
#year
#dhh_age

########---########---########---########---########---########---########---########---########---########---########---########---
# #3 CODE AGE RANGE GROUPS
########---########---########---########---########---########---########---########---########---########---########---########---

### BEGIN GEOGRAPHY
# cchs$jvg_wdg <- cases("WDG"=(cchs$wdgname = "Wellington" | cchs$wdgname = "Dufferin" | cchs$wdgname = "Guelph"),
#                      "Ontario-WDG"=TRUE #Should only take action once all other rules have been processed
# )
# 
# 
# cchs$jvg_wg <- cases("WG"=(cchs$wdgname = "Wellington" | cchs$wdgname = "Guelph"),
#                      "Ontario-WG"=TRUE #Should only take action once all other rules have been processed
# )
# 
# cchs$jvg_all <- cchs$wdgname

### END GEOGRAPHY


### CAN I ADD TRUE/FALSE OR jvm/jva/jvg assignments in subscripts?
# if (wdggroup==TRUE) {
#   print("TRUE") 
# } else if (wdggroup==FALSE) {
#   print("FALSE")
# } else if (is.null(wdggroup)) {
#   print("NULL")
# } else {
#   print("Neither")
# }

  
  
### BEGIN AGES
#Full 5 75+
cchs$jv_age_1 <- cases("12-18"=(cchs$dhh_age>=12 & cchs$dhh_age<=18),
                       "19-34"=(cchs$dhh_age>=19 & cchs$dhh_age<=34),
                       "35-54"=(cchs$dhh_age>=35 & cchs$dhh_age<=54),
                       "55-74"=(cchs$dhh_age>=55 & cchs$dhh_age<=74),
                       "75+"=(cchs$dhh_age>=75),
                       "NA"=TRUE #Should only take action once all other rules have been processed
)

#Full 5 65+
cchs$jv_age_2 <- cases("12-17"=(cchs$dhh_age>=12 & cchs$dhh_age<=17),
                       "18-34"=(cchs$dhh_age>=18 & cchs$dhh_age<=34),
                       "35-49"=(cchs$dhh_age>=35 & cchs$dhh_age<=49),
                       "50-64"=(cchs$dhh_age>=50 & cchs$dhh_age<=64),                       
                       "65+"=(cchs$dhh_age>=65),
                       "NA"=TRUE #Should only take action once all other rules have been processed
)

#Full 4 65+
cchs$jv_age_3 <- cases("12-19"=(cchs$dhh_age>=12 & cchs$dhh_age<=19),
                       "20-44"=(cchs$dhh_age>=20 & cchs$dhh_age<=44),
                       "45-64"=(cchs$dhh_age>=45 & cchs$dhh_age<=64),
                       "65+"=(cchs$dhh_age>=65),
                       "NA"=TRUE #Should only take action once all other rules have been processed
)

#Adult 4 75+
cchs$jv_age_4 <- cases("19-34"=(cchs$dhh_age>=19 & cchs$dhh_age<=34),
                       "35-54"=(cchs$dhh_age>=35 & cchs$dhh_age<=54),
                       "55-74"=(cchs$dhh_age>=55 & cchs$dhh_age<=74),
                       "75+"=(cchs$dhh_age>=75),
                       "NA"=TRUE #Should only take action once all other rules have been processed
)

#Adult 3 65+
cchs$jv_age_5 <- cases("12-39"=(cchs$dhh_age>=12 & cchs$dhh_age<=39),
                       "40-74"=(cchs$dhh_age>=40 & cchs$dhh_age<=74),
                       "75+"=(cchs$dhh_age>=75),
                       "NA"=TRUE #Should only take action once all other rules have been processed
)

#Older Adult Dental 4 75+
cchs$jv_age_6 <- cases("12-54"=(cchs$dhh_age>=12 & cchs$dhh_age<=54),
                       "55-64"=(cchs$dhh_age>=55 & cchs$dhh_age<=64),
                       "65-74"=(cchs$dhh_age>=65 & cchs$dhh_age<=74),
                       "75+"=(cchs$dhh_age>=75),
                       "NA"=TRUE #Should only be applied once all other rules have been processed
)

### END AGES
#~9 seconds
Sys.time()


########---########---########---########---########---########---########---########---########---########---########---########---
# #4 RECODE AND CREATE VARIABLE GROUPS
########---########---########---########---########---########---########---########---########---########---########---########---

#RECODING THE VARIABLE TO REMOVE N/A & NOT STATED RESPONSES
##WILL I USE CODES OR LABELS? CAN STATA USE LABELS?
#RECODE HINT FROM JOSH BELOW:
#cchs$jv_metric <- na-if(cchs$ccc_095_lab,"Not Stated")

for (i in metriclist) { ### LOOP 1
  metric<-tools::file_path_sans_ext(i)
  metriclab <- paste0(metric,"_lab")
  newmetricname<-paste0("jvm_",metric,"_lab")
  print(metric)
  
  
  ### CLEAN DATA - REMOVE NON ANSWERS
  cchs[,newmetricname] <- dplyr::recode(cchs[,metriclab],
                                        "Not stated"=NA_character_,
                                        "Not Stated"=NA_character_,
                                        "Don't know"=NA_character_,
                                        "Don't Know"=NA_character_,
                                        "Valid skip"=NA_character_,
                                        "Valid Skip"=NA_character_,
                                        "Not tested"=NA_character_,
                                        "Not Tested"=NA_character_,
                                        "Refusal"=NA_character_
  )
  
  ### RUN PROVIDED GROUPING
  source(paste0("./metricfiles/",metric,".r"))
  
  ### TRY/CATCH BLOCK FOR PROVIDED GROUPINGS?
  

} 
# END LOOP 1



Sys.time()
### CREATE SURVEY OBJECT BETWEEN LOOPS 1 AND 2
csurvey <- svrepdesign(data=cchs,
                       type="BRR",
                       weights=cchs$wts_s,
                       repweights=cchs[,2509:3508],
                       combined.weights=TRUE,
                       mse="TRUE"
)


########---########---########---########---########---########---########---########---########---########---########---########---
# #5 NOW WE CHECK CVS
########---########---########---########---########---########---########---########---########---########---########---########---

#metriclist <- c("ccc_095")
#rm(metriclist)
#metric<-c("ccc_095")
#i <- c("den_030")

for (i in metriclist) { ### LOOP 2
metric<-tools::file_path_sans_ext(i)
metriclab <- paste0(metric,"_lab")
newmetricname<-paste0("jvm_",metric,"_lab")


#jvg_group <- c("Wellington","Guelph")
#jvg_group <- c("Guelph")
jv_geo <- c("Dufferin")
#jvy_group <- c(2015,2016)
jv_age <- c("jv_age_6")

myresult_year <- svyby(make.formula(newmetricname), # variable to pass to function
                       by = ~year,  # grouping
                       design = subset(csurvey, wdgname %in% jv_geo), # design object with subset definition
                       vartype = c("ci","cvpct"), # report variation as ci, and cv percentage
                       na.rm=TRUE,
                       na.rm.all=TRUE,
                       FUN = svymean # specify function from survey package, mean here
)


myresult_age <- svyby(make.formula(newmetricname), # variable to pass to function
                      by = make.formula(jv_age),  # grouping by age range - which age grouping is defined above
                      design = subset(csurvey, wdgname %in% jv_geo), # design object with subset definition
                      vartype = c("ci","cvpct"), # report variation as ci, and cv percentage
                      na.rm=TRUE,
                      na.rm.all=TRUE,
                      FUN = svymean # specify function from survey package, mean here
)


myresult_sex <- svyby(make.formula(newmetricname), # variable to pass to function
                      by = ~dhh_sex_lab,  # grouping
                      design = subset(csurvey, wdgname %in% jv_geo), # design object with subset definition
                      vartype = c("ci","cvpct"), # report variation as ci, and cv percentage
                      na.rm=TRUE,
                      na.rm.all=TRUE,
                      FUN = svymean # specify function from survey package, mean here
)


myresult_geo <- svyby(make.formula(newmetricname), # variable to pass to function
                      by = ~wdgname,  # grouping
                      design = subset(csurvey, wdgname %in% jv_geo), # design object with subset definition
                      vartype = c("ci","cvpct"), # report variation as ci, and cv percentage
                      na.rm=TRUE,
                      na.rm.all=TRUE,
                      FUN = svymean # specify function from survey package, mean here
)


Sys.time()


########---########---########---########---########---########---########---########---########---########---########---########---
# #6 VALIDATE VALUES, CV AND CI
########---########---########---########---########---########---########---########---########---########---########---########---


#CALCULATIONS ON JDV_METRIC, BY JDV_AGE AND REGIONS (JDV_REGIONS?)
#CALCULATE BY YEAR, 2 YEAR, OR COMBINED


#AND FORMATTING?
#  FLAG HIGH CV/INVALID CV/QUALITY INDICATOR ABCDE

#  FLAG LOW COUNTS


########---########---########---########---########---########---########---########---########---########---########---########---
# #7 CREATE EXPORTS TO EXCEL
########---########---########---########---########---########---########---########---########---########---########---########---

#EXPORT METRIC 1 TO EXCEL SHEET 1
#write.csv(myresult_age,file=paste0(metrictest,"_Age.csv"))
#write.csv(myresult_geo,file=paste0(metrictest,"_Geo.csv"))
#write.csv(myresult_sex,file=paste0(metrictest,"_Sex.csv"))
#write.csv(myresult_year,file=paste0(metrictest,"_Year.csv"))

 write_xlsx(list(age = myresult_age, sex = myresult_sex, geo = myresult_geo, year = myresult_year),
            paste0("C:\\Desk\\RStudio\\CCHSData\\Dental\\metricfiles\\MetricOutput\\",metric,".xlsx"))


} # END LOOP 2

########---########---########---########---########---########---########---########---########---########---########---########---
# #8 END SCRIPT, CLOSE LOG
########---########---########---########---########---########---########---########---########---########---########---########---




#CLOSE AND SAVE, DONE LOOPING
Sys.time()




### EXAMPLE OUTPUT TO EXCEL FOR SECONDARY VALIDATION
###exd <- select_at(cchs, vars(ont_id, dhh_sex_lab, dhh_age, jv_age_6, wdgname, starts_with("incdv"), starts_with("den_"), starts_with("oht_"), wts_s))
###write_xlsx(exd, paste0("C:\\Desk\\RStudio\\CCHSData\\Dental\\ExcelExport_Dental.xlsx"))


