library(readxl)
library(lubridate)
library(dplyr)
library(magrittr)
library(xlsx)

setwd("D:\\Altrak 1978\\Project\\ServiceDashboard\\data_transformation")
mydata <- read_excel("jobrptmo.xlsx")

#data preparation
mydata <- mydata %>% 
  select(2:8, 11, 13, 14) %>%
  set_colnames(c("JobNo", "UnitModel", "UnitSN", "OpenDate", "CloseDate", 
                 "Customer", "JobDesc", "Parts", "Labour", "Others")) %>%
  filter(!is.na(JobNo)) %>% 
  filter(JobNo != "JOB NO") %>% 
  mutate(Parts = as.numeric(Parts), Labour = as.numeric(Labour), Others = as.numeric(Others)) %>%
  mutate(OpenDate = as.numeric(OpenDate), CloseDate = as.numeric(CloseDate)) %>% 
  mutate(OpenDate = as.Date(OpenDate, origin = "1899-12-30", tz = "UTC")) %>% 
  mutate(CloseDate = as.Date(CloseDate, origin = "1899-12-30", tz = "UTC"))


#adding job valuation
mydata$Service <- mydata$Labour + mydata$Others
mydata$TotalValue <- mydata$Service + mydata$Parts


#calculating open and closing date

mydata$TotalDays <- ifelse(is.na(mydata$CloseDate), 
                            difftime(Sys.Date(), mydata$OpenDate, units = "days"),
                            difftime(mydata$CloseDate, mydata$OpenDate, units = "days"))
mydata$OpenYear <- format(mydata$OpenDate, "%Y")


#determine job status
mydata$JobStatus <- ifelse(is.na(mydata$CloseDate),
                           "Outstanding", "Closed")

#determine outstanding job class
mydata <- mutate(mydata, OutstandingJobClass = case_when(
                 TotalDays <= 30 & JobStatus == "Outstanding" ~ "0-30 Days",
                 TotalDays >= 31 & TotalDays <= 60 & JobStatus == "Outstanding" ~ "31-60 Days", 
                 TotalDays >= 61 & TotalDays <= 90 & JobStatus == "Outstanding" ~ "61-90 Days",
                 TotalDays >= 91 & TotalDays <= 120 & JobStatus == "Outstanding" ~ "91-120 Days",
                 TotalDays >= 121 & TotalDays <= 180 & JobStatus == "Outstanding" ~ "121-180 Days",
                 TotalDays > 180 & JobStatus == "Outstanding" ~ ">180 Days",
                 TRUE ~ "Closed Job"))

#determine if the job is overhaul or non-overhaul
mydata <- mutate(mydata, OverhaulJob = case_when(
                 JobDesc == "OVERHAUL" ~ "Overhaul Job",
                 TRUE ~ "Non-Overhaul Job"))

                                  
#determine job type and branch code

mydata$JobType <- substr(mydata$JobNo, 1,2)
mydata$BranchCode <- substr(mydata$JobNo, 13,14)
mydata$Agency <- substr(mydata$JobNo, 9,11)


#lookup value for job category
jobcategory <- read_excel("Job Category.xlsx")
colnames(jobcategory) <- c("JobType", "JobCategory")
mydata <- merge(mydata, jobcategory, by="JobType")
mydata$JobCategory <- substr(mydata$JobCategory, 1, 12)

#lookup value for branch code
branchcode <- read_excel("Branch Code.xlsx")
colnames(branchcode) <- c("BranchCode", "BranchName")
mydata <- merge(mydata, branchcode, by="BranchCode")

#rearranging the order of columns
head(mydata)
finaldata <- mydata[ , c("JobNo", "Agency", "JobType", "UnitModel", "UnitSN", "JobCategory", "BranchCode", "BranchName",
                         "OpenYear", "OpenDate", "CloseDate", "TotalDays", "JobStatus",
                         "OutstandingJobClass", "Customer", "JobDesc", "OverhaulJob", "Parts", "Labour", 
                         "Others", "Service", "TotalValue")]

finaldata$JobType <- factor(finaldata$JobType)
finaldata$Agency <- factor(finaldata$Agency)
finaldata$JobCategory <- factor(finaldata$JobCategory)
finaldata$BranchCode <- factor(finaldata$BranchCode)
finaldata$BranchName <- factor(finaldata$BranchName)
finaldata$OpenYear <- factor(finaldata$OpenYear)
finaldata$JobStatus <- factor(finaldata$JobStatus)


#export final data
write.xlsx(finaldata, "serviceperformance.xlsx")