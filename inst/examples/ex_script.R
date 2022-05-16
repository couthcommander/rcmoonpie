#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
data=read.csv('myfile.csv')
#Setting Labels

label(data$record_id)="Record ID"
label(data$redcap_event_name)="Event Name"
label(data$visit_date) ="Visit Date"
label(data$randomization)="Randomization"
label(data$sex)="Sex"
label(data$age)="Age"
label(data$social_connectedness)="Social Connectedness"
label(data$comments)="Comments"
                 
#Setting Factors(will create new variable for factors)
data$redcap_event_name.factor = factor(data$redcap_event_name,levels=c("baseline", "followup"))
data$randomization.factor = factor(data$randomization,levels=c("0", "1"))
data$sex.factor = factor(data$sex,levels=c("0", "1", "666", "999"))
data$social_connectedness.factor = factor(data$social_connectedness,levels=c("0", "1", "2", "3", "4", "5", "6", "7", "666", "999"))

levels(data$redcap_event_name.factor)=c("Baseline","Follow-up")
levels(data$randomization.factor)=c("Control","Treatment")
levels(data$sex.factor)=c("Male","Female","Ambiguous","Missing")
levels(data$social_connectedness.factor)=c("0", "1", "2", "3", "4", "5", "6", "7", "Ambiguous","Missing")

# n <- 100
# write.csv(data.frame(
#   record_id = seq(n),
#   redcap_event_name = sample(c("baseline", "followup"), n, replace = TRUE),
#   visit_date = as.Date(sample(seq(365), n), origin = '2019-12-31'),
#   randomization = sample(c("0", "1"), n, replace = TRUE),
#   sex = sample(c("0", "1", "666", "999"), n, replace = TRUE, prob = c(0.45, 0.45, 0.05, 0.05)),
#   age = sample(19:65, n, replace = TRUE),
#   social_connectedness = sample(c("0", "1", "2", "3", "4", "5", "6", "7", "666", "999"), n, replace = TRUE, prob = c(rep(0.1, 8), 0.05, 0.15)),
#   comments = sample(letters, n, replace = TRUE)
# ), file = 'myfile.csv', row.names = FALSE)
