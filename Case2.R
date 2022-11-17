#===============================================================================
#GBA424_case2
#Code by Zhikai Hu
#===============================================================================

rm(list=ls())
library(writexl)
require(dplyr)
library(rlist)
df1 <- read.csv('evals_data.csv')
df2 <- read.csv('student_data.csv')

#merge tables
df <- merge(x = df1, y = df2, by = 'student_id', all.x = TRUE)

#remove unwanted columns
df <- df %>% select(-c(street, city, state, zip, phone1, phone2, refer_code))

#add needed columns
df <- df %>% mutate(reading_increase = score_reading - lag(score_reading))
df <- df %>% mutate(writing_increase = score_writing - lag(score_writing))
df <- df %>% mutate(mathNoCalc_increase = score_mathNoCalc - lag(score_mathNoCalc))
df <- df %>% mutate(mathCalc_increase = score_mathCalc - lag(score_mathCalc))
df <- df %>% mutate(reading_increase = ifelse(program == "intake", NA, reading_increase))
df <- df %>% mutate(writing_increase = ifelse(program == "intake", NA, writing_increase))
df <- df %>% mutate(mathNoCalc_increase = ifelse(program == "intake", NA, mathNoCalc_increase))
df <- df %>% mutate(mathCalc_increase = ifelse(program == "intake", NA, mathCalc_increase))
df <- df %>% mutate(previous_program = lag(program))
df <- df %>% mutate(previous_program = ifelse(program == "intake", NA, previous_program))
df$total_increase <- df$reading_increase + df$writing_increase + df$mathNoCalc_increase + df$mathCalc_increase

#subsetting different programs from the original df
df_tutoring <- subset(df, program == "tutoring")
df_skills <- subset(df, program == "skills")
df_refresh <- subset(df, program == "refresh")

#calculating the average grade increase of each program and put into table
avg_score_increase_by_program <- data.frame(program = c("tutoring", "skills", "refresh"),
                              reading = c(mean(df_tutoring$reading_increase),
                                          mean(df_skills$reading_increase),
                                          mean(df_refresh$reading_increase)),
                              writing = c(mean(df_tutoring$writing_increase),
                                          mean(df_skills$writing_increase),
                                          mean(df_refresh$writing_increase)),
                              mathNoCalc = c(mean(df_tutoring$mathNoCalc_increase),
                                             mean(df_skills$mathNoCalc_increase),
                                             mean(df_refresh$mathNoCalc_increase)),
                              mathCalc = c(mean(df_tutoring$mathCalc_increase),
                                           mean(df_skills$mathCalc_increase),
                                           mean(df_refresh$mathCalc_increase)))

#calculating scores increase by sequence conditions
sequence_list <- list()
sequence_columns <- c()
for (i in c("tutoring", "skills", "refresh")){
    for (j in c("intake", "tutoring", "skills", "refresh")) {
        sequence_columns <- append(sequence_columns, paste(j, "->", i, sep = ""))
        sequence_list <- list.append(sequence_list, subset(df, program == i & previous_program == j))
    }
}

avg_score_increase_by_sequence <- data.frame(sequence = sequence_columns,
                                             reading = unlist(lapply(sequence_list, function(x){
                                                 return(mean(x$reading_increase))
                                             })),
                                             writing = unlist(lapply(sequence_list, function(x){
                                                 return(mean(x$writing_increase))
                                             })),
                                             mathNoCalc = unlist(lapply(sequence_list, function(x){
                                                 return(mean(x$mathNoCalc_increase))
                                             })),
                                             mathCalc = unlist(lapply(sequence_list, function(x){
                                                 return(mean(x$mathCalc_increase))
                                             })))

#program effect by district
unique(df$district)
district_list <- c("Camas", "Sherwood", "Wilsonville", "Lake Oswego", "Beaverton", "St. Paul", "Ridgefield", "Riverdale")
program_efficiency_by_district <- data.frame(district = district_list,
                                             tutoring = unlist(lapply(district_list, function(x){
                                                 return(mean(subset(df, district == x & program == "tutoring")$total_increase))
                                             })),
                                             skills = unlist(lapply(district_list, function(x){
                                                 return(mean(subset(df, district == x & program == "skills")$total_increase))
                                             })),
                                             tutoring = unlist(lapply(district_list, function(x){
                                                 return(mean(subset(df, district == x & program == "refresh")$total_increase))
                                             }))
                                             )

#program by online / center
online_center_list <- list()
online_center_columns <- c()
for (i in c("tutoring", "skills", "refresh")){
    for (j in c("online", "center")) {
        online_center_columns <- append(online_center_columns, paste(i, "_", j, sep = ""))
        online_center_list <- list.append(online_center_list, subset(df, program == i & location == j))
    }
}

avg_score_increase_by_online_center <- data.frame(program_location = online_center_columns,
                                             reading = unlist(lapply(online_center_list, function(x){
                                                 return(mean(x$reading_increase))
                                             })),
                                             writing = unlist(lapply(online_center_list, function(x){
                                                 return(mean(x$writing_increase))
                                             })),
                                             mathNoCalc = unlist(lapply(online_center_list, function(x){
                                                 return(mean(x$mathNoCalc_increase))
                                             })),
                                             mathCalc = unlist(lapply(online_center_list, function(x){
                                                 return(mean(x$mathCalc_increase))
                                             })))
