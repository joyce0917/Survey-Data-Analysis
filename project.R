setwd("your_path")

# need columns: "Schoolwork (including research)", "Gaming", "Social Media", 
# "Job-related tasks", "Personal projects", "Entertainment", "Other"
survey_df = read.csv("36-303_Survey_Operating_Systems.csv", skip=1, header=FALSE)
# rename columns
colnames(survey_df)[1] = "time"
colnames(survey_df)[2] = "email"
colnames(survey_df)[3] = "consent"
colnames(survey_df)[4] = "age"
colnames(survey_df)[5] = "region"
colnames(survey_df)[6] = "gender"
colnames(survey_df)[7] = "college"
colnames(survey_df)[8] = "major"
colnames(survey_df)[9] = "grade"
colnames(survey_df)[10] = "prim_os"
colnames(survey_df)[11] = "prim_comp"
colnames(survey_df)[12] = "prim_type"
colnames(survey_df)[13] = "prim_use"
colnames(survey_df)[14] = "prim_satis"
colnames(survey_df)[15] = "next_prim_os"
colnames(survey_df)[16] = "sec_has"
colnames(survey_df)[17] = "sec_os"
colnames(survey_df)[18] = "sec_personal"
colnames(survey_df)[19] = "sec_type"
colnames(survey_df)[20] = "sec_use"
# add binary columns for usage
survey_df$prim_schoolwork = FALSE
survey_df$prim_gaming = FALSE
survey_df$prim_social_media = FALSE
survey_df$prim_job_related = FALSE
survey_df$prim_projects = FALSE
survey_df$prim_entertainment = FALSE
survey_df$prim_other = FALSE
survey_df$sec_schoolwork = FALSE
survey_df$sec_gaming = FALSE
survey_df$sec_social_media = FALSE
survey_df$sec_job_related = FALSE
survey_df$sec_projects = FALSE
survey_df$sec_entertainment = FALSE
survey_df$sec_other = FALSE
# populate binary variables, change college
for(i in 1:nrow(survey_df))
{
    survey_df[i,]$prim_schoolwork = grepl("Schoolwork", survey_df[i,]$prim_use)
    survey_df[i,]$prim_gaming = grepl("Gaming", survey_df[i,]$prim_use)
    survey_df[i,]$prim_social_media = grepl("Social media", survey_df[i,]$prim_use)
    survey_df[i,]$prim_job_related = grepl("Job-related", survey_df[i,]$prim_use)
    survey_df[i,]$prim_projects = grepl("Personal projects", survey_df[i,]$prim_use)
    survey_df[i,]$prim_entertainment = grepl("Entertainment", survey_df[i,]$prim_use)
    survey_df[i,]$prim_other = grepl("Other", survey_df[i,]$prim_use)
    survey_df[i,]$sec_schoolwork = grepl("Schoolwork", survey_df[i,]$sec_use)
    survey_df[i,]$sec_gaming = grepl("Gaming", survey_df[i,]$sec_use)
    survey_df[i,]$sec_social_media = grepl("Social media", survey_df[i,]$sec_use)
    survey_df[i,]$sec_job_related = grepl("Job-related", survey_df[i,]$sec_use)
    survey_df[i,]$sec_projects = grepl("Personal projects", survey_df[i,]$sec_use)
    survey_df[i,]$sec_entertainment = grepl("Entertainment", survey_df[i,]$sec_use)
    survey_df[i,]$sec_other = grepl("Other", survey_df[i,]$sec_use)
    if(grepl("CFA", survey_df[i,]$college))
    {
        survey_df[i,]$college = "CFA"
    }
    if(grepl("MCS", survey_df[i,]$college))
    {
        survey_df[i,]$college = "MCS"
    }
    if(grepl("Tepper", survey_df[i,]$college))
    {
        survey_df[i,]$college = "Tepper"
    }
    if(grepl("SCS", survey_df[i,]$college))
    {
        survey_df[i,]$college = "SCS"
    }
}


# overall OS distn
svg(filename="prim_os_overall.svg", width=7, height=6)
#png(filename="prim_os_overall.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df$prim_os)[-1], ylim=c(0, 60), col="steelblue4",
        xlab="Primary OS", ylab="Frequency", main="Respondent Primary OS n=123")
dev.off()

# overall grade distn
png(filename="year_overall.png", units="in", width=7.3, height=6, pointsize=12, res=256)
barplot(table(survey_df$grade)[c(2, 7, 3, 6, 4, 5)], ylim=c(0, 40), col="steelblue4",
        xlab="Year", ylab="Frequency", main="Respondent Year n=123")
grid()
dev.off()
### os distn by college
# cfa
png(filename="prim_os_cfa.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$college == "CFA",]$prim_os)[-1], ylim=c(0, 8), col="steelblue4",
        xlab="Primary OS", ylab="Frequency", main="CFA Respondent Primary OS n=11")
grid()
dev.off()
# cit
png(filename="prim_os_cit.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$college == "CIT",]$prim_os)[-1], ylim=c(0, 20), col="steelblue4",
        xlab="Primary OS", ylab="Frequency", main="CIT Respondent Primary OS n=30")
grid()
dev.off()
# scs
png(filename="prim_os_scs.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$college == "SCS",]$prim_os)[-1], ylim=c(0, 10), col="steelblue4",
        xlab="Primary OS", ylab="Frequency", main="SCS Respondent Primary OS n=18")
grid()
dev.off()
# dietrich
png(filename="prim_os_dietrich.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$college == "Dietrich",]$prim_os)[-1], ylim=c(0, 15), col="steelblue4",
        xlab="Primary OS", ylab="Frequency", main="Dietrich Respondent Primary OS n=23")
grid()
dev.off()

# Heinz
png(filename="prim_os_heinz.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$college == "Heinz",]$prim_os)[-1], ylim=c(0, 4), col="steelblue4",
        xlab="Primary OS", ylab="Frequency", main="Heinz Respondent Primary OS n=7")
grid()
dev.off()

# MCS
png(filename="prim_os_mcs.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$college == "MCS",]$prim_os)[-1], ylim=c(0, 10), col="steelblue4",
        xlab="Primary OS", ylab="Frequency", main="MCS Respondent Primary OS n=15")
grid()
dev.off()

# Tepper
png(filename="prim_os_tepper.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$college == "Tepper",]$prim_os)[-1], ylim=c(0, 10), col="steelblue4",
        xlab="Primary OS", ylab="Frequency", main="Tepper Respondent Primary OS n=16")
grid()
dev.off()

# asia
png(filename="prim_os_asia.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$region == "Asia",]$prim_os)[-1], ylim=c(0, 15), col="steelblue4",
        xlab="Primary OS", ylab="Frequency", main="Asia Respondent Primary OS n=26")
grid()
dev.off()

#primary OS by country
# europe
png(filename="prim_os_europe.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$region == "Europe",]$prim_os)[-1], ylim=c(0, 4), col="steelblue4",
        xlab="Primary OS", ylab="Frequency", main="Europe Respondent Primary OS n=4")
grid()
dev.off()

# North America
png(filename="prim_os_north_america.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$region == "North America",]$prim_os)[-1], ylim=c(0, 50), col="steelblue4",
        xlab="Primary OS", ylab="Frequency", main="North America Respondent Primary OS n=91")
grid()
dev.off()

# South America
png(filename="prim_os_south_america.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$region == "South America",]$prim_os)[-1], ylim=c(0, 2), col="steelblue4",
        xlab="Primary OS", ylab="Frequency", main="South America Respondent Primary OS n=1")
grid()
dev.off()

# primary OS by grade
# Freshman
png(filename="prim_os_freshman.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$grade == "Freshman",]$prim_os)[-1], ylim=c(0, 12), col="steelblue4",
        xlab="Primary OS", ylab="Frequency", main="Freshman Respondent Primary OS n=23")
grid()
dev.off()
# Sophomore
png(filename="prim_os_sophomore.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$grade == "Sophomore",]$prim_os)[-1], ylim=c(0, 8), col="steelblue4",
        xlab="Primary OS", ylab="Frequency", main="Sophomore Respondent Primary OS n=13")
grid()
dev.off()
# Junior
png(filename="prim_os_junior.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$grade == "Junior",]$prim_os)[-1], ylim=c(0, 8), col="steelblue4",
        xlab="Primary OS", ylab="Frequency", main="Junior Respondent Primary OS n=16")
grid()
dev.off()
# Senior
png(filename="prim_os_senior.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$grade == "Senior",]$prim_os)[-1], ylim=c(0, 8), col="steelblue4",
        xlab="Primary OS", ylab="Frequency", main="Senior Respondent Primary OS n=14")
grid()
dev.off()
# Masters
png(filename="prim_os_masters.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$grade == "Masters",]$prim_os)[-1], ylim=c(0, 20), col="steelblue4",
        xlab="Primary OS", ylab="Frequency", main="Masters Respondent Primary OS n=38")
grid()
dev.off()
# PhD
png(filename="prim_os_phd.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$grade == "PhD",]$prim_os)[-1], ylim=c(0, 8), col="steelblue4",
        xlab="Primary OS", ylab="Frequency", main="PhD Respondent Primary OS n=18")
grid()
dev.off()

#### secondary os by school
# cfa
png(filename="sec_os_cfa.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$college == "CFA",]$sec_os)[-1], ylim=c(0, 2), col="steelblue4",
        xlab="Secondary OS", ylab="Frequency", main="CFA Respondent Secondary OS n=5")
grid()
dev.off()
# cit
png(filename="sec_os_cit.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$college == "CIT",]$sec_os)[-1], ylim=c(0, 8), col="steelblue4",
        xlab="Secondary OS", ylab="Frequency", main="CIT Respondent Secondary OS n=13")
grid()
dev.off()
# scs
png(filename="sec_os_scs.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$college == "SCS",]$sec_os)[-1], ylim=c(0, 4), col="steelblue4",
        xlab="Secondary OS", ylab="Frequency", main="SCS Respondent Secondary OS n=8")
grid()
dev.off()
# dietrich
png(filename="sec_os_dietrich.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$college == "Dietrich",]$sec_os)[-1], ylim=c(0, 6), col="steelblue4",
        xlab="Secondary OS", ylab="Frequency", main="Dietrich Respondent Secondary OS n=10")
grid()
dev.off()

# Heinz
png(filename="sec_os_heinz.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$college == "Heinz",]$sec_os)[-1], ylim=c(0, 3), col="steelblue4",
        xlab="Secondary OS", ylab="Frequency", main="Heinz Respondent Secondary OS n=3")
grid()
dev.off()

# MCS
png(filename="sec_os_mcs.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$college == "MCS",]$sec_os)[-1], ylim=c(0, 2), col="steelblue4",
        xlab="Secondary OS", ylab="Frequency", main="MCS Respondent Secondary OS n=2")
grid()
dev.off()

# Tepper
png(filename="sec_os_tepper.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$college == "Tepper",]$sec_os)[-1], ylim=c(0, 2), col="steelblue4",
        xlab="Secondary OS", ylab="Frequency", main="Tepper Respondent Secondary OS n=5")
grid()
dev.off()

# secondary OS by region
# asia
png(filename="sec_os_asia.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$region == "Asia",]$sec_os)[-1], ylim=c(0, 6), col="steelblue4",
        xlab="Secondary OS", ylab="Frequency", main="Asia Respondent Secondary OS n=13")
grid()
dev.off()

# europe
png(filename="sec_os_europe.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$region == "Europe",]$sec_os)[-1], ylim=c(0, 2), col="steelblue4",
        xlab="Secondary OS", ylab="Frequency", main="Europe Respondent Secondary OS n=2")
grid()
dev.off()

# North America
png(filename="sec_os_north_america.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$region == "North America",]$sec_os)[-1], ylim=c(0, 20), col="steelblue4",
        xlab="Secondary OS", ylab="Frequency", main="North America Respondent Secondary OS n=32")
grid()
dev.off()

# EDA
png(filename="sec_os_overall.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df$sec_os)[-1], ylim=c(0, 25), col="steelblue4",
        xlab="Secondary OS", ylab="Frequency", main="Respondent Secondary OS n=47")
grid()
dev.off()
# secondary computer
png(filename="sec_comp_overall.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df$sec_type)[-1], ylim=c(0, 30), col="steelblue4",
        xlab="Secondary Computer", ylab="Frequency", main="Respondent Secondary Computer n=46")
grid()
dev.off()

png(filename="region_overall.png", units="in", width=9, height=6, pointsize=12, res=256)
barplot(table(factor(survey_df$region,
                     levels=c("Asia", "Europe", "North America", "South America", "Oceania", "Africa"))),
        ylim=c(0, 100), col="steelblue4",
        xlab="Region", ylab="Frequency", main="Respondent Region n=122")
grid()
dev.off()

# next primary OS by OS
png(filename="next_prim_os_overall.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df$next_prim_os)[-1], ylim=c(0, 70), col="steelblue4",
        xlab="Next Primary OS", ylab="Frequency", main="Respondent Next Primary OS n=123")
grid()
dev.off()

png(filename="next_prim_os_linux.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$prim_os == "Linux Distribution",]$next_prim_os)[-1],
        ylim=c(0, 10), col="steelblue4",
        xlab="Next Primary OS", ylab="Frequency", main="Linux Respondent Next Primary OS n=9")
grid()
dev.off()

png(filename="next_prim_os_windows.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$prim_os == "Windows",]$next_prim_os)[-1],
        ylim=c(0, 50), col="steelblue4",
        xlab="Next Primary OS", ylab="Frequency", main="Windows Respondent Next Primary OS n=53")
grid()
dev.off()

png(filename="next_prim_os_mac.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$prim_os == "Mac",]$next_prim_os)[-1],
        ylim=c(0, 60), col="steelblue4",
        xlab="Next Primary OS", ylab="Frequency", main="Mac Respondent Next Primary OS n=58")
grid()
dev.off()

png(filename="next_prim_os_other.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$prim_os == "Other",]$next_prim_os)[-1],
        ylim=c(0, 1), col="steelblue4",
        xlab="Next Primary OS", ylab="Frequency", main="Other Respondent Next Primary OS n=1")
grid()
dev.off()

png(filename="prim_comp_overall.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df$prim_comp)[-1],
        ylim=c(0, 130), col="steelblue4",
        xlab="Computer", ylab="Frequency", main="Respondent Primary Computer n=123")
grid()
dev.off()

# satisfaction by OS
png(filename="satisfaction_linux.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$prim_os == "Linux Distribution",]$prim_satis),
        ylim=c(0, 6), col="steelblue4",
        xlab="Satisfaction", ylab="Frequency", main="Linux Respondent Satisfaction n=9")
grid()
dev.off()

png(filename="satisfaction_windows.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$prim_os == "Windows",]$prim_satis),
        ylim=c(0, 30), col="steelblue4", prob=TRUE,
        xlab="Satisfaction", ylab="Frequency", main="Windows Respondent Satisfaction n=53")
grid()
dev.off()

png(filename="satisfaction_mac.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$prim_os == "Mac",]$prim_satis),
        ylim=c(0, 30), col="steelblue4",
        xlab="Satisfaction", ylab="Frequency", main="Mac Respondent Satisfaction n=58")
grid()
dev.off()

png(filename="satisfaction_other.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(table(survey_df[survey_df$prim_os == "Other",]$prim_satis),
        ylim=c(0, 1), col="steelblue4",
        xlab="Satisfaction", ylab="Frequency", main="Other Respondent Satisfaction n=1")
grid()
dev.off()

# create dataframe for usage
os = c("Mac", "Windows", "Linux Distribution", "iOS", "Other")
prop_prim_schoolwork = c(sum(survey_df[survey_df$prim_os == "Mac",]$prim_schoolwork) /
                         nrow(survey_df[survey_df$prim_os == "Mac",]),
                         sum(survey_df[survey_df$prim_os == "Windows",]$prim_schoolwork) /
                             nrow(survey_df[survey_df$prim_os == "Windows",]),
                         sum(survey_df[survey_df$prim_os == "Linux Distribution",]$prim_schoolwork) /
                             nrow(survey_df[survey_df$prim_os == "Linux Distribution",]),
                         sum(survey_df[survey_df$prim_os == "iOS",]$prim_schoolwork) /
                             nrow(survey_df[survey_df$prim_os == "iOS",]),
                         sum(survey_df[survey_df$prim_os == "Other",]$prim_schoolwork) /
                             nrow(survey_df[survey_df$prim_os == "Other",]))
prop_prim_gaming = c(sum(survey_df[survey_df$prim_os == "Mac",]$prim_gaming) /
                         nrow(survey_df[survey_df$prim_os == "Mac",]),
                     sum(survey_df[survey_df$prim_os == "Windows",]$prim_gaming) /
                         nrow(survey_df[survey_df$prim_os == "Windows",]),
                     sum(survey_df[survey_df$prim_os == "Linux Distribution",]$prim_gaming) /
                         nrow(survey_df[survey_df$prim_os == "Linux Distribution",]),
                     sum(survey_df[survey_df$prim_os == "iOS",]$prim_gaming) /
                         nrow(survey_df[survey_df$prim_os == "iOS",]),
                     sum(survey_df[survey_df$prim_os == "Other",]$prim_gaming) /
                         nrow(survey_df[survey_df$prim_os == "Other",]))
prop_prim_social = c(sum(survey_df[survey_df$prim_os == "Mac",]$prim_social_media) /
                         nrow(survey_df[survey_df$prim_os == "Mac",]),
                     sum(survey_df[survey_df$prim_os == "Windows",]$prim_social_media) /
                         nrow(survey_df[survey_df$prim_os == "Windows",]),
                     sum(survey_df[survey_df$prim_os == "Linux Distribution",]$prim_social_media) /
                         nrow(survey_df[survey_df$prim_os == "Linux Distribution",]),
                     sum(survey_df[survey_df$prim_os == "iOS",]$prim_social_media) /
                         nrow(survey_df[survey_df$prim_os == "iOS",]),
                     sum(survey_df[survey_df$prim_os == "Other",]$prim_social_media) /
                         nrow(survey_df[survey_df$prim_os == "Other",]))
prop_prim_job = c(sum(survey_df[survey_df$prim_os == "Mac",]$prim_job_related) /
                      nrow(survey_df[survey_df$prim_os == "Mac",]),
                  sum(survey_df[survey_df$prim_os == "Windows",]$prim_job_related) /
                      nrow(survey_df[survey_df$prim_os == "Windows",]),
                  sum(survey_df[survey_df$prim_os == "Linux Distribution",]$prim_job_related) /
                      nrow(survey_df[survey_df$prim_os == "Linux Distribution",]),
                  sum(survey_df[survey_df$prim_os == "iOS",]$prim_job_related) /
                      nrow(survey_df[survey_df$prim_os == "iOS",]),
                  sum(survey_df[survey_df$prim_os == "Other",]$prim_job_related) /
                      nrow(survey_df[survey_df$prim_os == "Other",]))
prop_prim_project = c(sum(survey_df[survey_df$prim_os == "Mac",]$prim_projects) /
                          nrow(survey_df[survey_df$prim_os == "Mac",]),
                      sum(survey_df[survey_df$prim_os == "Windows",]$prim_projects) /
                          nrow(survey_df[survey_df$prim_os == "Windows",]),
                      sum(survey_df[survey_df$prim_os == "Linux Distribution",]$prim_projects) /
                          nrow(survey_df[survey_df$prim_os == "Linux Distribution",]),
                      sum(survey_df[survey_df$prim_os == "iOS",]$prim_projects) /
                          nrow(survey_df[survey_df$prim_os == "iOS",]),
                      sum(survey_df[survey_df$prim_os == "Other",]$prim_projects) /
                          nrow(survey_df[survey_df$prim_os == "Other",]))
prop_prim_entertainment = c(sum(survey_df[survey_df$prim_os == "Mac",]$prim_entertainment) /
                                nrow(survey_df[survey_df$prim_os == "Mac",]),
                            sum(survey_df[survey_df$prim_os == "Windows",]$prim_entertainment) /
                                nrow(survey_df[survey_df$prim_os == "Windows",]),
                            sum(survey_df[survey_df$prim_os == "Linux Distribution",]$prim_entertainment) /
                                nrow(survey_df[survey_df$prim_os == "Linux Distribution",]),
                            sum(survey_df[survey_df$prim_os == "iOS",]$prim_entertainment) /
                                nrow(survey_df[survey_df$prim_os == "iOS",]),
                            sum(survey_df[survey_df$prim_os == "Other",]$prim_entertainment) /
                                nrow(survey_df[survey_df$prim_os == "Other",]))
prop_prim_other = c(sum(survey_df[survey_df$prim_os == "Mac",]$prim_other) /
                        nrow(survey_df[survey_df$prim_os == "Mac",]),
                    sum(survey_df[survey_df$prim_os == "Windows",]$prim_other) /
                        nrow(survey_df[survey_df$prim_os == "Windows",]),
                    sum(survey_df[survey_df$prim_os == "Linux Distribution",]$prim_other) /
                        nrow(survey_df[survey_df$prim_os == "Linux Distribution",]),
                    sum(survey_df[survey_df$prim_os == "iOS",]$prim_other) /
                        nrow(survey_df[survey_df$prim_os == "iOS",]),
                    sum(survey_df[survey_df$prim_os == "Other",]$prim_other) /
                        nrow(survey_df[survey_df$prim_os == "Other",]))
prim_use_df = data.frame(os, prop_prim_schoolwork, prop_prim_gaming, prop_prim_social,
                         prop_prim_job, prop_prim_project, prop_prim_entertainment, prop_prim_other)

# create graphs for activities by OS
png(filename="schoolwork_os.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(prim_use_df$prop_prim_schoolwork, names.arg=prim_use_df$os,
        col="steelblue4", main="Proportion Used for Schoolwork by OS",
        xlab="OS", ylab="proportion", ylim=c(0, 1))
grid()
dev.off()

png(filename="gaming_os.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(prim_use_df$prop_prim_gaming, names.arg=prim_use_df$os,
        col="steelblue4", main="Proportion Used for Gaming by OS",
        xlab="OS", ylab="proportion", ylim=c(0, 1))
grid()
dev.off()

png(filename="job_os.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(prim_use_df$prop_prim_job, names.arg=prim_use_df$os,
        col="steelblue4", main="Proportion Used for Job-related tasks by OS",
        xlab="OS", ylab="proportion", ylim=c(0, 1))
grid()
dev.off()

png(filename="project_os.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(prim_use_df$prop_prim_project, names.arg=prim_use_df$os,
        col="steelblue4", main="Proportion Used for Personal Projects by OS",
        xlab="OS", ylab="proportion", ylim=c(0, 1))
grid()
dev.off()

png(filename="entertainment_os.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(prim_use_df$prop_prim_entertainment, names.arg=prim_use_df$os,
        col="steelblue4", main="Proportion Used for Entertainment by OS",
        xlab="OS", ylab="proportion", ylim=c(0, 1))
grid()
dev.off()

png(filename="social_os.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(prim_use_df$prop_prim_social, names.arg=prim_use_df$os,
        col="steelblue4", main="Proportion Used for Social Media by OS",
        xlab="OS", ylab="proportion", ylim=c(0, 1))
grid()
dev.off()

png(filename="other_os.png", units="in", width=7, height=6, pointsize=12, res=256)
barplot(prim_use_df$prop_prim_other, names.arg=prim_use_df$os,
        col="steelblue4", main="Proportion Used for other activities by OS",
        xlab="OS", ylab="proportion", ylim=c(0, 1))
grid()
dev.off()

regions = c("North America", "Asia", "Europe", "South America")
colleges = c("CFA", "CIT", "Dietrich", "MCS", "SCS", "Tepper")
OSs = c("Windows", "Mac", "Linux Distribution", "iOS", "Other")
grades = c("Freshman", "Sophomore", "Junior", "Senior", "Masters", "PhD")

# stacked bar plot
#Create data
data = matrix(sample(1:30,30) , nrow=5)
colnames(data) = colleges # X values
rownames(data) = OSs
for(colg in colleges)
{
    data[,colg] = table(survey_df[survey_df$college == colg,]$prim_os)[OSs]
}
#create color palette:
#library(RColorBrewer)
#coul = brewer.pal(3, "Pastel2")
bar_colors = c("#79c36a", "#599ad3", "#f9a65a", "#9e66ab", "#cd7058")
# transform to percentage
data_percentage = apply(data, 2, function(x){x*100/sum(x,na.rm=T)})
# can include col=coul
svg(filename="prim_os_college.svg",width=7, height=6)
#png(filename="prim_os_college.png", units="in", width=7, height=6, pointsize=12, res=256)
par(mar = c(5,4,4,8))
barplot(data_percentage, border="white", xlab="college", col=bar_colors,
        legend.text=OSs, main="Primary OS by College",
        args.legend=list(x="right", bty="n", inset=c(-.33,0), xpd=TRUE))
dev.off()
# OS by regions
data = matrix(sample(1:30,20) , nrow=5)
colnames(data) = regions # X values
rownames(data) = OSs
for(reg in regions)
{
    data[,reg] = table(survey_df[survey_df$region == reg,]$prim_os)[OSs]
}
data_percentage = apply(data, 2, function(x){x*100/sum(x,na.rm=T)})
svg(filename="prim_os_region.svg",width=7, height=6)
#png(filename="prim_os_region.png", units="in", width=7, height=6, pointsize=12, res=256)
par(mar = c(5,4,4,8))
barplot(data_percentage, border="white", xlab="Region", col=bar_colors,
        legend.text=OSs, main="Primary OS by Region",
        args.legend=list(x="right", bty="n", inset=c(-.33,0), xpd=TRUE))
dev.off()
# OS by year
data = matrix(sample(1:30,30) , nrow=5)
colnames(data) = grades # X values
rownames(data) = OSs
for(grade in grades)
{
    data[,grade] = table(survey_df[survey_df$grade == grade,]$prim_os)[OSs]
}
data_percentage = apply(data, 2, function(x){x*100/sum(x,na.rm=T)})
svg(filename="prim_os_year.svg",width=9, height=6)
#png(filename="prim_os_year.png", units="in", width=9, height=6, pointsize=12, res=256)
par(mar = c(5,4,4,8))
barplot(data_percentage, border="white", xlab="Year", col=bar_colors,
        legend.text=OSs, main="Primary OS by Year",
        args.legend=list(x="right", bty="n", inset=c(-.24,0), xpd=TRUE))
dev.off()
