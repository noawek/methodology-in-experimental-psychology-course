


############################################### few comments before starting #################################################################


# we wrote this code under the newest version of R: version 4.2.3
# it is important to have this version in order to be able to install all the packages that are used in this code.


# we didn't execute a reliability test, because:
# It is impossible to calculate reliability for a questionnaire with one question
# Retest is not relevant in this case.





#################################################### extract file ################################################################################


# our file
fname <- file.choose()
mydata <- read.csv(fname, header = TRUE) # read the file





############################################### arranging our data #################################################################


# The arrangement of the data in narrow rows and columns
mydata2 <- mydata[3:32, ]
mydata3 <- mydata2[, c( "ResponseId" , "gender" , "age_4" , "A" , "A.1" , "A.2" , "A.3" ,"A.4" 
                       , "A.5" , "A.6" , "A.7" , "N" , "N.1" , "N.2" , "N.3" , 
                       "N.4" , "N.5" , "N.6" , "N.7" )]


# removing all the unnecessary data, after using it (so the environment won't be in overload)
remove(mydata, mydata2, list = character(0),inherits = FALSE)


# Converting each variable to numeric, so we can use the data for statistical calculations
mydata3$age_4 <- as.numeric(mydata3$age_4)
mydata3$gender <- as.numeric(mydata3$gender)
mydata3$A <- as.numeric(mydata3$A)
mydata3$A.1 <- as.numeric(mydata3$A.1)
mydata3$A.2 <- as.numeric(mydata3$A.2)
mydata3$A.3 <- as.numeric(mydata3$A.3)
mydata3$A.4 <- as.numeric(mydata3$A.4)
mydata3$A.5 <- as.numeric(mydata3$A.5)
mydata3$A.6 <- as.numeric(mydata3$A.6)
mydata3$A.7 <- as.numeric(mydata3$A.7)
mydata3$N <- as.numeric(mydata3$N)
mydata3$N.1 <- as.numeric(mydata3$N.1)
mydata3$N.2 <- as.numeric(mydata3$N.2)
mydata3$N.3 <- as.numeric(mydata3$N.3)
mydata3$N.4 <- as.numeric(mydata3$N.4)
mydata3$N.5 <- as.numeric(mydata3$N.5)
mydata3$N.6 <- as.numeric(mydata3$N.6)
mydata3$N.7 <- as.numeric(mydata3$N.7)





############################################### descriptive statistics #################################################################


# Summary of descriptive statistics for each column of our variables, omitting NA
summary(mydata3)


# Creating the mode() function that allows to calculate the mode
mode <- function(x, na.rm = FALSE) {
  
  if(na.rm){         # if na.rm is TRUE, remove NA values from input x
    x = x[!is.na(x)]
  }
  
  val <- unique(x)
  return(val[which.max(tabulate(match(x, val)))])
}



## Descriptive statistics of demographic data

# age
age_mean <- mean(mydata3$age_4)
sd_age <- sd(mydata3$age_4)
median_age <- median(mydata3$age_4)
mode_age <- mode(mydata3$age_4)

# gender
# number of participants from each group
number_of_women <- sum(mydata3$gender==4)
number_of_men <- sum(mydata3$gender==5)
number_of_other <- sum(mydata3$gender==6)
# ratio between the number of participants from each group and the overall number of participants
ratio_women_vs_all <- number_of_women/(number_of_women+number_of_men+number_of_other)
ratio_men_vs_all <- number_of_men/(number_of_women+number_of_men+number_of_other)
ratio_other_vs_all <- number_of_other/(number_of_women+number_of_men+number_of_other)
# the percentage of each gender from all
percentage_of_women_from_all <- ratio_women_vs_all*100
percentage_of_men_from_all <- ratio_men_vs_all*100
percentage_of_other_from_all <- ratio_other_vs_all*100

  

## Descriptive statistics of the variables, omitting NA

# Arousing condition 
Arousal <- as.vector(as.matrix(mydata3[,c("A", "A.1", "A.2" , "A.3" , "A.4" , "A.5" , "A.6" , "A.7")]))
mean_arousel <- mean(Arousal, na.rm = TRUE)
sd_arousel <- sd(Arousal, na.rm = TRUE)
median_arousel <- median(Arousal, na.rm = TRUE)
mode_arousel <- mode(Arousal, na.rm = TRUE)

# not arousing condition
not.Arousal <- as.vector(as.matrix(mydata3[,c("N" , "N.1" , "N.2" , "N.3" , "N.4" , "N.5" , "N.6" , "N.7")]))
mean_neutral <- mean(not.Arousal, na.rm = TRUE)
sd_neutral <- sd(not.Arousal, na.rm = TRUE)
median_neutral <- median(not.Arousal, na.rm = TRUE)
mode_neutral <- mode(not.Arousal, na.rm = TRUE)





############################################# sorting our data for data analysis #################################################################


# Sorting our data, by calculating the average of each of the conditions (arousing vs. not-arousing) for each of the subjects, without NA's
A.1 <- as.vector(as.matrix(mydata3[1,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.2 <- as.vector(as.matrix(mydata3[2,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.3 <- as.vector(as.matrix(mydata3[3,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.4 <- as.vector(as.matrix(mydata3[4,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.5 <- as.vector(as.matrix(mydata3[5,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.6 <- as.vector(as.matrix(mydata3[6,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.7 <- as.vector(as.matrix(mydata3[7,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.8 <- as.vector(as.matrix(mydata3[8,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.9 <- as.vector(as.matrix(mydata3[9,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.10 <- as.vector(as.matrix(mydata3[10,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.11 <- as.vector(as.matrix(mydata3[11,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.12 <- as.vector(as.matrix(mydata3[12,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.13 <- as.vector(as.matrix(mydata3[13,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.14 <- as.vector(as.matrix(mydata3[14,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.15 <- as.vector(as.matrix(mydata3[15,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.16 <- as.vector(as.matrix(mydata3[16,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.17 <- as.vector(as.matrix(mydata3[17,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.18 <- as.vector(as.matrix(mydata3[18,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.19 <- as.vector(as.matrix(mydata3[19,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.20 <- as.vector(as.matrix(mydata3[20,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.21 <- as.vector(as.matrix(mydata3[21,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.22 <- as.vector(as.matrix(mydata3[22,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.23 <- as.vector(as.matrix(mydata3[23,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.24 <- as.vector(as.matrix(mydata3[24,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.25 <- as.vector(as.matrix(mydata3[25,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.26 <- as.vector(as.matrix(mydata3[26,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.27 <- as.vector(as.matrix(mydata3[27,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.28 <- as.vector(as.matrix(mydata3[28,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.29 <- as.vector(as.matrix(mydata3[29,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A.30 <- as.vector(as.matrix(mydata3[30,c("A" , "A.1" , "A.2" , "A.3" ,"A.4", "A.5" , "A.6" , "A.7")]))
A..1 <- mean(A.1, na.rm = TRUE)
A..2 <- mean(A.2, na.rm = TRUE)
A..3 <- mean(A.3, na.rm = TRUE)
A..4 <- mean(A.4, na.rm = TRUE)
A..5 <- mean(A.5, na.rm = TRUE)
A..6 <- mean(A.6, na.rm = TRUE)
A..7 <- mean(A.7, na.rm = TRUE)
A..8 <- mean(A.8, na.rm = TRUE)
A..9 <- mean(A.9, na.rm = TRUE)
A..10 <- mean(A.10, na.rm = TRUE)
A..11 <- mean(A.11, na.rm = TRUE)
A..12 <- mean(A.12, na.rm = TRUE)
A..13 <- mean(A.13, na.rm = TRUE)
A..14 <- mean(A.14, na.rm = TRUE)
A..15 <- mean(A.15, na.rm = TRUE)
A..16 <- mean(A.16, na.rm = TRUE)
A..17 <- mean(A.17, na.rm = TRUE)
A..18 <- mean(A.18, na.rm = TRUE)
A..19 <- mean(A.19, na.rm = TRUE)
A..20 <- mean(A.20, na.rm = TRUE)
A..21 <- mean(A.21, na.rm = TRUE)
A..22 <- mean(A.22, na.rm = TRUE)
A..23 <- mean(A.23, na.rm = TRUE)
A..24 <- mean(A.24, na.rm = TRUE)
A..25 <- mean(A.25, na.rm = TRUE)
A..26 <- mean(A.26, na.rm = TRUE)
A..27 <- mean(A.27, na.rm = TRUE)
A..28 <- mean(A.28, na.rm = TRUE)
A..29 <- mean(A.29, na.rm = TRUE)
A..30 <- mean(A.30, na.rm = TRUE)
##
N.A.1 <- as.vector(as.matrix(mydata3[1,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.2 <- as.vector(as.matrix(mydata3[2,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.3 <- as.vector(as.matrix(mydata3[3,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.4 <- as.vector(as.matrix(mydata3[4,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.5 <- as.vector(as.matrix(mydata3[5,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.6 <- as.vector(as.matrix(mydata3[6,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.7 <- as.vector(as.matrix(mydata3[7,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.8 <- as.vector(as.matrix(mydata3[8,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.9 <- as.vector(as.matrix(mydata3[9,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.10 <- as.vector(as.matrix(mydata3[10,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.11 <- as.vector(as.matrix(mydata3[11,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.12 <- as.vector(as.matrix(mydata3[12,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.13 <- as.vector(as.matrix(mydata3[13,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.14 <- as.vector(as.matrix(mydata3[14,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.15 <- as.vector(as.matrix(mydata3[15,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.16 <- as.vector(as.matrix(mydata3[16,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.17 <- as.vector(as.matrix(mydata3[17,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.18 <- as.vector(as.matrix(mydata3[18,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.19 <- as.vector(as.matrix(mydata3[19,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.20 <- as.vector(as.matrix(mydata3[20,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.21 <- as.vector(as.matrix(mydata3[21,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.22 <- as.vector(as.matrix(mydata3[22,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.23 <- as.vector(as.matrix(mydata3[23,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.24 <- as.vector(as.matrix(mydata3[24,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.25 <- as.vector(as.matrix(mydata3[25,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.26 <- as.vector(as.matrix(mydata3[26,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.27 <- as.vector(as.matrix(mydata3[27,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.28 <- as.vector(as.matrix(mydata3[28,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.29 <- as.vector(as.matrix(mydata3[29,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A.30 <- as.vector(as.matrix(mydata3[30,c("N" , "N.1" , "N.2" , "N.3" ,"N.4", "N.5" , "N.6" , "N.7")]))
N.A..1 <- mean(N.A.1, na.rm = TRUE)
N.A..2 <- mean(N.A.2, na.rm = TRUE)
N.A..3 <- mean(N.A.3, na.rm = TRUE)
N.A..4 <- mean(N.A.4, na.rm = TRUE)
N.A..5 <- mean(N.A.5, na.rm = TRUE)
N.A..6 <- mean(N.A.6, na.rm = TRUE)
N.A..7 <- mean(N.A.7, na.rm = TRUE)
N.A..8 <- mean(N.A.8, na.rm = TRUE)
N.A..9 <- mean(N.A.9, na.rm = TRUE)
N.A..10 <- mean(N.A.10, na.rm = TRUE)
N.A..11 <- mean(N.A.11, na.rm = TRUE)
N.A..12 <- mean(N.A.12, na.rm = TRUE)
N.A..13 <- mean(N.A.13, na.rm = TRUE)
N.A..14 <- mean(N.A.14, na.rm = TRUE)
N.A..15 <- mean(N.A.15, na.rm = TRUE)
N.A..16 <- mean(N.A.16, na.rm = TRUE)
N.A..17 <- mean(N.A.17, na.rm = TRUE)
N.A..18 <- mean(N.A.18, na.rm = TRUE)
N.A..19 <- mean(N.A.19, na.rm = TRUE)
N.A..20 <- mean(N.A.20, na.rm = TRUE)
N.A..21 <- mean(N.A.21, na.rm = TRUE)
N.A..22 <- mean(N.A.22, na.rm = TRUE)
N.A..23 <- mean(N.A.23, na.rm = TRUE)
N.A..24 <- mean(N.A.24, na.rm = TRUE)
N.A..25 <- mean(N.A.25, na.rm = TRUE)
N.A..26 <- mean(N.A.26, na.rm = TRUE)
N.A..27 <- mean(N.A.27, na.rm = TRUE)
N.A..28 <- mean(N.A.28, na.rm = TRUE)
N.A..29 <- mean(N.A.29, na.rm = TRUE)
N.A..30 <- mean(N.A.30, na.rm = TRUE)


# means in the arousal condition for each of the subjects, by their order
Ar <- c(A..1,A..2,A..3,A..4,A..5,A..6,A..7,A..8,A..9,A..10,A..11,A..12,A..13
        ,A..14,A..15,A..16,A..17,A..18,A..19,A..20,A..21,A..22,A..23,A..24,A..25,A..26,A..27,A..28,A..29,A..30)


# means in the not-Arousal condition for each of the subjects, by their order
N.Ar <- c(N.A..1,N.A..2,N.A..3,N.A..4,N.A..5,N.A..6,N.A..7,N.A..8,N.A..9,N.A..10,N.A..11,N.A..12,N.A..13
          ,N.A..14,N.A..15,N.A..16,N.A..17,N.A..18,N.A..19,N.A..20,N.A..21,N.A..22,N.A..23,N.A..24,N.A..25,N.A..26,N.A..27,N.A..28,N.A..29,N.A..30)


# removing all the unnecessary data, after using it (so the environment won't be in overload)
remove(N.A.1, N.A.2, N.A.3, N.A.4, N.A.5, N.A.6, N.A.7, N.A.8, N.A.9, N.A.10, N.A.11, N.A.12, N.A.13, N.A.14, N.A.15, N.A.16, N.A.17, N.A.18, N.A.19, N.A.20, N.A.21, N.A.22, N.A.23, N.A.24, N.A.25, N.A.26, N.A.27, N.A.28, N.A.29, N.A.30, list = character(0),inherits = FALSE)
remove(N.A..1, N.A..2, N.A..3, N.A..4, N.A..5, N.A..6, N.A..7, N.A..8, N.A..9, N.A..10, N.A..11, N.A..12, N.A..13, N.A..14, N.A..15, N.A..16, N.A..17, N.A..18, N.A..19, N.A..20, N.A..21, N.A..22, N.A..23, N.A..24, N.A..25, N.A..26, N.A..27, N.A..28, N.A..29, N.A..30, list = character(0),inherits = FALSE)
remove(A.1, A.2, A.3, A.4, A.5, A.6, A.7, A.8, A.9, A.10, A.11, A.12, A.13, A.14, A.15, A.16, A.17, A.18, A.19, A.20, A.21, A.22, A.23, A.24, A.25, A.26, A.27, A.28, A.29, A.30, list = character(0),inherits = FALSE)
remove(A..1, A..2, A..3, A..4, A..5, A..6, A..7, A..8, A..9, A..10, A..11, A..12, A..13, A..14, A..15, A..16, A..17, A..18, A..19, A..20, A..21, A..22, A..23, A..24, A..25, A..26, A..27, A..28, A..29, A..30, list = character(0),inherits = FALSE)


# Combining the 2 conditions in our data set
mydata3['ar'] = Ar
mydata3['n.ar'] = N.Ar

# Or a separate table of the 2 conditions
m <- matrix(c(Ar, N.Ar), ncol = 2)





####################################################### assumptions check #################################################################


## normality assumption check

# Shapiro-Wilk normality test for the differences
diff <- Ar - N.Ar # creating a vector of the gaps for every subject
shapiro.test(diff) 

# normality check by graph
install.packages('ggpubr')
library('ggpubr')
ggqqplot(diff)

# normality check is the only assumption check which is necessary in our case





############################################################### tests #########################################################################


## the t-test:

t.test(Ar, N.Ar , paired = TRUE, alternative = "greater")



## Paired samples t-test: Bayesian t-test  

# Bayesian Power Analysis
install.packages('BayesFactor')
library('BayesFactor') # needed for the calculation of Bayes factors
bayes_paired <- ttestBF(N.Ar, Ar, paired = TRUE) # Bayesian t-test
bayes_paired # the null hypothesis versus the alternative hypothesis





############################################################### extras #########################################################################


## 2 extra plots to demonstrate the paired data: Visualization

# first demonstration:
install.packages('PairedData')
library('PairedData')
pd <- paired(N.Ar, Ar)
plot(pd, type = "profile") + theme_bw()

# second demonstration:
my_data5 <- data.frame(     # building a new data frame to present a graph
  group = rep(c("N.Ar", "Ar"), each = 30),
  atractiveness = c(N.Ar,  Ar)
)
library('ggpubr')  # the graph
ggboxplot(my_data5, x = "group", y = "atractiveness", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          order = c("N.Ar", "Ar"),
          ylab = "atractiveness", xlab = "Groups")




