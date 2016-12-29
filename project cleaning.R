# ****ABSTRACT******
# DESCRIBE EXACTLY HOW THE SURVEY WORKED (POINT VALUES, HOW I CALCULATED INDEX,
# HOW PPL ACCESSED IT, ETC.)

# 1. PRIMARY DATA CLEANING

x <- scan("raw data.csv", what="", sep="\n") 

y <- gsub("\t", ",", x)

as.vector(y, mode="any")
f <- strsplit(y, ",")

df <- data.frame(matrix(unlist(f), nrow=17387, byrow=T))

names(df) = c(paste("Q", 1:36, sep = "."), "age", "gender", "country")
df <- df[-1,]

df1 <- df
df1$Q.1 <- as.numeric(df$Q.1) - 1 # For some reason, my cleaning added 1 to the 
# 1st question responses... so I'm fixing that.

# Some more cleaning.

df1$country <- gsub("\"", "", df1$country) # I removed the quotation mark
# from the end of the country codes!
df1$country <- as.factor(df1$country) # Want country to be a factor, since it
# categorizes the people. 

# I elect to remove the following countries: 
# A1,"Anonymous Proxy", 
# A2,"Satellite Provider", and 
# O1,"Other Country" from the data.

df1 <- subset(df1, !(df1$country %in% c("A1","A2","01")))
levels(df1$country)[levels(df1$country) == "A1"] <- NA
levels(df1$country)[levels(df1$country) == "A2"] <- NA
levels(df1$country)[levels(df1$country) == "01"] <- NA

# **MISSING VALUES**

# The data set supposedly uses -1 to mark missing values. 
really <- sapply(df1[, 1:36], function(df1) any(df1 == -1))
really
# I think not!
# But just looking at the data set, I see a good many 0s. These should be 
# impossible, seeing as the responses are numbered 1-5. 0s are the true missing
# values...I'll just drop those observations altogether, since having 0 distorts
# the final total. 

df1[, 1:36] <- apply(df1[, 1:36], 2, function(df1) gsub("0", NA, df1))

df1 <- df1[!rowSums(is.na(df1[, 1:36])), ]

# "Gender" has missing values too. According to the codebook, 1 and 2 are male
# and female respectively. 

table(df1$gender)

# I originally wanted to discard 0s and 3s. But I'll later aggregate each 
# country's values into one representative person, and I have no way of 
# fairly assigning that person as male or female. So I might as well keep
# everyone non-gendered. The missing values can stay.

# All the question responses are character variables. I want them to be 
# numeric. While I'm at it I'll convert age to numeric too. 

df1[, 1:37] <- apply(df1[, 1:37], 2, as.numeric)

# 2. TOTALLING MEASUREMENTS, REMOVING BAD OBSERVATIONS

# Time to compile an anxiety/distance index for the scores...
# Attached document describing how the questions contrib to the indices.

# Now for the avoidance index. On most questions a higher response number 
# indicated higher avoidance. However, on certain questions a **lower** response 
# number indicated higher avoidance. So, I *subtracted* those responses.
# So, people who have higher avoidance should still end up with higher scores.
# On those *lower response questions*, they would have gotten the fewest points
# deducted.

df1$avoidance <- df1$Q.1 - df1$Q.3 + df1$Q.5 + df1$Q.7 + df1$Q.9 + df1$Q.11 + 
  df1$Q.13 - df1$Q.15 + df1$Q.17 - df1$Q.19 + df1$Q.21 + df1$Q.23 - df1$Q.25 - 
  df1$Q.27 - df1$Q.29 - df1$Q.31 - df1$Q.33 - df1$Q.35 

df1$anxiety <- df1$Q.2 + df1$Q.4 + df1$Q.6 + df1$Q.8 + df1$Q.10 + df1$Q.12 + 
  df1$Q.14 + df1$Q.16 + df1$Q.18 + df1$Q.20 - df1$Q.22 + df1$Q.24 + df1$Q.26 +
  df1$Q.28 + df1$Q.30 + df1$Q.32 + df1$Q.34 + df1$Q.36
  
# I'll just explore and make sure I haven't missed anything...
summary(df1$age)
# WHICH JERKS PUT IN THEIR AGE AS 2150 AND -90?
# Let's discard ages greater than 100.
df1 <- df1[-c(which(df1$age > 100)),]
# As for a lower bound, I'm assuming that nobody under 10 would take a survey 
# about close relationships
df1 <- df1[-c(which(df1$age < 10)),]

# These remove PA and VG from the running. 

levels(df1$country)[levels(df1$country) == "PA"] <- NA
levels(df1$country)[levels(df1$country) == "VG"] <- NA

# All right. While I'm sure a lot of these remaining people are lying about
# their age, there's really no way to tell. I suppose it's conceivable that a
# 99-year--old grandma or grandpa somewhere has taken this quiz. I'd hate to boot
# them out of the data set because of a few bad apples.

# 3. ACTUAL DATA EXPLORATION

# Now let's examine the anxiety and avoidance measures...

# Anxiety: the highest anxiety score a person could receive was 17 * 5 - 1, or
# 84. The lowest possible score was 17 - 5, or 12.

hist(df1$anxiety)
qqnorm(df1$anxiety)

# A little skewed-looking, but altogether not bad...

# Avoidance: the highest avoidance score possible was 9 * 5 - 9 = 36. Lowest 
# possible was 9 - 9 * 5 = -36. 

hist(df1$avoidance)

# Oof, that's quite skewed. Try a transformation: I'll add 50 and take sqrt.

hist(sqrt(df1$avoidance + 50)) 

# Not bad.

df1$avoidance.1 <- sqrt(df1$avoidance + 50)

# First I'd like to try regression to see if country really can explain the 
# variations in avoidance and anxiety.

lm.0 <- lm(anxiety ~ age + gender + country, data=df1)
summary(lm.0)
# A somewhat low R-squared. Onward!
qqnorm(lm.0$resid)
# Pretty straight, all things considered.
anova(lm.0)
# Country has a p-value of .002306. Not overwhelmingly significant, certainly.
# Age is a far better predictor. But I am undeterred!

# Let's see about avoidance. 

lm.1 <- lm(avoidance.1 ~ age + gender + country, data=df1)
summary(lm.1)
qqnorm(lm.1$resid)
# Worse-looking than the last one..but it could be worse...
anova(lm.1)
# Ouch. Country's p-value takes a definite hit down to 0.0211. But, well, I'll 
# keep going.

# Given that country can somewhat predict anxiety and avoidance, I'll average 
# the measurements for each country and create a representative observation. 
# This will allow me to see which countries are the most anxious and avoidant.

library(data.table)
agg <- setDT(df1)[, lapply(.SD, mean), by = country, .SDcols = -("gender")]

# The problem is that this lumps all age groups and genders together. 
# AGE:
# I can see if the ages are evenly distributed across countries by using the 
# regression to test whether COUNTRY can predict AGE with any degree of accuracy.
lm.2 <- lm(age ~ gender + avoidance.1 + anxiety + country, data=df1)
summary(lm.2)
anova(lm.2)

# It turns out that country is a PRETTY GOOD predictor of age. But on the bright
# side, it's the WEAKEST predictor out of all the ones in the model. 
# Also, the multiple R-squared is only about 0.05.

# So, I don't feel terrible about mixing the ages together.

# The results probably cluster based on gender as well. But since many countries
# only have one observation, and therefore one gender, it's difficult to 
# create separate groups based on gender. I eliminate gender as a variable. 

# I realize that this is an EXTREMELY sloppy move, but since I'm creating a 
# representative person for each country, that person can't encompass all 
# possible gender groups. Plus, this allows me to include non-binary people who 
# identify as neither or both!

dagg <- as.data.frame(agg)

library(rworldmap)
mapDevice()
data(dagg)
sPDF <- joinCountryData2Map(dagg, joinCode='ISO2', nameJoinColumn='country', 
                            verbose='TRUE')

mapDevice()
mapCountryData(sPDF, nameColumnToPlot='anxiety', 
               numCats=10, mapTitle="Worldwide anxiety", 
               addLegend=TRUE)




# We lost one observation from an unspecified Asian region, one from equally
# unspecified Europe, and Gibraltar. Gibraltar's ISO2 code is correct, but
# the package apparently doesn't include it... :(

mapCountryData(sPDF, nameColumnToPlot='avoidance.1', 
               numCats=10, mapTitle="Worldwide avoidance", 
               addLegend=TRUE)
dev.off()
