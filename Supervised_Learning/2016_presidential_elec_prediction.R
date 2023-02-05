library(readxl)
install.packages("ISLR")
library(ISLR)
ANES2016 <- read_excel("C:/Users/Study/OneDrive/Desktop/DU/Statastical_Learning/Home_assignment_1/ANES2016.XLSX")
View(ANES2016)
ANES2016_copy <- ANES2016

# task 1 a

ANES2016_trump <- subset(ANES2016_copy, Trump >= 0)
View(ANES2016_trump)
unique(ANES2016_trump$Trump)
ANES2016_trump$Trump <- ifelse(ANES2016_trump$Trump < 4,"Liberal","Conservative")
# convert as factor
ANES2016_trump$Trump <- as.factor(ANES2016_trump$Trump)
ANES2016_trump$Partner <- as.numeric(ANES2016_trump$Partner)
ANES2016_trump$PartyID <- as.factor(ANES2016_trump$PartyID)
ANES2016_trump$Marital <- as.numeric(ANES2016_trump$Marital)
str(ANES2016_trump)
#iterate hrough all columns
names(ANES2016_trump)

ANES2016_trump_LR_ID <- glm(Trump ~ ID,"binomial", data = ANES2016_trump, subset = (ID > 0))
summary(ANES2016_trump_LR)$coeff

ANES2016_trump_LR_Media <- glm(Trump ~ Media,"binomial", data = ANES2016_trump, subset = (Media >= 0))
summary(ANES2016_trump_LR_Media)$coeff

ANES2016_trump_LR_FamSize <- glm(Trump ~ FamSize,"binomial", data = ANES2016_trump, subset = (FamSize >= 0))
summary(ANES2016_trump_LR_FamSize)$coeff

ANES2016_trump_LR_Hilary <- glm(Trump ~ Hillary,"binomial", data = ANES2016_trump, subset = (Hillary >= 0))
summary(ANES2016_trump_LR_Hilary)$coeff

ANES2016_trump_LR_Age <- glm(Trump ~ Age,"binomial", data = ANES2016_trump, subset = (Age >= 0))
summary(ANES2016_trump_LR_Age)$coeff

ANES2016_trump_LR_Partner <- glm(Trump ~ Partner,"binomial", data = ANES2016_trump, subset = (Partner >= 0))
summary(ANES2016_trump_LR_Partner)$coeff

ANES2016_trump_LR_Education <- glm(Trump ~ Education,"binomial", data = ANES2016_trump, subset = (Education >= 0))
summary(ANES2016_trump_LR_Education)$coeff

ANES2016_trump_LR_SpouseEdu <- glm(Trump ~ SpouseEdu,"binomial", data = ANES2016_trump, subset = (SpouseEdu >= 0))
summary(ANES2016_trump_LR_SpouseEdu)$coeff

ANES2016_trump_LR_Employment <- glm(Trump ~ Employment,"binomial", data = ANES2016_trump, subset = (Employment >= 0))
summary(ANES2016_trump_LR_Employment)$coeff

ANES2016_trump_LR_Birthplace <- glm(Trump ~ Birthplace,"binomial", data = ANES2016_trump, subset = (Birthplace >= 0))
summary(ANES2016_trump_LR_Birthplace)$coeff

ANES2016_trump_LR_GBirth <- glm(Trump ~ GBirth,"binomial", data = ANES2016_trump, subset = (GBirth >= 0))
summary(ANES2016_trump_LR_GBirth)$coeff

ANES2016_trump_LR_Dependent <- glm(Trump ~ Dependent,"binomial", data = ANES2016_trump, subset = (Dependent >= 0))
summary(ANES2016_trump_LR_Dependent)$coeff

ANES2016_trump_LR_Housing <- glm(Trump ~ Housing,"binomial", data = ANES2016_trump, subset = (Housing >= 0))
summary(ANES2016_trump_LR_Housing)$coeff

ANES2016_trump_LR_Income <- glm(Trump ~ Income,"binomial", data = ANES2016_trump, subset = (Income >= 0))
summary(ANES2016_trump_LR_Income)$coeff

ANES2016_trump_LR_Education2 <- glm(Trump ~ Education2,"binomial", data = ANES2016_trump, subset = (Education2 >= 0))
summary(ANES2016_trump_LR_Education2)$coeff

ANES2016_trump_LR_PartyID <- glm(Trump ~ PartyID,"binomial", data = ANES2016_trump)
summary(ANES2016_trump_LR_PartyID)$coeff

ANES2016_trump_LR_Marital <- glm(Trump ~ Marital,"binomial", data = ANES2016_trump, subset = (Marital >= 0))
summary(ANES2016_trump_LR_Marital)$coeff


#task 1 b
#library for best subset selection
install.packages("leaps")
library(leaps)
names(ANES2016_trump)
dim(ANES2016_trump)
sum(is.na(ANES2016_trump$PartyID))    
ANES2016_trump <- na.omit(ANES2016_trump)

ANES2016_trump <- subset(ANES2016_trump[-c(1,7,9)]) 
dim(ANES2016_trump)
str(ANES2016_trump)
?regsubsets
ANES2016_trump_ss=regsubsets (PartyID ∼.,ANES2016_trump ,nvmax=15)
summary(ANES2016_trump_ss)
names(summary(ANES2016_trump_ss))

?plot.regsubsets
plot(summary(ANES2016_trump_ss)$rss ,xlab="Number of Variables ",ylab="RSS",     type="l")
plot(summary(ANES2016_trump_ss)$rsq ,xlab="Number of Variables ",ylab="RSS",     type="l")

#forward approach 
ANES2016_trump_fwd=regsubsets (PartyID∼.,ANES2016_trump ,nvmax=15, method ="forward")
summary(ANES2016_trump_fwd)


#backword approach

ANES2016_trump_bck=regsubsets (PartyID ∼.,ANES2016_trump ,nvmax=15, method ="backward")
summary(ANES2016_trump_bck)
summary(ANES2016_trump_bck)$rss
summary(ANES2016_trump_bck)$rsq
summary(ANES2016_trump_bck)$cp
summary(ANES2016_trump_bck)$adjr2


##testing
AIC(ANES2016_trump_bck, ANES2016_trump_ss)



ANES2016_trump_t <- ANES2016_trump
dim(ANES2016_trump_t)
ANES2016_trump_t$PartyID <- as.numeric(ANES2016_trump_t$PartyID)
ANES2016_trump_bck_t=regsubsets (PartyID ∼.,ANES2016_trump_t ,nvmax=15, method ="backward")
names.vari
summary(ANES2016_trump_multi)
ANES2016_trump_multi <- multinom(. ~., data = ANES2016_trump_t)

summary(ANES2016_trump_bck_t)$rss
summary(ANES2016_trump_bck_t)$rsq
summary(ANES2016_trump_bck_t)$cp


ANES2016_trump_t <- subset(ANES2016_copy, Trump >= 0)
ANES2016_trump <- subset(ANES2016_copy, Trump >= 0)
View(ANES2016_trump)
unique(ANES2016_trump$Trump)
ANES2016_trump_t$Trump <- ifelse(ANES2016_trump_t$Trump < 4,"Liberal","Conservative")

ANES2016_trump_t_s=regsubsets (PartyID ∼.,ANES2016_trump_t ,nvmax=18)
ANES2016_trump_t_f=regsubsets (PartyID ∼.,ANES2016_trump_t ,nvmax=18, method ="forward")
ANES2016_trump_t_b=regsubsets (PartyID ∼.,ANES2016_trump_t ,nvmax=18, method ="backward")

summary(ANES2016_trump_t_s)$rss
summary(ANES2016_trump_t_s)$rsq
summary(ANES2016_trump_t_s)$cp
summary(ANES2016_trump_t_s)$adjr2

summary(ANES2016_trump_t_f)$rss
summary(ANES2016_trump_t_f)$rsq
summary(ANES2016_trump_t_f)$cp
summary(ANES2016_trump_t_f)$adjr2

summary(ANES2016_trump_t_b)$rss
summary(ANES2016_trump_t_b)$rsq
summary(ANES2016_trump_t_b)$cp
summary(ANES2016_trump_t_b)$adjr2





summary(ANES2016_trump_bck_t)$adjr2