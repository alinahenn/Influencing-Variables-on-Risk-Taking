#Script: trial by trial analysis
#Paper: "Context Effects, Skin Conductance Responses and Personality Traits - Influencing Variables on Risk-Taking within a Modified Version of the Balloon Analog Risk Task."
#Status: 08.09.2021


#______________
#INSTALL LIBARIES
# install.packages('lme4')
# install.packages('interactions')
# install.packages('haven')
# install.packages('lmerTest')
# install.packages('texreg')
# install.packages('sjstats')
# install.packages('Matrix')
# install.packages('car')
# install.packages('ggplot2')
# install.packages('HLMdiag')
# install.packages('DHARMa')
# install.packages('lattice')
# install.packages('effects')
# install.packages('robustlmm')
# install.packages('sjPlot')
# install.packages('MuMIn')
# install.packages('Tidyr')
# install.packages('readxl')

#______________
#LIBARIES NEEDED
library(lme4) 
library(interactions)
library(haven)
library(HLMdiag)
library(emmeans)
library(lmerTest)
library(texreg)
library(sjstats)
library(carData)
library(readxl)
library(ggplot2)
library(DHARMa)
library(lattice)
library(Matrix)
library(effects)
library(robustlmm)
library(sjPlot)
library(glmmTMB)
library(sjPlot)
library (MuMIn)
library(tidyr)
library(gridExtra)
library(parameters)
library(gcookbook)
library(performance)
library(sjmisc)
library(sjlabelled)
library(bmlm) #perform within- and between-subject centering of variables in R. 
library(dplyr) #perform within- and between-subject centering of variables in R. 


#______________
BARTtrial <- read_excel("D:/PhD_ATHenn/Pilot_Entscheidungsfindung/TrialBTrial_BART.xlsx") # note: path must be updated!

#______________
#VIEW DATA
View(BARTtrial)
View(BARTtrial_neu) #only used to compare some models; here all missing values are deleted so that variables to be compared have the same number of items 

#______________
#CHECK STRUCTURE OF IMPORTED DATA
str(BARTtrial)
str(BARTtrial_neu)

#______________
#################################################################################
# PREPARATIONS
    
    #______________
    #RECODE VARIABLES into factors, numeric, integer and rename new variables-> important for mixed model!
        BARTtrial$ID <- factor(BARTtrial$ID) 
        BARTtrial$BART_order <- factor(BARTtrial$BART_order) # (1) G-BART->L-BART; (2) L-BART->G-BART
        BARTtrial$BART <- factor(BARTtrial$BART) 
        BARTtrial$condition <- factor(BARTtrial$Reward) # low outcome magnitude condition (0) vs high outcome magnitude condition (1)
        BARTtrial$Feedback <- factor(BARTtrial$Outcome) # burst balloon (0) vs whole balloon (1)
        BARTtrial$Trial <- as.integer(BARTtrial$Trial) # trial 1-60 
        BARTtrial$RT <- as.numeric (BARTtrial$RT) # latency time: Time the participant has waited until he / she has interrupted the inflation process
        BARTtrial$SCRc <- as.numeric (BARTtrial$SCR_reward) # skin conductance response during the inflation process
        BARTtrial$Sex <- factor(BARTtrial$Sex) # Gender of participants (female / male)

    #______________
    #Declare missing values as NA
        BARTtrial$RT[BARTtrial$RT == 0]  <- NA 
        BARTtrial$RT[BARTtrial$RT == -9999999]  <- NA 

        BARTtrial$SCRc[BARTtrial$SCRc == 0]  <- NA
        BARTtrial$SCRc[BARTtrial$SCRc == -9999999]  <- NA 

        BARTtrial$BIS_sum[BARTtrial$BIS_sum == -99]  <- NA 
        BARTtrial$BIS_sum[BARTtrial$BIS_sum == -9999999]  <- NA

    #______________
    #z-transformation 
       BARTtrial$Trial.z <- BARTtrial$Trial/sd(BARTtrial$Trial) # z-transformation of the trials

    #______________
    #Zentieren:
    BIS_sum.c <- scale(BARTtrial$BIS_sum, center = TRUE, scale = FALSE)
    SPSRQr.c <- scale(BARTtrial$SPSRQr, center = TRUE, scale = FALSE)
    SPSRQp.c <- scale(BARTtrial$SPSRQp, center = TRUE, scale = FALSE)    
    
    #______________
    #Delete NA / NaN from dataset --> for model comparisons when datasets do not match.
    BARTtrial_neu =na.omit(BARTtrial) 
    BARTtrial_neu
    anyNA(BARTtrial_neu) # check whether the data set still contains NA / NaN: False -> no more in it, True -> still contain them


#______________
#################################################################################
#FINAL MODEL + ROBUST + POST-HOC TEST

    #______________
    # FINAL MODEL
    mixed_m17 = lmer(RT ~ BART*BART_order + BART*BIS_sum  + BART *SPSRQp * SPSRQr + condition + SCRc +(1 + Trial.z*SCRc |ID)+(1 + Trial.z|ID:BART_order),REML = F, data=BARTtrial)
    summary(mixed_m17)
                               
    #______________
    #ROBUST MODEL
    robust_m1 <- rlmer(RT ~ BART*BART_order + BART*BIS_sum  + BART *SPSRQp * SPSRQr + condtion + SCRc +(1 + Trial.z*SCRc |ID)+(1 + Trial.z|ID:BART_order), data=BARTtrial, method = 'DASvar')
    summary(robust_m1)

    #______________
    #POST HOC TESTS     

    #BIS x BART:
    emtrends(mixed_m17, pairwise ~ BART, var = "BIS_sum")
    probe_interaction(mixed_m17, pred = BIS_sum, modx = BART, alpha = .05, conf.level = 0.95)   #-> in posthoc test self-reported SPSRQr no sig influence; descriptive VPs more risk averse the higher self-reported SPSRQp; higher slope with gain

    #SPSRQr x BART:
    emtrends(mixed_m17, pairwise ~ BART, var = "SPSRQr")
    probe_interaction(mixed_m17, pred = SPSRQr, modx = BART, alpha = .05)

    #SPSRQp x BART:
    emtrends(mixed_m17, pairwise ~ BART, var = "SPSRQp")
    probe_interaction(mixed_m17, pred = SPSRQp, modx = BART, alpha = .05)

    #BART order x BART:
    emmeans(mixed_m17, pairwise ~ BART_order|BART,  adjust = "tukey") 
             
    #SPSRQp x SPSRQr x BART:
    probe_interaction (mixed_m17, pred = SPSRQr, modx = SPSRQp, mod2 = BART, alpha = .05, plot.points = F) #
    probe_interaction (mixed_m17, pred = SPSRQp, modx = SPSRQr, mod2 = BART, alpha = .05, plot.points = F, interval = TRUE, int.type = "confidence", int.width = .8)

    #______________
    #Plot effects
    f <- allEffects(mixed_m17)
    print(f)
    plot(f)

    #______________
    #R^2
    r.squaredGLMM(mixed_m17) 
             
    #______________
    #means and standard deviations
    
    #BART
    with(BARTtrial, tapply(RT, list(BART), mean, na.rm=TRUE)) #mean for risk as a function of group
    with(BARTtrial, tapply(RT, list(BART), sd, na.rm=TRUE)) # standard deviation for risk as a function of group 
    
    #Condition
    with(BARTtrial, tapply(RT, list(condition), mean, na.rm=TRUE)) #mean for risk as a function of group
    with(BARTtrial, tapply(RT, list(condition), sd, na.rm=TRUE)) # standard deviation for risk as a function of group 
    
    #BART order
    with(BARTtrial, tapply(RT, list(BART, condition), mean, na.rm=TRUE)) #mean for risk as a function of group
    with(BARTtrial, tapply(RT, list(BART, condition), sd, na.rm=TRUE)) # standard deviation for risk as a function of group 


    #______________
    #Assumtions
    qqnorm(residuals(mixed_m17)) # Normality of residuals
    qqmath(mixed_m17)
    plot_model(mixed_m17, type='diag') ##problem with heterosc? --> use robust errors: library(robustlmm) needed
    
    qqnorm(residuals(mixed_m17)) # Normality of residuals
    qqmath(mixed_m17)
    plot_model(mixed_m17, type='diag') ##problem with heterosc? --> use robust errors: library(robustlmm) needed

#______________
#################################################################################
#TESTING OF MODELS: detailed representation
    
###NULL MODELS

    nullmodel1 <- lmer( RT ~ 1 + (1|ID), data = BARTtrial_neu, REML=FALSE)
    nullmodel2 <- lmer( RT ~ 1 + (1|ID) +(1|BART), data = BARTtrial_neu, REML=FALSE)
    nullmodel3 <- lmer( RT ~ 1 + (1|ID) +(1|Trial.z), data = BARTtrial_neu, REML=FALSE)
    nullmodel4 <- lmer( RT ~ 1 + (1+Trial.z|ID), data = BARTtrial_neu, REML=FALSE)
    nullmodel5 <- lmer( RT ~ 1 + (1+Trial.z|ID)+(1|BART), data = BARTtrial_neu, REML=FALSE)
    nullmodel6 <- lmer( RT ~ 1 + (1+Trial.z|ID)+(1+Trial.z|BART), data = BARTtrial_neu, REML=FALSE)
    nullmodel7 <- lmer( RT ~ 1 + (1+Trial.z*SCRc|ID), data = BARTtrial_neu, REML=FALSE)
    nullmodel8 <- lmer( RT ~ 1 + (1+Trial.z*SCRc|ID)+(1|BART), data = BARTtrial_neu, REML=FALSE)
    nullmodel9 <- lmer( RT ~ 1 + (1+Trial.z*SCRc|ID)+(1+Trial.z|BART), data = BARTtrial_neu, REML=FALSE)
    nullmodel10 <- lmer( RT ~ 1 + (1+Trial.z+SCRc.z|ID), data = BARTtrial_neu, REML=FALSE)
    nullmodel11 <- lmer( RT ~ 1 + (1+Trial.z+SCRc|ID)+(1|BART), data = BARTtrial_neu, REML=FALSE)
    nullmodel12 <- lmer( RT ~ 1 + (1+Trial.z+SCRc|ID)+(1+Trial.z|BART) , data = BARTtrial_neu, REML=FALSE)

    anova(nullmodel1, nullmodel2, nullmodel3, nullmodel4,nullmodel5, nullmodel6,nullmodel7,nullmodel8,nullmodel9,nullmodel10,nullmodel11, nullmodel12) 
    #Final nullmodel: nullmodel11
    # Results: see supplements; Figure S1.


###FIXED EFFECTS.
mixed_a1 = lmer(RT ~ BART + BART_order +  BIS_sum  + SPSRQp + SPSRQr + condition + SCRc + Trial.z + Sex + Age+ 
                  (1+ Trial.z+SCRc|ID)+(1 |BART), data = BARTtrial_neu, REML=FALSE)
  drop1(mixed_a1,test="Chisq")


mixed_a2 = lmer(RT ~ BART*BART_order +  BIS_sum  + SPSRQp  + SPSRQr + condition + SCRc + Trial.z + Sex + 
                  (1+ Trial.z+SCRc|ID)+(1 |BART), data = BARTtrial_neu, REML=FALSE)
  drop1(mixed_a2,test="Chisq")


mixed_a3 = lmer(RT ~ BART*BART_order +  BART*BIS_sum  + SPSRQp  + SPSRQr + condition + SCRc + Trial.z + Sex+
                  (1+ Trial.z+SCRc|ID)+(1 |BART), data = BARTtrial_neu, REML=FALSE)
  drop1(mixed_a3,test="Chisq")


mixed_a4 = lmer(RT ~ BART*BART_order +  BIS_sum  + BART* SPSRQp  + SPSRQr + condition + SCRc + Trial.z + Sex+Age+ 
                  (1+ Trial.z+SCRc|ID)+(1 | BART), data = BARTtrial_neu, REML=FALSE)
  drop1(mixed_a4,test="Chisq")


mixed_a5 = lmer(RT ~ BART*BART_order +  BIS_sum  + SPSRQp  + BART* SPSRQr + condition + SCRc + Trial.z + Sex + Age+ 
                  (1+ Trial.z+SCRc|ID)+(1 |BART), data = BARTtrial_neu, REML=FALSE)
  drop1(mixed_a5,test="Chisq")


mixed_a6 = lmer(RT ~ BART*BART_order +  BIS_sum  + SPSRQp  + SPSRQr + condition*BART + SCRc + Trial.z + Sex + Age+ 
                  (1+ Trial.z+SCRc|ID)+(1 |BART), data = BARTtrial_neu, REML=FALSE)
  drop1(mixed_a6,test="Chisq")


mixed_a7 = lmer(RT ~ BART*BART_order +  BIS_sum  + SPSRQp  +  SPSRQr + condition + BART* SCRc + Trial.z + Sex + Age+ 
                  (1+ Trial.z+SCRc|ID)+(1 |BART), data = BARTtrial_neu, REML=FALSE)
  drop1(mixed_a7,test="Chisq")


mixed_a8 = lmer(RT ~ BART*BART_order +  BIS_sum  + SPSRQp  +  SPSRQr + condition + SCRc + BART*Trial.z + Sex + Age+ 
                  (1+ Trial.z+SCRc|ID)+(1|BART), data = BARTtrial_neu, REML=FALSE)
  drop1(mixed_a8,test="Chisq")


mixed_a9 = lmer(RT ~ BART*BART_order +  BIS_sum  + SPSRQp  +  SPSRQr + condition + SCRc + Trial.z + BART*Sex+ Age+  
                  (1+ Trial.z+SCRc|ID)+(1 |BART), data = BARTtrial_neu, REML=FALSE)
  drop1(mixed_a9,test="Chisq")


mixed_a10 = lmer(RT ~ BART * BART_order +  BIS_sum  + SPSRQp + SPSRQr + condition + SCRc + Trial.z + Sex + Age*BART+  
                  (1+ Trial.z+SCRc|ID)+(1 |BART), data = BARTtrial_neu, REML=FALSE)
  drop1(mixed_a10,test="Chisq")


mixed_a11 = lmer(RT ~ BART*BART_order +  BART*BIS_sum  + BART *SPSRQp * SPSRQr + condition + SCRc + Trial.z + Sex + Age+  
                  (1+ Trial.z+SCRc|ID)+(1 |BART), data = BARTtrial_neu, REML=FALSE)
  drop1(mixed_a11,test="Chisq")


mixed_a12 = lmer(RT ~ BART*BART_order +  BART*BIS_sum  + BART *SPSRQp + BART* SPSRQr + condition + SCRc + Trial.z + Sex + Age+
                 (1+ Trial.z+SCRc|ID)+(1 |BART), data = BARTtrial_neu, REML=FALSE)
  drop1(mixed_a12,test="Chisq")


mixed_a13 = lmer(RT ~ BART*BART_order +  BART*BIS_sum  + BART *SPSRQp + BART* SPSRQr + BART*condition + SCRc + Trial.z + Sex +Age+
                 (1+ Trial.z+SCRc|ID)+(1 |BART), data = BARTtrial_neu, REML=FALSE)
  drop1(mixed_a13,test="Chisq")


mixed_a14 = lmer(RT ~ BART*BART_order +  BART*BIS_sum  + BART *SPSRQp + BART* SPSRQr + BART*condition + SCRc*BART + Trial.z + Sex + Age+ 
                 (1+ Trial.z+SCRc|ID)+(1 |BART), data = BARTtrial_neu, REML=FALSE)
  drop1(mixed_a14,test="Chisq")


mixed_a15 = lmer(RT ~ BART*BART_order +  BART*BIS_sum  + BART *SPSRQp + BART* SPSRQr + BART*condition + SCRc*BART + Trial.z*BART + Sex + Age+
                 (1+ Trial.z+SCRc|ID)+(1 |BART), data = BARTtrial_neu, REML=FALSE)
  drop1(mixed_a15,test="Chisq")


mixed_a16 = lmer(RT ~ BART*BART_order +  BART*BIS_sum  + BART *SPSRQp + BART* SPSRQr + BART*condition + SCRc*BART + Trial.z*BART + Sex*BART +Age+  
                 (1+ Trial.z+SCRc|ID)+(1 |BART), data = BARTtrial_neu, REML=FALSE)
  drop1(mixed_a16,test="Chisq")


mixed_a17 = lmer(RT ~ BART*BART_order +  BART*BIS_sum  + BART *SPSRQp + BART* SPSRQr + BART*condition + SCRc*BART +Trial.z*BART + Sex*BART +Age*BART+
                 (1+ Trial.z+SCRc|ID)+(1 |BART), data = BARTtrial_neu, REML=FALSE)
  drop1(mixed_a17,test="Chisq")


mixed_a18 = lmer(RT ~ BART + BART_order +  BIS_sum  + SPSRQp + SPSRQr + condition *Trial.z + SCRc + Sex + Age+
                 (1+ Trial.z+SCRc|ID)+(1 |BART), data = BARTtrial_neu, REML=FALSE)
  drop1(mixed_a18,test="Chisq")


mixed_a19 = lmer(RT ~ BART + BART_order +  BIS_sum  + SPSRQp + SPSRQr + condition + Trial.z * SCRc + Sex + Age+
                 (1+ Trial.z+SCRc|ID)+(1 |BART), data = BARTtrial_neu, REML=FALSE)
  drop1(mixed_a19,test="Chisq")


anova(mixed_a1,mixed_a2,mixed_a3,mixed_a4,mixed_a5,mixed_a6,mixed_a7,mixed_a8, mixed_a9,mixed_a10,
      mixed_a11,mixed_a12,mixed_a13,mixed_a14,mixed_a15, mixed_a16, mixed_a17,mixed_a18,mixed_a19)

##DROP
drop1(mixed_a11,test="Chisq")

mixed_a11drop = lmer(RT ~ BART*BART_order +  BART*BIS_sum  + BART *SPSRQp * SPSRQr + condition + SCRc + Trial.z  +
                       (1+ Trial.z+SCRc|ID)+(1 + Trial.z|BART_order), data = BARTtrial, REML=FALSE )

  
#MODELL nach drop
mixed_a11drop = lmer(RT ~ BART*BART_order +  BART*BIS_sum  + BART *SPSRQp * SPSRQr + condition + SCRc + Trial.z +(1+ Trial.z+SCRc|ID)+(1 |BART), data = BARTtrial, REML=FALSE )
summary(mixed_a11drop)


