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
    #z-transformation 
    BARTtrial$Trial.z <- BARTtrial$Trial/sd(BARTtrial$Trial) # z-transformation of the trials

    #______________
    #Declare missing values as NA
    BARTtrial$RT[BARTtrial$RT == 0]  <- NA 
    BARTtrial$RT[BARTtrial$RT == -9999999]  <- NA 
    
    BARTtrial$SCRc[BARTtrial$SCRc == 0]  <- NA
    BARTtrial$SCRc[BARTtrial$SCRc == -9999999]  <- NA 
    
    BARTtrial$BIS_sum[BARTtrial$BIS_sum == -99]  <- NA 
    BARTtrial$BIS_sum[BARTtrial$BIS_sum == -9999999]  <- NA
    
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

    #1. Null-Modell:  only one random intercept
    mixed_null.1 = lmer(RT ~ 1 + (1|ID),REML = F, data=BARTtrial_neu)
    summary(mixed_null.1)
    
    #2. Null-Modell
    mixed_null.2 = lmer(RT ~ 1 + (1 + Trial.z | ID),REML = F, data=BARTtrial_neu)
    summary(mixed_null.2)
    anova(mixed_null.1, mixed_null.2) 
    
    #3. + BART order
    mixed_null.3 = lmer(RT ~ 1 + (1 + Trial.z|ID) +(1 + Trial.z|ID:BART_order),REML = F, data=BARTtrial_neu)
    summary(mixed_null.3)
    anova(mixed_null.2, mixed_null.3) 
    
    #4 + SCRc
    mixed_null.4 = lmer(RT ~ 1 + (1 + Trial.z*SCRc|ID) +(1 + Trial.z|ID:BART_order),REML = F, data=BARTtrial_neu)
    summary(mixed_null.4)
    anova(mixed_null.2, mixed_null.4) 
        #--> final null model

###FIXED EFFECTS.

#(1) + Context (BART [Loss-BART vs G-BART])
    #1. + BART
    mixed_m1neu = lmer(RT ~ BART + (1 + Trial.z |ID),REML = F, data=BARTtrial_neu)
    mixed_m1 = lmer(RT ~ BART + (1 + Trial.z |ID),REML = F, data=BARTtrial)
    summary(mixed_m1)
    anova(mixed_null.2, mixed_m1)
         
#(2) +condition (outcome magnitude condition [low vs high])
    #2.
    mixed_m2 = lmer(RT ~ BART + condition + (1 + Trial.z |ID),REML = F, data=BARTtrial)
    mixed_m2neu = lmer(RT ~ BART + condition + (1 + Trial.z |ID),REML = F, data=BARTtrial_neu) # needed for comparison with m4neu
    summary(mixed_m2)
    anova(mixed_m1,mixed_m2)

    #3. 
    mixed_m3 = lmer(RT ~ BART * condition + (1 + Trial.z |ID),REML = F, data=BARTtrial)
    summary(mixed_m3)
    anova(mixed_m2,mixed_m3) # -->mixed_m2 (not sig)
       
#(3) + SCR (skin conductance response during the inflation process)
    #4.
    mixed_m4 = lmer(RT ~ BART + condition + SCRc+(1 + Trial.z*SCRc |ID),REML = F, data=BARTtrial) 
    mixed_m4neu = lmer(RT ~ BART + condition + SCRc+(1 + Trial.z*SCRc |ID),REML = F, data=BARTtrial_neu) # needed for comparison with m2neu
    summary(mixed_m4)
    anova(mixed_m4neu,mixed_m2neu)

    #5. 
    mixed_m5 = lmer(RT ~ BART * condition * SCRc + (1 + Trial.z*SCRc |ID),REML = F, data=BARTtrial)
    summary(mixed_m5)
    anova(mixed_m4,mixed_m5) 

    #6. 
    mixed_m6 = lmer(RT ~ BART * SCRc + condition*SCRc +  (1 + Trial.z*SCRc |ID),REML = F, data=BARTtrial)
    summary(mixed_m6)
    anova(mixed_m4,mixed_m6)


#(4) + BART order (Order of whether participants first played G-BART and then L-BART, or the other way around)
    #7
    mixed_m7 = lmer(RT ~ BART + condition + SCRc+ BART_order +(1 + Trial.z*SCRc |ID)+(1 + Trial.z|ID:BART_order),REML = F, data=BARTtrial)
    summary(mixed_m7)
    anova(mixed_m7,mixed_m4)

    #8
    mixed_m8 = lmer(RT ~ BART*BART_order + condition + SCRc+(1 + Trial.z*SCRc |ID)+(1 + Trial.z|ID:BART_order),REML = F, data=BARTtrial)
    mixed_m8neu = lmer(RT ~ BART*BART_order + condition + SCRc+(1 + Trial.z*SCRc |ID),REML = F, data=BARTtrial_neu) # needed for comparison with m12neu
    summary(mixed_m8)
    anova(mixed_m8,mixed_m4) 
    
    #9
    mixed_m9 = lmer(RT ~ BART*BART_order + condition*BART_order + SCRc+(1 + Trial.z*SCRc |ID)+(1 + Trial.z|ID:BART_order),REML = F, data=BARTtrial)
    summary(mixed_m9)
    anova(mixed_m8,mixed_m9)

#(5) +Gender  
    #10
    mixed_m10 = lmer(RT ~ BART*BART_order + condition + SCRc+ Sex +(1 + Trial.z*SCRc |ID)+(1 + Trial.z|ID:BART_order),REML = F, data=BARTtrial)
    summary(mixed_m10)
    anova(mixed_m8,mixed_m10)      
    
    #11
    mixed_m11 = lmer(RT ~ BART*BART_order + BART*Sex + condition*Sex + SCRc + (1 + Trial.z*SCRc |ID)+(1 + Trial.z|ID:BART_order),REML = F, data=BARTtrial)
    summary(mixed_m11)
    anova(mixed_m8,mixed_m11)

    #--> as gender had no sig. additional effect over and above mixed_m8 the variable was not further included. 


###questionnaires

#(6) + BIS 11 (The Barratt Impulsiveness Scale)
    #12
    mixed_m12 = lmer(RT ~ BART*BART_order + BIS_sum + condition + SCRc +(1 + Trial.z*SCRc |ID)+(1 + Trial.z|ID:BART_order),REML = F, data=BARTtrial)
    mixed_m12neu = lmer(RT ~ BART*BART_order + BIS_sum + condition + SCRc+(1 + Trial.z*SCRc |ID),REML = F, data=BARTtrial_neu) # needed for comparison with m8neu
    summary(mixed_m12)
    anova(mixed_m8neu, mixed_m12neu) 
    anova(mixed_m8, mixed_m12) 

    #13
    mixed_m13 = lmer(RT ~ BART*BART_order + BART*BIS_sum + condition + SCRc +(1 + Trial.z*SCRc |ID)+(1 + Trial.z|ID:BART_order),REML = F, data=BARTtrial)
    mixed_m13neu = lmer(RT ~ BART*BART_order + BART*BIS_sum + condition + SCRc+(1 + Trial.z*SCRc |ID),REML = F, data=BARTtrial_neu)
    summary(mixed_m13neu)
    summary(mixed_m13)
    anova(mixed_m8neu, mixed_m13neu) 
    anova(mixed_m8, mixed_m13)

    #14
    mixed_m14 = lmer(RT ~ BART*BART_order + BIS_sum * condition + SCRc +(1 + Trial.z*SCRc |ID)+(1 + Trial.z|ID:BART_order),REML = F, data=BARTtrial)
    summary(mixed_m14)
    anova(mixed_m13, mixed_m14) 


#(7) SPSRQ (SP and SR sub scales at once)
    ##15
    mixed_m15 = lmer(RT ~ BART*BART_order + BART*BIS_sum  + SPSRQp + SPSRQr + condition + SCRc +(1 + Trial.z*SCRc |ID)+(1 + Trial.z|ID:BART_order),REML = F, data=BARTtrial)
    summary(mixed_m15)
    anova(mixed_m13, mixed_m15)

    #16
    mixed_m16 = lmer(RT ~ BART*BART_order + BART*BIS_sum  + BART*SPSRQp + BART*SPSRQr + condition + SCRc +(1 + Trial.z*SCRc |ID)+(1 + Trial.z|ID:BART_order),REML = F, data=BARTtrial)
    summary(mixed_m16)
    anova(mixed_m13, mixed_m16)
    
    #17
    mixed_m17 = lmer(RT ~ BART*BART_order + BART*BIS_sum  + BART *SPSRQp * SPSRQr + condition + SCRc +(1 + Trial.z*SCRc |ID)+(1 + Trial.z|ID:BART_order),REML = F, data=BARTtrial)
    summary(mixed_m17)
    anova(mixed_m16, mixed_m17)


#(8) SPSRQ (SP andS SR sub scales one after the other)
  #(8a) +SPSRQ reward (SR)
    #18
    mixed_m18 = lmer(RT ~ BART*BART_order + BART*BIS_sum  + SPSRQr + condition + SCRc +(1 + Trial.z*SCRc |ID)+(1 + Trial.z|ID:BART_order),REML = F, data=BARTtrial)
    summary(mixed_m18)
    anova(mixed_m13, mixed_m18) 
    
    #19
    mixed_m19 = lmer(RT ~ BART*BART_order + BART*BIS_sum + BART*SPSRQr + condition + SCRc +(1 + Trial.z*SCRc |ID)+(1 + Trial.z|ID:BART_order),REML = F, data=BARTtrial)
    summary(mixed_m19)
    anova(mixed_m13, mixed_m19)

    #20
    mixed_m20 = lmer(RT ~ BART*BART_order + BART*BIS_sum  * SPSRQr + condition + SCRc +(1 + Trial.z*SCRc |ID)+(1 + Trial.z|ID:BART_order),REML = F, data=BARTtrial)
    summary(mixed_m17)
    anova(mixed_m19, mixed_m20)

  #(8b) SPSRQ punishment (SP)

    #21
    mixed_m21 = lmer(RT ~ BART*BART_order + BART*BIS_sum  * SPSRQr + condition + SCRc + SPSRQp  +(1 + Trial.z*SCRc |ID)+(1 + Trial.z|ID:BART_order),REML = F, data=BARTtrial)
    summary(mixed_m21)
    anova(mixed_m20, mixed_m21)
    
    #22
    mixed_m22 = lmer(RT ~ BART*BART_order + BART*BIS_sum  * SPSRQr + condition + SCRc + SPSRQp*BART +(1 + Trial.z*SCRc |ID)+(1 + Trial.z|ID:BART_order),REML = F, data=BARTtrial)
    summary(mixed_m22)
    anova(mixed_m20, mixed_m22) 
    
    #23
    mixed_m23 = lmer(RT ~ BART*BART_order + BART*BIS_sum  * SPSRQr+ BART*BIS_sum  *SPSRQp + condition + SCRc  +(1 + Trial.z*SCRc |ID)+(1 + Trial.z|ID:BART_order),REML = F, data=BARTtrial)
    summary(mixed_m23)
    anova(mixed_m20, mixed_m23)
    

anova(mixed_m17, mixed_m23)
    #--> Final model:
    mixed_m17 = lmer(RT ~ BART*BART_order + BART*BIS_sum  + BART *SPSRQp * SPSRQr + condition + SCRc +(1 + Trial.z*SCRc |ID)+(1 + Trial.z|ID:BART_order),REML = F, data=BARTtrial)
