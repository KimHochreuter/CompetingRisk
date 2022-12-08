#
#
#
#
#    
#    TODO: MAKE GITHUB PAGE FOR THIS AND CLEAN SCRIPT UP
#    TODO: CALCULATE POWER?
#
#
#
#





library(tidyverse)

#setwd('C:/')
data = read.csv('GBM_Clinical_Data.csv')

data = data %>% filter()
na.omit(data$Prog1_Time)


library(survival)


data$Prog1_type = factor(data$Prog1_type, levels = c('no progression', 'local-only', 'non-local', 'combined'), ordered = T)
levels(data$Prog1_type)
data$Prog1_Time[which(is.na(data$Prog1_Time) & data$Prog1_type == 'no progression')] = max(data$Prog1_Time, na.rm = T)

class(data)

data$prog_ind = recode(data$Prog1_type, `no progression` = 0, `local-only` = 1, `non-local` = 2, combined = 3)

df2 = data.frame(time = data$Prog1_Time[which(data$Prog1_Time != 'NA')], status = data$Prog1_type[which(data$Prog1_Time != 'NA')], x = data$MRIPostopResult[which(data$Prog1_Time != 'NA')])
df2 = na.omit(df2)
df2$status_int = recode(df2$status, `no progression` = 0, `local-only` = 1, `non-local` = 2, combined = 3)
df2$x_simple = recode(df2$x, `no residual tumour` = 'a', `residual tumour <10mm` = 'b', `residual tumour >=10x10mm` = 'c')

fit = survfit(Surv(time, status) ~ x, data = df2)

library(cmprsk)

fit0 = cmprsk::cuminc(ftime = df2$time, fstatus =  df2$status_int, cencode = 0, group = factor(df2$x_simple))

p0 = ggcompetingrisks(fit = fit0, multiple_panels = T, conf.int = T) + ylim(0,1)

##################################################################################
df2$local_status = 0
df2$local_status[df2$status_int == 1] = 1

fit1 = cmprsk::cuminc(ftime = df2$time, fstatus =  df2$local_status, cencode = 0, group = factor(df2$x_simple))
p1 = ggcompetingrisks(fit1, multiple_panels = T, conf.int = T) + ylim(0,1)

df2$distant_status = 0
df2$distant_status[df2$status_int == 2] = 1 
fit2 = cmprsk::cuminc(ftime = df2$time, fstatus =  df2$distant_status, cencode = 0, group = factor(df2$x_simple))
p2 = ggcompetingrisks(fit2, multiple_panels = T, conf.int = T) + ylim(0,1)


ggarrange(p0,p1,p2, ncol = 1, nrow = 3)

fit0$Tests

