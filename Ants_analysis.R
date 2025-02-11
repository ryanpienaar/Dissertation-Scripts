

#### Load packages ####
library(ggplot2) 
library(ggpubr)
library(rcompanion)
library(emmeans)
library(tidyverse)


#### Load data ####
ants <- read.csv("Ant Manuals.csv")
head(ants)
ants <- subset(ants, Burned!= "C") #### Removing external plots from dataset 

ants$Time <- as.POSIXct(ants$Time, format = "%H:%M") # Format time correctly

typeof(ants$Time)

# Is there an effect of time of day on ants emerging from the Mound
plot(ants$Emerging~ants$Time)
cor(ants$Total.act,as.numeric(ants$Time))
# The correlation is not strong

# Show the R squared with the plot
ggplot(data = ants, aes(x=Time, y = Total.act))+
  geom_point()+theme(plot.title = element_text(hjust = 0.5), text = element_text(size=15, family = "serif"))+
  stat_regline_equation(aes(label = after_stat(rr.label)), label.x = , label.y = 90)+
  scale_fill_manual(values = c("dark grey","white"))+
  ylab("Number of Ants Emerging and Returning per minute")+
  theme_classic()+
  theme(legend.position="bottom", aspect.ratio = 1)+
  theme(plot.title    = element_text(size=15, family = "serif", colour = "black"),
        plot.subtitle = element_text(size=15, family = "serif"),
        axis.title.x  = element_text(size=15, family = "serif", colour = "black"),
        axis.title.y  = element_text(size=15, family = "serif", colour = "black"),
        axis.text.x   = element_text(size=15, family = "serif", colour = "black"),
        axis.text.y   = element_text(size=15, family = "serif", colour = "black"),
        legend.text = element_text(size=15, family = "serif", colour = "black"),
        legend.title = element_blank(),
        panel.border = element_blank())
  





#### Total Activity Data ####
Act<- ants[,c(1,3,4,7)]
  


#### Test for normality and transform data ####
hist(Act$Total.act)
hist(transformTukey(Act$Total.act))

  
t_tuk <- transformTukey(Act$Total.act) # Transform using Transform Tukey function in rcompanion
Act$Total.act<- t_tuk  ### The dataframe now contains the transformed data



### Analysis ####


####Total activity by treatment ####
ano1 <-  aov(data=Act, Total.act~Burned+Rodents+Burned*Rodents+ (1|Block))
summary(ano1)
TukeyHSD(ano1)
emmeans(ano1, specs = "Rodents")


boxplot(data=Act, Total.act~Burned+Rodents+Burned*Rodents)  

max(Act$Total.act)

Act$combined <- interaction(Act$Burned,Act$Rodents)

 a<- ggplot(data = Act,aes(x =reorder(Burned, Burned, function(x)length(x)), Total.act, fill = Rodents)) + 
    stat_summary(geom = "bar", fun = mean, position = "dodge", color = "black", width = 0.5, show.legend = FALSE) +
    stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(0.5))+
    ylab("Ants Entering and Exiting the Mound per Minute")+
    xlab(NULL)+
   theme_classic()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
   #      panel.background = element_rect(fill="transparent"))+
   theme(plot.title = element_text(hjust = 0.5), text = element_text(size=15, family = "serif"))+
   scale_fill_manual(values = c("dark grey","white"))+
   theme(legend.position="bottom", aspect.ratio = 1)+
   theme(plot.title    = element_text(size=15, family = "serif", colour = "black"),
         plot.subtitle = element_text(size=15, family = "serif"),
         axis.title.x  = element_text(size=15, family = "serif", colour = "black"),
         axis.title.y  = element_text(size=15, family = "serif", colour = "black"),
         axis.text.x   = element_text(size=15, family = "serif", colour = "black"),
         axis.text.y   = element_text(size=15, family = "serif", colour = "black"),
         legend.text = element_text(size=15, family = "serif", colour = "black"),
         legend.title = element_blank(),
         panel.border = element_blank())+
   scale_y_continuous(expand = c(0, 0), limits = c(0,22))+
   coord_cartesian(ylim=c(0, 17))
#   scale_y_continuous(expand = c(0, 0), limits = c(0,22))

a 


 #### Mound Diameter ####
MD <- ants[,c(1,3,4,9)]
head(MD)    

  # Test for normailty and transform 
hist(MD$Mound.Diameter..cm.)
shapiro.test(MD$Mound.Diameter..cm.)  
shapiro.test((MD$Mound.Diameter..cm.)^2)


#### Not normal therefore use scheirer ray hare 
ano.MD <- scheirerRayHare(data = MD, Mound.Diameter..cm.~Burned+Rodents)
ano.MD
  
#### Include the random block effect
ano.MD <- scheirerRayHare(data = MD, Mound.Diameter..cm.~Burned+Rodents + (1|Block))
ano.MD

### ANOVA Tukey
ano.MD2 <- aov(data = MD, Mound.Diameter..cm. ~ Burned + Rodents + Burned * Rodents)
summary(ano.MD2)
TukeyHSD(ano.MD2)
ano.MD

#### Plot Graph 
label_blabk <- c("","")  

b<-ggplot(data = MD,aes(reorder(Burned,Burned, function(x)length(x)), Mound.Diameter..cm., fill = Rodents)) + 
    stat_summary(geom = "bar", fun = mean, position = "dodge", color = "black", width = 0.5, show.legend = TRUE) +
    stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(0.5))+
    ylab("Mound Diameter (cm)")+
    xlab("")+
    theme_classic()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill="transparent"))+
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size=15))+
    scale_fill_manual(values = c("dark grey","white"))+
    theme(aspect.ratio = 1)+
    scale_x_discrete(labels= label_blabk)+
     theme(plot.title    = element_text(size=15, family = "serif", colour = "black"),
        plot.subtitle = element_text(size=15, family = "serif"),
        axis.title.x  = element_text(size=15, family = "serif", colour = "black"),
        axis.title.y  = element_text(size=15, family = "serif", colour = "black"),
        axis.text.x   = element_text(size=15, family = "serif", colour = "black"),
        axis.text.y   = element_text(size=15, family = "serif", colour = "black"),
        legend.text = element_text(size=15, family = "serif", colour = "black"),
        legend.title = element_blank(),
        panel.border = element_blank())+
  scale_y_continuous(expand = c(0, 0), limits = c(0,197))+
  coord_cartesian(ylim=c(0, 110))+
  labs(fill = "Rodents")+ theme(legend.position = c(.27,.9) )
#         scale_y_continuous(expand = c(0, 0), limits = c(0,197))

  
b

max(MD$Mound.Diameter..cm.)



#### Mound height ####
MH <- ants[,c(1,2,3,4,10)]
head(MH)  

### Test for normailty and transform 
hist(MH$Mound.Height..cm.)
shapiro.test(MH$Mound.Height..cm.)


#### Not normal therefore use scheirer ray hare 
ano.MH <- scheirerRayHare(data = MH, Mound.Height..cm.~Burned+Rodents)
ano.MH

b.mh <- scheirerRayHare(data = MH, Mound.Height..cm. ~ Burned)

### ANOVA because there is sufficient sample size
ano.MH2 <- aov(data = MH, Mound.Height..cm.~Burned+Rodents + Burned *Rodents)
summary(ano.MH2)
TukeyHSD(ano.MH2)
emmeans(ano.MH2, specs = c("Burned", "Rodents"))

un.mh <- MH[MH$Plot=="UN",]
bs.mh <- MH[MH$Plot=="BS",]


### Again with block as a random effect
ano.MH <- scheirerRayHare(data = MH, Mound.Height..cm.~Burned+Rodents + (1|Block))
ano.MH

  
#### Plot Graph 
  
c<-ggplot(data = MH,aes(reorder(Burned, Burned, function(x)length(x)), Mound.Height..cm., fill = Rodents)) + 
  stat_summary(geom = "bar", fun = mean, position = "dodge", color = "black", width = 0.5, show.legend = FALSE) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(0.5))+
  ylab("Mound Height (cm)")+
  xlab(NULL)+
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="transparent"))+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=15))+
  scale_fill_manual(values = c("dark grey","white"))+
 theme(aspect.ratio = 1, legend.position = "bottom")+
  theme(plot.title    = element_text(size=15, family = "serif", colour = "black"),
        plot.subtitle = element_text(size=15, family = "serif"),
        axis.title.x  = element_text(size=15, family = "serif", colour = "black"),
        axis.title.y  = element_text(size=15, family = "serif", colour = "black"),
        axis.text.x   = element_text(size=15, family = "serif", colour = "black"),
        axis.text.y   = element_text(size=15, family = "serif", colour = "black"),
        legend.text = element_text(size=15, family = "serif", colour = "black"),
        legend.title = element_blank(),
        panel.border = element_blank())+
  scale_y_continuous(expand = c(0, 0), limits = c(0,30))+
  coord_cartesian(ylim=c(0, 23))
#  scale_y_continuous(expand = c(0, 0), limits = c(0,30))

max(MH$Mound.Height..cm.)

c



#### Disk Diameter ####
DH <- ants[,c(1,3,4,8)]
head(DH)  

#### Test for normality and transform 
hist(DH$Disk.Diameter..cm.)
shapiro.test(DH$Disk.Diameter..cm.) #p=0.6 Therefore it's normally distributed
  
#### ANova 
anodd<- aov(data = DH, DH$Disk.Diameter..cm.~Burned+Rodents+Burned*Rodents + (1|Block))
summary(anodd)
TukeyHSD(anodd)  


#### Plot Graph 
  max(DH$Disk.Diameter..cm.)
d<-ggplot(data = DH,aes(reorder(Burned, Burned, function(x)length(x)), Disk.Diameter..cm., fill = Rodents)) + 
  stat_summary(geom = "bar", fun = mean, position = "dodge", color = "black", width = 0.5, show.legend = FALSE) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(0.5))+
  ylab("Disk Diameter (cm)")+
  xlab(NULL)+
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="transparent"))+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=15))+
  scale_fill_manual(values = c("dark grey","white"))+
  theme(legend.position="bottom", aspect.ratio = 1)+
  theme(plot.title    = element_text(size=15, family = "serif", colour = "black"),
        plot.subtitle = element_text(size=15, family = "serif"),
        axis.title.x  = element_text(size=15, family = "serif", colour = "black"),
        axis.title.y  = element_text(size=15, family = "serif", colour = "black"),
        axis.text.x   = element_text(size=15, family = "serif", colour = "black"),
        axis.text.y   = element_text(size=15, family = "serif", colour = "black"),
        legend.text = element_text(size=15, family = "serif", colour = "black"),
        legend.title = element_blank(),
        panel.border = element_blank())+
  scale_y_continuous(expand = c(0, 0), limits = c(0,730))+
  coord_cartesian(ylim=c(0, 525))

#  scale_y_continuous(expand = c(0, 0), limits = c(0,730))

d



### Total Disk area per plot #####

#### Calculating area 
DA<- ants
DA$Area= DA$Disk.Diameter..cm.*pi
 
ylab <- expression(Disk ~ area ~ (cm^2)) 

e<-ggplot(data = DA, aes(Burned, Area, fill = Rodents)) + 
  stat_summary(geom = "bar", fun = mean, position = "dodge", color = "black", width = 0.5) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(0.5))+
  ylab(ylab)+
  xlab(element_blank())+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="transparent"))+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=15))+
  scale_fill_manual(values = c("dark grey","white"))+
  theme(legend.position="right", aspect.ratio = 1)
e
  
write.csv(DA,"DA.csv")





#### Density Data ####
density<- read.csv("Density_ants.csv")


#### Test fpr normality and transform 
hist(density$Count)
shapiro.test(density$Count)   #W = 0.90594, p-value = 0.05337

#### Anova 
anodensity <- aov(data = density, Count~Burned+Rodents+Burned*Rodents)
summary(anodensity)    
TukeyHSD(anodensity)

anodensity <- aov(data = density, Count~Burned+Rodents+Burned*Rodents +(1|Plot)) #### the random effect makes no difference
summary(anodensity) 

anostat <- aov(data = density, Count ~ Block)
summary(anostat)
TukeyHSD(anostat)
pairwise.t.test(density$Count, density$Block ,p.adjust.method="bonferroni")

#### Plot Graph 
max(density$Count)

f<-   ggplot(data = density,aes(fct_reorder(Burned, Count), Count, fill = Rodents)) + 
  stat_summary(geom = "bar", fun = mean, position = "dodge", color = "black", width = 0.5, show.legend = FALSE) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(0.5))+
  ylab("Disks per 30mx30m plot")+
  xlab(NULL)+
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="transparent"))+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=15))+
  scale_fill_manual(values = c("dark grey","white"))+
  theme(legend.position = "right", aspect.ratio = 1)+
  scale_x_discrete(labels= label_blabk)+
  theme(plot.title    = element_text(size=15, family = "serif", colour = "black"),
        plot.subtitle = element_text(size=15, family = "serif"),
        axis.title.x  = element_text(size=15, family = "serif", colour = "black"),
        axis.title.y  = element_text(size=15, family = "serif", colour = "black"),
        axis.text.x   = element_text(size=15, family = "serif", colour = "black"),
        axis.text.y   = element_text(size=15, family = "serif", colour = "black"),
        legend.text = element_text(size=15, family = "serif", colour = "black"),
        legend.title = element_blank(),
        panel.border = element_blank())+
  scale_y_continuous(expand = c(0, 0), limits = c(0,15))+
  coord_cartesian(ylim=c(0, 11))
#  scale_y_continuous(expand = c(0, 0), limits = c(0,15))

f  






#### Fix #####

ggplot(data = density,aes(fct_reorder(Burned, Count), Count, fill = Rodents)) + 
  stat_summary(geom = "bar", fun = mean, position = "dodge", color = "black", width = 0.5, show.legend = FALSE) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(0.5))+
  ylab("Disks per 30mx30m plot")+
  xlab(NULL)+
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="transparent"))+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=15))+
  scale_fill_manual(values = c("dark grey","white"))+
  theme(legend.position = "right", aspect.ratio = 1)+
  scale_x_discrete(labels= label_blabk)+
  theme(plot.title    = element_text(size=15, family = "serif", colour = "black"),
        plot.subtitle = element_text(size=15, family = "serif"),
        axis.title.x  = element_text(size=15, family = "serif", colour = "black"),
        axis.title.y  = element_text(size=15, family = "serif", colour = "black"),
        axis.text.x   = element_text(size=15, family = "serif", colour = "black"),
        axis.text.y   = element_text(size=15, family = "serif", colour = "black"),
        legend.text = element_text(size=15, family = "serif", colour = "black"),
        legend.title = element_blank(),
        panel.border = element_blank())+
  scale_y_continuous(expand = c(0, 0), limits = c(0,15))+
  coord_cartesian(ylim=c(0, 11))

 #### plots together ####
ggarrange(a,b,c,d, common.legend = TRUE, legend= "right",labels = c("A","B","C","D") )
ggarrange(a,b,c,d,e,f, common.legend = TRUE, legend= "bottom",labels = c("A","B","C","D","E","F"))
  
ggarrange(b,c,nrow = 2)


 
ggarrange(f,d,ncol = 1)
 
ggarrange(b,f,c,d, ncol=2)

######Actual plots used#####
ggarrange(f,d,ncol = 1)
ggarrange(b,c,nrow = 2)
ggarrange(a,c, nrow = 2)

ggarrange(f,b,d,c, ncol=2)  

###################################

count(ants,Plot)
#Plot  n
#1   BN 44 ## Stot= 39
#2   BS 26 ## Btot= 70
#3   UN 18 ## Ntot= 62
#4   US 13 ## Utot= 31



70/31 # Fire increased mound density by a factor of 2.26
(70-31)/31*100 #155.81% increase
62/39 # Rodents decreased mound density by a factor of 1.59
(62-39)/39*100 #58.97%

26/44*100
13/18*100

#### Total activity ####

mean(ants[ants$Plot=="BN",7])
mean(ants[ants$Plot=="BS",7])
mean(ants[ants$Plot=="UN",7])
mean(ants[ants$Plot=="US",7])

mean(ants[ants$Plot=="BN",7])+mean(ants[ants$Plot=="BS",7])#Burned = 71.19755
mean(ants[ants$Plot=="UN",7])+mean(ants[ants$Plot=="US",7])#Unburned = 73.00855

mean(ants[ants$Plot=="BN",7])+mean(ants[ants$Plot=="UN",7]) #No rodents = 83.93687
mean(ants[ants$Plot=="BS",7])+mean(ants[ants$Plot=="US",7]) #Rodents = 60.26923

(83.93687-60.26923)/60.26923*100 #Excluding rodents increased ant activity by 39.26986%

#### Mound diameter ####

mean(ants[ants$Plot=="BN",9]) #69.61
mean(ants[ants$Plot=="BS",9]) #58.96
mean(ants[ants$Plot=="UN",9]) #75
mean(ants[ants$Plot=="US",9]) #68.92

# Mean burned
(mean(ants[ants$Plot=="BN",9])+mean(ants[ants$Plot=="BS",9]))/2 #64.28759
# Mean unbunrned
(mean(ants[ants$Plot=="UN",9])+mean(ants[ants$Plot=="US",9]))/2 #71.96154
# Mean rodents
(mean(ants[ants$Plot=="BS",9]) + mean(ants[ants$Plot=="US",9]))/2 #63.94231
# Mean rodent exclusion
(mean(ants[ants$Plot=="BN",9])+mean(ants[ants$Plot=="UN",9]))/2 #72.30682

#Percent change
(72.30682-63.94231)/63.94231*100



(75+68.92308)/(69.61364+58.96154) 
(75+69.61364)/(68.92308+58.96154) #1.130813


69.61/58.96
75/68.92

#### Mound height ####

mhbn<- mean(ants[ants$Plot=="BN",10]) #14.82
mhbs<- mean(ants[ants$Plot=="BS",10]) #8.19
mhun<- mean(ants[ants$Plot=="UN",10]) #15.78
mhus<- mean(ants[ants$Plot=="US",10]) #3.31

(mhbn+mhbs)/2 #11.50524
(mhun+mhus)/2 #9.542735
(mhbn+mhun)/2 #15.29798
(mhbs+mhus)/2 #5.75

# Rodent percent change
(15.29798-5.75)/5.75*100 #166.0518


(mhbn+mhbs)/(mhun+mhus)
(mhun+mhbn)/(mhus+mhbs)

14.82/8.19
15.78/3.31

##### Disk diameter ####

mean(ants[ants$Plot=="BN",8]) #290.07
mean(ants[ants$Plot=="BS",8]) #323.15
mean(ants[ants$Plot=="UN",8]) #376.06
mean(ants[ants$Plot=="US",8]) #433.77


#mean burned
(mean(ants[ants$Plot=="BN",8])+mean(ants[ants$Plot=="BS",8]))/2 #306.5918
#mean unburned
(mean(ants[ants$Plot=="UN",8])+mean(ants[ants$Plot=="US",8]))/2 #404.9154

# Burn percent change
(404.9154-306.5918)/306.5918*100 #32.06889


(mean(ants[ants$Plot=="UN",8])+mean(ants[ants$Plot=="US",8])) / (mean(ants[ants$Plot=="BN",8])+mean(ants[ants$Plot=="BS",8])) # 1.320689
(mean(ants[ants$Plot=="BS",8])+mean(ants[ants$Plot=="US",8])) / (mean(ants[ants$Plot=="BN",8])+mean(ants[ants$Plot=="UN",8]))
323.15/290.07
433.77/376.06



#### Plot inactive mounds

g<- ggplot(data = ants, aes(x=Plot, y=Total.act))+
  geom_boxplot()
g

inactive <- ants[ants$Total.act<=0,]
head(inactive)

inactive %>% count(inactive$Plot=="US")

inactivev <- c(4,5,4,4)
inactive

mean(ants$Total.act)



##### New GGPLOT #########
a1 <- ggplot(data = Act,aes(Burned, Total.act, fill = Rodents)) + 
  geom_boxplot()+
  #stat_summary(geom = "point", fun = mean, position = "dodge", color = "black", width = 0.5) +
  #stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(0.5))+
  ylab("Number of Ants")+
  xlab("Fire Treatment")+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="transparent"), plot.margin = margin(1, 1, 1, 1, "cm"))+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=15))+
  scale_fill_manual(values = c("dark grey","white"))+
  theme(legend.position="right", aspect.ratio = 1)

a1

b1<-ggplot(data = MD,aes(Burned, Mound.Diameter..cm., fill = Rodents)) + 
  geom_boxplot()+
  #stat_summary(geom = "bar", fun = mean, position = "dodge", color = "black", width = 0.5) +
  #stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(0.5))+
  ylab("Mound Diameter (cm)")+
  xlab(element_blank())+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="transparent"))+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=15),
        plot.margin = margin(1, 1, 1, 1, "cm"))+
  scale_fill_manual(values = c("dark grey","white"))+
  theme(legend.position="right", aspect.ratio = 1)+
  scale_x_discrete(labels= label_blabk)

b1


library(egg)
ggarrange(a1,b1, nrow = 2)

c1 <-ggplot(data = MH,aes(Burned, Mound.Height..cm., fill = Rodents)) + 
  geom_boxplot()+
  #stat_summary(geom = "bar", fun = mean, position = "dodge", color = "black", width = 0.5) +
  #stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(0.5))+
  ylab("Mound Height (cm)")+
  xlab("Fire Treatment")+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="transparent"))+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=15))+
  scale_fill_manual(values = c("dark grey","white"))+
  theme(legend.position="right",aspect.ratio = 1)

c1

d1 <-ggplot(data = DH,aes(Burned, Disk.Diameter..cm., fill = Rodents)) + 
  geom_boxplot()+
  #stat_summary(geom = "bar", fun = mean, position = "dodge", color = "black", width = 0.5) +
  #stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(0.5))+
  ylab("Disk Diameter (cm)")+
  xlab("Fire Treatment")+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="transparent"))+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=15))+
  scale_fill_manual(values = c("dark grey","white"))+
  theme(legend.position="right", aspect.ratio = 1)

d1

e1 <-ggplot(data = density,aes(Burned, Area, fill = Rodents)) + 
  geom_boxplot()+
  #stat_summary(geom = "bar", fun = mean, position = "dodge", color = "black", width = 0.5) +
  #stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(0.5))+
  ylab(ylab)+
  xlab("Fire Treatment")+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="transparent"))+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=15))+
  scale_fill_manual(values = c("dark grey","white"))+
  theme(legend.position="right", aspect.ratio = 1)
e1

f1 <- ggplot(data = density,aes(Burned, Count, fill = Rodents)) + 
  geom_boxplot()+
  #stat_summary(geom = "bar", fun = mean, position = "dodge", color = "black", width = 0.5) +
  #stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(0.5))+
  ylab("Disks per 30mx30m plot")+
  xlab(element_blank())+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="transparent"))+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=15))+
  scale_fill_manual(values = c("dark grey","white"))+
  theme(legend.position = "right", aspect.ratio = 1)+
  scale_x_discrete(labels= label_blabk)

f1  

library(cowplot)
fig2 <- plot_grid(f1,d1, ncol = 1, labels = "AUTO")
fig2
fig3 <- plot_grid(b1,c1,ncol = 1, labels = "AUTO")
fig3



# Propgaule pressure
boxplot(density$Count~density$Plot)
summary(aov(density$Count~density$Plot))

