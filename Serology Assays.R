
###library
install.packages('heplots')
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
library(corrplot)
library(ggplot2)

###check data
Gam_53 <- read.csv('Table_Gam_53.csv')
str(Gam_53)
apply(Gam_53, 2, shapiro.test)

###Correlation
pairs(Gam_53)
chart.Correlation(Gam_53, histogram=TRUE, pch=19)
chart.Correlation(Gam_53, histogram=TRUE, pch=19, method = 'spearman')

###ANOVA
library(heplots) 
model.aov <- aov(data = Gam_53, formula = VNT ~ sVNT_Inh50 + sVNT_dilution20 + sVNT_dilution40 + ELISA_RBD + ELISA_NP)
summary(model.aov)

###Plots
Gam_groups <- read.csv('GAM_groups.csv')
str(Gam_groups)

#1
plot1 <- ggplot(Gam_groups, aes(x=Range, y=Inh50), cex.lab = 1.5, cex.axis = 1.5) 
plot1 + geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center', dotsize = 0.75)

#2
plot2 <- ggplot(Gam_groups, aes(x=Range, y=Inh50)) + geom_dotplot(binaxis='y', stackdir='center')
plot2 + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                            geom="pointrange", color="red")
plot2 + geom_boxplot()

#3
plot3 <- ggplot(Gam_groups, aes(x=Range, y=Inh50)) + geom_dotplot(binaxis='y', stackdir='center', dotsize = 0.75) + theme(text = element_text(size=15))
plot3 + stat_summary(fun.data="mean_sdl", fun.args = list(mult=1), geom="crossbar", width=1) + labs(x="Группы", y = 'Dil50%, points', tag = ) 

