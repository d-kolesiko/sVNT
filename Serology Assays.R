
###library
library("PerformanceAnalytics")
library(ggplot2)
library("GGally")
library(corrplot)
library(heplots) 
library(Hmisc)
library(devtools)

###check data
Fig <- read.csv('Fig3.csv')
str(Fig)
apply(Fig, 2, shapiro.test)

###Correlation 1 
pairs(Fig)
chart.Correlation(Fig, histogram=TRUE, pch=19)
chart.Correlation(Fig, histogram=TRUE, pch=19, method = 'spearman')

###Correlation 2 
Fig_3c <- Fig[,c(2,3,6,7)]
lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue") +
    geom_smooth(method = method, color = "red", ...)
  p
}

colnames(Fig_3c) <- make.names(c('VNT', 'sVNT', 'ELISA-RBD', 'ELISA-NP'))
gg1 <- ggpairs(
  Fig_3c, labeller = label_wrap_gen(5), columnLabels = gsub('.', ' ', colnames(Fig_3c), fixed = T), lower = list(continuous = wrap(lowerFn, method = 'lm')),
  diag = list(continuous = wrap("barDiag", colour = "blue", size = 1)),
  upper = list(continuous = wrap("cor", method = "spearman", size = 8, color = 'black'))
)
gg1 + theme(text = element_text(size = 15, lineheight = 1))


###ANOVA
library(heplots) 
model.aov <- aov(data = Fig, formula = VNT ~ sVNT_Inh50 + sVNT_dilution20 + sVNT_dilution40 + ELISA_RBD + ELISA_NP)
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

