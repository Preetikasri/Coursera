#Basic Inferential Data Analysis
#Statistical Inference Course-Coursera
#by Preetika Srivastava

#Data is about Guinea pig's teeth growth given two features a) supplement type(Orange juice, ascorbic acid), b) Doses (0.5,1,2)
#Problem: Data has to be analysed to figure out how the two features are affecting the response variable, i.e. Teeth Growth


data("ToothGrowth")
head(ToothGrowth)
View(ToothGrowth)
summary(ToothGrowth)
unique(ToothGrowth$supp)
unique(ToothGrowth$dose)

ToothGrowth$dose = as.factor(ToothGrowth$dose)

# Box plot analysis for Length vs Dose amount across two supplement types

library(ggplot2)
ggplot(aes(x=dose, y=len), data=ToothGrowth) + geom_boxplot(aes(fill=dose)) + xlab("Dose") + ylab("Tooth Length") + facet_grid(~ supp) + ggtitle("Tooth Length vs. Dose Amount \nby Delivery Method") + 
+     theme(plot.title = element_text(lineheight=.8, face="bold"))

#Fig 2_1 for the output of the above plot

#length vs supplement delivery method
ggplot(aes(x=supp, y=len), data=ToothGrowth) + geom_boxplot(aes(fill=supp)) + xlab("supplement Delivery") + ylab("Tooth Length") + facet_grid(~ dose) + ggtitle("Tooth Length vs. Delivery Method  \nby Dose Amount") + 
+     theme(plot.title = element_text(lineheight=.8, face="bold"))

#Fig 2_2 for the output of the above plot

#Comparing teeth growth using t-test
t.test(len~supp,data=ToothGrowth)

#Comparing teeth growth with dose values
ToothGrowth_sub_1N2 = subset(ToothGrowth, ToothGrowth$dose %in% c(1.0,2.0))
t.test(len~dose,data=ToothGrowth_sub_1N2)

ToothGrowth_sub_1N5 = subset(ToothGrowth, ToothGrowth$dose %in% c(1.0,0.5))
t.test(len~dose,data=ToothGrowth_sub_1N5)

ToothGrowth_sub_2N5 = subset(ToothGrowth, ToothGrowth$dose %in% c(0.5,2.0)
t.test(len~dose,data=ToothGrowth_sub_2N5)

#Based on welch two sample t-test, conclusions can be made

