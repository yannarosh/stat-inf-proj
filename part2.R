require(ggplot2)


my_data <- datasets::ToothGrowth

# Provide basic summary of the data

str(my_data)

summary(my_data)



# Plot the odontoblast length by supplement type

g <- ggplot(my_data, aes(x = supp, y = len))
g + geom_boxplot(aes(group = supp), fill = "salmon") + 
        xlab("supplement type") + 
        ylab("odontoblast length") + 
        ggtitle("Odontoblast length by supplement type")


# We want to investigate if there is a significant difference in odontoblast 
# length between the 2 delivery methods, orange juice and ascorbic acid.

# We will perform a two-sided test using the paired t-test 

#H_0: there is no difference in growth between the delivery methods
#H_1: there is a difference

t.test(len ~ supp, data = ToothGrowth, paired = FALSE)

# the p-value of the t-test is 0.06 > 0.05, so we retain the null hypothesis
# at the 95%
# level, i.e. we conclude that odontoblast growth is not significantly different 
# between the two delivery methods, with 95% confidence. We observe that 
# the confidence interval (-0.17, 7.57) contains 0, which is enough evidence to
# retain the null.

# Now investigate the effect of different vitamin C dosages.

p <- ggplot(my_data, aes(x = as.factor(dose), y = len))
p + geom_boxplot(aes(group = dose), fill = "steel blue") + 
        xlab("dose (mg. per day)") + 
        ylab("odontoblast length") + 
        ggtitle("Odontoblast length by vitamic C dose")

# It looks like higher dosages of vitamic C are associated with longer odontoblasts. 
# Let's test that hypothesis for dose levels 0.5 and 1 mg/day

# Again we use the paired t-test, this time for a one-sided test

#H_0: odontoblast is the same for vitamin C dose of 0.5 mg/day and 1 mg/day
#H_1: odontoblasts are longer for dose of 1 mg/day

t.test(len ~ dose, data = subset(my_data, dose %in% c(1, 0.5)), alt = "less", paired = FALSE)

# The p-value is practically 0 , so we reject the null for any reasonable
# significance level and we conclude that a dosage of 1mg/day is associated with longer
# odontoblasts than a dosage of 0.5 mg/day.


# Assumptions for the unpaired t-tests:

# 1. Independence of observations - each guinea pig was given exactly one dosage
# via exactly one delivery method.
# 2. Normality - odontoblast length is approximately normally distributed for each
# group of the independent variables (supp, dose)
# 3. Homogeneity of variances.
