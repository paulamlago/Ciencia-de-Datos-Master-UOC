current_working_directory <- getwd()
data <- read.csv(paste(current_working_directory,"/Wage.csv", sep = ""))

sapply(data, class)

plot(data$maritl, col="#FFFFFF", main="Maritl")
plot(data$race, col="#FFFFFF", main="Race")
plot(data$region, col="#FFFFFF", main="Region")
pie(table(data$jobclass), main="Jobclass")
pie(table(data$health), main="Health")
pie(table(data$health_ins), main="Health Insurance")

boxplot(data$year)
boxplot(data$age)
boxplot(data$logwage)
boxplot(data$wage)

summary(data$maritl)
summary(data$race)
summary(data$region)
summary(data$jobclass)
summary(data$health)
summary(data$health_ins)

summary(data$year)
summary(data$age)
summary(data$logwage)
summary(data$wage)

plot(x = data$race, y=data$wage)
plot(x = data$jobclass, y=data$wage, main = "Wage en función de JobClass")
plot(x = data$health, y=data$wage, main = "Wage en función de health")
plot(x = data$health_ins, y=data$wage, main = "Wage en función de health_ins")

qqnorm(y = data$wage, main = "Normal Q-Q Plot of the wage")
qqline(y = data$wage)
length(data$wage)

compute_S <- function(vector){
  # S
  # (xi-xmean)^2
  sum = 0
  m = mean(vector)
  for(a in vector){
    x = (a - m)^2
    sum = sum + x
  }
  #1/n - 1
  y = 1 / (length(vector) - 1)
  #sqrt
  S = sqrt(sum / (length(vector) - 1))
}

X <- mean(data$age)
S <- compute_S(data$age)

tn1 <- qt(p=(1 - 0.95)/2, df = length(data$age) - 1)

margen_error <- abs((tn1 * S)/sqrt(length(data$age)))
limite_superior <- X + margen_error
limite_inferior <- X - margen_error
limite_inferior 
limite_superior

#jobclass
# 1. industrial
data_industrial <- data$age[which(data$jobclass == levels(data$jobclass)[1])]
X <- mean(data_industrial)
S <- compute_S(data_industrial)

tn1 <- qt(p=(1 - 0.95)/2, df = length(data_industrial) - 1)

margen_error <- abs((tn1 * S)/sqrt(length(data_industrial)))
limite_superior <- X + margen_error
limite_inferior <- X - margen_error
limite_inferior 
limite_superior

data_information <- data$age[which(data$jobclass == levels(data$jobclass)[2])]
X <- mean(data_information)
S <- compute_S(data_information)

tn1 <- qt(p=(1 - 0.95)/2, df = length(data_information) - 1)

margen_error <- abs((tn1 * S)/sqrt(length(data_information)))
limite_superior <- X + margen_error
limite_inferior <- X - margen_error
limite_inferior 
limite_superior

# Contraste hipótesis

salary_insurance <- data$wage[which(data$health_ins == levels(data$health_ins)[1])]
salary_not_insurance <- data$wage[which(data$health_ins == levels(data$health_ins)[2])]

X1 <- mean(salary_insurance)
X2 <- mean(salary_not_insurance)

S1 <- compute_S(salary_insurance)
S2 <- compute_S(salary_not_insurance)

S <- sqrt(((length(salary_insurance) - 1) * S1^2 + (length(salary_not_insurance) - 1) * S2^2) / (length(data$wage) - 2))

error_estandar <- (S*sqrt((1 / length(salary_insurance)) + (1 / length(salary_not_insurance))))
t <- (X1 - X2) / error_estandar

vc <- qt(p=0.05, df=2999)
intervalo <- c((X1-X2)+(vc*error_estandar), (X1-X2)-(vc*error_estandar))
1 - pt(t, df = 2999)

wilcox.test(data$wage ~ data$health_ins, alternative = "greater", paired = FALSE, conf.int = 0.95)

# 4
salary_mean <- mean(data$wage)
salary <- c()
salary[which(data$wage >= salary_mean)] <- 1
salary[which(data$wage < salary_mean)] <- 0
plot(table(salary, data$health_ins), col="#FFFFFF", main = "Correlation between Salary over the mean and health insurance")
plot(table(salary, data$jobclass), col="#FFFFFF", main = "Correlation between Salary over the mean and job class")
boxplot(data$age ~salary, xlab = "Salary over mean", ylab = "Age")

chisq.test(table(salary, data$health_ins))
chisq.test(table(salary, data$jobclass))
chisq.test(table(salary, data$age))

model <- glm(salary ~ health_ins + jobclass + age, data=data, family="binomial")
summary(model)
odds <- exp(coef(model))
odds
plot(table(salary,data$education), las=1, col="#FFFFFF")
model_edu <- glm(salary ~ health_ins + jobclass + age + education, data=data, family="binomial")
summary(model_edu)
odds <- exp(coef(model_edu))
odds

trabajador <- data.frame(health_ins = levels(data$health_ins)[1], jobclass = levels(data$jobclass)[2], age = 42, education = levels(data$education)[4])

predict(model_edu, trabajador, type="response")

trabajador <- data.frame(health_ins = levels(data$health_ins)[1], jobclass = levels(data$jobclass)[1], age = 42, education = levels(data$education)[4])

predict(model_edu, trabajador, type="response")

# ANOVA

salary_less <- which(data$wage < 150.000)

boxplot(data$wage[salary_less] ~ data$education[salary_less], main = "Salarios en función de los estudios")

anova <- aov(wage ~ education, data = data[salary_less,])
summary(anova)
plot(anova)
mean_all_groups <- aggregate(data$wage[salary_less], by=list(data$education[salary_less]), FUN=mean)[,"x"]
length_all_groups <- aggregate(data$wage[salary_less], by=list(data$education[salary_less]), FUN=length)[,"x"]
mean_all <- sum(length_all_groups*mean_all_groups) / sum(length_all_groups)

sum_sq <- 0
for (j in levels(data$education)){
  data_j <- data$wage[intersect(salary_less, which(data$education == j))]
  nj <- length(data_j)
  c <- (mean(data_j) - mean_all)^2
  sum_sq <- sum_sq + nj*c
}
sum_sq

sum_sq <- 0
for (j in levels(data$education)){
  data_j <- data$wage[intersect(salary_less, which(data$education == j))]
  sum_group <- 0
  for (xij in data_j){
    sum_group <- sum_group + (xij - mean(data_j))^2
  }
  sum_sq <- sum_sq + sum_group
}
sum_sq

plot(anova)

# no paramétrico RAZA

salary_less_race <- intersect(which(data$wage < 150.000), which(data$race != levels(data$race)[4]))
data_no_others <- data[salary_less_race,]
data_no_others$race <- droplevels(data_no_others$race, levels(data$race)[4])

boxplot(data_no_others$wage ~ data_no_others$race, main = "Salary by race")

kruskal.test(wage ~ race, data = data_no_others)

# 6
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

t <- as.data.frame(data_no_others %>% 
  group_by(race, jobclass) %>%
  summarise(mean(wage)))

names(t) <- c("race", "jobclass", "wage")
ggplot(data = t, aes(race, wage, color=jobclass, group = jobclass)) + geom_point() + geom_line()

anova_rj <- aov(wage ~ race * jobclass, data = data_no_others)
summary(anova_rj)
plot(anova_rj)

t <- as.data.frame(data_no_others %>% 
                     group_by(race, education) %>%
                     summarise(mean(wage)))

names(t) <- c("race", "education", "wage")
ggplot(data = t, aes(education, wage, color=race, group = race)) + geom_point() + geom_line()

anova_re <- aov(wage ~ race * education, data = data_no_others)
summary(anova_re)
plot(anova_re)

install.packages("DescTools")
library(DescTools)
ScheffeTest(anova_rj)

install.packages('agricolae')
library(agricolae)
scheffe.test(anova_rj, "Race")
