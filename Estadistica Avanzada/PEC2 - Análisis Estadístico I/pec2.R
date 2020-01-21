current_working_directory <- getwd()
data <- read.csv(paste(current_working_directory,"/ESTRADL_clean.csv", sep = ""))
sapply(data, class)
data$Agemenar <- as.integer(data$Agemenar)

boxplot(data$Estradl)
summary(data$Estradl)

pie(table(data$Ethnic), main = "Ethnic")

pie(table(data$Area), main="Area")

summary(data$Ethnic)
summary(data$Area)

boxplot(data$Entage)
summary(data$Entage)

hist(data$Numchild[which(data$Numchild > 0)], main="Numchild")
summary(data$Numchild)

hist(data$Agefbo[which(data$Agefbo > 0)], main="Agefbo")
summary(data$Agefbo)

barplot(table(data$Anykids), main = "Anykids")
summary(data$Anykids)

boxplot(data$Agemenar, main = "Agemenar")
summary(data$Agemenar)

boxplot(data$BMI, main = "BMI")
summary(data$BMI)

hist(data$WHR, main = "WHR")
summary(data$WHR)



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

#Calculos
S <- compute_S(data$Agemenar)
#t
error_estandar = (S / sqrt(205))
t = (m - 14) / error_estandar

sol = t.test(x = data$Agemenar, mu = 14, aleternative = "less", conf.level = 0.98)
sol

pt(t, df = length(data$Agemenar) - 1)
talpha = qt(p=0.975, df = 8)
margen_error = (talpha * S)/sqrt(205)
limite_superior = m + margen_error
limite_inferior = m- margen_error

#Intervalo de confianza del estradiol al 95%

# S
S <- compute_S(data$Estradl)

sol = t.test(x = data$Estrad, mu = 36, aleternative = "less", conf.level = 0.95)
sol

tn1 = qt(p=(1 - 0.95) / 2, df = 204)
margen_error = abs((tn1 * S)/sqrt(205))
limite_superior = X + margen_error
limite_inferior = X - margen_error
limite_inferior 
limite_superior


# 4. Contraste entre muestras
X1 = mean(data$Estradl[which(data$Ethnic == "Caucasian")])
X2 = mean(data$Estradl[which(data$Ethnic == "African American")])

# S1
S1 <- compute_S(data$Estradl[which(data$Ethnic == "Caucasian")])
sd(data$Estradl[which(data$Ethnic == "Caucasian")])
# S2
S2 <- compute_S(data$Estradl[which(data$Ethnic == "African American")])
sd(data$Estradl[which(data$Ethnic == "African American")])
# S
S = sqrt(((length(data$Estradl[which(data$Ethnic == "Caucasian")]) - 1) * S1^2 + (length(data$Estradl[which(data$Ethnic == "African American")]) - 1) * S2^2) / (length(data$Estradl) - 2))

# T
error_estandar = S*sqrt((1 / length(data$Estradl[which(data$Ethnic == "Caucasian")])) + (1 / length(data$Estradl[which(data$Ethnic == "African American")])))
t = (X1 - X2) / error_estandar

#Valor crítico
talpha = qt(p=0.025, df=203)
limite_inferior = (X1-X2)-(abs(talpha)*error_estandar)
limite_superior = (X1-X2)+(abs(talpha)*error_estandar)
2*(1 - pt(t, df = 203))

sol = t.test(data$Estradl[which(data$Ethnic == "Caucasian")], data$Estradl[which(data$Ethnic == "African American")], alternative = "two.sided", conf.level = 0.95)
sol

# 5. estradiol hijos
X1 = mean(data$Estradl[which(data$Anykids == "Yes")])
X2 = mean(data$Estradl[which(data$Anykids == "No")])

# S1
S1 <- compute_S(data$Estradl[which(data$Anykids == "Yes")])

# S2
S2 <- compute_S(data$Estradl[which(data$Anykids == "No")])

# S
S = sqrt(((length(data$Estradl[which(data$Anykids == "Yes")]) - 1) * S1^2 + (length(data$Estradl[which(data$Anykids == "No")]) - 1) * S2^2) / (length(data$Estradl) - 2))

# T
error_estandar = S*sqrt((1 / length(data$Estradl[which(data$Anykids == "Yes")])) + (1 / length(data$Estradl[which(data$Anykids == "No")])))
t = (X1 - X2) / error_estandar

#Valor crítico
talpha = qt(p=0.05, df=203)
limite_inferior = (X1-X2)-(abs(talpha)*error_estandar)
limite_superior = (X1-X2)+(abs(talpha)*error_estandar)
talpha = qt(p=0.1, df=203)
limite_inferior = (X1-X2)-(abs(talpha)*error_estandar)
limite_superior = (X1-X2)+(abs(talpha)*error_estandar)

pv = 1 - pt(t, 203)

sol = t.test(x = data$Estradl[which(data$Anykids == "Yes")], y = data$Estradl[which(data$Anykids == "No")], alternative = "greater", conf.level = 0.90)
sol


# 6. Estudio longitudinal
current_working_directory <- getwd()
data <- read.csv(paste(current_working_directory,"/ESTRADL7.csv", sep = ""))
X1 <- mean(data$Estrad)
X2 <- mean(data$Estradl7)
S1 <- compute_S(data$Estrad)
S2 <- compute_S(data$Estradl7)



qqnorm(y = data$Estrad, main = "Normal Q-Q Plot of the original Estradiol value")
qqline(y = data$Estrad)
qqnorm(y = data$Estradl7, main = "Normal Q-Q Plot of the last Estradiol value")
qqline(y = data$Estradl7)

shapiro.test(data$Estrad)
shapiro.test(data$Estradl7)

wilcox.test(data$Estrad, data$Estradl7, paired = TRUE, alternative = "l", conf.level = 0.97)

diff <- c(data$Estrad - data$Estradl7)
diff.rank <- rank(abs(diff))
diff.rank.sign <- diff.rank *sign(diff)
ranks.pos <- sum(diff.rank.sign[diff.rank.sign > 0])
ranks.neg <- -sum(diff.rank.sign[diff.rank.sign < 0])
interval.low <- 25 - length(data$Estrad)
interval.high <- 30 + length(data$Estrad)
