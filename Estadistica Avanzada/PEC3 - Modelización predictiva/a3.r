if (!require("fmsb")) {install.packages("fmsb")}
library("fmsb")
if (!require("magrittr")) {install.packages("magrittr")}
library("magrittr")
install.packages("gridExtra")
library(gridExtra)
current_working_directory <- getwd()
data <- read.csv(paste(current_working_directory,"/datosA3.csv", sep = ""))


# Toda la muestra
model <- lm(HCTO ~ HB, data=data)
summary(model)
model$coefficients

plot(data$HB, data$HCTO)
abline(model, col = "red")
r2 <- summary(model)$r.squared

# División de la muestra

# a) Desnutrición
hcto_desnutri <- data$HCTO[which(data$DESNUTR == "si")]
hb_desnutri <- data$HB[which(data$DESNUTR == "si")]
model_desnutri <- lm(hcto_desnutri ~ hb_desnutri)
r2_desnutri<-summary(model_desnutri)$r.squared

plot(hb_desnutri, hcto_desnutri)
abline(model_desnutri, col = "red")
# b) Sin desnutrición
hcto_nutri <- data$HCTO[which(data$DESNUTR == "no")]
hb_nutri <- data$HB[which(data$DESNUTR == "no")]
plot(hcto_nutri, hb_nutri)
model_nutri <- lm(hcto_nutri ~ hb_nutri)
r2_nutri<-summary(model_nutri)$r.squared

plot(hb_nutri, hcto_nutri)
abline(model_nutri, col = "red")
# Regresión multiple

model_multiple <- lm(HCTO ~ HB + EDAD, data=data)
summary(model_multiple)
r2_multiple <- summary(model_multiple)$r.squared

# Regresión multiple 2
model_multiple_2 <- lm(HCTO ~ HB + EDAD + INFEC, data=data)
summary(model_multiple_2)
r2_multiple_2 <- summary(model_multiple_2)$r.squared
d_3 <- data.frame(data$HCTO, data$HB, data$EDAD, data$INFEC)
names(d_3) <- c("HCTO", "HB", "EDAD", "INFEC")
pairs(d_3)
# rm22
hcto_indexes_less_37 <- which(data$HCTO < 37)
d_3 <- data.frame(data$HCTO[hcto_indexes_less_37], data$HB[hcto_indexes_less_37], data$EDAD[hcto_indexes_less_37], data$INFEC[hcto_indexes_less_37])
names(d_3) <- c("HCTO", "HB", "EDAD", "INFEC")
pairs(d_3)
model_multiple_2_less37 <- lm(HCTO ~ HB + EDAD + INFEC, data=data[hcto_indexes_less_37,])
summary(model_multiple_2_less37)
r2_multiple_2 <- summary(model_multiple_2_less37)$r.squared

# predict
new_pacient <- data.frame(EDAD = 60, INFEC = "si", HB = 10.0)
p <- predict(model_multiple_2, new_pacient)
p2 <- predict(model_multiple_2_less37, new_pacient)

## 2

conting_diab <- table(data$INFEC, data$DIABETES)
plot(conting_diab, col = c("red", "blue"), main = "Infección vs diabetes", xlab = "Infección", ylab = "Diabetes")
chisq.test(conting_diab)


conting_desnutri <- table(data$INFEC, data$DESNUTR)
plot(conting_desnutri, col = c("red", "blue"), main = "Infección vs desnutrición", xlab = "Infección", ylab = "Desnutrición")
chisq.test(conting_desnutri)

conting_obes <- table(data$INFEC, data$OBES)
plot(conting_obes, col = c("red", "blue"), main = "Infección vs obesidad", xlab = "Infección", ylab = "Obesidad")
chisq.test(conting_obes)

conting_edad <- table(data$INFEC, data$EDAD)
plot(conting_edad, col = c("red", "blue"), main = "Infección vs edad", xlab = "Infección", ylab = "Edad")
chisq.test(conting_edad)

conting_hcto <- table(data$INFEC, data$HCTO)
plot(conting_hcto, col = c("red", "blue"), main = "Infección vs desnutri", xlab = "Infección", ylab = "Desnutrición")
chisq.test(conting_hcto)


oddsratioWald.proc <- function(n00, n01, n10, n11, alpha = 0.05){
  #
  #  Compute the odds ratio between two binary variables, x and y,
  #  as defined by the four numbers nij:
  #
  #    n00 = number of cases where x = 0 and y = 0
  #    n01 = number of cases where x = 0 and y = 1
  #    n10 = number of cases where x = 1 and y = 0
  #    n11 = number of cases where x = 1 and y = 1
  #
  OR <- (n00 * n11)/(n01 * n10)
  #
  #  Compute the Wald confidence intervals:
  #
  siglog <- sqrt((1/n00) + (1/n01) + (1/n10) + (1/n11))
  zalph <- qnorm(1 - alpha/2)
  logOR <- log(OR)
  loglo <- logOR - zalph * siglog
  loghi <- logOR + zalph * siglog
  #
  ORlo <- exp(loglo)
  ORhi <- exp(loghi)
  #
  oframe <- data.frame(LowerCI = ORlo, OR = OR, UpperCI = ORhi, alpha = alpha)
  oframe
}

AutomaticOR.proc <- function(x,y,alpha=0.05){
  #
  xtab <- table(x,y)
  n00 <- xtab[1,1]
  n01 <- xtab[1,2]
  n10 <- xtab[2,1]
  n11 <- xtab[2,2]
  #
  rawOR <- (n00*n11)/(n01*n10)
  if (rawOR < 1){
    n01 <- xtab[1,1]
    n00 <- xtab[1,2]
    n11 <- xtab[2,1]
    n10 <- xtab[2,2]
    iLevel <- 2
  }
  else{
    iLevel <- 1
  }
  outList <- vector("list",2)
  outList[[1]] <- paste("Odds ratio between the level [",dimnames(xtab)[[1]][1],"] of the first variable and the level [",dimnames(xtab)[[2]][iLevel],"] of the second variable:",sep=" ")
  outList[[2]] <- oddsratioWald.proc(n00,n01,n10,n11,alpha)
  outList
}

AutomaticOR.proc(data$INFEC, data$DIABETES)


fmsb::oddsratio(conting_diab)

AutomaticOR.proc(data$INFEC, data$DESNUTR)
AutomaticOR.proc(data$INFEC, data$OBES)
AutomaticOR.proc(data$INFEC, data$EDAD)
AutomaticOR.proc(data$INFEC, data$HEMAT)

result_odds <- data.frame(1.98, 3.48, 1.8)
names(result_odds) <- c("Diabetes", "Desnutricion", "Obesidad")

model <- glm(INFEC ~ DIABETES + DESNUTR + OBES + TIP_OPER + EDAD + HEMAT, data=data, family="binomial")
exp(coef(model))

e54 <- which(data$EDAD == 54)
# tipo operación
conting_tipo_oper <- table(data$INFEC, data$TIP_OPER)
plot(conting_tipo_oper, col = c("red", "blue", "green", "purple"), main = "Infección vs Tipo", xlab = "Infección", ylab = "Tipo de operación")
model <- glm(INFEC ~TIP_OPER, data=data, family="binomial")
exp(coef(model))

# regresión logística

model <- glm(INFEC ~ DIABETES, data=data, family="binomial")
summary(model)
exp(coefficients(model))

model2 <- glm(INFEC ~ DIABETES + EDAD + HEMAT, data=data, family="binomial")
summary(model2)
exp(coefficients(model2))
diabedad <- data.frame(data$EDAD, data$DIABETES)
plot(data$EDAD~data$DIABETES, ylab="EDAD", xlab="DIABETES")
model3 <- glm(DIABETES ~ EDAD, data=data, family="binomial")
exp(coefficients(model3))

#mejora
edadmayor65 <- which(data$EDAD >= 65)
edadmenor <- which(data$EDAD < 65)
edad_categorica <- c()
edad_categorica[edadmayor65] <- 1
edad_categorica[edadmenor] <- 0
hematocritomayor37 <- which(data$HCTO >= 37)
hematocritomenor37 <- which(data$HCTO < 37)
hematocritos <- c()
hematocritos[hematocritomayor37] <- 1
hematocritos[hematocritomenor37] <- 0
rm(hematocritomayor37)
rm(hematocritomenor37)
rm(edadmayor65)
rm(edadmenor)

new_data <- data.frame(data$INFEC, data$DIABETES, factor(edad_categorica), factor(hematocritos))
names(new_data) <- c("INFEC", "DIABETES", "EDAD_65", "HCTO_37")

model4 <- glm(INFEC ~ DIABETES + EDAD_65 + HCTO_37, data=new_data, family="binomial")
summary(model4)
ec <- exp(coefficients(model4))

new_data <- data.frame(data$INFEC, data$DIABETES, factor(edad_categorica), factor(hematocritos), data$DESNUTR)
names(new_data) <- c("INFEC", "DIABETES", "EDAD_65", "HCTO_37", "DESNUTR")
model5 <- glm(INFEC ~ DIABETES + EDAD_65 + HCTO_37 + DESNUTR, data=new_data, family="binomial")
summary(model5)
ec <- t(as.data.frame(exp(coefficients(model5)[2:4])))
install.packages("knitr")

kable(ec , caption = "Odds ratio"
       , align = c('c', 'c', 'c', 'c')
       , col.names = colnames(ec)
       , row.names = FALSE
)
predict(model5, data.frame(EDAD_65 = factor(0), DIABETES = "si", HCTO_37 =factor(0), DESNUTR = "no"), type = "response")
plot(table(new_data$INFEC, new_data$DIABETES), col = c("red", "blue"), main = "Infección vs Diabetes", xlab = "Infección", ylab = "Diabetes")
plot(table(new_data$INFEC, new_data$EDAD_65), col = c("red", "blue"), main = "Infección vs Edad>=65", xlab = "Infección", ylab = "Edad>=65")
plot(table(new_data$INFEC, new_data$HCTO_37), col = c("red", "blue"), main = "Infección vs HCTO>=37", xlab = "Infección", ylab = "HCTO>=37")
plot(table(new_data$INFEC, new_data$DESNUTR), col = c("red", "blue"), main = "Infección vs Desnutrición", xlab = "Infección", ylab = "Desnutrición")
