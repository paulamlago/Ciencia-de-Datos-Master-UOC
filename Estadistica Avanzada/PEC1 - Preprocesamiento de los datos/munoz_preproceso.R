current_working_directory <- getwd()
data <- read.csv2(paste(current_working_directory,"/DataEstradiol.csv", sep = ""))

data$Numchild[which(data$Numchild == 9)] <- NA
data$Agefbo[which(data$Agefbo == 99)] <- NA
data$Anykids[which(data$Anykids == 9)] <- NA
data$Agemenar[which(data$Agemenar == 99)] <- NA

nrow(data) #Numero de registros
ncol(data) #Numero de variables
colnames(data) #Nombre de las variables

str(data)
summary(data)

data$Id
class(data$Id)

sapply(data, class)
sapply(savedata, class)

savedata <- data
savedata$Id <- as.numeric(savedata$Id)
savedata$Estradl <- sapply(savedata$Estradl, gsub, pattern=",", replacement=".")
savedata$Estradl <- as.numeric(savedata$Estradl)

variables_cuantitativas <- c(1, 2, 4:ncol(data))
data[, variables_cuantitativas] <- sapply(data[, variables_cuantitativas], gsub, pattern=",", replacement=".")

data[, variables_cuantitativas] <- sapply(data[, variables_cuantitativas], as.numeric)

boxplot(data$Estradl,main="Estradiol", col="pink")
outliers <- sort(boxplot.stats(data$Estradl)$out)

remove_index <-which(data$Estradl %in% tail(outliers, 2))

data <- data[-remove_index,]

boxplot(data$Estradl,main="Estradiol", col="pink")
hist(data$Entage,main="Edad")
boxplot(data$Numchild,main="Número de hijos")
boxplot.stats(data$Numchild)$out
boxplot(data$Agefbo[which(data$Agefbo != 0)],main="Edad del primer hijo")
anomalias <- boxplot(data$Agefbo[which(data$Agefbo != 0)],main="Edad del primer hijo")$out
data$Agefbo[which(data$Agefbo == 4 | data$Agefbo == 6 | data$Agefbo == 7)] <- NA

boxplot(data$Anykids,main="Tiene hijos (1 o 0)")
which(data$Anykids == boxplot(data$Anykids,main="Tiene hijos (1 o 0)")$out)

hist(data$Agemenar,main="Edad menarquía")

boxplot(data$BMI,main="BMI")
boxplot.stats(data$BMI)$out
boxplot(data$WHR,main="WHR")
boxplot.stats(data$WHR)$out

indexes <- which(data$Numchild > 0 & data$Anykids > 1)
data[indexes,]$Anykids <- 1
data[indexes, ]

data[which(data$Agemenar > data$Entage), ]

index <- which(data$Agemenar > data$Entage)
real_agemenar <- data[which(data$Agemenar > data$Entage),]$Entage
real_entage <- data[which(data$Agemenar > data$Entage),]$Agemenar

data[index,]$Entage <- real_entage
data[index,]$Agemenar <- real_agemenar
which(data$Agefbo > 0 & data$Numchild == 0)
which(data$Agemenar > data$Agefbo & data$Agefbo != 0)
data[which(data$Agemenar > data$Agefbo & data$Agefbo != 0 & data$Numchild == 0), ]$Agefbo <- 0
data[which(data$Agefbo > 0 & data$Numchild == 0),]$Agefbo <- 0

inconsistent_data <- data[which(data$Agemenar > data$Agefbo & data$Agefbo != 0), ]


table(data$Ethnic)

which(stringr::str_detect(data$Ethnic, "Af"))
data[which(stringr::str_detect(data$Ethnic, "Af")), ]$Ethnic <- "African American"
data$Ethnic <- trimws(data$Ethnic)
data[which(data$Ethnic == "Caucacian" | data$Ethnic == "Caucsian"), ]$Ethnic <- "Caucasian"

na_index <- which(is.na(data$Numchild) | is.na(data$Agefbo) | is.na(data$Anykids) | is.na(data$Agemenar))

library(VIM)
# 1. African American
index_af <- which(data$Ethnic == "African American")
data$Numchild[index_af] <- kNN(data[index_af,], variable = "Numchild", k = 3)$Numchild
data$Agefbo[index_af] <- kNN(data[index_af,], variable = "Agefbo", k = 3)$Agefbo
data$Anykids[index_af] <- kNN(data[index_af,], variable = "Anykids", k = 3)$Anykids
data$Agemenar[index_af] <- kNN(data[index_af,], variable = "Agemenar", k = 3)$Agemenar
# 2.Caucasian
index_ca <- which(data$Ethnic == "Caucasian")
data$Numchild[index_ca] <- kNN(data[index_ca,], variable = "Numchild", k = 3)$Numchild
data$Agefbo[index_ca] <- kNN(data[index_ca,], variable = "Agefbo", k = 3)$Agefbo
data$Anykids[index_ca] <- kNN(data[index_ca,], variable = "Anykids", k = 3)$Anykids
data$Agemenar[index_ca] <- kNN(data[index_ca,], variable = "Agemenar", k = 3)$Agemenar

data[na_index,]
summary(data$Estradl)

write.csv(data, file=paste(current_working_directory,"/ESTRADL_clean.csv", sep = ""))