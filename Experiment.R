library("ggplot2")  # графики
library("car")  # vif
library("tibble")
library("asbio")
library("coin")
library("lawstat")
library("onewaytests")
library("welchADF")
library("WRS2")
library("doex")
start.time <- Sys.time()
mean_1<-0
mean_2<-exp(2)
n<-50
N<-1000
sigma_1<-exp(2)
sigma_2<-exp(3)
# Создание экспериментальных df
x_1 <- rnorm(n, mean = mean_1, sd = sigma_1)
x_2 <- rnorm(n, mean = mean_2, sd = sigma_2)
x_3 <- rnorm(n, mean = mean_1, sd = sigma_2)
DF_not <- data.frame(c(x_1,x_3),c(rep("A", n),rep("C", n)))
names(DF_not) <- c("point","label")
DF_yes <- data.frame(c(x_1,x_2),c(rep("A", n),rep("B", n)))
names(DF_yes) <- c("point","label")
#### Расчеты тестов ####
# Approximate F-test
#AF(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
#AF(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
# Alexandern-Govern test
#AG(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
#AG(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
# B-square test
#B2(0.01,DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
#B2(0.01,DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
# Brown-Forsythe test
#BF(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
#BF(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
# Box F-test
#BX(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
#BX(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
# Cochran F-test
#CF(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
#CF(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
# Fiducial Approach test
#FA(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
#FA(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
# Generalized F-test
#GF(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
#GF(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
# Johansen F-test
#JF(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
#JF(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
# Modified Brown-Forsythe test
#MBF(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
#MBF(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
# Modified generalized F-test
#MGF(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
#MGF(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
# Modified Welch Testt
#MW(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
#MW(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
# One Stage test
#OS(DF_not$point,DF_not$label,1,1000) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
#OS(DF_yes$point,DF_yes$label,1,1000) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
# One Stage Range test
#OSR(DF_not$point,DF_not$label,1,1000) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
#OSR(DF_yes$point,DF_yes$label,1,1000) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
# Parametric Bootstrap test
#PB(DF_not$point,DF_not$label,1000) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
#PB(DF_yes$point,DF_yes$label,1000) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
# Permutation F-test
#PF(DF_not$point,DF_not$label,1000) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
#PF(DF_yes$point,DF_yes$label,1000) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
# Revised generalized F-test
#RGF(DF_not$point,DF_not$label,1000) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
#RGF(DF_yes$point,DF_yes$label,1000) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
# Scott-Smith Test
#SS(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
#SS(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
# Welch-Aspin test
#WA(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
#WA(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
# Welch F-test
#WE(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
#WE(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли

#### Расчет по тесту 1 ####
Result_not <- array(data = NA, c(1,N), dimnames = NULL)
Result_yes <- array(data = NA, c(1,N), dimnames = NULL)
for(i in seq(1:N)) 
  {
  x_1 <- rnorm(n, mean = mean_1, sd = sigma_1)
  x_2 <- rnorm(n, mean = mean_2, sd = sigma_2)
  x_3 <- rnorm(n, mean = mean_1, sd = sigma_2)
  DF_not <- data.frame(c(x_1,x_3),c(rep("A", n),rep("C", n)))
  names(DF_not) <- c("point","label")
  DF_yes <- data.frame(c(x_1,x_2),c(rep("A", n),rep("B", n)))
  names(DF_yes) <- c("point","label")
  Test_not<-AF(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
  Test_yes<-AF(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
  Result_not[i]<-Test_not[4]
  Result_yes[i]<-Test_yes[4]
}
Result_AF<-c(sum(Result_not>=0.01),sum(Result_not<0.01),sum(Result_yes<=0.01),sum(Result_yes>0.01))
Result_AF
#### Расчет по тесту 2 ####
Result_not <- array(data = NA, c(1,N), dimnames = NULL)
Result_yes <- array(data = NA, c(1,N), dimnames = NULL)
for(i in seq(1:N)) 
{
  x_1 <- rnorm(n, mean = mean_1, sd = sigma_1)
  x_2 <- rnorm(n, mean = mean_2, sd = sigma_2)
  x_3 <- rnorm(n, mean = mean_1, sd = sigma_2)
  DF_not <- data.frame(c(x_1,x_3),c(rep("A", n),rep("C", n)))
  names(DF_not) <- c("point","label")
  DF_yes <- data.frame(c(x_1,x_2),c(rep("A", n),rep("B", n)))
  names(DF_yes) <- c("point","label")
  Test_not<-AG(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
  Test_yes<-AG(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
  Result_not[i]<-Test_not[3]
  Result_yes[i]<-Test_yes[3]
}
Result_AG<-c(sum(Result_not>=0.01),sum(Result_not<0.01),sum(Result_yes<=0.01),sum(Result_yes>0.01))
Result_AG
#### Расчет по тесту 3 ####
Result_not <- array(data = NA, c(1,N), dimnames = NULL)
Result_yes <- array(data = NA, c(1,N), dimnames = NULL)
for(i in seq(1:N)) 
{
  x_1 <- rnorm(n, mean = mean_1, sd = sigma_1)
  x_2 <- rnorm(n, mean = mean_2, sd = sigma_2)
  x_3 <- rnorm(n, mean = mean_1, sd = sigma_2)
  DF_not <- data.frame(c(x_1,x_3),c(rep("A", n),rep("C", n)))
  names(DF_not) <- c("point","label")
  DF_yes <- data.frame(c(x_1,x_2),c(rep("A", n),rep("B", n)))
  names(DF_yes) <- c("point","label")
  Test_not<-B2(0.01,DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
  Test_yes<-B2(0.01,DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
  Result_not[i]<-Test_not[3]
  Result_yes[i]<-Test_yes[3]
}
Result_B2<-c(sum(Result_not>=0.01),sum(Result_not<0.01),sum(Result_yes<=0.01),sum(Result_yes>0.01))
Result_B2
#### Расчет по тесту 4 ####
Result_not <- array(data = NA, c(1,N), dimnames = NULL)
Result_yes <- array(data = NA, c(1,N), dimnames = NULL)
for(i in seq(1:N)) 
{
  x_1 <- rnorm(n, mean = mean_1, sd = sigma_1)
  x_2 <- rnorm(n, mean = mean_2, sd = sigma_2)
  x_3 <- rnorm(n, mean = mean_1, sd = sigma_2)
  DF_not <- data.frame(c(x_1,x_3),c(rep("A", n),rep("C", n)))
  names(DF_not) <- c("point","label")
  DF_yes <- data.frame(c(x_1,x_2),c(rep("A", n),rep("B", n)))
  names(DF_yes) <- c("point","label")
  Test_not<-BF(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
  Test_yes<-BF(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
  Result_not[i]<-Test_not[3]
  Result_yes[i]<-Test_yes[3]
}
Result_BF<-c(sum(Result_not>=0.01),sum(Result_not<0.01),sum(Result_yes<=0.01),sum(Result_yes>0.01))
Result_BF



#### Расчет по тесту 5 ####
Result_not <- array(data = NA, c(1,N), dimnames = NULL)
Result_yes <- array(data = NA, c(1,N), dimnames = NULL)
for(i in seq(1:N)) 
{
  x_1 <- rnorm(n, mean = mean_1, sd = sigma_1)
  x_2 <- rnorm(n, mean = mean_2, sd = sigma_2)
  x_3 <- rnorm(n, mean = mean_1, sd = sigma_2)
  DF_not <- data.frame(c(x_1,x_3),c(rep("A", n),rep("C", n)))
  names(DF_not) <- c("point","label")
  DF_yes <- data.frame(c(x_1,x_2),c(rep("A", n),rep("B", n)))
  names(DF_yes) <- c("point","label")
  Test_not<-BX(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
  Test_yes<-BX(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
  Result_not[i]<-Test_not[4]
  Result_yes[i]<-Test_yes[4]
}
Result_BX<-c(sum(Result_not>=0.01),sum(Result_not<0.01),sum(Result_yes<=0.01),sum(Result_yes>0.01))
Result_BX















#### Расчет по тесту 6 ####
Result_not <- array(data = NA, c(1,N), dimnames = NULL)
Result_yes <- array(data = NA, c(1,N), dimnames = NULL)
for(i in seq(1:N)) 
{
  x_1 <- rnorm(n, mean = mean_1, sd = sigma_1)
  x_2 <- rnorm(n, mean = mean_2, sd = sigma_2)
  x_3 <- rnorm(n, mean = mean_1, sd = sigma_2)
  DF_not <- data.frame(c(x_1,x_3),c(rep("A", n),rep("C", n)))
  names(DF_not) <- c("point","label")
  DF_yes <- data.frame(c(x_1,x_2),c(rep("A", n),rep("B", n)))
  names(DF_yes) <- c("point","label")
  Test_not<-CF(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
  Test_yes<-CF(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
  Result_not[i]<-Test_not[3]
  Result_yes[i]<-Test_yes[3]
}
Result_CF<-c(sum(Result_not>=0.01),sum(Result_not<0.01),sum(Result_yes<=0.01),sum(Result_yes>0.01))
Result_CF
#### Расчет по тесту 7 ####
Result_not <- array(data = NA, c(1,N), dimnames = NULL)
Result_yes <- array(data = NA, c(1,N), dimnames = NULL)
for(i in seq(1:N)) 
{
  x_1 <- rnorm(n, mean = mean_1, sd = sigma_1)
  x_2 <- rnorm(n, mean = mean_2, sd = sigma_2)
  x_3 <- rnorm(n, mean = mean_1, sd = sigma_2)
  DF_not <- data.frame(c(x_1,x_3),c(rep("A", n),rep("C", n)))
  names(DF_not) <- c("point","label")
  DF_yes <- data.frame(c(x_1,x_2),c(rep("A", n),rep("B", n)))
  names(DF_yes) <- c("point","label")
  Test_not<-FA(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
  Test_yes<-FA(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
  Result_not[i]<-Test_not[1]
  Result_yes[i]<-Test_yes[1]
}
Result_FA<-c(sum(Result_not>=0.01),sum(Result_not<0.01),sum(Result_yes<=0.01),sum(Result_yes>0.01))
Result_FA
#### Расчет по тесту 8 ####
Result_not <- array(data = NA, c(1,N), dimnames = NULL)
Result_yes <- array(data = NA, c(1,N), dimnames = NULL)
for(i in seq(1:N)) 
{
  x_1 <- rnorm(n, mean = mean_1, sd = sigma_1)
  x_2 <- rnorm(n, mean = mean_2, sd = sigma_2)
  x_3 <- rnorm(n, mean = mean_1, sd = sigma_2)
  DF_not <- data.frame(c(x_1,x_3),c(rep("A", n),rep("C", n)))
  names(DF_not) <- c("point","label")
  DF_yes <- data.frame(c(x_1,x_2),c(rep("A", n),rep("B", n)))
  names(DF_yes) <- c("point","label")
  Test_not<-GF(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
  Test_yes<-GF(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
  Result_not[i]<-Test_not[1]
  Result_yes[i]<-Test_yes[1]
}
Result_GF<-c(sum(Result_not>=0.01),sum(Result_not<0.01),sum(Result_yes<=0.01),sum(Result_yes>0.01))
Result_GF


#### Расчет по тесту 9 ####
Result_not <- array(data = NA, c(1,N), dimnames = NULL)
Result_yes <- array(data = NA, c(1,N), dimnames = NULL)
for(i in seq(1:N)) 
{
  x_1 <- rnorm(n, mean = mean_1, sd = sigma_1)
  x_2 <- rnorm(n, mean = mean_2, sd = sigma_2)
  x_3 <- rnorm(n, mean = mean_1, sd = sigma_2)
  DF_not <- data.frame(c(x_1,x_3),c(rep("A", n),rep("C", n)))
  names(DF_not) <- c("point","label")
  DF_yes <- data.frame(c(x_1,x_2),c(rep("A", n),rep("B", n)))
  names(DF_yes) <- c("point","label")
  Test_not<-JF(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
  Test_yes<-JF(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
  Result_not[i]<-Test_not[4]
  Result_yes[i]<-Test_yes[4]
}
Result_JF<-c(sum(Result_not>=0.01),sum(Result_not<0.01),sum(Result_yes<=0.01),sum(Result_yes>0.01))
Result_JF

#### Расчет по тесту 10 ####
Result_not <- array(data = NA, c(1,N), dimnames = NULL)
Result_yes <- array(data = NA, c(1,N), dimnames = NULL)
for(i in seq(1:N)) 
{
  x_1 <- rnorm(n, mean = mean_1, sd = sigma_1)
  x_2 <- rnorm(n, mean = mean_2, sd = sigma_2)
  x_3 <- rnorm(n, mean = mean_1, sd = sigma_2)
  DF_not <- data.frame(c(x_1,x_3),c(rep("A", n),rep("C", n)))
  names(DF_not) <- c("point","label")
  DF_yes <- data.frame(c(x_1,x_2),c(rep("A", n),rep("B", n)))
  names(DF_yes) <- c("point","label")
  Test_not<-MBF(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
  Test_yes<-MBF(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
  Result_not[i]<-Test_not[4]
  Result_yes[i]<-Test_yes[4]
}
Result_MBF<-c(sum(Result_not>=0.01),sum(Result_not<0.01),sum(Result_yes<=0.01),sum(Result_yes>0.01))
Result_MBF

#### Расчет по тесту 11 ####
Result_not <- array(data = NA, c(1,N), dimnames = NULL)
Result_yes <- array(data = NA, c(1,N), dimnames = NULL)
for(i in seq(1:N)) 
{
  x_1 <- rnorm(n, mean = mean_1, sd = sigma_1)
  x_2 <- rnorm(n, mean = mean_2, sd = sigma_2)
  x_3 <- rnorm(n, mean = mean_1, sd = sigma_2)
  DF_not <- data.frame(c(x_1,x_3),c(rep("A", n),rep("C", n)))
  names(DF_not) <- c("point","label")
  DF_yes <- data.frame(c(x_1,x_2),c(rep("A", n),rep("B", n)))
  names(DF_yes) <- c("point","label")
  Test_not<-MGF(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
  Test_yes<-MGF(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
  Result_not[i]<-Test_not[1]
  Result_yes[i]<-Test_yes[1]
}
Result_MGF<-c(sum(Result_not>=0.01),sum(Result_not<0.01),sum(Result_yes<=0.01),sum(Result_yes>0.01))
Result_MGF
#### Расчет по тесту 12 ####
Result_not <- array(data = NA, c(1,N), dimnames = NULL)
Result_yes <- array(data = NA, c(1,N), dimnames = NULL)
for(i in seq(1:N)) 
{
  x_1 <- rnorm(n, mean = mean_1, sd = sigma_1)
  x_2 <- rnorm(n, mean = mean_2, sd = sigma_2)
  x_3 <- rnorm(n, mean = mean_1, sd = sigma_2)
  DF_not <- data.frame(c(x_1,x_3),c(rep("A", n),rep("C", n)))
  names(DF_not) <- c("point","label")
  DF_yes <- data.frame(c(x_1,x_2),c(rep("A", n),rep("B", n)))
  names(DF_yes) <- c("point","label")
  Test_not<-MW(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
  Test_yes<-MW(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
  Result_not[i]<-Test_not[3]
  Result_yes[i]<-Test_yes[3]
}
Result_MW<-c(sum(Result_not>=0.01),sum(Result_not<0.01),sum(Result_yes<=0.01),sum(Result_yes>0.01))
Result_MW
#### Расчет по тесту 13 ####
Result_not <- array(data = NA, c(1,N), dimnames = NULL)
Result_yes <- array(data = NA, c(1,N), dimnames = NULL)
for(i in seq(1:N)) 
{
  x_1 <- rnorm(n, mean = mean_1, sd = sigma_1)
  x_2 <- rnorm(n, mean = mean_2, sd = sigma_2)
  x_3 <- rnorm(n, mean = mean_1, sd = sigma_2)
  DF_not <- data.frame(c(x_1,x_3),c(rep("A", n),rep("C", n)))
  names(DF_not) <- c("point","label")
  DF_yes <- data.frame(c(x_1,x_2),c(rep("A", n),rep("B", n)))
  names(DF_yes) <- c("point","label")
  Test_not<-SS(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
  Test_yes<-SS(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
  Result_not[i]<-Test_not[3]
  Result_yes[i]<-Test_yes[3]
}
Result_SS<-c(sum(Result_not>=0.01),sum(Result_not<0.01),sum(Result_yes<=0.01),sum(Result_yes>0.01))
Result_SS
#### Расчет по тесту 14 ####
Result_not <- array(data = NA, c(1,N), dimnames = NULL)
Result_yes <- array(data = NA, c(1,N), dimnames = NULL)
for(i in seq(1:N)) 
{
  x_1 <- rnorm(n, mean = mean_1, sd = sigma_1)
  x_2 <- rnorm(n, mean = mean_2, sd = sigma_2)
  x_3 <- rnorm(n, mean = mean_1, sd = sigma_2)
  DF_not <- data.frame(c(x_1,x_3),c(rep("A", n),rep("C", n)))
  names(DF_not) <- c("point","label")
  DF_yes <- data.frame(c(x_1,x_2),c(rep("A", n),rep("B", n)))
  names(DF_yes) <- c("point","label")
  Test_not<-WA(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
  Test_yes<-WA(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
  Result_not[i]<-Test_not[3]
  Result_yes[i]<-Test_yes[3]
}
Result_WA<-c(sum(Result_not>=0.01),sum(Result_not<0.01),sum(Result_yes<=0.01),sum(Result_yes>0.01))
Result_WA
#### Расчет по тесту 15 ####
Result_not <- array(data = NA, c(1,N), dimnames = NULL)
Result_yes <- array(data = NA, c(1,N), dimnames = NULL)
for(i in seq(1:N)) 
{
  x_1 <- rnorm(n, mean = mean_1, sd = sigma_1)
  x_2 <- rnorm(n, mean = mean_2, sd = sigma_2)
  x_3 <- rnorm(n, mean = mean_1, sd = sigma_2)
  DF_not <- data.frame(c(x_1,x_3),c(rep("A", n),rep("C", n)))
  names(DF_not) <- c("point","label")
  DF_yes <- data.frame(c(x_1,x_2),c(rep("A", n),rep("B", n)))
  names(DF_yes) <- c("point","label")
  Test_not<-WE(DF_not$point,DF_not$label) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
  Test_yes<-WE(DF_yes$point,DF_yes$label) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
  Result_not[i]<-Test_not[3]
  Result_yes[i]<-Test_yes[3]
}
Result_WE<-c(sum(Result_not>=0.01),sum(Result_not<0.01),sum(Result_yes<=0.01),sum(Result_yes>0.01))
Result_WE
#### Расчет по тесту 16 ####
Result_not <- array(data = NA, c(1,N), dimnames = NULL)
Result_yes <- array(data = NA, c(1,N), dimnames = NULL)
for(i in seq(1:N)) 
{
  x_1 <- rnorm(n, mean = mean_1, sd = sigma_1)
  x_2 <- rnorm(n, mean = mean_2, sd = sigma_2)
  x_3 <- rnorm(n, mean = mean_1, sd = sigma_2)
  DF_not <- data.frame(c(x_1,x_3),c(rep("A", n),rep("C", n)))
  names(DF_not) <- c("point","label")
  DF_yes <- data.frame(c(x_1,x_2),c(rep("A", n),rep("B", n)))
  names(DF_yes) <- c("point","label")
  Test_not<-OS(DF_not$point,DF_not$label,1,1000) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
  Test_yes<-OS(DF_yes$point,DF_yes$label,1,1000) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
  Result_not[i]<-Test_not[1]
  Result_yes[i]<-Test_yes[1]
}
Result_OS<-c(sum(Result_not>=0.01),sum(Result_not<0.01),sum(Result_yes<=0.01),sum(Result_yes>0.01))
Result_OS

#### Расчет по тесту 17 ####
Result_not <- array(data = NA, c(1,N), dimnames = NULL)
Result_yes <- array(data = NA, c(1,N), dimnames = NULL)
for(i in seq(1:N)) 
{
  x_1 <- rnorm(n, mean = mean_1, sd = sigma_1)
  x_2 <- rnorm(n, mean = mean_2, sd = sigma_2)
  x_3 <- rnorm(n, mean = mean_1, sd = sigma_2)
  DF_not <- data.frame(c(x_1,x_3),c(rep("A", n),rep("C", n)))
  names(DF_not) <- c("point","label")
  DF_yes <- data.frame(c(x_1,x_2),c(rep("A", n),rep("B", n)))
  names(DF_yes) <- c("point","label")
  Test_not<-OSR(DF_not$point,DF_not$label,1,1000) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
  Test_yes<-OSR(DF_yes$point,DF_yes$label,1,1000) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
  Result_not[i]<-Test_not[1]
  Result_yes[i]<-Test_yes[1]
}
Result_OSR<-c(sum(Result_not>=0.01),sum(Result_not<0.01),sum(Result_yes<=0.01),sum(Result_yes>0.01))
Result_OSR
#### Расчет по тесту 18 ####
Result_not <- array(data = NA, c(1,N), dimnames = NULL)
Result_yes <- array(data = NA, c(1,N), dimnames = NULL)
for(i in seq(1:N)) 
{
  x_1 <- rnorm(n, mean = mean_1, sd = sigma_1)
  x_2 <- rnorm(n, mean = mean_2, sd = sigma_2)
  x_3 <- rnorm(n, mean = mean_1, sd = sigma_2)
  DF_not <- data.frame(c(x_1,x_3),c(rep("A", n),rep("C", n)))
  names(DF_not) <- c("point","label")
  DF_yes <- data.frame(c(x_1,x_2),c(rep("A", n),rep("B", n)))
  names(DF_yes) <- c("point","label")
  Test_not<-PB(DF_not$point,DF_not$label,1000) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
  Test_yes<-PB(DF_yes$point,DF_yes$label,1000) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
  Result_not[i]<-Test_not[1]
  Result_yes[i]<-Test_yes[1]
}
Result_PB<-c(sum(Result_not>=0.01),sum(Result_not<0.01),sum(Result_yes<=0.01),sum(Result_yes>0.01))
Result_PB
#### Расчет по тесту 19 ####
Result_not <- array(data = NA, c(1,N), dimnames = NULL)
Result_yes <- array(data = NA, c(1,N), dimnames = NULL)
for(i in seq(1:N)) 
{
  x_1 <- rnorm(n, mean = mean_1, sd = sigma_1)
  x_2 <- rnorm(n, mean = mean_2, sd = sigma_2)
  x_3 <- rnorm(n, mean = mean_1, sd = sigma_2)
  DF_not <- data.frame(c(x_1,x_3),c(rep("A", n),rep("C", n)))
  names(DF_not) <- c("point","label")
  DF_yes <- data.frame(c(x_1,x_2),c(rep("A", n),rep("B", n)))
  names(DF_yes) <- c("point","label")
  Test_not<-PF(DF_not$point,DF_not$label,10) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
  Test_yes<-PF(DF_yes$point,DF_yes$label,10) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
  Result_not[i]<-Test_not[1]
  Result_yes[i]<-Test_yes[1]
}
Result_PF<-c(sum(Result_not>=0.01),sum(Result_not<0.01),sum(Result_yes<=0.01),sum(Result_yes>0.01))
Result_PF
#### Расчет по тесту 20 ####
Result_not <- array(data = NA, c(1,N), dimnames = NULL)
Result_yes <- array(data = NA, c(1,N), dimnames = NULL)
for(i in seq(1:N)) 
{
  x_1 <- rnorm(n, mean = mean_1, sd = sigma_1)
  x_2 <- rnorm(n, mean = mean_2, sd = sigma_2)
  x_3 <- rnorm(n, mean = mean_1, sd = sigma_2)
  DF_not <- data.frame(c(x_1,x_3),c(rep("A", n),rep("C", n)))
  names(DF_not) <- c("point","label")
  DF_yes <- data.frame(c(x_1,x_2),c(rep("A", n),rep("B", n)))
  names(DF_yes) <- c("point","label")
  Test_not<-RGF(DF_not$point,DF_not$label,10) # Если p>0.01, то сработало правильно. Если p<0.01, то различия нет, но мы его нашли
  Test_yes<-RGF(DF_yes$point,DF_yes$label,10) # Если p<0.01, то сработало правильно. Если p>0.01, то различие есть, но мы его не нашли
  Result_not[i]<-Test_not[1]
  Result_yes[i]<-Test_yes[1]
}
Result_RGF<-c(sum(Result_not>=0.01),sum(Result_not<0.01),sum(Result_yes<=0.01),sum(Result_yes>0.01))
Result_RGF

Result<-rbind(Result_AF,Result_AG,Result_B2,Result_BF,Result_BX,Result_CF,Result_FA,Result_GF,Result_JF,Result_MBF,Result_MGF,Result_MW,Result_OS,Result_OSR,Result_PB,Result_PF,Result_RGF,Result_SS,Result_WA,Result_WE)
rownames(Result)<-c('AF','AG','B2','BF','BX','CF','FA','GF','JF','MBF','MGF','MW','OS','OSR','PB','PF','RGF','SS','WA','WE')
colnames(Result)<-c('TP','FP','TN','FN')
Result<-as.data.frame(Result)
Result$Recall<-Result$TP/(Result$TP+Result$FN)
Result$Selectivity<-Result$TN/(Result$TN+Result$FP)
Result$Precision<-Result$TP/(Result$TP+Result$FP)
Result$Accuracy<-(Result$TP+Result$TN)/(2*N)
Result$FOR<-Result$FN/(Result$FN+Result$TN)
Result$F_score<-2*Result$Recall*Result$Precision/(Result$Recall+Result$Precision)
Result

library(openxlsx)
write.xlsx(Result,"D:/Наука/Статьи и работы начатые/Ч003 - тесты для R/334.xlsx", sheetName="40")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

