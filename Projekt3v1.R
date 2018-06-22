#Bartłomiej Fatyga
#wczytanie bibliotek
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(broom)
library(reshape2)
library(tseries)
#wczytanie danych
samochodziki <- read_csv("cars.csv")
View(samochodziki)
#zamiana jednostek
samochodziki <- samochodziki %>% mutate(mpg = (3.78541178/(mpg * 1.609344))*100, displacement = displacement * 16.38706,
                                        weight = weight*0.45359237)

#zmiana nazw kolumn
colnames(samochodziki) <- c("lp100k","cylinders","displacement", "horsepower", "weight", "acceleration", "year", "origin", "name")
str(samochodziki)
#Podstawowe statystki
summary(samochodziki[,1:7])
#wizualizacja danych
#spalanie od pojemności
samochodziki %>% 
  ggplot(aes(x = displacement,  y = lp100k, size = horsepower)) +
  geom_smooth() +
  geom_jitter(aes(col = as.factor(origin)), alpha = 0.4) +
  scale_colour_discrete(name  ="Pochodzenie",
                        labels=c("USA", "Europa", "Japonia"))
#spalanie od cylindrów
samochodziki %>% 
  ggplot(aes(x = as.factor(cylinders),  y = lp100k)) +
  geom_boxplot() +
  geom_jitter(aes(col = as.factor(origin))) +
  scale_colour_discrete(name  ="Pochodzenie",
                        labels=c("USA", "Europa", "Japonia")) 
#spalanie od koni mechanicznych
samochodziki %>%
  ggplot(aes(x = horsepower, y = lp100k)) +
  geom_point(aes(col = as.factor(origin)), alpha = 0.6) +
  geom_smooth() +
  scale_colour_discrete(name  ="Pochodzenie",
                        labels=c("USA", "Europa", "Japonia")) 
samochodziki %>%
  ggplot(aes(x = horsepower, y = lp100k)) +
  geom_point(aes(col = as.factor(origin)), alpha = 0.6) +
  geom_smooth() +
  scale_colour_discrete(name  ="Pochodzenie",
                        labels=c("USA", "Europa", "Japonia"))
#spalanie od wagi
samochodziki %>%
  ggplot(aes(x = weight, y = lp100k)) +
  geom_point(aes(col = as.factor(origin)), alpha = 0.8) +
  geom_smooth() +
  scale_colour_discrete(name  ="Pochodzenie",
                        labels=c("USA", "Europa", "Japonia")) 
#spalanie od przyspieszenia
samochodziki %>%
  ggplot(aes(x = acceleration, y = lp100k, size = horsepower)) +
  geom_jitter(aes(col = as.factor(origin)), alpha = 0.5) +
  geom_smooth() +
  scale_colour_discrete(name  ="Pochodzenie",
                        labels=c("USA", "Europa", "Japonia")) 
#spalanie od roku produkcji
samochodziki %>%
  ggplot(aes(x = as.factor(year), y = lp100k)) +
  geom_boxplot() +
  geom_jitter(aes(col = as.factor(origin)), width = .3) +
  scale_colour_discrete(name  ="Pochodzenie",
                        labels=c("USA", "Europa", "Japonia"))

#Wstępne wnioski:
#1. Pojemność jest powiązan z liczbą koni mechanicznych i pochodzeniem
#2. Cylindry również powiązane są liczbą koni mechanicznych oraz pochodzeniem
#3. Widać rosnącą zależność między liczbą koni,a spalaniem
#4. Również widać rosnącą zależność między wagą samochodu, a spalaniem
#5. Nie widać znaczącej zależności miedzy przyspieszenie, a spalaniem
#6. Rok produkcji ma duże znaczenie przy średnim spalaniu, widzimy wyraźny trend spadkowy, co do spalania w przeciągu lat
#7. Pochodzonie jest raczej związane z wielkością produkowanych samochodów, a nie ze spalaniem

#Macierz korelacji
cor(samochodziki[1:7])
corrplot(cor(samochodziki[1:7]), method = "number")
#Macierz korelacji niewiele wyjaśnia, gdyż jak zauważyliśmy wcześniej mamy korelację pomiędzy liczbą cylindrów, pojemnością, liczbą koni mechanicznych oraz wagą

#Dobór zmiennych
#Posłużymy się metodą backward selection oraz przyjrzymy się Adjusted R-Squared
summary(lm(lp100k ~ cylinders+displacement+horsepower+weight+acceleration+year+as.factor(origin), data = samochodziki))
AIC(lm(lp100k ~ cylinders+displacement+horsepower+weight+acceleration+year+as.factor(origin), data = samochodziki))
summary(lm(lp100k ~ cylinders+displacement+horsepower+weight+year+as.factor(origin), data = samochodziki))
AIC(lm(lp100k ~ cylinders+displacement+horsepower+weight+year+as.factor(origin), data = samochodziki))
summary(lm(lp100k ~ displacement+horsepower+weight+year+as.factor(origin), data = samochodziki))
AIC(lm(lp100k ~ displacement+horsepower+weight+year+as.factor(origin), data = samochodziki))
summary(lm(lp100k ~ horsepower+weight+year+as.factor(origin), data = samochodziki))
AIC(lm(lp100k ~ horsepower+weight+year+as.factor(origin), data = samochodziki))
summary(lm(lp100k ~ horsepower+weight+year, data = samochodziki)) #<---- Najlepszy model
AIC(lm(lp100k ~ horsepower+weight+year, data = samochodziki))

model_classic <- lm(lp100k ~ horsepower+weight+year, data = samochodziki)
summary(model_classic)

#szukanie outliers
model_classic %>% augment() %>% arrange(desc(.hat), .cooksd) %>% head()

#Zdecydowałem się logarytmować zmienne objaśniające dla uzyskania lepszych wyników
#LOL jednak nie
model_log10 <- lm(lp100k ~ log10(horsepower)+log10(weight)+log10(year), data = samochodziki)
summary(model_log10)
plot(lm(lp100k ~ horsepower+weight+year, data = samochodziki))

#Nie zwracać uwagi
mean(model_classic$residuals)
sd(model_classic$residuals)

#Wykres wartości rzeczywistych do reszt
samochodziki %>% 
  mutate(dopasowane = model_classic$fitted.values) %>% 
  arrange(lp100k) %>%
  mutate(index = 1:nrow(samochodziki)) %>%
  ggplot(aes(x = index)) +
  geom_point(aes(y = lp100k), color = "red") +
  geom_point(aes(y = dopasowane), color = "blue")

#Znowu nie zwracać uwagi  
coefs_mod_log10 <- summary(model_log10)$coefficients
coefs_mod_log10

#Przedziały ufności
#90% przedział ufności dla horsepower
coefs_mod_log10[2,1] + 
  qnorm(c(.05, .95)) * coefs_mod_log10[2,2]
#95%
coefs_mod_log10[2,1] + 
  qnorm(c(.025, .975)) * coefs_mod_log10[2,2]
#99%
coefs_mod_log10[2,1] + 
  qnorm(c(.005, .995)) * coefs_mod_log10[2,2]
#90% przedział ufności dla weight
coefs_mod_log10[3,1] + 
  qnorm(c(.05, .95)) * coefs_mod_log10[3,2]
#95%
coefs_mod_log10[3,1] + 
  qnorm(c(.025, .975)) * coefs_mod_log10[3,2]
#99%
coefs_mod_log10[3,1] + 
  qnorm(c(.005, .995)) * coefs_mod_log10[3,2]
#90% przedział ufności dla year
coefs_mod_log10[4,1] + 
  qnorm(c(.05, .95)) * coefs_mod_log10[4,2]
#95%
coefs_mod_log10[4,1] + 
  qnorm(c(.025, .975)) * coefs_mod_log10[4,2]
#99%
coefs_mod_log10[4,1] + 
  qnorm(c(.005, .995)) * coefs_mod_log10[4,2]

#parametr przy log10
par_log10hors_vect <- rep(NA, 10 ^ 5)
par_log10weig_vect <- rep(NA, 10 ^ 5)
par_log10year_vect <- rep(NA, 10 ^ 5)

data <- samochodziki %>% 
  mutate(log_horsepower = log10(horsepower), log_weight = log10(weight), log_year = log10(year)) %>%
  select(lp100k, log_horsepower, log_weight, log_year) %>%
  filter(!(is.na(log_horsepower + log_weight + log_year + lp100k)))
str(data)

set.seed(123)
for(i in 1:10^5){
  boot_sample <- sample(1:nrow(data), 
                        nrow(data), 
                        replace = T)
  model <- lm(lp100k ~ log_horsepower + log_weight + log_year, 
              data = data[boot_sample, ])
  par_log10hors_vect[i] <- coef(model)[2]
  par_log10weig_vect[i] <- coef(model)[3]
  par_log10year_vect[i] <- coef(model)[4]
}

#90% bootstrapowy przedzial ufnosci 
##dla horsepower
quantile(par_log10hors_vect, c(0.05, 0.95))

#95%
quantile(par_log10hors_vect, c(0.025, 0.975))

#99%
quantile(par_log10hors_vect, c(0.005, 0.995))
#90% bootstrapowy przedzial ufnosci 
##dla weight
quantile(par_log10weig_vect, c(0.05, 0.95))

#95%
quantile(par_log10weig_vect, c(0.025, 0.975))

#99%
quantile(par_log10weig_vect, c(0.005, 0.995))
#90% bootstrapowy przedzial ufnosci 
##dla year
quantile(par_log10year_vect, c(0.05, 0.95))

#95%
quantile(par_log10year_vect, c(0.025, 0.975))

#99%
quantile(par_log10year_vect, c(0.005, 0.995))


set.seed(321)
par_log10hors_df <- tibble(bootstrap = par_log10hors_vect,
                           classic = 
                             coefs_mod_log10[2,1] + 
                             rnorm(10^5) * 
                             coefs_mod_log10[2,2]) %>%
  gather(type, parameter)


par_log10hors_df %>%
  ggplot(aes(x = parameter, col = type)) + 
  geom_density()

set.seed(321)
par_log10weig_df <- tibble(bootstrap = par_log10weig_vect,
                           classic = 
                             coefs_mod_log10[3,1] + 
                             rnorm(10^5) * 
                             coefs_mod_log10[3,2]) %>%
  gather(type, parameter)


par_log10weig_df %>%
  ggplot(aes(x = parameter, col = type)) + 
  geom_density()

set.seed(321)
par_log10year_df <- tibble(bootstrap = par_log10year_vect,
                           classic = 
                             coefs_mod_log10[4,1] + 
                             rnorm(10^5) * 
                             coefs_mod_log10[4,2]) %>%
  gather(type, parameter)


par_log10year_df %>%
  ggplot(aes(x = parameter, col = type)) + 
  geom_density()




#=======================================================
#STREFA TESTÓW STREFA TESTÓW STREFA TESTÓW STREFA TESTÓW
#=======================================================

coefs_mod <- summary(model_classic)$coefficients
coefs_mod

#Przedziały ufności
#90% przedział ufności dla horsepower
coefs_mod[2,1] + 
  qnorm(c(.05, .95)) * coefs_mod[2,2]
#95%
coefs_mod[2,1] + 
  qnorm(c(.025, .975)) * coefs_mod[2,2]
#99%
coefs_mod[2,1] + 
  qnorm(c(.005, .995)) * coefs_mod[2,2]
#90% przedział ufności dla weight
coefs_mod[3,1] + 
  qnorm(c(.05, .95)) * coefs_mod[3,2]
#95%
coefs_mod[3,1] + 
  qnorm(c(.025, .975)) * coefs_mod[3,2]
#99%
coefs_mod[3,1] + 
  qnorm(c(.005, .995)) * coefs_mod[3,2]
#90% przedział ufności dla year
coefs_mod[4,1] + 
  qnorm(c(.05, .95)) * coefs_mod[4,2]
#95%
coefs_mod[4,1] + 
  qnorm(c(.025, .975)) * coefs_mod[4,2]
#99%
coefs_mod[4,1] + 
  qnorm(c(.005, .995)) * coefs_mod[4,2]

#parametr przy log10
par_hors_vect <- rep(NA, 10 ^ 5)
par_weig_vect <- rep(NA, 10 ^ 5)
par_year_vect <- rep(NA, 10 ^ 5)

data_test <- samochodziki
str(data_test)

set.seed(123)
for(i in 1:10^5){
  boot_sample <- sample(1:nrow(data_test), 
                        nrow(data_test), 
                        replace = T)
  model <- lm(lp100k ~ horsepower + weight + year, 
              data = data_test[boot_sample, ])
  par_hors_vect[i] <- coef(model)[2]
  par_weig_vect[i] <- coef(model)[3]
  par_year_vect[i] <- coef(model)[4]
}

#90% bootstrapowy przedzial ufnosci 
##dla horsepower
quantile(par_hors_vect, c(0.05, 0.95))

#95%
quantile(par_hors_vect, c(0.025, 0.975))

#99%
quantile(par_hors_vect, c(0.005, 0.995))
#90% bootstrapowy przedzial ufnosci 
##dla weight
quantile(par_weig_vect, c(0.05, 0.95))

#95%
quantile(par_weig_vect, c(0.025, 0.975))

#99%
quantile(par_weig_vect, c(0.005, 0.995))
#90% bootstrapowy przedzial ufnosci 
##dla year
quantile(par_year_vect, c(0.05, 0.95))

#95%
quantile(par_year_vect, c(0.025, 0.975))

#99%
quantile(par_year_vect, c(0.005, 0.995))


set.seed(321)
par_hors_df <- tibble(bootstrap = par_hors_vect,
                           classic = 
                             coefs_mod[2,1] + 
                             rnorm(10^5) * 
                             coefs_mod[2,2]) %>%
  gather(type, parameter)


par_hors_df %>%
  ggplot(aes(x = parameter, col = type)) + 
  geom_density()

set.seed(321)
par_weig_df <- tibble(bootstrap = par_weig_vect,
                           classic = 
                             coefs_mod[3,1] + 
                             rnorm(10^5) * 
                             coefs_mod[3,2]) %>%
  gather(type, parameter)


par_weig_df %>%
  ggplot(aes(x = parameter, col = type)) + 
  geom_density()

set.seed(321)
par_year_df <- tibble(bootstrap = par_year_vect,
                           classic = 
                             coefs_mod[4,1] + 
                             rnorm(10^5) * 
                             coefs_mod[4,2]) %>%
  gather(type, parameter)


par_year_df %>%
  ggplot(aes(x = parameter, col = type)) + 
  geom_density()

#Wykres rzeczywistych od dopasowanych w bootstrap
samochodziki %>% 
  mutate(dopasowane_bootstrap =  model_classic$coefficients[1] + horsepower*mean(par_hors_vect) + weight*mean(par_weig_vect) + year*mean(par_year_vect),
         dopasowane = model_classic$fitted.values) %>%
  arrange(lp100k) %>%
  mutate(index = 1:nrow(samochodziki)) %>%
  ggplot(aes(x = index)) +
  geom_point(aes(y = lp100k), color = "red") +
  geom_point(aes(y = dopasowane_bootstrap), color = "blue")

#Takie tam wykresiki
plot(density(model_classic$residuals))
#Jarque-Bera
jarque.bera.test(model_classic$residuals)
