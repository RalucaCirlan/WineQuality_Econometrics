#Proiect

# Cuprins:
#   Regresia simpla
#   Regresia multipla

# Fisier:
#   wineQT.csv

# Resetare mediu de lucru
rm(list = ls()) 
directory <- "C:/Users/Raluk/Desktop/Proiect_Econometrie/WineQT.csv"

# Instalarea pachetelor
PackageNames <- c(
  "tidyverse", "stargazer", "magrittr", "lmtest", "sandwich", 
  "olsrr", "moments", "whitestrap", "ggplot2", "DataCombine", "car"
)

install.packages("ggplot2")
library(ggplot2)
install.packages("lmtest")
library(lmtest)
install.packages("magrittr")
library(magrittr)
install.packages("tseries")
library(tseries)

for (i in PackageNames) {
  if (!require(i, character.only = TRUE)) {
    install.packages(i, dependencies = TRUE)
    require(i, character.only = TRUE)
  }
}
# ==================================================================================
# DOCUMENTAȚIE SET DE DATE
# ==================================================================================
# Setul de date „Wine Quality Dataset” include următoarele variabile:
#   - alcohol: Conținutul de alcool (%).
#   - quality: Scorul calității (0-10).
#   - citric.acid: Concentrația de acid citric (g/dm3).
#   - residual.sugar: Zaharuri reziduale rămase după fermentare (g/dm3).
#   - density: Densitatea vinului (kg/m3).
#   - pH: Nivelul de aciditate al vinului (0-14).

# Modul de colectare:
#   - Datele provin din teste chimice și evaluări senzoriale pe vinuri portugheze.

# Periodicitatea:
#   - Nu este temporal, fiecare rând reprezintă un lot de vin.

# Sursa:
#   - https://www.kaggle.com/datasets/yasserh/wine-quality-dataset


# ==================================================================================
# Regresia simpla - Analiza vinului
# ==================================================================================

# Citirea datelor
wine_simple <- read.csv("C:/Users/Raluk/Desktop/econometrie/Proiect_Econometrie/WineQT.csv")

# Statistici descriptive
summary(wine_simple)

# Corelarea variabilelor
cor(wine_simple)

# Tratarea valorilor lipsă
wine_simple <- na.omit(wine_simple)

#=======================================================================
# Analiza grafica
#=======================================================================

# Scatterplot: Relația dintre alcool și calitate
ggplot(data = wine_simple, aes(x = alcohol, y = quality)) +
  theme_minimal(base_size = 14) +
  geom_point(color = "darkblue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relația dintre Alcool și Calitate",
       x = "Nivelul de alcool (%)",
       y = "Calitatea vinului (scor)")

# Histogramă: Distribuția alcoolului
ggplot(wine_simple, aes(x = alcohol)) +
  theme_minimal(base_size = 14) +
  geom_histogram(bins = 20, fill = "steelblue", color = "black", alpha = 0.8) +
  labs(title = "Distribuția Nivelului de Alcool",
       x = "Nivelul de alcool (%)",
       y = "Frecvența")

# Scatterplot: Zaharuri reziduale vs. Calitate
ggplot(wine_simple, aes(x = residual.sugar, y = quality)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  theme_minimal(base_size = 14) +
  labs(title = "Zaharuri Reziduale vs. Calitate",
       x = "Zaharuri Reziduale (g/dm3)",
       y = "Calitatea vinului (scor)")

#=======================================================================
# Model de regresie simplă
#=======================================================================

# Construirea modelului
model_simple <- lm(formula = quality ~ alcohol, data = wine_simple)
summary(model_simple)

# Testarea semnificației coeficientului "alcohol"
coefficient <- coef(model_simple)["alcohol"]
se <- sqrt(vcov(model_simple)["alcohol", "alcohol"])
tstat <- coefficient / se
print(tstat)

# Calcularea p-value
p_value <- 2 * pt(abs(tstat), df = model_simple$df.residual, lower.tail = FALSE)
print(p_value)

# Bonitatea modelului
r_squared <- summary(model_simple)$r.squared
print(r_squared)

# Testări pentru ipoteze
bp_test <- bptest(model_simple)
print(bp_test)
dw_test <- dwtest(model_simple)
print(dw_test)

#=======================================================================
# Model logaritmic pentru calitate
#=======================================================================

# Crearea variabilei logaritmice
wine_simple <- wine_simple %>% mutate(lquality = log(quality))

# Construirea modelului logaritmic
model_log <- lm(formula = lquality ~ alcohol, data = wine_simple)
summary(model_log)

# Testarea heteroscedasticității
bp_test_log <- bptest(model_log)
print(bp_test_log)

# White Test
wine_simple <- wine_simple %>% mutate(alcohol_squared = alcohol^2)
model_white <- lm(formula = residuals(model_log)^2 ~ alcohol + alcohol_squared, data = wine_simple)
summary(model_white)

# Testarea autocorelației
dw_test_log <- dwtest(model_log)
print(dw_test_log)

#=======================================================================
# Corectarea autocorelației: variabilă lag pentru reziduuri
#=======================================================================

# Crearea variabilei lag
wine_simple <- wine_simple %>% mutate(residuals_log = residuals(model_log))
econ_data <- data.frame(wine_simple, resid_mod1 = residuals(model_log)) %>%
  mutate(lag1 = dplyr::lag(resid_mod1, n = 1)) %>%
  na.omit()

# Reconstruirea modelului cu variabila lag
model_with_lag <- lm(formula = lquality ~ alcohol + lag1, data = econ_data)
summary(model_with_lag)

# Retestarea heteroscedasticității și autocorelației
bp_test_final <- bptest(model_with_lag)
print(bp_test_final)
dw_test_final <- dwtest(model_with_lag)
print(dw_test_final)

#=======================================================================
# Concluzii
#=======================================================================
if (bp_test_final$p.value > 0.05) {
  print("Heteroscedasticitate eliminată.")
} else {
  print("Heteroscedasticitate încă prezentă. Se recomandă erori standard robuste.")
}

if (dw_test_final$p.value > 0.1) {
  print("Autocorelație eliminată.")
} else {
  print("Autocorelație încă prezentă. Se recomandă alte soluții, cum ar fi ARIMA sau GLS.")
}

#==================================================================================
# Regresia multipla




#==================================================================================
# Citirea datelor
wine_multiple <- read.csv("C:/Users/Raluk/Desktop/Proiect_Econometrie/WineQT.csv")

# Selectarea variabilelor relevante
wine_multiple %<>% select(quality, alcohol, sulphates, volatile.acidity)

# Verificarea valorilor variabilei dependente pentru logaritmare
summary(wine_multiple$quality)

# Aplicarea logaritmării, cu verificarea valorilor > 0
wine_multiple <- wine_multiple %>%
  mutate(lquality = ifelse(quality > 0, log(quality), NA))

# Verificarea variabilei logaritmate
summary(wine_multiple$lquality)

# Eliminarea observațiilor cu NA (dacă există)
wine_multiple <- wine_multiple %>% filter(!is.na(lquality))

# Modelul de regresie liniară
model_multiple <- lm(lquality ~ alcohol + sulphates + volatile.acidity, wine_multiple)
summary(model_multiple)

# Adăugarea valorilor previzionate și reziduurilor în dataset
wine_multiple %<>% 
  mutate(qhat = fitted(model_multiple), 
         uhat = residuals(model_multiple))

# Afișarea primelor 10 rânduri pentru verificare
wine_multiple %>% 
  select(quality, lquality, qhat, uhat) %>% 
  head(10)
#-------------------------------------------------------
# Bonitatea (R-squared si R-squared ajustat)
#-------------------------------------------------------
summary(model_multiple)
summary(model_multiple)$r.squared
summary(model_multiple)$adj.r.squared

#-------------------------------------------------------
# Testul T (semnificatia coeficientilor)
#-------------------------------------------------------

# Calcularea erorii standard pentru variabila alcohol
(coefficient1 <- coef(model_multiple)["alcohol"])
(se1 <- vcov(model_multiple) %>%
    diag %>% 
    sqrt %>% 
    .["alcohol"]) # Eroarea standard pentru variabila "alcohol"

# Calcularea statisticii t = coeficient / eroare standard
(t_stat1 <- coefficient1 / se1)

# Calcularea erorii standard pentru variabila sulphates
(coefficient2 <- coef(model_multiple)["sulphates"])
(se2 <- vcov(model_multiple) %>%
    diag %>% 
    sqrt %>% 
    .["sulphates"]) # Eroarea standard pentru variabila "sulphates"

# Calcularea statisticii t = coeficient / eroare standard
(t_stat2 <- coefficient2 / se2)

# Calcularea erorii standard pentru variabila volatile.acidity
(coefficient3 <- coef(model_multiple)["volatile.acidity"])
(se3 <- vcov(model_multiple) %>%
    diag %>% 
    sqrt %>% 
    .["volatile.acidity"]) # Eroarea standard pentru variabila "volatile.acidity"

# Calcularea statisticii t = coeficient / eroare standard
(t_stat3 <- coefficient3 / se3)

# Grade de libertate (n - k - 1)
(df_r <- model_multiple$df.residual)

# Metoda 1: Compararea statisticii t cu valoarea critica t la un nivel de semnificatie de 5% (test bilateral)
t_critic <- qt(p = 0.975, df = df_r, lower.tail = TRUE)
t_critic

# Daca t_stat > t_critic, coeficientul este semnificativ statistic

# Metoda 2: Calcularea p-value pentru testarea ipotezei

# p-value pentru test bilateral
p_value_bilateral <- 2 * pt(q = abs(t_stat1), df = df_r, lower.tail = FALSE)
p_value_bilateral

# p-value pentru test unilateral dreapta
p_value_unilateral_dreapta <- pt(q = t_stat1, df = df_r, lower.tail = FALSE)
p_value_unilateral_dreapta

# p-value pentru test unilateral stanga
p_value_unilateral_stanga <- pt(q = t_stat1, df = df_r, lower.tail = TRUE)
p_value_unilateral_stanga

# Daca p-value < 0.05 , respingem H0 si concluzionam ca coeficientul este semnificativ

# Metoda 3: Calcularea intervalului de incredere

# Interval de incredere pentru un nivel de incredere de 95%
IC_min_95 <- coefficient1 - t_critic * se1
IC_max_95 <- coefficient1 + t_critic * se1
c(IC_min_95, IC_max_95)

# Interval de incredere pentru un nivel de incredere de 90%
t_critic_90 <- qt(p = 0.95, df = df_r, lower.tail = TRUE)
IC_min_90 <- coefficient1 - t_critic_90 * se1
IC_max_90 <- coefficient1 + t_critic_90 * se1
c(IC_min_90, IC_max_90)

# Intervalele de incredere arata ca coeficientul pentru variabila "alcohol" este intre [0.323, 0.399] pentru un nivel de incredere de 95% si intre [0.329, 0.393] pentru 90%.

#-------------------------------------------------------
# Testul F (semnificatia coeficientilor)
#-------------------------------------------------------

# Definirea modelului nerestricționat (complet)
model_ur <- lm(lquality ~ alcohol + sulphates + volatile.acidity, wine_multiple)
summary(model_ur)  # Rezumatul modelului nerestricționat

#-----------------------------------
# Coeficientul volatile.acidity
#-----------------------------------
# Ipoteză: H0: beta[volatile.acidity] = 0
# Model restricționat (fără volatile.acidity)
model_r1 <- lm(lquality ~ alcohol + sulphates, wine_multiple)
summary(model_r1)

# SSR pentru modelul restricționat
(ssr_r1 <- sum(resid(model_r1)^2))

# SSR și DF pentru modelul nerestricționat
(ssr_ur <- sum(resid(model_ur)^2))
(df_ur <- model_ur$df.residual)
q <- 1  # Numărul de restricții

# Calcularea F-stat 
(F_stat <- ((ssr_r1 - ssr_ur) / q) / (ssr_ur / df_ur))

# Calcularea valorilor critice F-critical și p-value
qf(p = 0.95, df1 = 1, df2 = df_ur)
(F_pvalue <- pf(q = F_stat, df1 = 1, df2 = df_ur, lower.tail = F))

#-----------------------------------
# Coeficientul sulphates
#-----------------------------------
# Ipoteză: H0: beta[sulphates] = 0
# Model restricționat (fără sulphates)
model_r2 <- lm(lquality ~ alcohol + volatile.acidity, wine_multiple)
summary(model_r2)

# SSR pentru modelul restricționat
(ssr_r2 <- sum(resid(model_r2)^2))

# Calcularea F-statisticii
F_stat2 <- ((ssr_r2 - ssr_ur) / q) / (ssr_ur / df_ur)

# Calcularea valorii critice F și p-value
F_critical2 <- qf(p = 0.95, df1 = q, df2 = df_ur)
F_pvalue2 <- pf(q = F_stat2, df1 = q, df2 = df_ur, lower.tail = FALSE)

#-----------------------------------
# Coeficientul alcohol
#-----------------------------------
# Ipoteză: H0: beta[alcohol] = 0
# Model restricționat (fără alcohol)
model_r3 <- lm(lquality ~ volatile.acidity + sulphates, wine_multiple)
summary(model_r3)

# Calcularea SSR pentru modelul restricționat
(ssr_r3 <- sum(resid(model_r3)^2))

# Calcularea F-statisticii
F_stat3 <- ((ssr_r3 - ssr_ur) / q) / (ssr_ur / df_ur)

# Calcularea valorii critice F și p-value
F_critical3 <- qf(p = 0.95, df1 = q, df2 = df_ur)
F_pvalue3 <- pf(q = F_stat3, df1 = q, df2 = df_ur, lower.tail = FALSE)

#--------------------------------------
# Testarea simultană a coeficienților
#--------------------------------------
# Ipoteză: H0: beta[volatile.acidity] = beta[sulphates] = 0
# Model restricționat (doar cu alcohol)
model_r4 <- lm(lquality ~ alcohol, wine_multiple)
summary(model_r4)

# Calcularea SSR pentru modelul restricționat
(ssr_r4 <- sum(resid(model_r4)^2))

# Calcularea F-statisticii simultane
q2 <- 2  # Două restricții
(F_stat4 <- ((ssr_r4 - ssr_ur) / q2) / (ssr_ur / df_ur))

# Calcularea valorii critice F și p-value
F_critical4 <- qf(p = 0.95, df1 = q2, df2 = df_ur)
F_pvalue4 <- pf(q = F_stat4, df1 = q2, df2 = df_ur, lower.tail = FALSE)

# Testare simultană utilizând comanda linearHypothesis
library(car)
linearHypothesis(model_ur, c("volatile.acidity = 0", "sulphates = 0"))

#-----------------------------------
# Testarea coeficientului sulphates individual
#-----------------------------------
# Model restricționat
model_r5 <- lm(lquality ~ sulphates, wine_multiple)
summary(model_r5)

# Calcularea SSR pentru modelul restricționat
(ssr_r5 <- sum(resid(model_r5)^2))

# Calcularea F-statisticii
(F_stat5 <- ((ssr_r5 - ssr_ur) / q2) / (ssr_ur / df_ur))

#-----------------------------------
# Testarea coeficientului volatile.acidity individual
#-----------------------------------
# Model restricționat
model_r6 <- lm(lquality ~ volatile.acidity, wine_multiple)
summary(model_r6)

# Calcularea SSR pentru modelul restricționat
(ssr_r6 <- sum(resid(model_r6)^2))

# Calcularea F-statisticii
(F_stat6 <- ((ssr_r6 - ssr_ur) / q2) / (ssr_ur / df_ur))


#==================================================================================
# Ipotezele modelului de regresie
#==================================================================================

#------------------------------------------------------------------
# Ip1 - Linearitatea relației între variabile
#------------------------------------------------------------------
# Afișarea formulei modelului pentru verificarea structurii liniare
coeff <- coef(model_multiple)
cat("Modelul de regresie liniară exprimat matematic:\n")
cat("quality = ", round(coeff[1], 3), " + ", 
    round(coeff[2], 3), " * alcohol + ", 
    round(coeff[3], 3), " * sulphates + ", 
    round(coeff[4], 3), " * volatile.acidity\n")
# modeul respecta ipoteza, deoarece poate fi exprimat ca o combinatie liniara a coeficientilor si variabilelor independente

#------------------------------------------------------------------
# Ip2 - Nr de observatii > nr variabile independente
#------------------------------------------------------------------
nobs(model_multiple) > (model_multiple$rank - 1)
# rezultatul este TRUE => modeul respecta ipoteza

#------------------------------------------------------------------
# Ip3 - Modelul de regresie este corect specificat 
#------------------------------------------------------------------
# Relatia dintre variabile trebuie sa fie exprimata corect in ecuatia modelului
# Daca modelul este specificat gresit (de exemplu, relatia este non-liniara, dar modelul
# presupune o relatie liniara), rezultatele pot fi inselatoare
# Nu exista dovezi ca specificarea modelului este incorecta

#------------------------------------------------------------------
# Ip4 - Variabilele independente trebuie sa aiba varianta pozitiva
#------------------------------------------------------------------
var(wine_multiple$alcohol)
var(wine_multiple$volatile.acidity)
var(wine_multiple$sulphates) 

# alcohol = 1.171147
# acidity = 0.03226808
# sulphates = 0.02903572
# toate valorile returnate sunt pozitive => modeul respecta ipoteza

#------------------------------------------------------------------
# Ip5 - Media reziduurilor ar trebui sa fie egala cu 0
#------------------------------------------------------------------
mean(model_multiple$residuals) 
# media reziduurilor = -1.11827e-17(foarte aproape de 0) => modeul respecta ipoteza

#------------------------------------------------------------------
# Ip6 -  Multicoliniaritatea variabilelor
#------------------------------------------------------------------
vif(model_multiple) 
# alcohol: VIF = 1.045099
# sulphates: VIF = 1.084288
# volatile.acidity: VIF = 1.121242
# nu avem VIF>10 => modeul respecta ipoteza

#------------------------------------------------------------------
# Ip7 - Reziduurile nu sunt corelate cu variabilele independente
#------------------------------------------------------------------
cor.test(wine_multiple$alcohol, model_multiple$residuals) 
cor.test(wine_multiple$volatile.acidity, model_multiple$residuals)
cor.test(wine_multiple$sulphates, model_multiple$residuals) 
# p-value > 0.1 pentru toate corelatiile => modeul respecta ipoteza

#------------------------------------------------------------------
# Ip8 - Reziduurile sunt homoscedastice
#------------------------------------------------------------------
bptest(model_multiple)  # Testul Breusch-Pagan
white_test(model_multiple)  # Testul White
#  p-value > 0.05 la ambele teste deci ipoteza este acceptata

#------------------------------------------------------------------
# Ip9 - Reziduurile nu sunt autocorelate
#------------------------------------------------------------------
# Pasul 1: Vizualizarea autocorelatiei cu graficul ACF
acf(model_multiple$residuals)

# Pasul 2: Testul Durbin-Watson pentru autocorelare de ordin 1
# H0: Reziduurile nu sunt autocorelate
# H1: Reziduurile sunt autocorelate
dwtest(model_multiple) 
# p-value < 0.1 => ipoteza nula este respinsa

# Pasul 3: Testul Breusch-Godfrey pentru autocorelare de ordin superior
bgtest(model_multiple) 
bgtest(model_multiple, order = 2)  
bgtest(model_multiple, order = 3) 
# p-value < 0.1 => reziduurile sunt autocorelate

#-----------------------------------------------------------------------------
# Pentru ca reziduurile sunt autocorelate aplicam -> Corectarea autocorelatiei
#-----------------------------------------------------------------------------
# 1.Cream un nou set de date pentru corectarea autocorelației
wine_residuals_data <- data.frame(wine_multiple, resid_mod1 = model_multiple$residuals)

# 2.Cream variabila lag1 pentru reziduuri
library(DataCombine)
wine_residuals_data_1 <- slide(wine_residuals_data, Var = "resid_mod1", NewVar = "lag1", slideBy = -1)

# 3.Eliminăm valorile NA
wine_multiple1 <- na.omit(wine_residuals_data_1)

# 4.Reimplementăm modelul cu variabila lag1 adăugată
model_multiple1 <- lm(lquality ~ alcohol + sulphates + volatile.acidity + lag1, data = wine_multiple1)
summary(model_multiple1)

# 5.Verificarea reziduurilor în noul model
#Graficul ACF pentru reziduurile noului model
par(mar = c(5, 4, 2, 2)) 
acf(model_multiple1$residuals, main = "ACF pentru reziduurile modelului corectat")

# Testul Durbin-Watson si Breusch-Godfrey
dw_test_corrected <- dwtest(model_multiple1)
print(dw_test_corrected)
bgtest(model_multiple1)

#-----------------------------------------------------------------------------
# Ip10 -  Reziduurile sunt normal distribuite
# Reestimarea modelului cu eliminarea valorilor aberante pe baza Cook's Distance

# Verificăm și eliminăm outlierii pas cu pas
ols_plot_cooksd_chart(model_multiple1)

# Cream un nou set de date eliminând observațiile identificate
wine_multiple_cook <- wine_multiple1[-c(103, 922, 32, 94, 54, 144, 462, 81, 368, 514, 
                                        445, 271, 705, 904, 700, 583, 580, 707, 1055, 
                                        906, 1064, 1046, 755, 737, 1054)]

# Reestimăm modelul
model_multiple2 <- lm(lquality ~ alcohol + volatile.acidity + sulphates, wine_multiple_cook)
summary(model_multiple2)

# Vezi care sunt rândurile curente înainte de eliminare
head(wine_multiple1)

# Asigură-te că rândurile sunt numerotate corespunzător
rownames(wine_multiple1)

# Elimină din nou rândurile
wine_multiple_cook <- wine_multiple1[-c(3, 55, 74, 91, 120, 217, 56, 192, 254, 236, 
                                        103, 104, 253, 301, 302, 314, 378, 402, 430, 
                                        475, 481, 480, 502, 509, 525, 553, 554, 595, 
                                        594, 715, 718, 719, 820, 833, 820, 781, 766, 
                                        820, 833, 931, 945, 960, 961, 994, 959), ]

# Confirmă eliminarea rândurilor
dim(wine_multiple_cook)


# Verificăm reziduurile noului model
ols_plot_cooksd_chart(model_multiple2)
jarque.bera.test(model_multiple2$residuals)  # Test Jarque-Bera pentru normalitate

# Continuăm eliminarea valorilor aberante dacă reziduurile nu sunt încă normale
wine_multiple_cook1 <- wine_multiple_cook[-c(12, 134, 125, 154, 206, 486), ]
model_multiple3 <- lm(lquality ~ alcohol + volatile.acidity + sulphates, wine_multiple_cook1)
summary(model_multiple3)
jarque.bera.test(model_multiple3$residuals)

# Verificăm din nou normalitatea
wine_multiple_cook2 <- wine_multiple_cook1[-c(158, 246, 217, 291, 353, 355, 349, 624, 588, 
                                              728, 708, 753, 901, 666, 741, 183, 419, 163, 
                                              57, 272, 219, 218, 208, 125), ]
model_multiple4 <- lm(lquality ~ alcohol + volatile.acidity + sulphates, wine_multiple_cook2)
summary(model_multiple4)
jarque.bera.test(model_multiple4$residuals)  # Testul Jarque-Bera final

# Analiza grafică pentru reziduurile modelului final
par(mfrow = c(2, 2))  # Layout pentru grafice
plot(model_multiple4)  # Graficul reziduurilor
ols_plot_resid_qq(model_multiple4)  # QQ-plot pentru normalitate
ols_plot_resid_hist(model_multiple4)  # Histograma reziduurilor

# Modelul final corectat
summary(model_multiple4)


#-----------------------------------------------------------------------------

# Pasul 1: Graficul QQ-plot si histograma reziduurilor
if(!require(tseries)){install.packages('tseries')}

plot(model_multiple1) 
ols_plot_resid_qq(model_multiple1) # QQ-plot
ols_plot_resid_hist(model_multiple1)  # histograma

# Pasul 2: Analiza asimetriei si kurtosis
skewness(model_multiple1$uhat)
kurtosis(model_multiple1$uhat) 

# Pasul 3: Testul Jarque-Bera pentru normalitate
jarque.bera.test(model_multiple1$residuals) 


# Eliminarea valorilor aberante folosind Cook's Distance
ols_plot_cooksd_chart(model_multiple1) 

# Cream un nou set de date eliminand observatiile identificate
wine_multiple_cook <- wine_multiple1[-c(3,55,74,91,120,217,56,192,254,236,103,104,253,301,302,314,378,402,430,475,481,480,
                                        502,509,525,553,554,595,594,715,718,719,820,833,820,781,766,820,833,931,945,960,961,994,959), ]

# Reestimam modelul
model_multiple2 <- lm(lquality ~alcohol + volatile.acidity + sulphates, wine_multiple_cook) 
jarque.bera.test(model_multiple2$residuals) #nu sunt normal distribuite
ols_plot_cooksd_chart(model_multiple2) 

# continuam eliminarea valorilor aberante iterative
wine_multiple_cook1 <- wine_multiple_cook[-c(12,134,125,154,206,486), ]
model_multiple3 <- lm(lquality ~alcohol + volatile.acidity + sulphates, wine_multiple_cook1) 
jarque.bera.test(model_multiple3$residuals) # nu sunt normal distribuite
ols_plot_cooksd_chart(model_multiple3) 

#verificam din nou
wine_multiple_cook2 <- wine_multiple_cook1[-c(158,246,217,291,353,355,349,624,588,728,708,753,901,666,741,183,419,163,57,272,219,218,208,125), ]
model_multiple4 <- lm(lquality ~alcohol + volatile.acidity + sulphates, wine_multiple_cook2) 
jarque.bera.test(model_multiple4$residuals)  # sunt normal distribuite (p-value > 0.1)


# Modelul final corectat
model_multiple4 <- lm(lquality ~alcohol + volatile.acidity + sulphates, wine_multiple_cook2) 
summary(model_multiple4)



##Regresia multipla cu dummy + schimbare forma 
# Citește datele
wine_dummy <- read.csv("C:/Users/Raluk/Desktop/Proiect_Econometrie/WineQT.csv")
#----------------------------------------------------------------------

# Creează variabila dummy alcohol_high_low
wine_dummy %<>% mutate(alcohol_high_low = ifelse(alcohol >= median(alcohol, na.rm = TRUE), 1, 0))

# Selectează variabilele de interes
wine_dummy %<>% select(quality, alcohol, volatile.acidity, sulphates, alcohol_high_low)

# Log-transformări pentru variabilele continue
wine_dummy %<>% mutate(
  lquality = log(quality + 1),  # Transformarea logaritmică (dacă e nevoie)
  lalcohol = log(alcohol),
  lvolatile.acidity = log(volatile.acidity + 1),
  lsulphates = log(sulphates + 1)
)

# Vizualizează primele 30 rânduri pentru noile variabile
wine_dummy %>% select(lquality, lalcohol, lvolatile.acidity, lsulphates, alcohol_high_low) %>% head(30)

# Regresia liniară inițială
model_dummy <- lm(lquality ~ lalcohol + lvolatile.acidity + lsulphates + alcohol_high_low, data = wine_dummy)
summary(model_dummy)

# Adaugă fitted values și interacțiunea
wine_dummy %<>% mutate(
  lqualityhat = fitted(model_dummy), 
  alcohol_high_lowXlalcohol = alcohol_high_low * lalcohol
)

# Regresia liniară cu interacțiune
model_dummy <- lm(lquality ~ lalcohol + lsulphates + lvolatile.acidity + alcohol_high_low + alcohol_high_lowXlalcohol, data = wine_dummy)
summary(model_dummy)

# Adaugă variabile pentru "low" și "high"
wine_dummy %<>% mutate(
  alcohol_low = 1 - alcohol_high_low,
  type = factor(alcohol_high_low, levels = c(1, 0), labels = c('high', 'low'))
)

# Vizualizează datasetul actualizat
head(wine_dummy)
#-------------------------------------
ggplot(data = wine_dummy, mapping = aes(x = lalcohol, col = factor(alcohol_high_low))) + 
  theme_bw() + 
  geom_point(aes(y = lquality)) + 
  geom_line(aes(y = lqualityhat)) +
  guides(color = guide_legend(title = "Alcohol Level")) +
  labs(x = "Log of Alcohol", y = "Log of Quality", title = "Actual vs. Predicted Quality by Alcohol Level")
#Graficul arată cum valorile ajustate de model (lqualityhat) compară cu cele reale (lquality), evidențiind trenduri mai clare pentru high alcohol.


ggplot(wine_dummy, aes(x = lalcohol, y = lquality, col = factor(alcohol_high_low), group = factor(alcohol_high_low))) + 
  theme_bw() + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE, linewidth = 0.5) + 
  guides(color = guide_legend(title = "Alcohol Level")) +
  labs(x = "Log of Alcohol", y = "Log of Quality", title = "Log of Quality by Log of Alcohol")
# Graficul arată trenduri liniare între log-alcool și log-calitate, cu o relație mai puternică pentru high alcohol.



# Bonitatea modelului------------------------


# Bonitatea modelului este data de R-squared si R-squared ajuutate 
# care arata ce procent din variatie este explaicat de model


# Regresie multipla cu 3 regresori
summary(model_dummy)
summary(model_dummy)$r.squared
summary(model_dummy)$adj.r.squared

# ! Diferenta dintre R-patrat si R-patrat ajustat este ca R-patrat
# presupune ca fiecare variabila ind din model explica variatia var
# dep, in timp ce R-patrat ajustat da procentul de variatie explicat
# doar de acele variabile ind care in realitate afecteaza var dep.



# Testul T pentru semnificatia coeficientilor----------
# Afisarea coeficientilor
(coefficient1 <- coef(model_dummy)["lalcohol"])
(coefficient2 <- coef(model_dummy)["lsulphates"])
(coefficient3 <- coef(model_dummy)["lvolatile.acidity"])
(coefficient4 <- coef(model_dummy)["alcohol_high_low"])
(coefficient5 <- coef(model_dummy)["alcohol_high_lowXlalcohol"])



# Afisarea erorii standard
(se1 <- vcov(model_dummy) %>%
    diag %>% 
    sqrt %>% 
    .["lalcohol"]) # obtinerea erorii standard (se) a regresorului "lalcohol"

# Calcularea testului t = coeficient/se
(tstat1 <- coefficient1/se1)


# Afisarea erorii standard
(se2 <- vcov(model_dummy) %>%
    diag %>% 
    sqrt %>% 
    .["lsulphates"]) # obtinerea erirolor stadarde (se) ale regresorului "lsulphates"

# Calcularea testului t = coeficient/se
(tstat2 <- coefficient2/se2)

# Afisarea erorii standard
(se3 <- vcov(model_dummy) %>%
    diag %>% 
    sqrt %>% 
    .["lvolatile.acidity"]) # obtinerea erirolor stadarde (se) ale regresorului "lvolatile.acidity"

# Calcularea testului t = coeficient/se
(tstat3 <- coefficient3/se3)

# Afisarea erorii standard
(se4 <- vcov(model_dummy) %>%
    diag %>% 
    sqrt %>% 
    .["alcohol_high_low"]) # obtinerea erorii standard (se) a regresorului "alcohol_high_low"

# Calcularea testului t = coeficient/se
(tstat4 <- coefficient4/se4)

# Afisarea erorii standard
(se5 <- vcov(model_dummy) %>%
    diag %>% 
    sqrt %>% 
    .["alcohol_high_lowXlalcohol"]) # obtinerea erorii standard (se) a regresorului "alcohol_high_lowXlalcohol"

# Calcularea testului t = coeficient/se
(tstat5 <- coefficient5/se5)

# Gradele de libertate (n-k-1)
(df_r <- model_dummy$df.residual)

# t-critic la nivel de semnificatie de 5%  
qt(p = 0.975, df = df_r, lower.tail = TRUE)

# tsrat > tcritic =>  coef semnificativi statistic

#====================================================================================================================

#-------------Impartirea setului de date----------

# Incarcarea librariilor
library(tidyverse)
library(caret)
library(mltools)
library(MLmetrics)

# Instalare si activare pachete


#Impartirea setului de date si prognoza pt model multiplu-------------------------------------

training.samples <- wine_dummy$lquality %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- wine_dummy[training.samples, ]
test.data <- wine_dummy[-training.samples, ]


# Modelul REGRESIE MULTIPLA  de tip LOG-LOG 
model <- lm(lquality ~ lalcohol + lsulphates + lvolatile.acidity + alcohol_high_low, wine_dummy)
summary(model)


# Model de regresie de tip log-log pentru setul de antrenare
model_antrenare <- lm(lquality ~ lalcohol + lvolatile.acidity + lsulphates + alcohol_high_low, data = train.data) 
summary(model_antrenare)


# Predictia modelului pe setul de testare
y_pred <- predict(model_antrenare, newdata = test.data)
y_pred

# RMSE - Root Mean Squared Error =>  RMSE=0.1  < 1 => predictie buna 
RMSE(y_pred, test.data$lquality)

# MAE - Mean Absolute Error => MAE=0.09 < 1 => predictie buna 
MAE(y_pred, test.data$lquality)


# MSE - Mean Squared Error => MSE=0.01 < 1 =>
# predictie buna
MSE(y_pred, test.data$lquality)

# MAPE - Mean Absolute Percentage Error => MAPE=0.05 < 1 => predictie buna 
MAPE(y_pred, test.data$lquality)

# Out of sample forecasting 
out_of_sample <- data.frame(alcohol = c(12,12.5,14.9),
                            sulphates = c(0.89,0.82,2),
                            volatile.acidity = c(0.8,0.9,1.5),
                            alcohol_high_low = c(1,1,1)) # Nivel ridicat de alcool (dummy = 1)

# Logaritmam
out_of_sample_log <- out_of_sample %>%
  mutate(lalcohol = log(alcohol),
         lvolatile.acidity=log(volatile.acidity),
         lsulphates=log(sulphates))

# Deselectam variabilele de care nu avem nevoie
out_of_sample_log <- out_of_sample_log %>%
  select(-alcohol,-sulphates,-volatile.acidity)

# Prognoza
y_pred_outsample <- predict(model, newdata = out_of_sample_log)
y_pred_outsample
exp(y_pred_outsample) 


#----------------neverificat---------------------------------------------------------------
# Regresie Neliniara ----------------------------------------------------------------------

# Ramsey RESET-----------------------------------------
# Crearea variabilei dummy alcohol_high_low
neliniar <- neliniar %>%
  mutate(alcohol_high_low = ifelse(alcohol >= median(alcohol, na.rm = TRUE), 1, 0))

# Modelul de regresie
model_neliniar <- lm(quality ~ alcohol + sulphates + volatile.acidity + alcohol_high_low, neliniar)
summary(model_neliniar)

# Adăugăm variabilele pentru testul RESET
neliniar %<>% mutate(
  yhat = fitted(model_neliniar),     # Valori ajustate (fitted values)
  yhatsq = yhat^2,                  # Pătratul valorilor ajustate
  yhatcube = yhat^3                 # Cubul valorilor ajustate
)

# RESET, testarea semnificației comune a coeficienților yhatsq și yhatcube
model1_RESET <- update(model_neliniar, ~ . + yhatsq + yhatcube) # Adăugăm pătratul și cubul în model
summary(model1_RESET)

# Testul RESET
# H0: Modelul este bine specificat
# H1: Modelul nu este bine specificat
linearHypothesis(model1_RESET, c("yhatsq = 0", "yhatcube = 0"))

# Regresie log-lin ------------------------------------------------------
# Logaritmarea variabilei dependente
neliniar <- neliniar %>%
  mutate(lquality = log(quality))

# Modelul de regresie log-lin
model2 <- lm(lquality ~ alcohol + sulphates + volatile.acidity + alcohol_high_low, neliniar)
summary(model2)

# Adăugăm valorile ajustate logaritmice, pătratul și cubul acestora
neliniar %<>% mutate(lyhat = fitted(model2),
                     lyhatsq = lyhat^2,
                     lyhatcube = lyhat^3)

# RESET pentru modelul log-lin
model2_RESET <- update(model2, ~ . + lyhatsq + lyhatcube)
summary(model2_RESET)

# Testul RESET pentru modelul log-lin
linearHypothesis(model2_RESET, c("lyhatsq = 0", "lyhatcube = 0"))

# Introducerea termenilor pătratici ------------------------------------------------------
# Generarea pătratelor variabilelor independente
neliniar <- neliniar %>%
  mutate(alcohol_sq = alcohol^2,
         sulphates_sq = sulphates^2,
         volatile_acidity_sq = volatile.acidity^2)

# Modelul cu termeni pătratici adăugați
model3 <- lm(quality ~ alcohol + sulphates + volatile.acidity + alcohol_high_low +
               alcohol_sq + sulphates_sq + volatile_acidity_sq, neliniar)
summary(model3)

# Adăugăm valorile ajustate, pătratul și cubul acestora pentru modelul cu termeni pătratici
neliniar %<>% mutate(yhat1 = fitted(model3),
                     yhat1sq = yhat1^2,
                     yhat1cube = yhat1^3)

# RESET pentru modelul cu termeni pătratici
model3_RESET <- update(model3, ~ . + yhat1sq + yhat1cube)
summary(model3_RESET)

# Testul RESET pentru modelul cu termeni pătratici
linearHypothesis(model3_RESET, c("yhat1sq = 0", "yhat1cube = 0"))

# Modelul log-lin cu termeni pătratici ------------------------------------------------------
# Modelul log-lin care include termeni pătratici
model4 <- lm(lquality ~ alcohol + sulphates + volatile.acidity + alcohol_high_low +
               alcohol_sq + sulphates_sq + volatile_acidity_sq, neliniar)
summary(model4)

# Adăugăm valorile ajustate, pătratul și cubul acestora pentru modelul log-lin cu termeni pătratici
neliniar %<>% mutate(lyhat1 = fitted(model4),
                     lyhat1sq = lyhat1^2,
                     lyhat1cube = lyhat1^3)

# RESET pentru modelul log-lin cu termeni pătratici
model4_RESET <- update(model4, ~ . + lyhat1sq + lyhat1cube)
summary(model4_RESET)

# Testul RESET pentru modelul log-lin cu termeni pătratici
linearHypothesis(model4_RESET, c("lyhat1sq = 0", "lyhat1cube = 0"))



#============================================PANA AICI AM FACUT DUMMY============================

# Testul Chow -------------------------------------------------------------------
if (!require("strucchange")) install.packages("strucchange")
library(strucchange)
# Test pentru `alcohol`
sctest(neliniar$quality ~ neliniar$alcohol, type = "Chow", point = 14) 
# Interpretare: p-value > 0.1 => nu există rupturi

# Test pentru `volatile.acidity`
sctest(neliniar$quality ~ neliniar$volatile.acidity, type = "Chow", point = 14) 
# Interpretare: p-value > 0.1 => nu există rupturi

# Test pentru `sulphates`
sctest(neliniar$quality ~ neliniar$sulphates, type = "Chow", point = 14) 
# Interpretare: p-value > 0.1 => nu există rupturi

# Test pentru variabila dummy `alcohol_high_low`
sctest(neliniar$quality ~ neliniar$alcohol_high_low, type = "Chow", point = 14) 
# Interpretare: p-value > 0.1 => nu există rupturi



training.samples <- neliniar$quality %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- neliniar[training.samples, ]
test.data <- neliniar[-training.samples, ]

# Inspectăm grafic forma relației între `alcohol` și `quality`
ggplot(train.data, aes(alcohol, quality)) +
  geom_point() +
  stat_smooth() # Forma neliniară a relației poate fi observată


model <- lm(quality ~ alcohol, data = train.data)
summary(model)


# Prognoze pe test data
predictions <- model %>% predict(test.data)
# Performanta modelului
data.frame(
  RMSE = RMSE(predictions, test.data$quality),
  R2 = R2(predictions, test.data$quality)
)


# Inspectam grafic modelul de regresie liniara
ggplot(train.data, aes(quality,alcohol ) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)

# Implementam modelul de regresie polinomiala de ordin superior pana cand coeficientii nu mai sunt semnificativi
model2 <- lm(quality ~ alcohol + I(alcohol^2), data = train.data)
summary(model2)

model3 <- lm(quality ~ alcohol + I(alcohol^2) + I(alcohol^3), data = train.data)
summary(model3)

model4 <- lm(quality ~ alcohol + I(alcohol^2) + I(alcohol^3) + I(alcohol^4), data = train.data)
summary(model4)

# Prognoze pe setul de testare
predictions2 <- model4 %>% predict(test.data)

# Performanta modelului
performance_poly <- data.frame(
  RMSE = RMSE(predictions2, test.data$quality),
  R2 = R2(predictions2, test.data$quality)
)
print(performance_poly)

# Inspectam grafic modelul de regresie polinomiala de ordin 4
ggplot(train.data, aes(alcohol, quality)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 4, raw = TRUE))

# Spline --------------------------------------------
library(splines)

# Specificam nodurile pentru spline
knots <- quantile(train.data$alcohol, p = c(0.25, 0.5, 0.75))
model_spline <- lm(quality ~ bs(alcohol, knots = knots), data = train.data)
summary(model_spline)

# Prognoza pe setul de testare
predictions_spline <- model_spline %>% predict(test.data)

# Performanta modelului spline
performance_spline <- data.frame(
  RMSE = RMSE(predictions_spline, test.data$quality),
  R2 = R2(predictions_spline, test.data$quality)
)
print(performance_spline)

# Reprezentare grafica
ggplot(train.data, aes(alcohol, quality)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))


if (!requireNamespace("mgcv", quietly = TRUE)) {
  install.packages("mgcv")
}

# Activarea pachetului
library(mgcv)

# Modelul aditiv generalizat (GAM) --------------------------------------------
model_gam <- gam(quality ~ s(alcohol), data = train.data)
summary(model_gam)

# Prognoze pentru modelul GAM
predictions_gam <- model_gam %>% predict(test.data)

# Performanta modelului GAM
performance_gam <- data.frame(
  RMSE = RMSE(predictions_gam, test.data$quality),
  R2 = R2(predictions_gam, test.data$quality)
)
print(performance_gam)

# Reprezentare grafica pentru modelul GAM
ggplot(train.data, aes(alcohol, quality)) +
  geom_point() +
  stat_smooth(method = "gam", formula = y ~ s(x))


# Comparam acuratetea si bonitatea modelelor
# RMSE: 
# liniar = 0.6671
# polinomial = 0.6650
# spline = 0.6839
# GAM = 0.6787
# R2:
# liniar = 24.03%
# polinomial = 24.60%
# spline =  20.84%
# GAM = 21.97%



#--------------------- Metode de penalitate----------------------------------------------------------
PackageNames <- c("tidyverse", "stargazer", "magrittr", "car", "strucchange",
                  "ggplot2","caret", "splines","mgcv","glmnet","psych")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Regresia Ridge 
wine_pen <- read.csv("C:/Users/Raluk/Desktop/Proiect_Econometrie/WineQT.csv")


# Selectăm variabilele relevante
neliniar %<>% select(quality, alcohol, volatile.acidity, sulphates, alcohol_high_low)

# Transformăm variabila răspuns în logaritm
neliniar <- neliniar %>% mutate(lquality = log(quality))

# Model liniar de bază
model_penalitate <- lm(lquality ~ alcohol + sulphates + volatile.acidity + alcohol_high_low, neliniar)
summary(model_penalitate)

# Prognoză pentru un scenariu specific
prognoza <- data.frame(
  alcohol = c(14),
  sulphates = c(0.8),
  volatile.acidity = c(0.9),
  alcohol_high_low = c(1)
)
y_pred_scenariu <- predict(model_penalitate, newdata = prognoza)
y_pred_scenariu

# Definim variabila răspuns
y <- neliniar$lquality

# Definim predictorii
x <- data.matrix(neliniar[, c('alcohol', 'sulphates', 'volatile.acidity', 'alcohol_high_low')])

# Estimăm modelul Ridge (alpha = 0)
model <- glmnet(x, y, alpha = 0)
summary(model)

# Validare încrucișată pentru a găsi lambda optim
cv_model <- cv.glmnet(x, y, alpha = 0)
best_lambda <- cv_model$lambda.min
best_lambda  # Valoarea optimă pentru lambda

# Vizualizare grafică pentru validarea încrucișată
plot(cv_model)

# Reimplementăm modelul cu lambda optim
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)  # Coeficienții modelului optim

# Diagrama Trace pentru coeficienți în funcție de lambda
plot(model, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:4, legend = colnames(x), cex = .7)

# Prognoze in-sample
y_predicted <- predict(model, s = best_lambda, newx = x)

# Prognoză out-of-sample pentru un scenariu specific
new <- matrix(c(14, 0.8, 0.9, 1), nrow = 1, ncol = 4) 
y_pred_out_of_sample <- predict(best_model, s = best_lambda, newx = new)
y_pred_out_of_sample

# Calcularea R^2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse / sst
rsq  # R^2
#=====================================================================PANA AICI====================

# Regresia LASSO --------------------------------------
# Estimăm modelul LASSO (alpha = 1)
model <- glmnet(x, y, alpha = 1)

# Validare încrucișată pentru regresia LASSO
cv_model <- cv.glmnet(x, y, alpha = 1)

# Valoarea optimă a lui lambda
best_lambda <- cv_model$lambda.min
best_lambda 

# Testarea valorii lambda
plot(cv_model)

# Reimplementăm modelul cu lambda optim
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) # Coeficienții modelului optim

# Diagrama Trace pentru coeficienți în funcție de lambda
plot(model, xvar = "lambda", label = TRUE)
legend("bottomright", lwd = 1, col = 1:4, legend = colnames(x), cex = .7)

# Prognoze in-sample
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# Prognoză out-of-sample pentru un scenariu specific
new <- matrix(c(14, 0.8, 0.9, 1), nrow = 1, ncol = 4) 
predict(best_model, s = best_lambda, newx = new)

# Calcularea lui R^2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse / sst
rsq #33.77%

# Elastic Net regression -------------------------------

# Modelul Elastic Net cu validare incrucisata
model <- cv.glmnet(x, y, alpha = 0.5)
cv_model <- cv.glmnet(x, y, alpha = 0.5)

# Valoarea optima a lui lambda
best_lambda <- cv_model$lambda.min
best_lambda  # Valoarea optimă a lui lambda

# Testarea valorii lambda
plot(cv_model) 

# Reimplementam modelul cu valoarea lambda optimă
best_model <- glmnet(x, y, alpha = 0.5, lambda = best_lambda)
coef(best_model)  # Coeficienții variabilelor 

# Prognoze in-sample
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# Prognoza out-of-sample pentru un scenariu specific
new <- matrix(c(14, 0.8, 0.9, 1), nrow = 1, ncol = 4) 
predicted_out_of_sample <- predict(best_model, s = best_lambda, newx = new)
predicted_out_of_sample

# Calcularea lui R^2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse / sst
rsq  # 33.77%


# Vom compara valorile lui rsq si in functie de acestea vom alege modelul cu cea
# mai mare bonitate drept modelul optim =>Lasso cea mai buna

#-------------------------

# Algoritmul Boruta 

# Vom converti variabilele categorice în variabile factor 
convert <- c(1:5)  # Coloană indicativă - ajustează dacă este necesar
neliniar[,convert] <- data.frame(apply(neliniar[convert], 2, as.factor))

# Instalare și activare Boruta (dacă este necesar)
if(!require(Boruta)){install.packages('Boruta')}
library(Boruta)

# Algoritmul Boruta
set.seed(500)
boruta_train <- Boruta(lquality ~ ., data = neliniar, doTrace = 2)

# Rezultatele Boruta
print(boruta_train)

# Selectăm atributele importante
selected_attributes <- getSelectedAttributes(boruta_train, withTentative = TRUE)
print(selected_attributes)

# Reimplementăm un model de regresie liniară cu atributele selectate
model_boruta <- lm(lquality ~ alcohol + volatile.acidity + sulphates + alcohol_high_low, neliniar)
summary(model_boruta)

#---------------------------------------------------------------------------------------------
# Regresia cu variabila dummy ---
model_dummy1 <- lm(lquality ~ alcohol + sulphates + volatile.acidity + alcohol_high_low, neliniar)
summary(model_dummy1)

# Heteroschedasticitatea
bptest(model_dummy1)  # Breusch-Pagan test pentru heteroschedasticitate

# White Test pentru heteroschedasticitate
library(lmtest)
white_test <- bptest(model_dummy1, ~ fitted(model_dummy1) + I(fitted(model_dummy1)^2))
white_test  # Verificare homoschedasticitate

# Non-autocorelarea
acf(model_dummy1$residuals, main = "ACF pentru reziduuri - Model Dummy 1")
dwtest(model_dummy1)  # Testul Durbin-Watson
bgtest(model_dummy1, order = 2)  # Testul Breusch-Godfrey pentru autocorelare de ordin 2


# Cream un nou set de date pentru corectarea autocorrelarii
econ_data <- data.frame(neliniar, resid_mod1 = model_dummy1$residuals)

# Rezolvarea erorii: Ordonăm datele după variabila temporală 'volatile.acidity'
econ_data <- econ_data[order(econ_data$volatile.acidity), ]

# Cream variabila lag1
econ_data_1 <- slide(econ_data, Var = "resid_mod1", NewVar = "lag1", slideBy = -1)
neliniar1 <- na.omit(econ_data_1)  # Eliminam valorile NA

# Reimplementam modelul cu noua variabila lag1 adaugata in model
model_dummy2 <- lm(lquality ~ alcohol + sulphates + volatile.acidity + alcohol_high_low + lag1, data = neliniar1)
summary(model_dummy2)

# ACF pentru reziduurile modelului 2
par(mar = c(5, 5, 2, 2))  # Setari pentru margini
acf(model_dummy2$residuals, main = "ACF pentru reziduuri - Model Dummy 2")

# Durbin-Watson pentru modelul 2
dwtest(model_dummy2)  # p-value > 0.1 => reziduuri non-autocorelate

# Breusch-Godfrey pentru modelul 2
bgtest(model_dummy2)  # p-value > 0.1 => reziduuri non-autocorelate
bgtest(model_dummy2, order = 2)  # p-value > 0.1 => reziduuri non-autocorelate

# Normalitate
ggplot(neliniar1, aes(sample = model_dummy2$residuals)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot pentru Reziduuri")

library(tseries)
jarque.bera.test(model_dummy2$residuals)  # Testul Jarque-Bera pentru normalitate

