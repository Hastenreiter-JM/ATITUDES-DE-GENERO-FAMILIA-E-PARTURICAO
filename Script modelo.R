# Este programa é software livre: você pode redistribuí-lo e/ou modificá-lo
# sob os termos da Licença Pública Geral GNU publicada pela Free Software Foundation,
# na versão 3 da Licença ou (a seu critério) qualquer versão posterior.

library(readxl)
library(dplyr)
library(poLCA)
library(stringr)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(MASS)
library(fitdistrplus)
library(DHARMa)

setwd("D:/UFMG/Artigos e Trabalhos/Parturição WVS/Dados 2")

# Dados -----------------------------------------------------------------------------------------------------------------------------------------------
load("D:/UFMG/Artigos e Trabalhos/Parturição WVS/Dados 2/Dados finais.RData")
rm(genero,probs,tabela1,tabela2,data5,data3)

dados2 = data4 %>%
  filter(Idade >= 47) %>% 
  dplyr::select(Ano, Peso_1000, Ano_Birth, Idade, Sexo, SMarital,Filhes,Edu,SOcu,Renda,Religião,Raça,GrupoEQ,GrupoOr) %>% 
  mutate(Coorte = 1910 + 10*findInterval(Ano_Birth, seq(1920, 2010, 10))) %>%
  mutate(Filhes = case_when(Filhes == "No children" ~ 0,
                            Filhes == "No child" ~ 0,
                            Filhes == "1 child" ~ 1,
                            Filhes == "2 children" ~ 2,
                            Filhes == "3 children" ~ 3,
                            Filhes == "4 children" ~ 4,
                            Filhes == "5 children" ~ 5,
                            Filhes == "6 children" ~ 6,
                            Filhes == "7 children" ~ 7,
                            Filhes == "8 or more children" ~ 8,
                            Filhes == "7" ~ 7,
                            Filhes == "8" ~ 8,
                            Filhes == "9" ~ 8,
                            Filhes == "10" ~ 8,
                            Filhes == "12" ~ 8,
                            Filhes == "13" ~ 8,
                            Filhes == "14" ~ 8,
                            Filhes == "18" ~ 8,
                            Filhes == "22" ~ 8,
                            .default = NA )) %>%
  mutate(Sexo = case_when(Sexo == "Female" ~ "Mulheres",Sexo == "Male" ~ "Homens"))  %>%
  mutate(Edu = case_when(Edu == "Bachelor or equivalent (ISCED 6){ISCED 6}" ~ "Terciário",
                         Edu == "Doctoral or equivalent (ISCED 8){ISCED 8}" ~ "Terciário",
                         Edu == "Master or equivalent (ISCED 7){ISCED 7}" ~ "Terciário",
                         Edu == "University - level education, with degree" ~ "Terciário",
                         Edu == "Complete secondary school: technical/ vocational type" ~ "Ensino Médio",
                         Edu == "Upper secondary education (ISCED 3){ISCED 3} " ~ "Ensino Médio",
                         Edu == "Some university-level education, without degree" ~ "Ensino Médio",
                         Edu == "Complete primary school" ~ "Ensino Fundamental",
                         Edu == "Incomplete secondary school: technical/ vocational type" ~ "Ensino Fundamental",
                         Edu == "Lower secondary education (ISCED 2){ISCED 2}" ~ "Ensino Fundamental",
                         Edu == "Incomplete primary school" ~ "Ensino Fundamental Incompleto",
                         Edu == "Early childhood education (ISCED 0) / no education{ISCED 0}" ~ "Ensino Fundamental Incompleto",
                         Edu == "No formal education" ~ "Ensino Fundamental Incompleto",
                         Edu == "Don't know{DK}" ~ NA,
                         Edu == "No answer" ~ NA,
                         Edu == "No answer{NA}" ~ NA))%>%
  mutate(SOcu = case_when(SOcu == "Full time (30 hours a week or more)" ~ "Empregado",
                         SOcu == "Full time employee (30 hours a week or more)" ~ "Empregado",
                         SOcu == "Homemaker not otherwise employed" ~ "Housemaker",
                         SOcu == "Housewife not otherwise employed" ~ "Housemaker",
                         SOcu == "Part time (less than 30 hours a week)" ~ "Empregado",
                         SOcu == "Part time employee (less than 30 hours a week)" ~ "Empregado",
                         SOcu == "Retired/ pensioned" ~ "Aposentado",
                         SOcu == "Retired/pensioned" ~ "Aposentado",
                         SOcu == "Self employed" ~ "Auto-empregado",
                         SOcu == "Student" ~ "Estudante",
                         SOcu == "Unemployed" ~ "Desempregado",
                         SOcu == "Don't know" ~ NA,
                         SOcu == "No answer" ~ NA,
                         SOcu == "Other " ~ NA)) %>%
mutate(SMarital = case_when(SMarital == "Divorced" ~ "Divorciado",
                       SMarital == "Living together as married" ~ "Casado/Unido",
                       SMarital == "Married" ~ "Casado/Unido",
                       SMarital == "Separated" ~ "Separado",
                       SMarital == "Single" ~ "Nunca Casado",
                       SMarital == "Single/Never married" ~ "Nunca Casado",
                       SMarital == "Widowed" ~ "Viúvo",
                       SMarital == "No answer" ~ NA)) %>%
  mutate(Renda = ifelse(Renda == "No answer" | Renda == "Don't know" | Renda == "Don´t know",NA, Renda))%>%
  mutate(Raça = ifelse(Raça == "No answer",NA, Raça))

dados2 = dados2 %>% 
  mutate(Edu = relevel(factor(Edu),ref="Ensino Fundamental Incompleto"))%>% 
  mutate(SOcu = relevel(factor(SOcu),ref="Aposentado"))%>% 
  mutate(SMarital = relevel(factor(SMarital),ref="Nunca Casado"))%>% 
  mutate(Renda = relevel(factor(Renda),ref="High"))%>% 
  mutate(Raça = relevel(factor(Raça),ref="BR: Caucasian (White)"))%>% 
  mutate(GrupoOr = relevel(factor(GrupoOr),ref="Nem Família e Nem Trabalho"))%>% 
  mutate(GrupoEQ = relevel(factor(GrupoEQ),ref="Equalitário")) %>%
  mutate(Bin = as.integer(ifelse(Filhes == 0, 1, 0)))


dados3 = dados2 %>% na.omit()

#summary(as.factor(dados3$Edu))
#smarital,edu,socu

summary(as.factor(dados3$Filhes))
hist(dados3$Filhes)
mean(dados3$Filhes)
var(dados3$Filhes)

PerformanceAnalytics::chart.Correlation(dados3[,c(3,4,7)], histogram=TRUE, pch=19)


plot(fitdist(dados3$Filhes, "pois"))
plot(fitdist(dados3$Filhes, "nbinom"))

## Modelo Poisson --------------------------------------------------------------------------------------------------------------------------------------------
mod_simples = glm(Filhes ~ I(Ano_Birth -1920) + GrupoEQ + GrupoOr,
                  data = dados3, weights = Peso_1000, family = poisson(link = "log"))
mod_completo = glm(Filhes ~ I(Ano_Birth -1920) + Sexo +  SMarital + Edu + Renda + Raça + GrupoEQ + GrupoOr,
                  data = dados3, weights = Peso_1000, family = poisson(link = "log"))

anova(mod_completo, test="LR")
mod_mediano = glm(Filhes ~ I(Ano_Birth -1920) + Sexo +  SMarital + Edu + GrupoEQ + GrupoOr,
                   data = dados3, weights = Peso_1000, family = poisson(link = "log"))

print(summary(mod_simples),show.residuals=TRUE)
print(summary(mod_completo),show.residuals=TRUE)

qchisq(0.95,mod_simples$df.residual);mod_simples$deviance;qchisq(0.99,mod_simples$df.residual)
# Deviance inferior ao intervalo -> menor deviance: melhor ajuste, mas idealmente deve ficar no intervalo

qchisq(0.95,mod_completo$df.residual);mod_completo$deviance;qchisq(0.99,mod_completo$df.residual)
# Deviance inferior ao intervalo -> menor deviance: melhor ajuste, mas idealmente deve ficar no intervalo

#msme::P_disp(mod_simples)

AIC(mod_simples);AIC(mod_mediano);AIC(mod_completo) # modelo completo melhor
BIC(mod_simples);BIC(mod_mediano);BIC(mod_completo) # modelo completo melhor

pchisq(mod_simples$deviance,df=mod_simples$df.residual, lower.tail=FALSE)
pchisq(mod_mediano$deviance,df=mod_mediano$df.residual, lower.tail=FALSE)
pchisq(mod_completo$deviance,df=mod_completo$df.residual, lower.tail=FALSE)

x2p = sum( residuals(mod_simples, type = "pearson")^2 );x2p;1-pchisq(x2p, mod_simples$df.residual) # 0.9999997 Quase um -> saturado?
x2p = sum( residuals(mod_completo, type = "pearson")^2 );x2p;1-pchisq(x2p, mod_completo$df.residual) # um -> saturado? n
x2p = sum( residuals(mod_mediano, type = "pearson")^2 );x2p;1-pchisq(x2p, mod_mediano$df.residual) # um -> saturado? n

#car::deltaMethod(mod_simples,"exp(Ano_Birth)"); car::deltaMethod(mod_simples,"exp(GrupoEQTradicional)")

plot(simulateResiduals(fittedModel = mod_simples)) # sobredisposição
plot(simulateResiduals(fittedModel = mod_completo)) # sobredisposição
AER::dispersiontest(mod_simples,trafo=1)
AER::dispersiontest(mod_completo,trafo=1)

anova(mod_simples,mod_completo,test="Chisq") # pvalor== 0, modelo completo é melhor
anova(mod_mediano,mod_completo,test="Chisq") # modelo completo é melhor com 90% de confiança
jtools::summ(mod_simples)

par(mfrow=c(1,2))
plot(predict(mod_simples), residuals(mod_simples), main = "Gráfico de Resíduos versus Valores Ajustados",
     ylab = "Resíduos", xlab = "mod_simples", pch = 16, col = "cadetblue4")
plot(predict(mod_completo), residuals(mod_completo), main = "Gráfico de Resíduos versus Valores Ajustados",
     ylab = "Resíduos", xlab = "mod_completo", pch = 16, col = "cadetblue4")


par(mfrow=c(1,2))
plot(predict(mod_simples), dados3$Filhes, main = "Gráfico de Resíduos versus Valores Ajustados",
     ylab = "Resíduos", xlab = "mod_simples", pch = 16, col = "cadetblue4")
plot(predict(mod_completo), dados3$Filhes, main = "Gráfico de Resíduos versus Valores Ajustados",
     ylab = "Resíduos", xlab = "mod_completo", pch = 16, col = "cadetblue4")


print(summary(mod_simples),show.residuals=TRUE)
print(summary(mod_mediano),show.residuals=TRUE)
print(summary(mod_completo),show.residuals=TRUE)
# car::deltaMethod(mod_simples,"exp(I(Ano_Birth - 1920))"); car::deltaMethod(mod_simples,"exp(GrupoEQTradicional)")

simples = data.frame(Variavel = names(coef(mod_simples)), LI = confint(mod_simples)[,1], Coef = coef(mod_simples), LS = confint(mod_simples)[,2],
                     Pvalor = summary(mod_simples)$coefficients[,4] )
completo = data.frame(Variavel = names(coef(mod_completo)), LI = confint(mod_completo)[,1], Coef = coef(mod_completo), LS = confint(mod_completo)[,2],
                     Pvalor = summary(mod_completo)$coefficients[,4] )

simples2 = cbind(Var = simples[,1],exp(simples[,-c(1,5)]), Pvalor = simples[,5])
completo2 = cbind(Var = completo[,1],exp(completo[,-c(1,5)]), Pvalor = completo[,5])

simples2$ast = ifelse(simples2$Pvalor <.001,"***",
                      ifelse(simples2$Pvalor < 0.01 & simples2$Pvalor >= .001, "**",
                             ifelse(simples2$Pvalor < 0.05 & simples2$Pvalor >= .01, "*",
                                    ifelse(simples2$Pvalor < 0.1 & simples2$Pvalor >= .05, ".",""))))
completo2$ast = ifelse(completo2$Pvalor <.001,"***",
                      ifelse(completo2$Pvalor < 0.01 & completo2$Pvalor >= .001, "**",
                             ifelse(completo2$Pvalor < 0.05 & completo2$Pvalor >= .01, "*",
                                    ifelse(completo2$Pvalor < 0.1 & completo2$Pvalor >= .05, ".",""))))

setwd("D:/UFMG/Artigos e Trabalhos/Parturição WVS")
xlsx::write.xlsx(simples2,"Resultado modelo.xlsx",sheetName = "Simples")
xlsx::write.xlsx(completo2,"Resultado modelo.xlsx",append = T,sheetName = "Completo")

tabela_eq = rbind(table(dados3$Edu,dados3$GrupoEQ),
table(dados3$Renda,dados3$GrupoEQ),
table(dados3$Raça,dados3$GrupoEQ),
table(dados3$SMarital,dados3$GrupoEQ))

tabela_or = rbind(table(dados3$Edu,dados3$GrupoOr),
table(dados3$Renda,dados3$GrupoOr),
table(dados3$Raça,dados3$GrupoOr),
table(dados3$SMarital,dados3$GrupoOr))

xlsx::write.xlsx(tabela_eq,"Composição dos grupos.xlsx",sheetName = "Equidade de Gênero")
xlsx::write.xlsx(tabela_or,"Composição dos grupos.xlsx",append = T,sheetName = "Preferência")

