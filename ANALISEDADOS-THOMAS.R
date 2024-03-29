#importando planilha no formato excel
library(readxl)

#ler arquivo em excel 
ipeadatavoos <- read_excel("ipeadatavoos.xlsx")

#mostrando ao usuario a tabela em uma melhor exibi��o
View(ipeadatavoos)

#exibindo alguns valores b�sicos 
summary(ipeadatavoos)

# Armazenando valores em variaveis para c�lculos est�tisticos
# soma
soma <- sum(ipeadatavoos[2])
soma

#tamanho de linhas de dados da tabela 
tamanhotable <- length(ipeadatavoos$Data)
tamanhotable

#media
media <- soma/tamanhotable
media

#mediana
mediana <- median(ipeadatavoos$`Transporte a�reo - horas voadas - voos internacionais - Hora - Ag�ncia Nacional de Avia��o Civil (Anac) - ANAC_HVINT -`)
mediana

#desviopadr�o
desviopadrao <- sd(ipeadatavoos$`Transporte a�reo - horas voadas - voos internacionais - Hora - Ag�ncia Nacional de Avia��o Civil (Anac) - ANAC_HVINT -`)
desviopadrao

#varian�a
varianca <- sqrt(desviopadrao)
varianca

# valor maximo
valmax <- max(ipeadatavoos$`Transporte a�reo - horas voadas - voos internacionais - Hora - Ag�ncia Nacional de Avia��o Civil (Anac) - ANAC_HVINT -`)
valmax

#valor minimo
valmim <- min(ipeadatavoos$`Transporte a�reo - horas voadas - voos internacionais - Hora - Ag�ncia Nacional de Avia��o Civil (Anac) - ANAC_HVINT -`)
valmim

#coeficiente de varia��o
cvar <- (desviopadrao/media)*100
cvar

#Moda
Moda <- "AMODAL"
Moda

#Quartis
quartis <- quantile(ipeadatavoos$`Transporte a�reo - horas voadas - voos internacionais - Hora - Ag�ncia Nacional de Avia��o Civil (Anac) - ANAC_HVINT -`)
quartis

#exibindo gr�fico de histograma
hist(ipeadatavoos$`Transporte a�reo - horas voadas - voos internacionais - Hora - Ag�ncia Nacional de Avia��o Civil (Anac) - ANAC_HVINT -`)

#Observando a frequ�ncia de horas de voos absolutas
freq <- table (ipeadatavoos$`Transporte a�reo - horas voadas - voos internacionais - Hora - Ag�ncia Nacional de Avia��o Civil (Anac) - ANAC_HVINT -`)
freq

#tabela de frequ�ncia relativas
freq_rel <- prop.table(freq)
freq_rel

#porcentagem (100*freq_rel_table)
porcentagem_freq_rel <- 100* prop.table(freq_rel)
porcentagem_freq_rel

#frequencia acumulada
freq_ac <- cumsum(ipeadatavoos$`Transporte a�reo - horas voadas - voos internacionais - Hora - Ag�ncia Nacional de Avia��o Civil (Anac) - ANAC_HVINT -`)
freq_ac

#inserir dados na tabela e criar linha de total
freq <- c(freq,sum(freq))
freq_rel <- c(freq_rel, sum(freq_rel))
porcentagem_freq_rel <- c(porcentagem_freq_rel, sum(porcentagem_freq_rel))
names(freq)[43] <- "TOTAL"

#varia��o das horas de voo
title(main = "Horas de voo por ano")
barplot(ipeadatavoos$`Transporte a�reo - horas voadas - voos internacionais - Hora - Ag�ncia Nacional de Avia��o Civil (Anac) - ANAC_HVINT -`, names.arg = ipeadatavoos$Data)

#tabela final, organizando por coluna, e usando um arredondamento de valores"round"
tabelaf <- cbind (freq, freq_rel= round(freq_rel,digits = 2), porcentagem_freq_rel= round(porcentagem_freq_rel, digits = 2),freq_ac= round(freq_ac, digits = 2))
tabelaf
