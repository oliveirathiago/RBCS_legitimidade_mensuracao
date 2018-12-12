#####################################################################################################################
#####################################################################################################################
### Este script traz o código necessário para a replicação do artigo "Legitimidade policial: um modelo de mensuração",
### publicado pela Revista Brasileira de Ciências Sociais (RBCS). ###################################################
#####################################################################################################################
#####################################################################################################################
rm(list=ls())
library(foreign)          # Pacote foreign, para ler bancos em outros formatos
library(tcltk)            # Pacote tcltk, para seleção de diretório de trabalho
library(MplusAutomation)  # Pacote MplusAutomation, para conversão dos dados em .dat
library(dplyr)            # Pacote dplyr, para manipulação de dados
library(psych)            # Pacote psych, para diversas análises
options(scipen=999)       # Supressão de notação científica

setwd(tk_choose.dir()) # Selecione a pasta onde deseja depositar as figuras criadas
controle <- read.spss(file.choose(),    # Selecione o banco de dados
                      to.data.frame=T) 
controle <- read.spss(file.choose(),    # Selecione o banco de dados
                      to.data.frame=T) 

#####################################################################################################################
### Cleaning data: excluindo missings e ajustando as escalas ordinais
controle$P3301[controle$P3301=="Não Sabe (Esp)" | controle$P3301=="Não Respondeu (Esp)"] <- NA #excluindo missings
controle$P3302[controle$P3302=="Não Sabe (Esp)" | controle$P3302=="Não Respondeu (Esp)"] <- NA #excluindo missings
controle$P3303[controle$P3303=="Não Sabe (Esp)" | controle$P3303=="Não Respondeu (Esp)"] <- NA #excluindo missings
controle$P3304[controle$P3304=="Não Sabe (Esp)" | controle$P3304=="Não Respondeu (Esp)"] <- NA #excluindo missings
controle$P3305[controle$P3305=="Não Sabe (Esp)" | controle$P3305=="Não Respondeu (Esp)"] <- NA #excluindo missings
controle$P3306[controle$P3306=="Não Sabe (Esp)" | controle$P3306=="Não Respondeu (Esp)"] <- NA #excluindo missings
controle$P3301 <- ordered(controle$P3301, levels = c("Nunca", "Raramente", "Às vezes", "Quase sempre", "Sempre")) #pj1
controle$P3302 <- ordered(controle$P3302, levels = c("Nunca", "Raramente", "Às vezes", "Quase sempre", "Sempre")) #pj2
controle$P3303 <- ordered(controle$P3303, levels = c("Nunca", "Raramente", "Às vezes", "Quase sempre", "Sempre")) #pj3
controle$P3304 <- ordered(controle$P3304, levels = c("Nunca", "Raramente", "Às vezes", "Quase sempre", "Sempre")) #pj4
controle$P3305 <- ordered(controle$P3305, levels = c("Nunca", "Raramente", "Às vezes", "Quase sempre", "Sempre")) #pj5
controle$P3306 <- ordered(controle$P3306, levels = c("Nunca", "Raramente", "Às vezes", "Quase sempre", "Sempre")) #pj6
#
controle$P2501[controle$P2501=="Não sabe (Esp)" | controle$P2501=="Não Respondeu (Esp)" | 
                 controle$P2501=="Não se aplica (Esp)"] <- NA #excluindo missings
controle$P2502[controle$P2502=="Não sabe (Esp)" | controle$P2502=="Não Respondeu (Esp)" | 
                 controle$P2502=="Não se aplica (Esp)"] <- NA #excluindo missings
controle$P2503[controle$P2503=="Não sabe (Esp)" | controle$P2503=="Não Respondeu (Esp)" | 
                 controle$P2503=="Não se aplica (Esp)"] <- NA #excluindo missings
controle$P2504[controle$P2504=="Não sabe (Esp)" | controle$P2504=="Não Respondeu (Esp)" | 
                 controle$P2504=="Não se aplica (Esp)"] <- NA #excluindo missings
controle$P2505[controle$P2505=="Não sabe (Esp)" | controle$P2505=="Não Respondeu (Esp)" | 
                 controle$P2505=="Não se aplica (Esp)"] <- NA #excluindo missings
controle$P2506[controle$P2506=="Não sabe (Esp)" | controle$P2506=="Não Respondeu (Esp)" | 
                 controle$P2506=="Não se aplica (Esp)"] <- NA #excluindo missings
controle$P2507[controle$P2507=="Não sabe (Esp)" | controle$P2507=="Não Respondeu (Esp)" | 
                 controle$P2507=="Não se aplica (Esp)"] <- NA #excluindo missings
controle$compliance1 <- ifelse(controle$P2501=="Sim", 1, 0) #compliance with the law
controle$compliance2 <- ifelse(controle$P2502=="Sim", 1, 0) #compliance with the law
controle$compliance3 <- ifelse(controle$P2503=="Sim", 1, 0) #compliance with the law
controle$compliance4 <- ifelse(controle$P2504=="Sim", 1, 0) #compliance with the law
controle$compliance5 <- ifelse(controle$P2505=="Sim", 1, 0) #compliance with the law
controle$compliance6 <- ifelse(controle$P2506=="Sim", 1, 0) #compliance with the law
controle$compliance7 <- ifelse(controle$P2507=="Sim", 1, 0) #compliance with the law
#
controle$P33A01[controle$P33A01=="Não Sabe (Esp)" | controle$P33A01=="Não Respondeu (Esp)"] <- NA #excluindo missings
controle$P33A02[controle$P33A02=="Não Sabe (Esp)" | controle$P33A02=="Não Respondeu (Esp)"] <- NA #excluindo missings
controle$P33A03[controle$P33A03=="Não Sabe (Esp)" | controle$P33A03=="Não Respondeu (Esp)"] <- NA #excluindo missings
controle$P33A01 <- ordered(controle$P33A01, levels = c("Nunca", "Raramente", "Às vezes", "Quase sempre", "Sempre")) #alignment 1
controle$P33A02 <- ordered(controle$P33A02, levels = c("Nunca", "Raramente", "Às vezes", "Quase sempre", "Sempre")) #alignment 2
controle$P33A03 <- ordered(controle$P33A03, levels = c("Nunca", "Raramente", "Às vezes", "Quase sempre", "Sempre")) #alignment 3
controle$nal1 <- NA
controle$nal2 <- NA
controle$nal3 <- NA
controle$nal1[controle$P33A01=="Nunca"] <- 1
controle$nal1[controle$P33A01=="Raramente"] <- 2
controle$nal1[controle$P33A01=="Às vezes"] <- 3
controle$nal1[controle$P33A01=="Quase sempre"] <- 4
controle$nal1[controle$P33A01=="Sempre"] <- 5
controle$nal2[controle$P33A02=="Nunca"] <- 1
controle$nal2[controle$P33A02=="Raramente"] <- 2
controle$nal2[controle$P33A02=="Às vezes"] <- 3
controle$nal2[controle$P33A02=="Quase sempre"] <- 4
controle$nal2[controle$P33A02=="Sempre"] <- 5
controle$nal3[controle$P33A03=="Nunca"] <- 1
controle$nal3[controle$P33A03=="Raramente"] <- 2
controle$nal3[controle$P33A03=="Às vezes"] <- 3
controle$nal3[controle$P33A03=="Quase sempre"] <- 4
controle$nal3[controle$P33A03=="Sempre"] <- 5
#
controle$P4601[controle$P4601=="Não sabe (Esp)"] <- "Nem bom, nem ruim (ESP.)" #ajustando a escala
controle$P4602[controle$P4602=="Não sabe (Esp)"] <- "Nem bom, nem ruim (ESP.)" #ajustando a escala
controle$P4603[controle$P4603=="Não sabe (Esp)"] <- "Nem bom, nem ruim (ESP.)" #ajustando a escala
controle$P4604[controle$P4604=="Não sabe (Esp)"] <- "Nem bom, nem ruim (ESP.)" #ajustando a escala
controle$P4605[controle$P4605=="Não sabe (Esp)"] <- "Nem bom, nem ruim (ESP.)" #ajustando a escala
controle$P4606[controle$P4606=="Não sabe (Esp)"] <- "Nem bom, nem ruim (ESP.)" #ajustando a escala
controle$P4607[controle$P4607=="Não sabe (Esp)"] <- "Nem bom, nem ruim (ESP.)" #ajustando a escala
controle$P4601[controle$P4601=="Não tem feito nada (ESP.)"] <- "Muito ruim" #ajustando a escala
controle$P4602[controle$P4602=="Não tem feito nada (ESP.)"] <- "Muito ruim" #ajustando a escala
controle$P4603[controle$P4603=="Não tem feito nada (ESP.)"] <- "Muito ruim" #ajustando a escala
controle$P4604[controle$P4604=="Não tem feito nada (ESP.)"] <- "Muito ruim" #ajustando a escala
controle$P4605[controle$P4605=="Não tem feito nada (ESP.)"] <- "Muito ruim" #ajustando a escala
controle$P4606[controle$P4606=="Não tem feito nada (ESP.)"] <- "Muito ruim" #ajustando a escala
controle$P4607[controle$P4607=="Não tem feito nada (ESP.)"] <- "Muito ruim" #ajustando a escala
controle$P4601[controle$P4601=="Não se aplica (Esp)" | controle$P4601=="Não Respondeu (Esp)"] <- NA #excluindo missings
controle$P4602[controle$P4602=="Não se aplica (Esp)" | controle$P4602=="Não Respondeu (Esp)"] <- NA #excluindo missings
controle$P4603[controle$P4603=="Não se aplica (Esp)" | controle$P4603=="Não Respondeu (Esp)"] <- NA #excluindo missings
controle$P4604[controle$P4604=="Não se aplica (Esp)" | controle$P4604=="Não Respondeu (Esp)"] <- NA #excluindo missings
controle$P4605[controle$P4605=="Não se aplica (Esp)" | controle$P4605=="Não Respondeu (Esp)"] <- NA #excluindo missings
controle$P4606[controle$P4606=="Não se aplica (Esp)" | controle$P4606=="Não Respondeu (Esp)"] <- NA #excluindo missings
controle$P4607[controle$P4607=="Não se aplica (Esp)" | controle$P4607=="Não Respondeu (Esp)"] <- NA #excluindo missings
controle$P4601 <- ordered(controle$P4601, levels = c("Muito ruim", "Ruim", "Nem bom, nem ruim (ESP.)", "Bom", "Muito bom")) # eff1
controle$P4602 <- ordered(controle$P4602, levels = c("Muito ruim", "Ruim", "Nem bom, nem ruim (ESP.)", "Bom", "Muito bom")) # eff2
controle$P4603 <- ordered(controle$P4603, levels = c("Muito ruim", "Ruim", "Nem bom, nem ruim (ESP.)", "Bom", "Muito bom")) # eff3
controle$P4604 <- ordered(controle$P4604, levels = c("Muito ruim", "Ruim", "Nem bom, nem ruim (ESP.)", "Bom", "Muito bom")) # eff4
controle$P4605 <- ordered(controle$P4605, levels = c("Muito ruim", "Ruim", "Nem bom, nem ruim (ESP.)", "Bom", "Muito bom")) # eff5
controle$P4606 <- ordered(controle$P4606, levels = c("Muito ruim", "Ruim", "Nem bom, nem ruim (ESP.)", "Bom", "Muito bom")) # eff6
controle$P4607 <- ordered(controle$P4607, levels = c("Muito ruim", "Ruim", "Nem bom, nem ruim (ESP.)", "Bom", "Muito bom")) # eff7
#
controle$P36A01[controle$P36A01 == "NÃO SABE(ESP)" | controle$P36A01 == "NÃO RESPONDEU (ESP)"] <- NA
controle$P36A02[controle$P36A02 == "NÃO SABE(ESP)" | controle$P36A02 == "NÃO RESPONDEU (ESP)"] <- NA
controle$P36A03[controle$P36A03 == "NÃO SABE(ESP)" | controle$P36A03 == "NÃO RESPONDEU (ESP)"] <- NA
controle$P36A01 <- ordered(controle$P36A01, levels = c("Discorda totalmente", "Discorda em parte", "Não concorda nem discorda", 
                                                       "Concorda em parte", "Concorda totalmente")) #duty
controle$P36A02 <- ordered(controle$P36A02, levels = c("Discorda totalmente", "Discorda em parte", "Não concorda nem discorda", 
                                                       "Concorda em parte", "Concorda totalmente"))
controle$P36A03 <- ordered(controle$P36A03, levels = c("Concorda totalmente", "Concorda em parte", "Não concorda nem discorda",
                                                       "Discorda em parte", "Discorda totalmente"))
controle$duty1 <- NA
controle$duty2 <- NA
controle$duty3 <- NA
controle$duty1[controle$P36A01=="Discorda totalmente"] <- 1
controle$duty1[controle$P36A01=="Discorda em parte"] <- 2
controle$duty1[controle$P36A01=="Não concorda nem discorda"] <- 3
controle$duty1[controle$P36A01=="Concorda em parte"] <- 4
controle$duty1[controle$P36A01=="Concorda totalmente"] <- 5
controle$duty2[controle$P36A02=="Discorda totalmente"] <- 1
controle$duty2[controle$P36A02=="Discorda em parte"] <- 2
controle$duty2[controle$P36A02=="Não concorda nem discorda"] <- 3
controle$duty2[controle$P36A02=="Concorda em parte"] <- 4
controle$duty2[controle$P36A02=="Concorda totalmente"] <- 5
controle$duty3[controle$P36A03=="Discorda totalmente"] <- 5
controle$duty3[controle$P36A03=="Discorda em parte"] <- 4
controle$duty3[controle$P36A03=="Não concorda nem discorda"] <- 3
controle$duty3[controle$P36A03=="Concorda em parte"] <- 2
controle$duty3[controle$P36A03=="Concorda totalmente"] <- 1
controle$P25B01[controle$P25B01 == "Não sabe (Esp)" | controle$P25B01 == "Não Respondeu (Esp)"] <- NA
controle$P25B02[controle$P25B02 == "Não sabe (Esp)" | controle$P25B02 == "Não Respondeu (Esp)"] <- NA
controle$P25B03[controle$P25B03 == "Não sabe (Esp)" | controle$P25B03 == "Não Respondeu (Esp)"] <- NA
controle$P25B04[controle$P25B04 == "Não sabe (Esp)" | controle$P25B04 == "Não Respondeu (Esp)"] <- NA
controle$P25B05[controle$P25B05 == "Não sabe (Esp)" | controle$P25B05 == "Não Respondeu (Esp)"] <- NA
controle$P25B06[controle$P25B06 == "Não sabe (Esp)" | controle$P25B06 == "Não Respondeu (Esp)"] <- NA
controle$P25B07[controle$P25B07 == "Não sabe (Esp)" | controle$P25B07 == "Não Respondeu (Esp)"] <- NA
controle$P25B01 <- ordered(controle$P25B01, levels = c("Nunca", "Raramente", "Às vezes", "Quase sempre", "Sempre"))
controle$P25B02 <- ordered(controle$P25B02, levels = c("Nunca", "Raramente", "Às vezes", "Quase sempre", "Sempre"))
controle$P25B03 <- ordered(controle$P25B03, levels = c("Nunca", "Raramente", "Às vezes", "Quase sempre", "Sempre"))
controle$P25B04 <- ordered(controle$P25B04, levels = c("Nunca", "Raramente", "Às vezes", "Quase sempre", "Sempre"))
controle$P25B05 <- ordered(controle$P25B05, levels = c("Nunca", "Raramente", "Às vezes", "Quase sempre", "Sempre"))
controle$P25B06 <- ordered(controle$P25B06, levels = c("Nunca", "Raramente", "Às vezes", "Quase sempre", "Sempre"))
controle$P25B07 <- ordered(controle$P25B07, levels = c("Nunca", "Raramente", "Às vezes", "Quase sempre", "Sempre"))
controle$P25A01[controle$PP25A01 == "Não sabe ((Esp)" | controle$P24B01 == "Não Respondeu (Esp)"] <- NA
controle$P25A02[controle$PP25A02 == "Não sabe ((Esp)" | controle$P24B02 == "Não Respondeu (Esp)"] <- NA
controle$P25A03[controle$PP25A03 == "Não sabe ((Esp)" | controle$P24B03 == "Não Respondeu (Esp)"] <- NA
controle$P25A04[controle$PP25A04 == "Não sabe ((Esp)" | controle$P24B04 == "Não Respondeu (Esp)"] <- NA
controle$P25A05[controle$PP25A05 == "Não sabe ((Esp)" | controle$P24B05 == "Não Respondeu (Esp)"] <- NA
controle$P25A06[controle$PP25A06 == "Não sabe ((Esp)" | controle$P24B06 == "Não Respondeu (Esp)"] <- NA
controle$P25A07[controle$PP25A07 == "Não sabe ((Esp)" | controle$P24B07 == "Não Respondeu (Esp)"] <- NA
controle$P25A01 <- ifelse(controle$P25A01=="Errado", T, F)
controle$P25A02 <- ifelse(controle$P25A02=="Errado", T, F)
controle$P25A03 <- ifelse(controle$P25A03=="Errado", T, F)
controle$P25A04 <- ifelse(controle$P25A04=="Errado", T, F)
controle$P25A05 <- ifelse(controle$P25A05=="Errado", T, F)
controle$P25A06 <- ifelse(controle$P25A06=="Errado", T, F)
controle$P25A07 <- ifelse(controle$P25A07=="Errado", T, F)

controle$comp.sum <- NA
controle$comp.sum <- controle$compliance1 + controle$compliance2 + controle$compliance3 + controle$compliance4 +
                      controle$compliance5 + controle$compliance6 + controle$compliance7

controle$cluster_t1 <- factor(as.character(controle$cluster_t1), exclude = "Sem informação") # areas-chave

#####################################################################################################################
#####################################################################################################################

# Preparando o banco de dados para Mplus:
data.to.mplus <- controle[,c("QUEST", 
                             "P3301", "P3302", "P3303", "P3304", "P3305", "P3306",
                             "P4601", "P4602", "P4603", "P4604", "P4605", "P4606", "P4607",
                             "compliance1", "compliance2", "compliance3", "compliance4", "compliance5", "compliance6", "compliance7",
                             "P25B01", "P25B02", "P25B03", "P25B04", "P25B05", "P25B06", "P25B07",
                             "nal1", "nal2", "nal3",
                             "duty1", "duty2", "duty3",
                             "comp.sum",
                             "cluster_t1")]
 
prepareMplusData(data.to.mplus, "data.dat") #cria arquivo .dat para Mplus

#####################################################################################################################
#####################################################################################################################
