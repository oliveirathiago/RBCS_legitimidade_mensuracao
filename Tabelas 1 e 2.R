#####################################################################################################################
#####################################################################################################################
### Este script traz os códigos para replicar as Tabelas 1 e 2 do artigo.

library(foreign)          # Pacote foreign, para ler bancos em outros formatos
library(tcltk)            # Pacote tcltk, para seleção de diretório de trabalho
library(dplyr)            # Pacote dplyr, para manipulação de dados
library(psych)            # Pacote psych, para diversas análises
options(scipen=999)       # Supressão de notação científica

setwd(tk_choose.dir()) # Selecione o diretório de trabalho

controle <- read.spss(file.choose(),    # Selecione o banco de dados
                      to.data.frame=T)

scores <- read.table("scores.txt") # lê o arquivo com os escores do fator "legitimidade policial estimado no Mplus

colnames(scores)[7] <- "QUEST"
colnames(scores)[12] <- "legitimacy"
df <- merge(controle, scores, by="QUEST") # data frame com os escores do fator criado

alp <- alpha(controle[,c("nal1", "nal2", "nal3", "duty1", "duty2")]) # Criação do índice formativo de legitimidade
cor(df$legitimacy, alp$scores) # Correlação entre a variável latente e o índice formativo

Tabela1 <- group_by(df, P48) %>% summarise(m = mean(legitimacy), sd = sd(legitimacy)) # Tabela 1
Tabela2 <- group_by(df, P4701) %>% summarise(m = mean(legitimacy), sd = sd(legitimacy)) # Tabela 2
