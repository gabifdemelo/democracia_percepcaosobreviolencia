#========================================#
# VIOLENCIA E DEMOCRACIA                 #    
#========================================#
# Recife - Pernambuco - Brasil           #
# Janeiro 2019                           #    
#----------------------------------------#
                   
# Maria Gabriella Fidelis de Melo        #

#----------------------------------------#
# Any question contact the developers    #
# #UseFreeSoftware                       #
#----------------------------------------#

# instalar pacotes necessarios:
# install.packages(c( "stargazer","haven","readl", "stringr", "dplyr", "MASS","ordinal","erer", "ggplot2", ""))
# carregar pacote necessario:
library(stargazer); library(haven); library(readxl); library(stringr); library(dplyr); library(stargazer); library(MASS);
library(ordinal); library(erer); library(ggplot2); library(rcompanion)

#Banco de dados LAPOP:
LAPOP_2006 <- read_dta("Dados/Dados originais/LAPOP_2006.dta")
LAPOP_2008 <- read_dta("Dados/Dados originais/LAPOP_2008.dta")
LAPOP_2010 <- read_dta("Dados/Dados originais/LAPOP_2010.dta")
LAPOP_2012 <- read_dta("Dados/Dados originais/LAPOP_2012.dta")
LAPOP_2014 <- read_dta("Dados/Dados originais/LAPOP_2014.dta")
LAPOP_2016_2017 <- read_dta("Dados/Dados originais/LAPOP_2016:2017.dta")

# selecionar variaveis de interesse na base #


#Criar uma nova base de Dados para cada banco, com apenas as duas variaveis necessarias, em cada ano, o codigo correto e: DATANOVO <- DATAANTIGO [,c("COD1", "COD2")]
# LAPOP 2006:
lapop_2006_novo <- LAPOP_2006 [, c("ING4","A4")]
lapop_2008_novo <- LAPOP_2008[, c("ing4","a4")]
lapop_2010_novo <- LAPOP_2010 [, c("ing4","a4")]
lapop_2012_novo <- LAPOP_2012 [, c("ing4","a4")]
lapop_2014_novo <- LAPOP_2014 [, c("ing4","a4")]
lapop_2016_2017_novo <- LAPOP_2016_2017 [, c("ing4","a4")]

# criar coluna de ano para trabalhos futuros:
Lapop_2006_novo <- 2006
Lapop_2008_novo <- 2008
Lapop_2010_novo <- 2010
Lapop_2012_novo <- 2012
Lapop_2014_novo <- 2014
Lapop_2016_2017_novo <- 2017

# Renomear a coluna do banco de dados lapop_2006_novo, que esta em letra maiuscula, diferindo-se dos demais bancos que apresentam os codigos ing4 e a4 em letra minuscula:
colnames(lapop_2006_novo) <- c("ing4", "a4")

# agora, devemos juntar todas as bases de todos os anos (2006-2016/17), ou seja, recombinar usando a função "rbind":
data_modelo1 <- rbind(lapop_2006_novo,lapop_2008_novo,lapop_2010_novo,lapop_2012_novo,lapop_2014_novo,lapop_2016_2017_novo)

# Agora, todos os bancos de todos os anos estão no data_modelo1 que acabamos de criar atribuindo as bases.

####### RECODIFICAÇAO DA VARIAVEL INDEPENDENTE SALIENCIA DA VIOLENCIA (a4, no banco de dados) ###################

# Recodificar Saliencia da Violencia (a4) que esta como categorica, para numerica. Assim ela conseguira se relacionar com a VD avaliacao_democracia que e numerica:
data_modelo1 <- mutate(data_modelo1, Saliencia_Violencia = ifelse(a4 == 57 , 1, 0)) 


################## NOMEAR A VARIAVEL DEPENDENTE ING4 PARA AVALIACAO_DEMOCRACIA ##########################

colnames(data_modelo1)[1] <- "avaliacao_democracia"

#
data_modelo1$avaliacao_democracia <- factor(data_modelo1$avaliacao_democracia, 
                                            levels = c("1", "2", "3", "4","5", "6", "7"),
                                            ordered = T)

#================================#
# MODELO DE REGRESSAO LOGISTICA #
#================================#

#---------------------------------#
# MODELO 1 - Reg. Log. Ordinal
#---------------------------------#

#Transformar a vd avaliacao_democracia" em uma variavel numérica:
data_modelo1$avaliacao_democracia <- as.factor(data_modelo1$avaliacao_democracia)

# executar modelo
modelo1 <- clm(avaliacao_democracia ~ +
                 Saliencia_Violencia, data = data_modelo1)

#Tirar a prova Real com duas amostras aleatorias para observar se os resultados estão de acordo com o resultado da regressao:
dados_modelo1.5000 <- sample_n(dados_modelo1, 5000)
dados_modelo1.1000 <- sample_n(dados_modelo1, 1000)

# visualizar resultados do modelo:
summary(modelo1)
stargazer(modelo1,  type = "text", title = "Resultados Modelo 1", style = "ajps", apply.coef = exp,  p.auto=FALSE)

# Estatisticas de ajuste 
nagelkerke(fit = modelo1)

#---- plot models ----#
cp1 <- plot_odds(modelo1, "")
cp1



#=====================================
# DESCRITIVAS VD e VI

summary(data_modelo1$avaliacao_democracia)
summary(data_modelo1$Saliencia_Violencia)

#==================================
# criar um Ranking com os Temas/problemas mais graves listados pelos brasileiros, de 2006 até 2016/17 presente no artigo:

# contar respotas de tema
table_tema <- data.frame(table(data_modelo1$a4))

# criar variavel de prop.
table_tema$tema_prop <- (table_tema$Freq / (sum(table_tema$Freq))) *100
table_tema$tema_prop <- round(table_tema$tema_prop, 2)

# ordenar banco do tema.prop
table_tema <- table_tema[order(table_tema$tema_prop),]

# selecionar 10 temas mais salientes para melhor visualização do grafico:
table_tema <- table_tema[32:41,]

# nomear e checar cada nome com os codigos no survey:
table_tema$nomes <- c("Desigualdade", 
                      "Delinquência, crime, violência", 
                      "Educação, falta de, má qualidade",
                      "Outro", 
                      "Economia, problemas com, crise de", 
                      "Segurança (falta de)", 
                      "Corrupção", 
                      "Desemprego/falta de emprego",
                      "Violência" ,"Saúde, falta de serviço")


# ordernar por nomes:
table_tema$nomes <- factor(table_tema$nomes, levels = table_tema$nomes[order(table_tema$Freq)])

# plotagem do grafico:
ggplot(table_tema, aes(x = table_tema$nomes, y = table_tema$tema_prop))+
  geom_bar(stat = "identity", fill = "#30011e")+
  geom_label(aes(x = table_tema$nomes, y = table_tema$tema_prop, label = table_tema$tema_prop), vjust=0)+
  labs(x = "", y = "Porcentagem do Total de Respostas Válidas") +
  coord_flip()

#salvar:
ggsave("tema_saliencia.png", width = 8, height = 4, units = "in")



############ FIM ###########################################




















