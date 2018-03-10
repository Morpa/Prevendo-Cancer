##Prevendo Cancer:
#Coletando e carregando os dados:
dados <- read.csv("~/Documents/PROJETOS_ESTUDO/PrevendoCancer/biopsy.csv", stringsAsFactors = FALSE)
str(dados)
head(dados)

#Explorando os dados:
dados <- dados[-1]
str(dados)
any(is.na(dados))

#Transformando variável em fator:
table(dados$class)
dados$class <- factor(dados$class, levels = c("benign", "malignant"), labels = c("Benigno", "Maligno"))
str(dados)

#Verificando a prorpoção:
round(prop.table(table(dados$class)) * 100, digits = 2)

summary(dados[c("V2", "V3", "V4")])

normalizar <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

dados_norm <- as.data.frame(lapply(dados[1:9], normalizar))

summary(dados_norm[c("V2", "V3", "V4")])


#Criando os dados de Treino e de Teste:
library(class)
dados_treino <- dados_norm[1:599,]
dados_teste <- dados_norm[600:699,]
length(dados_treino)
length(dados_teste)

#Criando os labels:
dados_treino_labels <- dados[1:599, 10]
dados_teste_labels <- dados[600:699, 10]
length(dados_treino_labels)
length(dados_teste_labels)

#Criando o modelo:
modelo <- knn(train = dados_treino, 
              test = dados_teste,
              cl = dados_treino_labels, 
              k = 5)

class(modelo)

#Carregando gmodels:
install.packages("gmodels")
library(gmodels)

# Criando uma tabela cruzada dos dados previstos x dados atuais:
CrossTable(x = dados_teste_labels, y = modelo, prop.chisq = FALSE)

#Calculando taxa de erro:
prev = NULL
taxa_erro = NULL

suppressWarnings(
  for(i in 1:25){
    set.seed(101)
    prev = knn(train = dados_treino, test = dados_teste, cl = dados_treino_labels, k = i)
    taxa_erro[i] = mean(dados$class != prev)
  }
)


#Plotando taxa de erro:
install.packages("ggplot2")
library(ggplot2)
k.values <- 1:25
df_erro <- data.frame(taxa_erro, k.values)
df_erro

ggplot(df_erro, aes(x = k.values, y = taxa_erro)) +
  geom_point() + 
  geom_line(lty = "dotted", color = "red")



