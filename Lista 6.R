# Análise de dados [UFPE - 2019]
# Lista 6
# Antônio Fernandes
# Link github: https://github.com/alvesat/lista6


# Questão 9

  #Abrindo os pacotes necessários

library("tidyverse")
library("ggplot2")

  #Parte I do capítulo 7: Visualizacao

  # Gráfico de barras para visualizar uma determinada distribuição de uma variavel categorica

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

  # Computando a altura das barras

diamonds %>%
  count(cut)

  # Histograma da variável quando esta e continua 

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

  # Identificando valores presentes em cada limite do grafico anterior

diamonds %>% 
  count(cut_width(carat, 0.5))

  # Identificando outros padroes do histograma (reduzindo o tamanho das barras)

smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

  # Visualiando diversos histogramas por meio do grafico de linhas

ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

  # Identificando os diamantes pela quantidade de quilates com um histograma

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

  # Identificando o tempo de erupção em um parque para verificar eventuais clusters com um histograma

ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25)

  #Parte II do capítulo 7: Identifiando outliers

  # Identificando valores extremos

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

 
  # Acrescentando um 'zoom' no histograma para melhor visualizar os casos extremos 

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))
  
  # Identificando os valores extremos que estão no intervalo menor que 3 e maior que 20

unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>%
  arrange(y)
unusual

   #Parte III do capítulo 7: Tratando valores extremos e ausentes


  # Removendo os valores considerados outliers da análise

diamonds2 <- diamonds %>% 
  filter(between(y, 3, 20))

  # Substituindo os valores outliers por NA

diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

  # Grafico de dispersao

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()

  # Grafico de dispersao com os avisos suprimidos

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE)

  # Comparando os voos cancelados e nao cancelados (comparacao entre os casos com valores e os missings)

install.packages("nycflights13")

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)
  

  ##Parte III do capitulo 7: Variacao entre variaveis

  # Explorando a relação entre uma variável categórica e uma variável continua (qualidade
  # e preço do diamante)

      # Por meio de um gráfico de linhas

ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

      # Por meio de um gráfico de barras

ggplot(diamonds) + 
  geom_bar(mapping = aes(x = cut))

      # Por meio de um gráfico de densidade

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

      # Por meio de um boxplot

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()


  # Identificando a kilometragem em pista por tipo de veículo do banco 'mtcars'

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

  # reordenando os valores pela mediana da kilometragem

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))

  # reordenando o eixo das variaveis

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()

  # Visualizando a variacao entre duas variaveis categoricas (qualidade do diamante por cor)

ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

  # Criando uma tabela para verificar a relação entre qualidade do diamante e a cor

diamonds %>% 
  count(color, cut)

  # Grafico de calor com a relação entre qualidade do diamante e a cor

diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

  # Verificando a relacao entre duas variaveis continuas por meio de um gráfico

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

  # Grafico de dispersao com transparencia

ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100)

  # Grafico de dispersao com pontos

ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

  # BoxPlot com grupos por quilate

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

  #Boxplot de acordo com a barra por tamanho de grupo
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

  ##Parte IV do capitulo 7: Padroes e modelos


  #Identificando padrões por meio de um scatterplot 
ggplot(data = faithful) + 
  geom_point(mapping = aes(x = eruptions, y = waiting))

  #Modelo de regressao linear entre preco e quilate do banco diamantes


mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))

  # Verificando a relacao entre o corte do diamante e o residuo do modelo

ggplot(data = diamonds2) + 
  geom_boxplot(mapping = aes(x = cut, y = resid))

#Questão 10

# Lendo arquivo com as informacoes

load("C:/Users/Antonio/Documents/Dados/Listas/AD_5/Lista 6/vote_growth_usa.RData")

#Executando modelo de regressao linear

reg <- lm(Vote ~ Growth, data = bd)

#obtendo resultado do modelo 

summary(reg)

#Questão 11

# Selecionando os anos específicos

bd$Year <- as.character(bd$Year) # Convertendo para caracter 
bd$Year <- as.numeric(bd$Year) # Convertendo para numerico


vote <- bd[ which(bd$Year < 1933), ]

reg.1 <- lm(Vote ~ Growth, data = vote)

#obtendo resultado do modelo

summary(reg.1)
 

