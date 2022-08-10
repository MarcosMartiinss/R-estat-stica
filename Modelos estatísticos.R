# Qual pergunta queremos responder?
# Definindo o projeto de curso
# o que afeta a qualidade do ar nas cidades? Como?

install.packages("Ecdat")
library(Ecdat)
data(Airq) # carregando o banco de dados do pacote
names(Airq) # exibe os nomes das variáveis

# Descrevendo as variáveis

# VARIÁVEL RESPOSTA
# airq: índice de qualidade do ar (quanto menor, melhor) 

# VARIÁVEIS EXPLICATIVAS
# vala: valor das empresas nas cidades ( milhares de dólares)
# rain: quantidade de chuva (em polegadas)
# coas: posição costeira da cidade( sim ou não) variável binaria
# dens: densidade populacional (milha quadrada)
# medi: renda média per capita (dólares)

# Análise descritiva ou exploratória

summary(Airq) # sumário das variáveis

# as variáveis podem ser continuas ou categóricas (dividas em categorias)
# a variável respota é a qualidade do ar (airq)

# gráficos

plot(airq ~ coas, data=Airq)

# Criando um modelo estatístico
# y ~ x, y em função de x, o y é variável resposta e x a variável explicativa
# y (resposta) ~ x (explicativa)
# uma explica a respostada outra
# parece obvio, mas com uma grande quantidade de dados, isso pode acabar
# se complicando, então sempre converse com o seu cliente ou superior
# para saber qual resposta ele quer que seja respondida
# para mais variáveis explicativa basta colocar o +
# y ~ x1 + x2 + x3

# -----------------------------------------------------------------------------

# Montando o modelo
# modelo de regressão linear, quando temos duas variáveis continuas no modelo
# e buscamos entender a relação entre essas duas variáveis e colocar
# uma reta nelas
modelo1 = lm(airq ~ vala, data=Airq) # lm (modelo linear)
#alguns dados podem não ser lineares
# -----------------------------------------------------------------------------


# o summary do banco de dados mostra as informações das variáveis
# agora um summary do modelo mostra informações sobre análise estatística
summary(modelo1) # para saber a significância do modelo
plot(airq ~ vala, data=Airq) # plot de regressão linear

# procuramos saber se a variável explicativa tem relação com a variável resposta
# então vala tem efeito no airq? para saber isso, procuramos o p-valor no summary
# o p-valor indica a significância do modelo ou da variável
# se o p-valor < 0.05, significa que a cada 100x que você realizar o seu
# experimento, em apenas 5% das vezes ele dará um resultado diferente.
# ou um p < 0.05, indica que você tem 95% de chance de estar certo daquilo
# que você está falando
# agr se o p-valor > 0.05, não existe efeito esperado
# a variável vala n é menor que 0.05, então não é significativo 
# a variável vala não influenciou a qualidade do ar nas cidades ("airq")

# não fique buscando um p-valor, se na primeira análise o p-valor já é maior
# que 0.05, então mude a variável, mas não fique mexendo nos seus dados para
# dar um resultado menor, pois o p-valor é só um indicativo
# ---------------------------------------------------------------------------


# A variável coas afeta a variável airq?
modelo2 = lm(airq ~ coas, data = Airq)
summary(modelo2)
# a variável coas é significativo, Então as cidadades costeiras influencia
# a qualidade do ar das cidades

plot(airq ~ coas, data=Airq)
# o gráfico mostra que as cidades costeiras tem um menor indice de poluição
# no ar do que as cidades não costeira, então deve ter alguma coisa que 
# melhora a qualidade do ar, uma corrente de ar talvez?
# -----------------------------------------------------------------------------

# A variávelmedi afeta a qualidade do ar?
modelo3 = lm(airq ~ medi, data=Airq)
summary(modelo3)
# p-valor de 0.18, não tem significiância, não afeta o ar

plot(airq ~ medi, data=Airq)
# a variável não afetou a qualidade do ar

# A quantidade de chuva influencia na qualidade do ar?
modelo4 = lm(airq ~ rain, data=Airq)
summary(modelo4)
# a quantidade de chuva não afeta a qualidade do ar

# a densidade populacional afeta a qualidade do ar?
modelo5 = lm(airq ~ dens, data=Airq)
summary(modelo5)
# não existe um efeito da densidade populacional da qualidade do ar

# -----------------------------------------------------------------------------


# retas de modelos não significativos são pcionais nos gráficos, pois se não
# tem efeito , não há necessidades de colocar.

# -----------------------------------------------------------------------------

# retas nos gráficos
plot(airq ~ medi, data = Airq)

# equação da reta: y=a+b*x
# "a"(intercepto) é a onde a reta vai tocar no eixo y
# "b" é a inclinação da reta
# "x" é a variável explicativa
# para pegar os valores, basta da um summery no modelo e pegar o 
# intercept e a variável que no caso é o medi
# o add=TRUE é pra colocar a reta no mesmo gráfico
curve(9.936e+01 + 5.638e-04*x, add=TRUE)

# -----------------------------------------------------------------------------

# melhorando o gráfico
plot(airq ~ medi, data = Airq, xlab="Renda média per capita",
     ylab="Qualidade do ar",
     # estilo de bolinha no gráfico
     pch=1,
     # mudando a cor da bolina
     col="blue",
     # aumentando o tamanho da fonte em ambos os eixos
     cex.lab=1.3,
     # coloca um titulo principal
     main="Renda média - 2010")
curve(9.936e+01 + 5.638e-04*x, add=TRUE, col="darkblue", lwd=2, lty= 2)

# no momento só trabalhamos com variáveis isolada, mas essas variáveis
# podem influênciar outras variáveis, sem ser a variável resposta
# um exemplo, será que a variavel medi influência a variável coas?
# -----------------------------------------------------------------------------


# melhorando mais os gráficos 
plot(airq ~ vala, data = Airq, xlab="Valor das empresas ($)",
     ylab="Qualidade do ar",
     # estilo de bolinha no gráfico
     pch=1,
     # aumentando o tamanho das bolinhas
     cex = 2,
     # mudando a cor da bolina
     col="blue",
     # aumentando o tamanho da fonte em ambos os eixos
     cex.lab=1.3,
     # coloca um titulo principal
     main="Renda média - 2010")
curve(96.451419 + 0.001969*x, add=TRUE, col="blue", lwd=2, lty= 2)

# boxplot
plot(airq ~ coas, data = Airq, xlab="Posição costeira",
     ylab="Qualidade do ar",
     # estilo de bolinha no gráfico
     pch=1,
     # mudando a cor da bolina
     col="lightblue",
     # mudando o tamanho do eixo y
     ylim=c(50, 170),
     # aumentando o tamanho da fonte em ambos os eixos
     cex.lab=1.3,
     # coloca um titulo principal
     main="Análise da qualidade do Ar")

# pacote do ggplot2 só pra gráficos
# -----------------------------------------------------------------------------

# regressão múltipla é quando temos mais de 1 variável explicativa para
# 1 variável resposta, engloba categóricas e continuas 
# o motivo desa regressão múltipla é pqlo fato de que separadas, as variáveis
# explicativas, não tiveram muito efeito na variável resposta, mas então,
# vamos verificar se essas variáveis explicativas tem efeito entre elas.

modeloRM1 =lm(airq ~ vala + coas, data=Airq)
summary(modeloRM1)
# então existe um efeito da posição costeira e do valor das empresas na
# qualidade do ar
# -----------------------------------------------------------------------------

# gráfico regressão múltipla
# vamos mistura uma variável categórica com a contínua
# variável contínua vai ficar no eixo x e a categórica como reta
plot(airq ~ vala, data=Airq, xlab="Valor das empresas ($)",
     ylab="Qualidade do ar")
curve(1.171e+02 + 1.999e-03*x, add=TRUE) # cidade não costeira
curve(1.171e+02 + 1.999e-03*x+-2.968e+01,
      lty=2, add=TRUE) # cidade costeira
legend("bottomright", c("Não-costeiras", "Costeiras"), pch=1,
       lty=c(1,2),
       # tirando a borda da legenda
       bty="n")

# A qualdade do ar das cidades é afetada tanto pelo valor das empresas quanto
# pela posição costeira das cidades. Quanto maior o valor das emrpesas,
# pior a qualidade do ar das cidades. Além disso, as cidades não-costeiras
# apresentam qualidade do ar pior do que as cidades costeiras.

# colocando mais uma variável explicativa no modelo
modeloRM2 = lm(airq ~ vala + coas + dens, data = Airq)
summary(modeloRM2)
# á variável densidade populacional, não é significativa, mas ela influência as
# outras variáveis explicativa do nosso modelo, então não é recomendado
# retirar variável de qualquer jeito, existe métodos para se retirar variáveis.
# ----------------------------------------------------------------------------

# a densidade não foi significativas, mas será que eu posso retirar ela do meu 
# modelo? já que ela afeta outras variáveis positivamente.
# para saber se podemos retiarar essa variável, a gente faz um constraste
# de modelo, primeiro criamos um modelo completo e depois tiramos essa variável
# específica e constrastamos esses modelos com uma análise de variância 
# e a análise de variância, vai saber dizer se tem essa diferença real entre os
# modelos.

# contraste de modelos
# comparar un modelo completo com um modelo sem a variável em questão (dens)
modelocompleto = lm(airq ~ vala + coas + dens, data = Airq)
modeloimcompleto = lm(airq ~ vala + coas, data = Airq)

# os modelos são iquais?
# se p>0.5 não existe diferença entre os modelos e se não tiver diferença
# a gente fica com o modelo mais simples, pois, menos variáveis, menos custo
# agora se o p<0.05 os modelos são diferentes e a variável não deve ser retirada
# do modelo

# anova vai fazer uma análise de variância entre esses modelos e tambem outras
# estatísticas, como a média, o desvio etc.
anova(modelocompleto, modeloimcompleto)
# o p-valor foi maior que 0.05, então retiramos a variável dens, pois a variável
# não está trazendo informações significativa pra gente, então não há problema
# de retirar ela.

#-------------------------------------------------------------------------------

# Gráfico final
#-------------

plot(airq ~ vala, data=Airq, xlab="Valor das empresas ($)",
     ylab="Qualidade do ar", cex.lab= 1.3, col="blue")
curve(1.171e+02 + 1.999e-03*x, add=TRUE, col ="blue", lwd=1.4) # cidade não costeira
curve(1.171e+02 + 1.999e-03*x+-2.968e+01,
      lty=2, add=TRUE, col ="red", lwd=1.4) # cidade costeira
legend("bottomright", c("Não-costeiras", "Costeiras"), pch=1,
       lty=c(1,2),
       # tirando a borda da legenda
       bty="n", col=c("blue", "red"))
# ------------------------------------------------------------------------------

# Conclusão
# ----------

# Oque afeta a qualidade do ar nas cidades?
# As variáveis que afetaram foram: (a) o valor das empresas e (b) a posição 
# costeira das cidades. Quanto maior o valor das empresas, pior a qualidade do
# ar. Cidades costeiras apresentam uma melhor qualidade do ar.