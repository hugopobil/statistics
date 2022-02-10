
# Hugo Pasqual del Pobil
# Andres Mahias

### Objetivo: 
#   Realizar un an?lisis exploratorio multidimensional (num?rico y gr?fico) de un conjunto de 
#   variables cuantitativas (que est?n relacionadas entre s?) y al mensos una variable categ?rica. La 
#   tem?tica de los datos es a elecci?n del grupo.

### Variables: 
#   Se elige un conjunto de datos que contenga variables cuantitativas correlacionadas y 
#   al menos una variable categ?rica. 

### Analisis:
#   Se estudiar?n anal?tica y gr?ficamente las dependencias entre las variables y a trav?s de 
#   los niveles de la variable categ?rica. 
#   Se incluir?n gr?ficos m?ltiples de caja, histogramas, gr?ficos matriciales etc. 
#   Se estudiar?n agrupaciones de individuos, si procede (no procede)
#   Se obtendr? alg?n modelo de regresi?n simple
#   Se contar? todo en t?rminos de los objetivos previamente establecidos

# TRABAJO FINAL FUNDAMENTOS -----------------------------------------------

rm(list=ls())
library(quantmod)
library(ggplot2)
library(dplyr)
library(corrgram)
library(Hmisc)
library(broom)
library(magrittr)
library(PortfolioAnalytics)

getSymbols("WBK", from="2019-01-01", to="2021-07-01")
getSymbols('REG', from="2019-01-01", to="2021-07-01")
getSymbols('ALV.DE', from="2019-01-01", to="2021-07-01")
getSymbols('BAYN.DE', from="2019-01-01", to="2021-07-01")
getSymbols('ISP.MI', from="2019-01-01", to="2021-07-01")
getSymbols("MSCI", from="2019-01-01", to="2021-07-01")

# Con motivo de diferente número de datos al importar (por diferencias
# en los días laborables de las bolsas), recortamos los datos para
# que cuadren perfectamente.

WBK <- WBK[1:629]
REG <- REG[1:629]
ALV.DE <- ALV.DE[1:629]
BAYN.DE <- BAYN.DE[1:629]
ISP.MI <- ISP.MI[1:629]
MSCI <- MSCI[1:629]

# ISP.MI tiene valores NA. Para los gráficos los vamos a mantener
# pero a la hora de hacer calculos exploratorios debemos quitarlos.

df_cotizaciones <- data.frame(WBK[,6],
                              REG[,6],
                              ALV.DE[,6],
                              BAYN.DE[,6],
                              ISP.MI[,6],
                              MSCI[,6])
df_cotizaciones
# Visualizamos los datos
head(df_cotizaciones)
# Dimensiones del dataframe
dim(df_cotizaciones)

# Gráfico de evolución de precios: este gráfico nos permite visualizar con
# más facilidad la evolución de los precios que representándolos todos
# juntos, y evita complicaciones al establecer el mismo punto de inicio
# a nivel gráfico para todos los activos

tidy(as.xts(df_cotizaciones)) %>% 
  
  ggplot(aes(x=index,y=value, color=series)) + 
  geom_line() +
  facet_grid(series~.,scales = "free") + 
  labs(title = "Evolución del precio de las acciones seleccionadas",
       
       subtitle = "End of Day Adjusted Prices",
       caption = " Fuente: Paquete Quantmod") +
  
  xlab("Fecha") + ylab("Precio") +
  scale_color_manual(values = c("Red", "Black", "DarkBlue","Orange","Brown","Purple"))



# Boxplot for all Adjusted Close Prices
# Observamos una dispersi?n del camino intercuart?lico variada para los stocks
# elegidos.
# ALV.DE presenta menos volatilizad en el precio ajustado que los 
# otros comparables


# Visualizaci?n -----------------------------------------------------------
# Box plot for Adjusted Prices
par(mfrow=c(2,3))
for (i in 1:6){
  boxplot(df_cotizaciones[,i],
          main=colnames(df_cotizaciones[i]),
          xlab="",
          col="orange")
}

# Histogram for Adjusted Prices
par(mfrow=c(2,3))
for (j in 1:6) {
  hist(df_cotizaciones[,j],
       main=colnames(df_cotizaciones)[j],
       xlab="",
       col="orange")
}

# La frequencia de precio indica cual es el precio mas estable para el stock
# 3 de 4 muestran una frecuencia en los valores precios mas elevados
par(mfrow=c(2,3))
for (j in 1:6) {
  plot(density(df_cotizaciones[,j],
               kernel="gaussian"),
       main=colnames(df_cotizaciones)[j],
       xlab="",
       col="orange",
       lwd=2)
}

par(mfrow=c(2,3))
for (j in 1:6) {
  plot(density(df_cotizaciones[,j],
               kernel="gaussian"),
       main=colnames(df_cotizaciones)[j],
       xlab="",
       col="orange",
       lwd=1)
  
  lines(density(df_cotizaciones[,j],
                kernel="epanechnikov"),
        main=colnames(df_cotizaciones)[j],
        xlab="",
        col="blue",
        lwd=1)
}

# Visualization of price evolution for stocks selected
par(mfrow=c(1,1))

index <- seq(1,629,1)

ggplot(df_cotizaciones) +
  geom_line(aes(y = WBK.Adjusted / max(WBK.Adjusted),
                x = index), colour = "green") +
  
  geom_line(aes(y = REG.Adjusted / max(REG.Adjusted),
                x = index), colour = "blue") +
  
  geom_line(aes(y = ALV.DE.Adjusted / max(ALV.DE.Adjusted),
                x = index), colour = "red") +
  
  geom_line(aes(y = BAYN.DE.Adjusted / max(BAYN.DE.Adjusted),
                x = index), colour = "orange") +
  
  geom_line(aes(y = ISP.MI.Adjusted / max(ISP.MI.Adjusted),
                x = index), colour = "yellow") +
  
  geom_line(aes(y = MSCI.Adjusted / max(MSCI.Adjusted),
                x = index), colour = "black") +
  
  labs(x = "Día", y = "%", title = "Rendimientos diarios / Máximo rendimiento diario")


# Se representan los datos como porcentage de sus maximos para poder 
# comparar la evolucion

# Covarianza y Correlaciones ----------------------------------------------
cov_cotizaciones <- cov(df_cotizaciones)
cov_cotizaciones

# Autovalores y autovectores de S
eigen(cov_cotizaciones) 
eigen(cov_cotizaciones)$values

# Traza de S
sum(eigen(cov_cotizaciones)$values)
det(cov_cotizaciones)

cor_cotizaciones <- cor(df_cotizaciones)
cor_cotizaciones

# Autovalores y autovectores de R
eigen(cor_cotizaciones) 
eigen(cor_cotizaciones)$value

# Traza de S
sum(eigen(cor_cotizaciones)$values) 
det(cor_cotizaciones) 

# Vector de medias de un subconjunto
colMeans(df_cotizaciones[,1:5])

# Rendimientos ------------------------------------------------------------
n <- dim(df_cotizaciones)[1]
n
return_WBK<- df_cotizaciones$WBK.Adjusted[2:n] /df_cotizaciones$WBK.Adjusted[1:n-1] - 1
return_REG<- df_cotizaciones$REG.Adjusted[2:n] /df_cotizaciones$REG.Adjusted[1:n-1] - 1
return_ALV.DE<- df_cotizaciones$ALV.DE.Adjusted[2:n] /df_cotizaciones$ALV.DE.Adjusted[1:n-1] - 1
return_BAYN.DE<- df_cotizaciones$BAYN.DE.Adjusted[2:n] /df_cotizaciones$BAYN.DE.Adjusted[1:n-1] - 1
return_ISP.MI<- df_cotizaciones$ISP.MI.Adjusted[2:n] /df_cotizaciones$ISP.MI.Adjusted[1:n-1] - 1
return_MSCI <- df_cotizaciones$MSCI.Adjusted[2:n] / df_cotizaciones$MSCI.Adjusted[1:n-1] - 1

# Creamos un tibble (d.f) con los rendimientos de los activos.

df_returns <- data.frame(
  return_WBK = return_WBK,
  return_REG = return_REG,
  return_ALV.DE = return_ALV.DE,
  return_BAYN.DE = return_BAYN.DE,
  return_ISP.MI = return_ISP.MI,
  return_MSCI = return_MSCI)

# Le añadimos la fecha al data.frame, sacándola del dataframe cotizaciones.
rownames(df_returns) <- rownames(df_cotizaciones[2:629,])

df_returns
# This graph will help us determine which returns are more volatile
# Se disponen los colores mas oscuros a pricipio para que sea mas visual 
# y representativo

index_returns <- seq(1,628,1)
par(mfrow=c(1,1))
plot.default(index_returns, return_WBK, type = "l", col ="green")
lines(index_returns, return_REG, type = "l", col ="blue")
lines(index_returns, return_ALV.DE, type = "l", col ="red")
lines(index_returns, return_BAYN.DE, type = "l", col ="orange")
lines(index_returns, return_ISP.MI, type = "l", col ="yellow")
lines(index_returns, return_MSCI, type = "l", col ="black")


summary_function <- function(portfolio){
  summary <- summary(portfolio)
  return(summary)
}

# Comprobamos que funciona
summary_function(ISP.MI)
summary_function(WBK)
summary_function(REG)
summary_function(BAYN.DE)
summary_function(ALV.DE)
summary_function(MSCI)

# El siguiente summary muestra el resumen de los retornos de los stocks
# Se multiplican los valores por 100, al ser porcentages
# Se puede observar cuales son los stocks mas volatiles
df_summary_returns <- sapply(df_returns, summary_function) * 100
df_summary_returns

# Variable Dummy ----------------------------------------------------------
p_break <- 303 

# El punto de break covid es 303, correspondiente al 16 de marzo.

covid <- c(rep(0,p_break),rep(1,nrow(df_cotizaciones)-p_break))

rcovid <- covid[2:n]

#----------------------------------------------------------------------
# Box-Plot con variable categórica
# Rendimientos antes y después del covid
#----------------------------------------------------------------------

# Usamos log por que si no, los resultados del boxplot
# no nos son útiles (bunching en 0)

par(mfrow=c(2,3))
for(j in 1:6) {
  boxplot(
    log(df_returns[[j]]) ~ rcovid, main=colnames(df_returns)[j],ylab="",xlab ="",col="orange"
  )
}


# Rendimientos (a secas)
# Tambien usamos log.

par(mfrow=c(2,3))
for (j in 1:6) {
  boxplot(log(df_returns[,j]), main=colnames(df_returns)[j], xlab="", col="orange")
}

# Saltan warnings de que se están omitiendo datos -Inf (son NAs),
# pero el código funciona bien.


### Parejas de Series ###

pairs(df_returns, panel= panel.smooth, main= "Rendimientos", col="orange")


# Matrices de covarianza y correlaciones
cov(df_returns)
cor(df_returns)

matrizcorr <- rcorr(as.matrix(df_returns), type="pearson")

# Matriz de correlaciones
matrizcorr$r

# Matriz de p-valores
matrizcorr$P

# Correlograma
par(mfrow=c(1,1))
corrgram(df_returns, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, main="Correlaciones entre rendimientos") 

# Test de Shapiro (hipótesis nula = normalidad)
sapply(seq(1,6), function(j) shapiro.test(df_returns[[j]]))

# Gráficos de Regresión simple

WBK_regresion <- lm(df_returns$return_WBK ~ df_returns$return_MSCI)
ISP.MI_regresion <- lm(df_returns$return_ISP.MI ~ df_returns$return_MSCI)
REG_regresion <- lm(df_returns$return_REG ~ df_returns$return_MSCI)
ALV.DE_regresion <- lm(df_returns$return_ALV.DE ~ df_returns$return_MSCI)
BAYN.DE_regresion <- lm(df_returns$return_BAYN.DE ~ df_returns$return_MSCI)

grafico_regresion <- function(regresion,ticker) {
Rp <- predict(regresion)
b0 <- round(regresion$coefficients[1],4)
b1 <- round(regresion$coefficients[2],4)
b0
b1
qplot(
  data = df_returns,
  x = return_MSCI,
  y = .data[[ticker]],
  main = "Modelo regresion simple",
  colour = as.factor(rcovid),
  ylab = ticker,
  xlab = "MSCI",
  geom = c("point"),
  method = "lm"
) + geom_line(aes(y = Rp), lwd = 1.2, color = "blue") +
  geom_text(x = 0, y = 0.09,colour="black", aes(label = paste(ticker, " = ", b0, " + ", b1, "*", "MSCI")))
}

grafico_regresion(WBK_regresion,"return_WBK")
grafico_regresion(ISP.MI_regresion,"return_ISP.MI")
grafico_regresion(REG_regresion,"return_REG")
grafico_regresion(ALV.DE_regresion,"return_ALV.DE")
grafico_regresion(BAYN.DE_regresion,"return_BAYN.DE")

# Mediante ggplot, no es posible utilizar la función par.
# Hay alternativas equivalentes en internet que no he conseguido comprender,
# ya que son relativamente complejas.

# cartera

cartera <- Return.portfolio(df_returns[,1:5], c(1/5,1/5,1/5,1/5,1/5), rebalance_on = "years")
mean(cartera)
sd(cartera)

hist(cartera, breaks = 30)
charts.PerformanceSummary(cartera, main = "Rendimiento de la cartera")



