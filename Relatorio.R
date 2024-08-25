---
title: "Relatório Final"
author: Wesley Roberto dos Santos 8°, Arcanjo Gabriel Puchalski Silva 4°, Erick Murilo Esmaniotto
  Corninski 4°
date: "2023-11-15"
output:
  html_document: default
  always_allow_html: true
  word_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

```

## **Taxa de Crimes nos Estados Unidos entre 1960-2014**

Este conjunto de dados tem como objetivo fornecer informações sobre tendências de crime, homicídio, população, padrões e taxas para análise e modelação preditiva. O conjunto de dados é particularmente valioso para as agências de aplicação da lei, como o FBI, podem beneficiar das informações obtidas através deste conjunto de dados para melhorar a prevenção do crime e as estratégias de afetação de recursos, então nosso objetivo é extrair informações úteis através da análise, saber quais os crimes mais comuns, qual a década mais perigosa e etc.

**Link da Base de dados** --> [link](https://www.kaggle.com/datasets/mahmoudshogaa/us-crime-rates-1960-2014/data)

![](https://media.tenor.com/9yaCKAT8LKYAAAAC/crime-scene.gif)

### **Começando as análises...**

##### *Importando a base de dados*

```{r message=FALSE}
library(readr)
base = read_csv("C:/Users/Windows/Desktop/US_Crime_Rates_1960_2014.csv")

```

```{r echo=FALSE, results='asis'}
# Mostra as primeiras 3 linhas
knitr::kable(head(base, 3), size = "tiny")
cat("...")  
# Mostra as últimas 3 linhas
knitr::kable(tail(base, 3), size = "tiny")

```

------------------------------------------------------------------------

##### *Sumário básico dos Dados*

```{r, echo=FALSE}
options(width = 200)
summary(base[, 1:6])

```

```{r echo=FALSE}
options(width = 200)
summary(base[, 7:12])
```

------------------------------------------------------------------------

## **Os crimes cresceram juntamente com o aumento da população?**
_Agora vamos criar um gráfico de comparação entre a população e o total de crimes usando a função ggplot_

```{r, echo=FALSE}

library(ggplot2)

# Crie um gráfico de dispersão
ggplot(base, aes(x = Populacao, y = Total)) +
  geom_line() +
  labs(title = "Relação entre População e Total de Crimes",
       x = "População",
       y = "Total de Crimes")
```

------------------------------------------------------------------------

## **Total de Crime conforme os Anos** 
_Um gráfico do Total de crimes pelo passar dos anos, usando novamente a função ggplot_

```{r echo=FALSE, warning=FALSE, error=FALSE, message=FALSE}

# shiny_app/app.R
#shinyAppDir("C:/Users/Windows/Desktop/Trabalhos e Provas atuais/Faculdade/Linguagem R/Bases de dados/shiny_app")


library(ggplot2)
library(plotly)



bar_chart <- ggplot(base, aes(x = Ano, y = Total)) +
  geom_bar(stat = "identity") +
  labs(title = "Total de Crimes ao longo dos Anos",
       x = "Ano",
       y = "Total")


interactive_plot <- ggplotly(bar_chart, dynamicTicks = TRUE)


interactive_plot

```

---

## **Porcentagem dos Crimes por todo o período**
_Um gráfico de pizza geral de todos os crimes, usando a função ggplot com coord_polar, para uma análise mais específica de quais crimes são mais cometidos_

```{r echo=FALSE}
library(plotly)
library(ggplot2)

data_soma = data.frame(
  Categoria = c("Estupro", "Homicidio", "Roubo_Residencia", "Assalto_Agravante", "Furto_Residencia", "Roubo_Veiculo", "Furto_Roubo_Geral"),
  Valor = c(3999314, 952448, 22904744, 37464999, 133320955, 56573743, 327797108)
)

data_soma$Porcentagem <- (data_soma$Valor / sum(data_soma$Valor)) * 100

# Crie um gráfico de pizza usando ggplot2
grafico_pizza <- ggplot(data_soma, aes(x = "", y = Valor, fill = Categoria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Gráfico de Pizza", fill = "Categoria", y = "Valor") +
  geom_text(aes(label = sprintf("%.1f%%", Porcentagem)), position = position_stack(vjust = 0.5))  

#grafico_interativo = ggplotly(grafico_pizza)

# Exiba o gráfico
print(grafico_pizza)
```

-------------------

## **Há mais crimes Violentos ou de Propriedade?**
_Novamente uma análise usando gráfico de pizza, mas agora sobre a quantidade de crimes Violentos e de Propriedade_

```{r echo=FALSE}
library(plotly)
library(ggplot2)

data_soma1 = data.frame(
  Categoria1 = c("Violento", "Propriedade"),
  Valor1 = c(65384309, 517687418)
)

data_soma1$Porcentagem1 <- (data_soma1$Valor1 / sum(data_soma1$Valor1)) * 100

# Crie um gráfico de pizza usando ggplot2
grafico_pizza1 <- ggplot(data_soma1, aes(x = "", y = Valor1, fill = Categoria1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Gráfico de Pizza", fill = "Categoria", y = "Valor") +
  geom_text(aes(label = sprintf("%.1f%%", Porcentagem1)), position = position_stack(vjust = 0.5))

print(grafico_pizza1)
```

---------

## **Agrupando por Décadas**
_Agrupando por décadas usando a função group_by conseguimos ter uma análise de um período de 10 anos_

```{r echo=FALSE, warning=FALSE, message=FALSE}
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)


dados_long <- base %>%
  gather(key = "Crime", value = "Quantidade", -c(Ano)) %>%
  mutate(Decada = 10 * (Ano %/% 10))


dados_agrupados <- dados_long %>%
  filter(Crime == "Total") %>%
  group_by(Decada) %>%
  summarise(Soma_Total = sum(Quantidade))


kable(dados_agrupados)


```

```{r echo=FALSE}
library(ggplot2)

ggplot(dados_agrupados, aes(x = factor(Decada), y = Soma_Total)) +
  geom_bar(stat = "identity") +
  labs(title = "Soma dos Crimes por Década",
       x = "Década",
       y = "Total de Crimes") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()
```
-------

## **Previsão de anos seguintes**
_Agora para uma previsão gráfica de crimes dos próximos 3 anos (2015, 2016 e 2017), precisamos usar do time series e forecast_

```{r echo=FALSE, message=FALSE, warning=FALSE, fig.width=8, fig.height=6, dpi=100}
library(forecast)
library(caret)
library(tidyverse)
library(ggplot2)

serie_temporal <- ts(base$Total, start = min(base$Ano), end = max(base$Ano), frequency = 1)

# Treinamento do modelo ARIMA
modelo_arima <- auto.arima(serie_temporal)

# Sumário do modelo
summary(modelo_arima)

# Previsões para o ano de 2015
previsoes_2015 <- forecast(modelo_arima, h = 3)$mean

ggplot() +
  geom_line(aes(x = time(serie_temporal), y = serie_temporal), color = "blue", size = 1) +
  geom_line(aes(x = time(previsoes_2015), y = previsoes_2015), color = "red", size = 2) +
  labs(title = "Previsão de Crimes para os próximos 3 anos",
       y = "Total de Crimes",
       x = "Ano") +
  theme_minimal()
```
---

## **Conclusão**
_Através da análise de gráficos, conseguimos o que queríamos, além de sabermos os principais crimes no período de 1960-2014 nos Estados Unidos e sabermos algumas informações sobre cada década, também conseguimos estimar uma previsão para oos próximos 3 anos (2015, 2016 e 2017)._
