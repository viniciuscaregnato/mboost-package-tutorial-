# mboost-package-tutorial-
rewriting the commands from https://cran.r-project.org/web/packages/mboost/vignettes/mboost_tutorial.pdf

https://cran.r-project.org/web/packages/mboost/vignettes/mboost_tutorial.pdf

install.packages("TH.data")
library (mboost)
library(TH.data)

data("bodyfat", package = "TH.data")

View(bodyfat)

#1. In the original publication (Garcia et al. 2005), the presented prediction formula was based on a linear model with backward-elimination for variable selection.#
#backward elimination é retirar a variavel com p valor acima do nivel, e rodar de novo a regressao sem ela
#entao repetir o processo ate que todas as variaveis nao possam ser rejeitadas (p-valor menor que significancia)
lm1 <- lm(DEXfat ~ hipcirc + kneebreadth + anthro3a, data = bodyfat)
coef(lm1)
summary(lm1)

#2. a mesma regressao pode ser feita atraves de boosting#
glm1 <- glmboost(DEXfat ~ hipcirc + kneebreadth + anthro3a, data = bodyfat)
summary(glm1)
coef(glm1)

#3. se considerar todas as variaveis disponiveis#
glm2 <- glmboost(DEXfat ~ ., data = bodyfat)
summary(glm2)
coef(glm2)

#ou  escrever assim

preds <- names(bodyfat[, names(bodyfat) != "DEXfat"]) #nome dos predictors
fm <- as.formula(paste("DEXfat ~", paste(preds, collapse = "+")))
glm2_2 <- glmboost(fm, data = bodyfat)
coef(glm2_2) #nao houve contribuição de anthro4
coef(glm2, which = "") #assim verifica anthro4

#4. plottar o grafico
plot(glm2) #sem offset
plot (glm2, off2int = TRUE)  #com offset, altera a linha do intercept
plot(glm2, ylim = range(coef(glm2, which = preds))) #vizualizaçao sem intercept


#5. para base-learners sem intercepto, deve-se centralizar as covariaveis antes de ajsutar o modelo.
#assim, nao obrigamos a regressao a passar pela origem #
lm_no_intercept <- glmboost(DEXfat ~ .-1, data = bodyfat) #automaticamente centraliza covariaveis
plot(lm_no_intercept) #grafico sem intercepto e sem centralizar cov# 
previsao <- predict(lm_no_intercept, data = bodyfat$DEXfat)
plot(previsao, bodyfat$DEXfat,
     xlab = "previstos",
     ylab = "observados",
     main = "previstos x observados",
     pch = 16, col = "blue"
     )
