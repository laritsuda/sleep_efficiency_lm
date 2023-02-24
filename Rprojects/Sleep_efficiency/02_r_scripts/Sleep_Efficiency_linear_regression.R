# This script uses a dataset called Sleep Efficiency to create a linear regression 
# model to predict the Sleep Efficiency based on independent variables

# First step is to load all the packages we will need to use for this project

##################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
##################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines", "reshape2", "PerformanceAnalytics", "nortest", "rgl",
             "car", "olsrr", "jtools", "ggstance", "GGally")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Second step is to import the csv file containing the data to create our dataset

##################################################################################
#                               REGRESSÃO LINEAR MULTIPLA                         #
#                       STEP 01 - CARREGAMENTO DA BASE DE DADOS               #
##################################################################################

sleep <- read.csv("~/R_projects/Sleep/Sleep_Efficiency.csv", header=TRUE, stringsAsFactors=FALSE)

#################################################################################
#                    OBSERVANDO OS DADOS CARREGADOS DA BASE Sleep           #
#################################################################################

glimpse(sleep) #Visualização das observações e das especificações referentes
#às variáveis da base de dados

# The glimpse function is pretty similar to the str function 

# So this command lets you know that you have a total of  15 columns, and 452 rows
#in the dataset


# From the total of 15 variables we have:
# 1 ID,
# 4 qualitative variables: Gender, Bedtime, Wakeup time, and Smoking Status
# 4 discrete (integer type) variables: Age, REM, Deep, Light
# 5 double type variables: Sleep duration, Efficiency, Awakenings, Caffeine 
#consumption, Alcohol consumption and Exercise frequency

#Estatísticas univariadas
summary(sleep)

##################################################################################
#                               REMOVE TIMESTAMP COLUMNS                          #
##################################################################################
sleep = sleep[,-4:-5]

##################################################################################
#                               REMOVE NULL VALUES                                #
##################################################################################
sleep<-na.omit(sleep)
# R gives the descriptive statistics for all the 15 variables, including the character (qualitative) ones.
 
# Plotting pairs of variables 
#We can plot some pairs of variables to find out if they are correlated, or we can cross all variables using

# Age vs. Sleep Efficiency

ggplotly(
  ggplot(sleep, aes(x = Age, y = Sleep.efficiency)) +
    geom_point(color = "darkorchid") +
    xlab("Age") +
    ylab("Sleep_efficiency") +
    theme_classic()
)

# Sleep.efficiency vs. Exercise.frequency

ggplotly(
  ggplot(sleep, aes(x = Sleep.efficiency, y = Light.sleep.percentage)) +
    geom_point(color = "darkorchid") +
    xlab("Sleep_efficiency") +
    ylab("Light sleep percentage") +
    theme_classic()
)

str(sleep)
# R says Sleep duration is double. In fact, it is. 
unique(sleep$Sleep.duration)

# R says Caffeine, Alcohol and Awakenings is double. But for me it looks like integer. 
unique(sleep$Caffeine.consumption)
unique(sleep$Alcohol.consumption)
unique(sleep$Awakenings)

#Anyways, now the qualitative variables, Gender and Smoking Status, should be dummized

# And you need to decide how to treat a time type in this case
# from this type of variable, you can extract many things, such as day of the 
#week, season, or even flag types such as wakeup time before 6 am, or bedtime 
#after 12 am

quant_sleep <- sleep[, c("Age", "Sleep.duration","Sleep.efficiency","REM.sleep.percentage","Deep.sleep.percentage", "Light.sleep.percentage", "Awakenings", "Caffeine.consumption","Alcohol.consumption","Exercise.frequency")]

##################################################################################
#                               ESTUDO DAS CORRELAÇÕES                           #
##################################################################################

chart.Correlation((quant_sleep), histogram = TRUE)
#### ON HOLD ####
#################################################################################
#                   transform bedtime,wakeup time in flags                      #
#################################################################################

#sleep$Bedtime2 <- strptime(sleep$Bedtime, format="%Y-%m-%d %H:%M:%S")
#sleep$Bedtime3 <- format(strptime(sleep$Bedtime, format="%Y-%m-%d %H:%M:%S"), format = "%H:%M:%S")
#this wont matter at this point. if model proved to be bad, yoy can add the flags


#################################################################################
#                            PROCEDIMENTO N-1 DUMMIES                           #
#################################################################################

sleep_dummies <- dummy_columns(.data = sleep,
                                    select_columns = c("Gender",
                                                       "Smoking.status"),
                                    remove_selected_columns = T,
                                    remove_most_frequent_dummy = T)

#Visualizando a base de dados dummizada
sleep_dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)


##################################################################################
#                       ESTIMAÇÃO DA REGRESSÃO LINEAR MÚLTIPLA                   #
##################################################################################
#Modelagem com todas as variáveis
modelo_sleep <- lm(Sleep.efficiency ~ . - ID, sleep_dummies)

#Parâmetros do modelo_sleep
summary(modelo_sleep)

##################################################################################
#                                 PROCEDIMENTO STEPWISE                          #
##################################################################################

step_sleep <- step(modelo_sleep, k = 3.841459)

summary(step_sleep)

### NORMALIZACZO Z-SCORE PARA ENCONTRAR A IMPORTANCIA DAS VARIAVEIS
summ(step_sleep, confint = T, digits = 3, ci.width = .95) #função summ do pacote jtools
export_summs(step_sleep, scale = T, digits = 5)


##################################################################################
#            TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE        #
##################################################################################

#Teste de Shapiro-Francia
sf.test(step_sleep$residuals)

#Plotando os resíduos do modelo step_planosaude 
ggplotly(
  sleep %>%
    mutate(residuos = step_sleep$residuals) %>%
    ggplot(aes(x = residuos)) +
    geom_histogram(color = "bisque4", 
                   fill = "orange", 
                   bins = 15,
                   alpha = 0.6) +
    labs(x = "Resíduos",
         y = "Frequências") + 
    theme_bw()
)

#Acrescentando uma curva normal teórica para comparação entre as distribuições
ggplotly(
  sleep %>%
    mutate(residuos = step_sleep$residuals) %>%
    ggplot(aes(x = residuos)) +
    geom_histogram(aes(y = ..density..), 
                   color = "bisque4", 
                   fill = "orange", 
                   bins = 15,
                   alpha = 0.6) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(step_sleep$residuals),
                              sd = sd(step_sleep$residuals)),
                  aes(color = "Curva Normal Teórica"),
                  size = 1) +
    scale_color_manual("Legenda:",
                       values = "darkorchid") +
    labs(x = "Resíduos",
         y = "Frequência") +
    theme_bw()
)


##################################################################################
#                              TRANSFORMAÇÃO DE BOX-COX                          #
##################################################################################
#Para calcular o lambda de Box-Cox
lambda_BC <- powerTransform(sleep_dummies$Sleep.efficiency) #função powerTransform pertence ao pacote car#
lambda_BC

#Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo
sleep_dummies["bc_sleep_efficiency"] <- (((sleep_dummies$Sleep.efficiency ^ lambda_BC$lambda) - 1) / 
                            lambda_BC$lambda)


#Visualizando a nova variável na base de dados
sleep_dummies %>%
  select(ID, Age, Sleep.duration, bc_sleep_efficiency, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

#Estimando um novo modelo múltiplo
modelo_bc <- lm(formula = bc_sleep_efficiency ~ . -ID -Sleep.efficiency, 
                data = sleep_dummies)

#Parâmetros do modelo
summary(modelo_bc)

#Aplicando o procedimento Stepwise
step_modelo_bc <- step(modelo_bc, k = 3.841459)

summary(step_modelo_bc)
#Note que a variável 'disclosure' acaba voltando ao modelo na forma funcional não linear!

#Verificando a normalidade dos resíduos do modelo step_modelo_bc
sf.test(step_modelo_bc$residuals)

#Kernel density estimation (KDE) - forma não-paramétrica para estimar a
#função densidade de probabilidade de uma variável aleatória
sleep_dummies %>%
  ggplot() +
  geom_density(aes(x = step_sleep$residuals), fill = "darkorchid") +
  labs(x = "Resíduos do Modelo Stepwise",
       y = "Densidade") +
  theme_bw()
