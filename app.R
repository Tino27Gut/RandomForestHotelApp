#==================================================#
#                      INICIO                       #
#==================================================#

# Carga de librerías
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(plotly)
library(caret)
library(randomForest)
library(lubridate)
library(pROC)
library(reshape2)
library(tidyr)
library(DT)
library(rlang)

# Carga y exploración inicial de datos
hotel_1 <- read.csv("01_data/booking.csv")
hotel <- hotel_1
str(hotel)

#==================================================#
#                        ETL                        #
#==================================================#

# Transformación de tipos de datos
hotel$type.of.meal <- as.factor(hotel$type.of.meal)
hotel$market.segment.type <- as.factor(hotel$market.segment.type)
hotel$booking.status <- as.factor(hotel$booking.status)
hotel$room.type <- as.numeric(substr(hotel$room.type, nchar(hotel$room.type), nchar(hotel$room.type)))
hotel$date.of.reservation <- as.Date(hotel$date.of.reservation, format = "%m/%d/%Y")

# Procesamiento de fechas y eliminación de columnas
hotel$Booking_ID <- NULL
hotel$day <- day(hotel$date.of.reservation)
hotel$month <- month(hotel$date.of.reservation)
hotel$year <- year(hotel$date.of.reservation)
hotel$date.of.reservation <- NULL

# Manejo de valores faltantes
colSums(is.na(hotel))
hotel <- na.omit(hotel)
colSums(is.na(hotel))

#==================================================#
#                        EDA                        #
#==================================================#

# Definición de paleta de colores
cold_palette <- c("#2E86C1", "#5DADE2", "blue", "#85C1E9", "#2874A6", "#AED6F1")

# Gráfico 1: Distribución de estados de reserva
Grafico_EDA_1 <- ggplot(hotel, aes(x = booking.status, fill = booking.status)) +
  geom_bar() +
  labs(title = "Distribución de Estados de Reserva",
       x = "Estado de Reserva",
       y = "Conteo") +
  theme_minimal() +
  scale_fill_manual(values = cold_palette) +
  theme(plot.title = element_text(hjust = 0.5))

EDA_1 <- ggplotly(Grafico_EDA_1)


# Gráfico 2: Distribución de lead time
Grafico_EDA_2 <- ggplot(hotel, aes(x = lead.time)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Lead Time",
       x = "Lead Time",
       y = "Frecuencia") +
  theme_minimal() +
  scale_fill_manual(values = cold_palette) +
  theme(plot.title = element_text(hjust = 0.5))

EDA_2 <- ggplotly(Grafico_EDA_2)


# Gráfico 3: Precio promedio por estado de reserva
Grafico_EDA_3 <- ggplot(hotel, aes(x = booking.status, y = average.price, fill = booking.status)) +
  geom_boxplot() +
  labs(title = "Precio Promedio por Estado de Reserva",
       x = "Estado de Reserva", 
       y = "Precio Promedio") +
  theme_minimal() +
  scale_fill_manual(values = cold_palette) +
  theme(plot.title = element_text(hjust = 0.5))


# Gráfico 4: Distribución del tipo de comida
Grafico_EDA_4 <- ggplot(hotel, aes(x = type.of.meal, fill = type.of.meal)) +
  geom_bar() +
  labs(title = "Distribución del Tipo de Comida",
       x = "Tipo de Comida",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = cold_palette) +
  theme(plot.title = element_text(hjust = 0.5))


# Gráfico 5: Segmento de mercado por estado de reserva
Grafico_EDA_5 <- ggplot(hotel, aes(x = market.segment.type, fill = booking.status)) +
  geom_bar(position = "fill") +
  labs(title = "Segmento de Mercado por Estado de Reserva",
       x = "Segmento de Mercado",
       y = "Proporción") +
  theme_minimal() +
  scale_fill_manual(values = cold_palette) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))


# Gráfico 6: Lead time vs precio promedio
Grafico_EDA_6 <- ggplot(hotel, aes(x = lead.time, y = average.price, color = market.segment.type)) +
  geom_point() +
  labs(title = "Lead Time vs Precio Promedio por Segmento de Mercado") +
  theme_minimal() +
  scale_color_manual(values = cold_palette) +
  theme(plot.title = element_text(hjust = 0.5))


# Gráfico 7: Matriz de correlación
library(tidyverse)
library(reshape2)
library(plotly)

numeric_vars <- hotel %>% select_if(is.numeric)
cor_matrix <- cor(numeric_vars)
cor_melted <- cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column("Var1") %>%
  gather(key = "Var2", value = "Correlation", -Var1)

cor_plot <- ggplot(cor_melted, aes(x = Var1, y = Var2, 
                                   fill = Correlation, 
                                   text = paste0("Variable 1: ", Var1,
                                                 "<br>Variable 2: ", Var2,
                                                 "<br>Correlación: ", round(Correlation, 2)))) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = sprintf("%.2f", Correlation)),
            color = ifelse(abs(cor_melted$Correlation) > 0.5, "white", "black"),
            size = 3.5) +
  scale_fill_gradient2(low = "#4575B4",
                       mid = "white",
                       high = "#D73027",
                       midpoint = 0,
                       limits = c(-1, 1)) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)) +
  coord_fixed()

EDA_7 <- ggplotly(cor_plot, tooltip = "text", width = 1000, height = 800) %>%
  layout(title = list(text = "Matriz de Correlación Interactiva",
                      x = 0.5,
                      font = list(size = 20)),
         margin = list(l = 80, r = 80, t = 100, b = 80)) %>%
  style(textsize = 12)


# Gráfico 8: Densidad del número de adultos
Grafico_EDA_8 <- ggplot(hotel, aes(x = number.of.adults, fill = booking.status)) +
  geom_density(alpha = 0.7) +
  labs(title = "Densidad del Número de Adultos según el Estado de Reserva") +
  theme_minimal() +
  scale_fill_manual(values = cold_palette) +
  theme(plot.title = element_text(hjust = 0.5))

EDA_8 <- ggplotly(Grafico_EDA_8)


# Gráficos 9-13: Análisis de variables importantes
EDA_9 <- ggplot(hotel, aes(x = booking.status, y = lead.time, fill = booking.status)) +
  geom_boxplot() +
  labs(title = "Lead Time según Estado de Reserva",
       x = "Estado de Reserva",
       y = "Lead Time") +
  theme_minimal() +
  scale_fill_manual(values = cold_palette) +
  theme(plot.title = element_text(hjust = 0.5))
EDA_9 <- ggplotly(EDA_9)


EDA_10 <- ggplot(hotel, aes(x = booking.status, y = average.price, fill = booking.status)) +
  geom_violin(trim = FALSE) +
  labs(title = "Precio Promedio según Estado de Reserva",
       x = "Estado de Reserva",
       y = "Precio Promedio") +
  theme_minimal() +
  scale_fill_manual(values = cold_palette) +
  theme(plot.title = element_text(hjust = 0.5))
EDA_10 <- ggplotly(EDA_10)


EDA_11 <- ggplot(hotel, aes(x = special.requests, fill = booking.status)) +
  geom_bar(position = "dodge") +
  labs(title = "Número de Solicitudes Especiales según Estado de Reserva",
       x = "Número de Solicitudes Especiales",
       y = "Frecuencia") +
  theme_minimal() +
  scale_fill_manual(values = cold_palette) +
  theme(plot.title = element_text(hjust = 0.5))
EDA_11 <- ggplotly(EDA_11)


EDA_12 <- ggplot(hotel, aes(x = day, fill = booking.status)) +
  geom_density(alpha = 0.7) +
  labs(title = "Densidad de Días según Estado de Reserva",
       x = "Día del Mes",
       y = "Densidad") +
  theme_minimal() +
  scale_fill_manual(values = cold_palette) +
  theme(plot.title = element_text(hjust = 0.5))
EDA_12 <- ggplotly(EDA_12)


EDA_13 <- ggplot(hotel, aes(x = factor(month), fill = booking.status)) +
  geom_bar(position = "dodge") +
  labs(title = "Meses del Año según Estado de Reserva",
       x = "Mes",
       y = "Frecuencia") +
  theme_minimal() +
  scale_fill_manual(values = cold_palette) +
  theme(plot.title = element_text(hjust = 0.5))
EDA_13 <- ggplotly(EDA_13)



#==================================================#
#              MODELO SIN BALANCEAR                 #
#==================================================#

# Validación de clases
table(hotel$booking.status)
n_canceled <- sum(hotel$booking.status == "Canceled")
n_not_canceled <- sum(hotel$booking.status == "Not_Canceled")

prop_table <- data.frame(
  canceled = n_canceled / nrow(hotel),
  not_canceled = n_not_canceled / nrow(hotel)
)

# Partición de datos + modelo
training_indexes <- createDataPartition(hotel$booking.status, p=0.7, list=FALSE)
hotel_training <- hotel[training_indexes, ]
hotel_testing <- hotel[-training_indexes, ]

hotel_model <- readRDS("02_rfmodels/Sin Balancear.rds")
correct_hotel <- sum(diag(hotel_model$confusion))
total_hotel <- sum(hotel_model$confusion)
accuracy_hotel <- correct_hotel / total_hotel
print(paste("Model Accuracy:", round(accuracy_hotel,4)))

# Validación de predicciones
predictions_hotel <- predict(hotel_model, hotel_testing)
confusion_hotel <- table(hotel_testing$booking.status, predictions_hotel)
accuracy_hotel_testing <- sum(diag(confusion_hotel)) / sum(confusion_hotel)
print(paste("Model Accuracy (Test):", round(accuracy_hotel_testing,4)))

# Matriz de Confusión Gráfica
confusion_df <- as.data.frame(confusion_hotel)
colnames(confusion_df) <- c("Actual", "Predicted", "Freq")

sin_balancear_plot <- ggplot(data = confusion_df, aes(x = Actual, y = Predicted)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_text(aes(label = Freq), vjust = 1) +
  labs(title = "Matriz de Confusión",
       x = "Real",
       y = "Predicho") +
  theme_minimal()

sin_balancear_plot <- ggplotly(sin_balancear_plot)

# Error OOB
oob_error <- hotel_model$err.rate[, 1]
num_trees <- seq_along(oob_error)
oob_data <- data.frame(Trees = num_trees, OOB_Error = oob_error)

oob_ggplot <- ggplot(oob_data, aes(x = Trees, y = OOB_Error)) +
  geom_line(color = "blue", linewidth = 1.2) +
  labs(title = "Error OOB del Modelo Random Forest",
       x = "Número de Árboles",
       y = "Tasa de Error OOB") +
  theme_minimal()

sin_balancear_oob_plot <- ggplotly(oob_ggplot) %>%
  layout(title = list(text = "Error OOB", x = 0.5),
         hovermode = "x unified")

# Importancia de variables
importance <- importance(hotel_model)
importance_df <- data.frame(
  Variable = rownames(importance),
  Importance = importance[, 1]
) %>%
  arrange(desc(Importance))
rownames(importance_df) <- NULL

sin_balancear_importance_plot <- ggplot(importance_df, 
                                        aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Importancia de las Variables",
       x = "Variable", y = "Importancia") +
  theme_minimal()
sin_balancear_importance_plot <- ggplotly(sin_balancear_importance_plot)



#==================================================#
#  Modelo sin balancear con variables importantes  #
#==================================================#

# Selección de variables importantes
variables_importantes <- hotel_training %>%
select(lead.time, average.price, special.requests, day, booking.status)

train_index <- createDataPartition(variables_importantes$booking.status, p = 0.7, list = FALSE)
train_data <- variables_importantes[train_index, ]
test_data <- variables_importantes[-train_index, ]

hotel_model_importante <- readRDS("02_rfmodels/Sin Balancear Top 5.rds")

predicciones_importantes <- predict(hotel_model_importante, newdata = test_data)

# Curva ROC
probabilidades <- predict(hotel_model_importante, newdata = test_data, type = "prob")
roc_curve <- roc(test_data$booking.status, as.numeric(probabilidades[, 2]))

# Matriz de Confusión
conf_matrix <- confusionMatrix(predicciones_importantes, test_data$booking.status)
conf_data <- as.data.frame(conf_matrix$table)

sin_balancear_top_plot <- ggplot(conf_data, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Matriz de Confusión", x = "Real", y = "Predicho", fill = "Frecuencia") +
  theme_minimal()
sin_balancear_top_plot <- ggplotly(sin_balancear_top_plot)

# Error OOB
sin_balancear_top_oob_error <- hotel_model_importante$err.rate[, 1]
sin_balancear_top_num_trees <- seq(1,100)
sin_balancear_top_oob_data <- data.frame(Trees = sin_balancear_top_num_trees, OOB_Error = sin_balancear_top_oob_error)

sin_balancear_top_oob_ggplot <- ggplot(sin_balancear_top_oob_data, aes(x = Trees, y = OOB_Error)) +
  geom_line(color = "blue", linewidth = 1.2) +
  labs(title = "Error OOB del Modelo Random Forest",
       x = "Número de Árboles", y = "Tasa de Error OOB") +
  theme_minimal()
sin_balancear_top_oob_plot <- ggplotly(sin_balancear_top_oob_ggplot) %>%
  layout(title = list(text = "Error OOB", x = 0.5), hovermode = "x unified")

# Importancia de variables
sin_balancear_top_importance <- importance(hotel_model_importante)
sin_balancear_top_importance_df <- data.frame(
  Variable = rownames(sin_balancear_top_importance),
  Importance = sin_balancear_top_importance[, 1]
) %>% arrange(desc(Importance))
rownames(sin_balancear_top_importance_df) <- NULL

sin_balancear_top_importance_plot <- ggplot(sin_balancear_top_importance_df, 
                                            aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Importancia de las Variables", x = "Variable", y = "Importancia") +
  theme_minimal()
sin_balancear_top_importance_plot <- ggplotly(sin_balancear_top_importance_plot)

#==================================================#
#            BALANCEADO CON UPSAMPLING             #
#==================================================#

# Balanceo de datos
hotel_upsample <- upSample(x = hotel[, -which(names(hotel) == "booking.status")],
                           y = hotel$booking.status) %>%
  rename(booking.status = "Class")

# Verificación de proporciones
n_canceled_upsample <- sum(hotel_upsample$booking.status == "Canceled")
n_not_canceled_upsample <- sum(hotel_upsample$booking.status == "Not_Canceled")
prop_table_upsample <- data.frame(
  canceled = n_canceled_upsample / nrow(hotel_upsample),
  not_canceled = n_not_canceled_upsample / nrow(hotel_upsample)
)

# Partición y modelado
training_indexes_upsample <- createDataPartition(hotel_upsample$booking.status, p=0.7, list=FALSE)
hotel_training_upsample <- hotel_upsample[training_indexes_upsample, ]
hotel_testing_upsample <- hotel_upsample[-training_indexes_upsample, ]

hotel_model_upsample <- readRDS("02_rfmodels/Upsample.rds")

# Validación del modelo
predictions_hotel_upsample <- predict(hotel_model_upsample, hotel_testing_upsample)
confusion_hotel_upsample <- table(hotel_testing_upsample$booking.status, predictions_hotel_upsample)
accuracy_hotel_testing_upsample <- sum(diag(confusion_hotel_upsample)) / sum(confusion_hotel_upsample)

# Importancia de variables
importance_upsample <- importance(hotel_model_upsample)
importance_upsample_df <- data.frame(
  Variable = rownames(importance_upsample),
  Importance = importance_upsample[, 1]
) %>% arrange(desc(Importance))
rownames(importance_upsample_df) <- NULL

upsample_importance_plot <- ggplot(importance_upsample_df, 
                                   aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Importancia de las Variables", x = "Variable", y = "Importancia") +
  theme_minimal()
upsample_importance_plot <- ggplotly(upsample_importance_plot)

# Matriz de confusión gráfica
confusion_df_upsample <- as.data.frame(confusion_hotel_upsample) %>%
  setNames(c("Actual", "Predicted", "Freq"))

upsampling_plot <- ggplot(data = confusion_df_upsample, aes(x = Actual, y = Predicted)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_text(aes(label = Freq), vjust = 1) +
  labs(title = "Matriz de Confusión", x = "Real", y = "Predicho") +
  theme_minimal()
upsampling_plot <- ggplotly(upsampling_plot)

# Error OOB
upsample_oob_error <- hotel_model_upsample$err.rate[, 1]
upsample_num_trees <- seq(1,100)
upsample_oob_data <- data.frame(Trees = upsample_num_trees, OOB_Error = upsample_oob_error)

upsample_oob_ggplot <- ggplot(upsample_oob_data, aes(x = Trees, y = OOB_Error)) +
  geom_line(color = "blue", linewidth = 1.2) +
  labs(title = "Error OOB", x = "Número de Árboles", y = "Tasa de Error OOB") +
  theme_minimal()
upsample_oob_plot <- ggplotly(upsample_oob_ggplot) %>%
  layout(title = list(text = "Error OOB", x = 0.5), hovermode = "x unified")

#==================================================#
#            UPSAMPLING TOP 5 VARIABLES            #
#==================================================#

# Preparación de datos
hotel_up_top8 <- hotel %>%
  select(lead.time, average.price, special.requests, month, day, booking.status)

hotel_upsample_top <- upSample(
  x = hotel_up_top8[, -which(names(hotel_up_top8) == "booking.status")],
  y = hotel_up_top8$booking.status
) %>% rename(booking.status = "Class")

# Verificar proporciones
n_canceled_upsample_top <- sum(hotel_upsample_top$booking.status == "Canceled")
n_not_canceled_upsample_top <- sum(hotel_upsample_top$booking.status == "Not_Canceled")

prop_table_upsample_top <- data.frame(
  canceled = n_canceled_upsample_top / nrow(hotel_upsample_top),
  not_canceled = n_not_canceled_upsample_top / nrow(hotel_upsample_top)
)

# Partición de datos
training_indexes_upsample_top <- createDataPartition(hotel_upsample_top$booking.status, p=0.7, list=FALSE)
hotel_training_upsample_top <- hotel_upsample_top[training_indexes_upsample_top, ]
hotel_testing_upsample_top <- hotel_upsample_top[-training_indexes_upsample_top, ]

hotel_model_upsample_top <- readRDS("02_rfmodels/Upsample Top 5.rds")

# Evaluación del modelo
predictions_hotel_upsample_top <- predict(hotel_model_upsample_top, hotel_testing_upsample_top)
confusion_hotel_upsample_top <- table(hotel_testing_upsample_top$booking.status, predictions_hotel_upsample_top)

# Matriz de confusión gráfica
confusion_df_upsample_top <- as.data.frame(confusion_hotel_upsample_top) %>%
  setNames(c("Actual", "Predicted", "Freq"))

upsampling_top_plot <- ggplot(data = confusion_df_upsample_top, aes(x = Actual, y = Predicted)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_text(aes(label = Freq), vjust = 1) +
  labs(title = "Matriz de Confusión", x = "Real", y = "Predicho") +
  theme_minimal()
upsampling_top_plot <- ggplotly(upsampling_top_plot)

# Error OOB
upsample_top_oob_error <- hotel_model_upsample_top$err.rate[, 1]
upsample_top_num_trees <- seq(1,100)
upsample_top_oob_data <- data.frame(
  Trees = upsample_top_num_trees,
  OOB_Error = upsample_top_oob_error
)

upsample_top_oob_ggplot <- ggplot(upsample_top_oob_data, aes(x = Trees, y = OOB_Error)) +
  geom_line(color = "blue", linewidth = 1.2) +
  labs(title = "Error OOB", x = "Número de Árboles", y = "Tasa de Error OOB") +
  theme_minimal()
upsample_top_oob_plot <- ggplotly(upsample_top_oob_ggplot) %>%
  layout(title = list(text = "Error OOB", x = 0.5), hovermode = "x unified")

# Importancia de las variables
importance_upsample_top <- importance(hotel_model_upsample_top)
importance_upsample_top_df <- data.frame(
  Variable = rownames(importance_upsample_top),
  Importance = importance_upsample_top[, 1]
)

importance_upsample_top_df <- importance_upsample_top_df[order(-importance_upsample_top_df$Importance), ]
rownames(importance_upsample_top_df) <- NULL

upsample_top_importance_plot <- ggplot(importance_upsample_top_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Importancia de las Variables",
       x = "Variable",
       y = "Importancia") +
  theme_minimal()
upsample_top_importance_plot <- ggplotly(upsample_top_importance_plot)

prop_table_upsample_top %>%
  gather(key = "Booking_Status", value = "Proportion") %>%
  ggplot(aes(x = Booking_Status, y = Proportion, fill = Booking_Status)) +
  geom_bar(stat = "identity") +
  labs(title = "Proporción de Clases en Datos Balanceados (Upsampling)",
       x = "Estado de Reserva",
       y = "Proporción") +
  theme_minimal()

#=======================================#
#   Balanceado con DownSampling         #
#=======================================#

#-------------------------
# Preparación de datos
#-------------------------

# Realizar downsampling
hotel_downsample <- downSample(
  x = hotel[, -which(names(hotel) == "booking.status")],
  y = hotel$booking.status
)

# Renombrar la columna de la clase
hotel_downsample <- hotel_downsample %>%
  rename(booking.status = "Class")

# Verificar proporciones
table(hotel_downsample$booking.status)

canceled_downsample <- sum(hotel_downsample$booking.status == "Canceled")
not_canceled_downsample <- sum(hotel_downsample$booking.status == "Not_Canceled")

prop_table_downsample <- data.frame(
  canceled = canceled_downsample / nrow(hotel_downsample),
  not_canceled = not_canceled_downsample / nrow(hotel_downsample)
)

prop_table_downsample

#-------------------------
# Partición de datos
#-------------------------

training_indexes_downsample <- createDataPartition(hotel_downsample$booking.status, p=0.7, list=FALSE)
hotel_training_downsample <- hotel_downsample[training_indexes_downsample, ]
hotel_testing_downsample <- hotel_downsample[-training_indexes_downsample, ]

#-------------------------
# Creación del modelo
#-------------------------

hotel_model_downsample <- readRDS("02_rfmodels/Downsample.rds")
print(hotel_model_downsample)

# Validación de accuracy
correct_hotel_downsample <- sum(diag(hotel_model_downsample$confusion))
total_hotel_downsample <- sum(hotel_model_downsample$confusion)
accuracy_hotel_downsample <- correct_hotel_downsample / total_hotel_downsample
print(paste("Model Accuracy:", round(accuracy_hotel_downsample,4)))

#-------------------------
# Evaluación del modelo
#-------------------------

# Validación de predicciones
predictions_hotel_downsample <- predict(hotel_model_downsample, hotel_testing_downsample)
confusion_hotel_downsample <- table(hotel_testing_downsample$booking.status, predictions_hotel_downsample)
print(confusion_hotel_downsample)

accuracy_hotel_testing_downsample <- sum(diag(confusion_hotel_downsample)) / sum(confusion_hotel_downsample)
print(paste("Model Accuracy (Test):", round(accuracy_hotel_testing_downsample,4)))

#-------------------------
# Visualizaciones
#-------------------------

# Matriz de confusión gráfica
conf_data_downsample <- as.data.frame(confusion_hotel_downsample)
names(conf_data_downsample) <- c("Valor_Real", "Prediccion", "Frecuencia")

downsampling_plot <- ggplot(conf_data_downsample, 
                            aes(x = Valor_Real, y = Prediccion, fill = Frecuencia)) +
  geom_tile() +
  geom_text(aes(label = Frecuencia), vjust = 1) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Matriz de Confusión",
    x = "Real",
    y = "Predicho",
    fill = "Frecuencia"
  ) +
  theme_minimal()
downsampling_plot <- ggplotly(downsampling_plot)
downsampling_plot

# Tasa de error
plot(hotel_model_downsample, 
     main = "Tasa de Error OOB en el Modelo Random Forest con Downsampling",
     col = "darkred")
legend("topright", 
       legend = colnames(hotel_model_downsample$err.rate), 
       col = 1:2, lty = 1:2, cex = 0.8)

# Gráfico OOB interactivo
downsample_oob_error <- hotel_model_downsample$err.rate[, 1]
downsample_num_trees <- seq(1,100)

downsample_oob_data <- data.frame(
  Trees = downsample_num_trees,
  OOB_Error = downsample_oob_error
)

downsample_oob_ggplot <- ggplot(downsample_oob_data, aes(x = Trees, y = OOB_Error)) +
  geom_line(color = "blue", linewidth = 1.2) +
  labs(
    title = "Error OOB",
    x = "Número de Árboles",
    y = "Tasa de Error OOB"
  ) +
  theme_minimal()

downsample_oob_plot <- ggplotly(downsample_oob_ggplot) %>%
  layout(
    title = list(text = "Error OOB", x = 0.5),
    hovermode = "x unified"
  )

downsample_oob_plot

#-------------------------
# Importancia de variables
#-------------------------

importance_downsample <- importance(hotel_model_downsample)
importance_downsample_df <- data.frame(
  Variable = rownames(importance_downsample),
  Importance = importance_downsample[, 1]
)

importance_downsample_df <- importance_downsample_df[order(-importance_downsample_df$Importance), ]
rownames(importance_downsample_df) <- NULL

importance_downsample_df

# Visualización de importancia
downsample_importance_plot <- ggplot(importance_downsample_df, 
                                     aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Importancia de las Variables",
    x = "Variable",
    y = "Importancia"
  ) +
  theme_minimal()
downsample_importance_plot <- ggplotly(downsample_importance_plot)
downsample_importance_plot

#================================================#
#  Modelo con downsample Top 5 variables          #
#================================================#

#-------------------------
# Preparación de datos
#-------------------------

# Seleccionar top 5 variables
hotel_top5 <- hotel %>%
  select(lead.time, average.price, special.requests, day, month, booking.status)

# Realizar downsampling
hotel_downsample_top5 <- downSample(
  x = hotel_top5[, -which(names(hotel_top5) == "booking.status")],
  y = hotel_top5$booking.status
)

hotel_downsample_top5 <- hotel_downsample_top5 %>%
  rename(booking.status = "Class")

# Verificar proporciones
table(hotel_downsample_top5$booking.status)

canceled_downsample_top5 <- sum(hotel_downsample_top5$booking.status == "Canceled")
not_canceled_downsample_top5 <- sum(hotel_downsample_top5$booking.status == "Not_Canceled")

prop_table_downsample_top5 <- data.frame(
  canceled = canceled_downsample_top5 / nrow(hotel_downsample_top5),
  not_canceled = not_canceled_downsample_top5 / nrow(hotel_downsample_top5)
)

prop_table_downsample_top5

#-------------------------
# Partición de datos
#-------------------------

training_indexes_downsample_top5 <- createDataPartition(hotel_downsample_top5$booking.status, p = 0.7, list = FALSE)
hotel_training_downsample_top5 <- hotel_downsample_top5[training_indexes_downsample_top5, ]
hotel_testing_downsample_top5 <- hotel_downsample_top5[-training_indexes_downsample_top5, ]

#-------------------------
# Creación del modelo
#-------------------------

hotel_model_downsample_top5 <- readRDS("02_rfmodels/Downsample Top 5.rds")
print(hotel_model_downsample_top5)

#-------------------------
# Evaluación del modelo
#-------------------------

correct_hotel_downsample_top5 <- sum(diag(hotel_model_downsample_top5$confusion))
total_hotel_downsample_top5 <- sum(hotel_model_downsample_top5$confusion)
accuracy_hotel_downsample_top5 <- correct_hotel_downsample_top5 / total_hotel_downsample_top5
print(paste("Model Accuracy (Train):", round(accuracy_hotel_downsample_top5, 4)))

predictions_hotel_downsample_top5 <- predict(hotel_model_downsample_top5, hotel_testing_downsample_top5)
confusion_hotel_downsample_top5 <- table(hotel_testing_downsample_top5$booking.status, predictions_hotel_downsample_top5)
print(confusion_hotel_downsample_top5)

accuracy_hotel_testing_downsample_top5 <- sum(diag(confusion_hotel_downsample_top5)) / sum(confusion_hotel_downsample_top5)
print(paste("Model Accuracy (Test):", round(accuracy_hotel_testing_downsample_top5, 4)))

#-------------------------
# Visualizaciones
#-------------------------

# Matriz de confusión
confusion_df_downsample_top <- as.data.frame(confusion_hotel_downsample_top5)
colnames(confusion_df_downsample_top) <- c("Actual", "Predicted", "Frequency")

downsampling_top_plot <- ggplot(confusion_df_downsample_top, 
                                aes(x = Actual, y = Predicted, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = Frequency), vjust = 1) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Matriz de Confusión",
    x = "Real",
    y = "Predicho",
    fill = "Frecuencia"
  ) +
  theme_minimal()
downsampling_top_plot <- ggplotly(downsampling_top_plot)
downsampling_top_plot

# Importancia de variables
varImpPlot(hotel_model_downsample_top5, 
           main = "Importancia de las Variables - Random Forest (Top 5 Variables)")

importance_df <- as.data.frame(importance(hotel_model_downsample_top5))
importance_df$Variable <- rownames(importance_df)
importance_df <- importance_df[order(importance_df$MeanDecreaseGini, decreasing = TRUE), ]

ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  coord_flip() +
  labs(
    title = "Importancia de las Variables",
    x = "Variable",
    y = "Importancia (Mean Decrease Gini)"
  ) +
  theme_minimal()

# Error OOB
plot(hotel_model_downsample_top5,
     main = "Error OOB del Modelo Random Forest (Top 5 Variables)",
     col = "blue",
     lwd = 2)

# Gráfico OOB interactivo
downsample_top_oob_error <- hotel_model_downsample_top5$err.rate[, 1]
downsample_top_num_trees <- seq(1,100)

downsample_top_oob_data <- data.frame(
  Trees = downsample_top_num_trees,
  OOB_Error = downsample_top_oob_error
)

downsample_top_oob_ggplot <- ggplot(downsample_top_oob_data, 
                                    aes(x = Trees, y = OOB_Error)) +
  geom_line(color = "blue", linewidth = 1.2) +
  labs(
    title = "Error OOB",
    x = "Número de Árboles",
    y = "Tasa de Error OOB"
  ) +
  theme_minimal()

downsample_top_oob_plot <- ggplotly(downsample_top_oob_ggplot) %>%
  layout(
    title = list(text = "Error OOB", x = 0.5),
    hovermode = "x unified"
  )

downsample_top_oob_plot

#-------------------------
# Métricas de importancia
#-------------------------

importance_downsample_top <- importance(hotel_model_downsample_top5)
importance_downsample_top_df <- data.frame(
  Variable = rownames(importance_downsample_top),
  Importance = importance_downsample_top[, 1]
)

importance_downsample_top_df <- importance_downsample_top_df[
  order(-importance_downsample_top_df$Importance), ]
rownames(importance_downsample_top_df) <- NULL

importance_downsample_top_df

# Visualización de importancia
downsample_top_importance_plot <- ggplot(importance_downsample_top_df, 
                                         aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Importancia de las Variables",
    x = "Variable",
    y = "Importancia"
  ) +
  theme_minimal()
downsample_top_importance_plot <- ggplotly(downsample_top_importance_plot)
downsample_top_importance_plot

#================================================#
#        Comparación de Modelos                   #
#================================================#

#-------------------------
# Matrices de confusión
#-------------------------

caret_confusion_hotel <- confusionMatrix(
  hotel_testing$booking.status,
  predictions_hotel
)
caret_confusion_hotel

caret_confusion_hotel_top5 <- confusionMatrix(
  predicciones_importantes,
  test_data$booking.status
)
caret_confusion_hotel_top5

caret_confusion_hotel_upsample <- confusionMatrix(
  hotel_testing_upsample$booking.status,
  predictions_hotel_upsample
)
caret_confusion_hotel_upsample

caret_confusion_hotel_upsample_top5 <- confusionMatrix(
  hotel_testing_upsample_top$booking.status, 
  predictions_hotel_upsample_top
)
caret_confusion_hotel_upsample_top5

caret_confusion_hotel_downsample <- confusionMatrix(
  hotel_testing_downsample$booking.status,
  predictions_hotel_downsample
)
caret_confusion_hotel_downsample

caret_confusion_hotel_downsample_top5 <- confusionMatrix(
  hotel_testing_downsample_top5$booking.status,
  predictions_hotel_downsample_top5
)
caret_confusion_hotel_downsample_top5

#-------------------------
# Métricas comparativas
#-------------------------

model_list <- list(
  hotel_model,
  hotel_model_importante,
  hotel_model_upsample,
  hotel_model_upsample_top,
  hotel_model_downsample,
  hotel_model_downsample_top5
)

model_names <- c(
  "Sin Balancear",
  "Sin Balancear Top 5",
  "Upsample",
  "Upsample Top 5",
  "Downsample",
  "Downsample Top 5"
)

model_metrics <- data.frame(
  Model = c(
    "Sin Balancear",
    "Sin Balancear Top 5",
    "Upsample",
    "Upsample Top 5",
    "Downsample",
    "Downsample Top 5"
  ),
  Accuracy = c(
    round(caret_confusion_hotel$overall["Accuracy"],4),
    round(caret_confusion_hotel_top5$overall["Accuracy"],4),
    round(caret_confusion_hotel_upsample$overall["Accuracy"],4),
    round(caret_confusion_hotel_upsample_top5$overall["Accuracy"],4),
    round(caret_confusion_hotel_downsample$overall["Accuracy"],4),
    round(caret_confusion_hotel_downsample_top5$overall["Accuracy"],4)
  ),
  Precision = c(
    round(caret_confusion_hotel$byClass["Precision"],4),
    round(caret_confusion_hotel_top5$byClass["Precision"],4),
    round(caret_confusion_hotel_upsample$byClass["Precision"],4),
    round(caret_confusion_hotel_upsample_top5$byClass["Precision"],4),
    round(caret_confusion_hotel_downsample$byClass["Precision"],4),
    round(caret_confusion_hotel_downsample_top5$byClass["Precision"],4)
  ),
  Recall = c(
    round(caret_confusion_hotel$byClass["Recall"],4),
    round(caret_confusion_hotel_top5$byClass["Recall"],4),
    round(caret_confusion_hotel_upsample$byClass["Recall"],4),
    round(caret_confusion_hotel_upsample_top5$byClass["Recall"],4),
    round(caret_confusion_hotel_downsample$byClass["Recall"],4),
    round(caret_confusion_hotel_downsample_top5$byClass["Recall"],4)
  ),
  F1_Score = c(
    round(2*(
      (caret_confusion_hotel$byClass["Precision"]*caret_confusion_hotel$byClass["Recall"])/
        (caret_confusion_hotel$byClass["Precision"]+caret_confusion_hotel$byClass["Recall"])
    ),4),
    round(2*(
      (caret_confusion_hotel_top5$byClass["Precision"]*caret_confusion_hotel_top5$byClass["Recall"])/
        (caret_confusion_hotel_top5$byClass["Precision"]+caret_confusion_hotel_top5$byClass["Recall"])
    ),4),
    round(2*(
      (caret_confusion_hotel_upsample$byClass["Precision"]*caret_confusion_hotel_upsample$byClass["Recall"])/
        (caret_confusion_hotel_upsample$byClass["Precision"]+caret_confusion_hotel_upsample$byClass["Recall"])
    ),4),
    round(2*(
      (caret_confusion_hotel_upsample_top5$byClass["Precision"]*caret_confusion_hotel_upsample_top5$byClass["Recall"])/
        (caret_confusion_hotel_upsample_top5$byClass["Precision"]+caret_confusion_hotel_upsample_top5$byClass["Recall"])
    ),4),
    round(2*(
      (caret_confusion_hotel_downsample$byClass["Precision"]*caret_confusion_hotel_downsample$byClass["Recall"])/
        (caret_confusion_hotel_downsample$byClass["Precision"]+caret_confusion_hotel_downsample$byClass["Recall"])
    ),4),
    round(2*(
      (caret_confusion_hotel_downsample_top5$byClass["Precision"]*caret_confusion_hotel_downsample_top5$byClass["Recall"])/
        (caret_confusion_hotel_downsample_top5$byClass["Precision"]+caret_confusion_hotel_downsample_top5$byClass["Recall"])
    ),4)
  ),
  stringsAsFactors = FALSE # Para qué sirve esto? -> Evita que lo character se convierta en factor por temas de compatibilidad
)


create_metric_plot <- function(metric_name) {
  ggplot(
    model_metrics[order(model_metrics[[metric_name]], decreasing = TRUE), ], 
    aes(x = reorder(Model, -!!sym(metric_name)), y = !!sym(metric_name))
  ) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = !!sym(metric_name), 
                  y = !!sym(metric_name) / 2),  # Ajuste de la posición y
              size = 5,                  
              fontface = "bold",         
              color = "white",           
              hjust = 0.5) +             
    labs(title = paste("Comparación de modelos por", metric_name),
         x = "Modelos",
         y = metric_name) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Crear gráficos para cada métrica
plot_accuracy <- create_metric_plot("Accuracy")
plot_precision <- create_metric_plot("Precision")
plot_recall <- create_metric_plot("Recall")
plot_f1 <- create_metric_plot("F1_Score")

# Convertir a gráficos interactivos con plotly
interactive_accuracy <- ggplotly(plot_accuracy)
interactive_precision <- ggplotly(plot_precision)
interactive_recall <- ggplotly(plot_recall)
interactive_f1 <- ggplotly(plot_f1)

# Mostrar los gráficos
interactive_accuracy
interactive_precision
interactive_recall
interactive_f1


# Guardado de modelos de RF

#named_model_list <- setNames(model_list, model_names)

#for (nombre in names(named_model_list)) {
#  saveRDS(named_model_list[[nombre]], paste0(nombre,".rds"))
#}



#==================================================#
#                      SHINY                       #
#==================================================#


# UI
ui <- navbarPage(
  "Hoteles Random Forest",
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet", 
              href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"),
    tags$style(HTML("
      body { 
        background-color: #f4f4f4; 
        font-family: Arial, sans-serif;
      }
      .navbar { background-color: #7db8a2; color: white; }
      .navbar-brand, .navbar-nav > li > a { color: white; }
      .navbar-nav > li > a:hover { color: #d6dd90; }
      .box { margin: 20px; }
    "))
  ),
  
  # Tab 1: Introducción
  tabPanel(
    tagList(icon("home"), "Home"),
    div(class = "fondo-imagen"), 
    tags$style(HTML("
      .center-content {
        display: flex;
        justify-content: center;
        align-items: center;
        flex-direction: column;
        text-align: center;
      }
    ")),
    fluidRow(
      box(
        width = 12,
        div(
          style = "text-align: center;",
          img(src="arbol_intro.png", height = "400px", width = "400px"))
      ),
      
      box(class = "center-content",
          h1("Integrantes del Equipo"),
          width = 12,
          fluidRow(
            tags$ul(
              style = "list-style-type: none; padding-left: 0; color: black; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; font-size: 20px;",
              tags$li(
                tags$a(
                  href = "https://www.linkedin.com/in/martin-gutierrez-data-analyst/", 
                  "Gutiérrez, Martín",
                  target = "_blank", 
                  style = "color: black; text-decoration: none;",
                  onmouseover = "this.style.color='blue';",
                  onmouseout = "this.style.color='black';"
                )
              ),
              tags$li(
                tags$a(
                  href = "https://www.linkedin.com/in/maricel-flamini-19a433222/", 
                  "Flamini, Maricel",
                  target = "_blank", 
                  style = "color: black; text-decoration: none;",
                  onmouseover = "this.style.color='blue';",
                  onmouseout = "this.style.color='black';"
                )
              )
            )
            
          )
      ),
      
    ),
    
  ),
  
  # Tab 2: Teoría
  tabPanel("Teoría",
           div(class = "fondo-imagen"), 
           fluidRow(
             column(6,
                    box(width = 12,
                        h3("¿Qué es Random Forest?"),
                        p(HTML("<span style='font-size: 16px; line-height: 1.5;'>
                              Random Forest es un algoritmo de aprendizaje supervisado que utiliza múltiples árboles de decisión para mejorar la precisión del modelo. 
                              Funciona mediante un ensamblado (ensemble) que reduce problemas como el <b>overfitting</b> al combinar los resultados de varios árboles.
                              </span>")),
                        
                        h3("¿Cómo funciona?"),
                        p(HTML("<span style='font-size: 16px; line-height: 1.5;'>El proceso se basa en:</span>")),
                        tags$ul(
                          tags$li(HTML("<span style='font-size: 15px;'>Subconjuntos aleatorios de datos: Cada árbol se entrena con un subconjunto de los datos originales (<b>bagging</b>).</span>")),
                          tags$li(HTML("<span style='font-size: 15px;'>Selección aleatoria de características: En cada división del árbol, se utiliza un subconjunto aleatorio de variables.</span>")),
                          tags$li(HTML("<span style='font-size: 15px;'>Predicción final:</span>")),
                          tags$ul(
                            tags$li(HTML("<span style='font-size: 14px;'>Clasificación: se toma el <b>voto mayoritario</b>.</span>")),
                            tags$li(HTML("<span style='font-size: 14px;'>Regresión: se calcula el <b>promedio</b> de las predicciones.</span>"))
                          )
                        ),
                        
                        h3("Ventajas y desventajas"),
                        p(HTML("<span style='font-size: 16px; line-height: 1.5;'>A continuación, sus puntos fuertes y débiles:</span>")),
                        tags$ul(
                          tags$li(HTML("<span style='font-size: 15px;'><b>Ventajas:</b></span>")),
                          tags$ul(
                            tags$li(HTML("<span style='font-size: 14px;'>Robusto frente a valores atípicos.</span>")),
                            tags$li(HTML("<span style='font-size: 14px;'>Maneja bien datos con muchas características.</span>")),
                            tags$li(HTML("<span style='font-size: 14px;'>Reduce el riesgo de <b>overfitting</b>.</span>"))
                          ),
                          tags$li(HTML("<span style='font-size: 15px;'><b>Desventajas:</b></span>")),
                          tags$ul(
                            tags$li(HTML("<span style='font-size: 14px;'>Puede ser menos interpretable.</span>")),
                            tags$li(HTML("<span style='font-size: 14px;'>Requiere más recursos computacionales para grandes conjuntos de datos.</span>"))
                          )
                        ),
                        
                        h3("Implementación en R"),
                        p(HTML("<span style='font-size: 16px; line-height: 1.5;'>Ejemplo básico con el paquete <b>randomForest</b>:</span>")),
                        p(HTML("<code>library(randomForest)</code>")),
                        p(HTML("<code>modelo <- randomForest(y ~ ., data = datos, ntree = 100, mtry = 3)</code>")),
                        p(HTML("<span style='font-size: 14px;'>• <b>ntree</b>: Número de árboles (100 en este caso).</span>")),
                        p(HTML("<span style='font-size: 14px;'>• <b>mtry</b>: Número de características consideradas en cada división (3 aquí).</span>"))
                    )
             ),
             column(6,
                    box(width = 12,
                        img(src = "Arbol.jpg", alt = "Imagen de Random Forest", width = "100%")
                    )
             )
           )
  ),
  
  # Tab 3: Dataset
  tabPanel("Dataset",
           div(class = "fondo-imagen"), 
           fluidRow(
             column(12,
                    box(width = 12,
                        h4("Descripción de las Variables"),
                        tags$ul(
                          tags$li(HTML("<span style='font-size: 14px;'><b>Booking_ID:</b> Identificador único para cada reserva.</span>")),
                          tags$li(HTML("<span style='font-size: 14px;'><b>number of adults:</b> Número de adultos incluidos en la reserva.</span>")),
                          tags$li(HTML("<span style='font-size: 14px;'><b>number of children:</b> Número de niños incluidos en la reserva.</span>")),
                          tags$li(HTML("<span style='font-size: 14px;'><b>number of weekend nights:</b> Número de noches de fin de semana incluidas en la reserva.</span>")),
                          tags$li(HTML("<span style='font-size: 14px;'><b>number of week nights:</b> Número de noches entre semana incluidas en la reserva.</span>")),
                          tags$li(HTML("<span style='font-size: 14px;'><b>type of meal:</b> Tipo de comida incluida en la reserva.</span>")),
                          tags$li(HTML("<span style='font-size: 14px;'><b>car parking space:</b> Indica si se solicitó o incluyó un espacio de estacionamiento.</span>")),
                          tags$li(HTML("<span style='font-size: 14px;'><b>room type:</b> Tipo de habitación reservada.</span>")),
                          tags$li(HTML("<span style='font-size: 14px;'><b>lead time:</b> Número de días entre la fecha de reserva y la fecha de llegada.</span>")),
                          tags$li(HTML("<span style='font-size: 14px;'><b>market segment type:</b> Tipo de segmento de mercado asociado con la reserva.</span>")),
                          tags$li(HTML("<span style='font-size: 14px;'><b>repeated:</b> Indica si la reserva es repetida.</span>")),
                          tags$li(HTML("<span style='font-size: 14px;'><b>P-C:</b> Número de reservas anteriores canceladas por el cliente antes de la reserva actual.</span>")),
                          tags$li(HTML("<span style='font-size: 14px;'><b>P-not-C:</b> Número de reservas anteriores no canceladas por el cliente antes de la reserva actual.</span>")),
                          tags$li(HTML("<span style='font-size: 14px;'><b>average price:</b> Precio promedio asociado con la reserva.</span>")),
                          tags$li(HTML("<span style='font-size: 14px;'><b>special requests:</b> Número de solicitudes especiales realizadas por el huésped.</span>")),
                          tags$li(HTML("<span style='font-size: 14px;'><b>date of reservation:</b> Fecha de la reserva.</span>")),
                          tags$li(HTML("<span style='font-size: 14px;'><b>booking status:</b> Estado de la reserva (cancelada o no cancelada).</span>"))
                        )
                    )
             ),
             fluidRow(
               column(12,
                      box(width = 12,
                          h4("Vista del Dataset"),
                          DTOutput("dataset_table")
                      )
               )
             ),
             fluidRow(
               column(6,
                      box(width = 12,
                          h4("Summary del Dataset"),
                          verbatimTextOutput("summary_output")
                      )
               ),
               column(6,
                      box(width = 12,
                          h4("Estructura del Dataset"),
                          verbatimTextOutput("str_output")
                      )
               )
             )
           )
  ),
  
  # Tab 4: EDA
  tabPanel("EDA",
           div(class = "fondo-imagen"), 
           fluidRow(
             box(div(style = "padding: 10px;", plotlyOutput("EDA_1")), width = 12),
             hr(style = "border: 1px solid #000; margin: 10px 0;"),
             box(div(style = "padding: 10px;", plotlyOutput("EDA_2")), width = 12),
             hr(style = "border: 1px solid #000; margin: 10px 0;"),
             box(div(style = "padding: 10px;", plotlyOutput("EDA_9")), width = 12),
             hr(style = "border: 1px solid #000; margin: 10px 0;"),
             box(div(style = "padding: 10px;", plotlyOutput("EDA_10")), width = 12),
             hr(style = "border: 1px solid #000; margin: 10px 0;"),
             box(div(style = "padding: 10px;", plotlyOutput("EDA_11")), width = 12),
             hr(style = "border: 1px solid #000; margin: 10px 0;"),
             box(div(style = "padding: 10px;", plotlyOutput("EDA_12")), width = 12),
             hr(style = "border: 1px solid #000; margin: 10px 0;"),
             box(div(style = "padding: 10px;", plotlyOutput("EDA_13")), width = 12),
             hr(style = "border: 1px solid #000; margin: 10px 0;"),
             box(div(style = "padding: 10px;", plotlyOutput("EDA_7", width = "100%", height = "100%")), width = 12)
           )
  ),
  
  # Tab 5: Modelos
  navbarMenu("Modelos",
             div(class = "fondo-imagen"), 
             # 5.1 Sin Balancear
             tabPanel("Sin Balancear",
                      fluidRow(
                        h3("Modelo 1 : Sin Balancear + Todas las Variables"),
                        column(6,
                               box(width = 12,
                                   plotlyOutput("sin_balancear_plot"),
                                   plotlyOutput("sin_balancear_importance_plot")
                               )
                        ),
                        column(6,
                               box(width = 12,
                                   verbatimTextOutput("caret_confusion_hotel"),
                                   plotlyOutput("sin_balancear_oob_plot")
                               )
                        ),
                        hr(style = "border: 1px solid #000; margin: 10px 0;")
                      ),
                      fluidRow(
                        h3("Modelo 2 : Sin Balancear + Top 5 Variables"),
                        column(6,
                               box(width = 12,
                                   plotlyOutput("sin_balancear_top_plot"),
                                   plotlyOutput("sin_balancear_top_importance_plot")
                               )
                        ),
                        column(6,
                               box(width = 12,
                                   verbatimTextOutput("caret_confusion_hotel_top5"),
                                   plotlyOutput("sin_balancear_top_oob_plot")
                               )
                        )
                      )
             ),
             
             # 5.2 Upsampling
             tabPanel("Upsampling",
                      fluidRow(
                        h3("Modelo 3 : Upsampling + Todas las Variables"),
                        column(6,
                               box(width = 12,
                                   plotlyOutput("upsampling_plot"),
                                   plotlyOutput("upsample_importance_plot")
                               )
                        ),
                        column(6,
                               box(width = 12,
                                   verbatimTextOutput("caret_confusion_hotel_upsample"),
                                   plotlyOutput("upsample_oob_plot")
                               )
                        ),
                        hr(style = "border: 1px solid #000; margin: 10px 0;")
                      ),
                      fluidRow(
                        h3("Modelo 4 : Upsampling + Top 5 Variables"),
                        column(6,
                               box(width = 12,
                                   plotlyOutput("upsampling_top_plot"),
                                   plotlyOutput("upsample_top_importance_plot")
                               )
                        ),
                        column(6,
                               box(width = 12,
                                   verbatimTextOutput("caret_confusion_hotel_upsample_top5"),
                                   plotlyOutput("upsample_top_oob_plot")
                               )
                        )
                      )
             ),
             
             # 5.3 Downsampling
             tabPanel("Downsampling",
                      fluidRow(
                        h3("Modelo 5 : Downsampling + Todas las Variables"),
                        column(6,
                               box(width = 12,
                                   plotlyOutput("downsampling_plot"),
                                   plotlyOutput("downsample_importance_plot")
                               )
                        ),
                        column(6,
                               box(width = 12,
                                   verbatimTextOutput("caret_confusion_hotel_downsample"),
                                   plotlyOutput("downsample_oob_plot")
                               )
                        ),
                        hr(style = "border: 1px solid #000; margin: 10px 0;")
                      ),
                      fluidRow(
                        h3("Modelo 6 : Upsampling + Top 5 Variables"),
                        column(6,
                               box(width = 12,
                                   plotlyOutput("downsampling_top_plot"),
                                   plotlyOutput("downsample_top_importance_plot")
                               )
                        ),
                        column(6,
                               box(width = 12,
                                   verbatimTextOutput("caret_confusion_hotel_downsample_top5"),
                                   plotlyOutput("downsample_top_oob_plot")
                               )
                        )
                      )
             )
  ),
  
  # Tab 6: Comparación de modelos
  tabPanel("Comparación de modelos",
           div(class = "fondo-imagen"), 
           fluidRow(
             p("Accuracy (Exactitud): Mide cuántas de las predicciones fueron acertadas."),
             p("Accuracy: ¿Que tanto acierta el modelo en sus predicciones?"),
             box(div(style = "padding: 10px;", plotlyOutput("interactive_accuracy")), width = 12),
             
             hr(style = "border: 1px solid #000; margin: 10px 0;"),
             p("Precision (Precisión): Mide cuántas de las predicciones positivas del modelo fueron correctas."),
             p("Precision: ¿Qué tan confiable es cuando dice 'positivo'?"),
             box(div(style = "padding: 10px;", plotlyOutput("interactive_precision")), width = 12),
             
             hr(style = "border: 1px solid #000; margin: 10px 0;"),
             p("Recall (Sensibilidad) Mide cuántos de los casos positivos reales fueron correctamente identificados."),
             p("Recall: ¿Qué tan bien encuentra todos los 'positivos'?"),
             box(div(style = "padding: 10px;", plotlyOutput("interactive_recall")), width = 12),
             
             hr(style = "border: 1px solid #000; margin: 10px 0;"),
             p("F1-Score Es la media armónica de Precision y Recall. Útil cuando hay un trade-off entre ambas métricas."),
             p("F1-Score: Equilibrio entre Precision y Recall."),
             box(div(style = "padding: 10px;", plotlyOutput("interactive_f1")), width = 12),
           )
  ),
  
  # Tab 7: Interactivo
  tabPanel("Interactivo",
           div(class = "fondo-imagen"), 
           sidebarLayout(
             sidebarPanel(
               numericInput("lead_time", "Tiempo de anticipación (días):", value = 30),
               numericInput("average_price", "Precio promedio ($):", value = 100),
               numericInput("special_requests", "Solicitudes especiales:", value = 0),
               numericInput("day", "Día de la reserva:", value = 15),
               numericInput("month", "Mes de la reserva:", value = 6),
               numericInput("Dias", "Cantidad de días:", value = 1),
               
               actionButton("predict_btn", "Predecir", icon = icon("calculator"), 
                            class = "btn-success"),
               actionButton("reset_btn", "Resetear", icon = icon("redo"), 
                            class = "btn-danger")
             ),
             mainPanel(
               textOutput("prediction_result"),
               textOutput("economic_analysis"),
               uiOutput("imagen")
             )
           )
  )
)

# Server
server <- function(input, output, session) {
  
  # Dataset
  output$dataset_table <- renderDT({
    datatable(hotel_1,
              extensions = c('Buttons'),
              options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'print')
              )
    )
  })
  
  output$summary_output <- renderPrint({
    summary(hotel)
  })
  
  output$str_output <- renderPrint({
    str(hotel)
  })
  
  # EDA
  output$EDA_1 <- renderPlotly({EDA_1})
  output$EDA_2 <- renderPlotly({EDA_2})
  output$EDA_7 <- renderPlotly({EDA_7})
  output$EDA_9 <- renderPlotly({EDA_9})
  output$EDA_10 <- renderPlotly({EDA_10})
  output$EDA_11 <- renderPlotly({EDA_11})
  output$EDA_12 <- renderPlotly({EDA_12})
  output$EDA_13 <- renderPlotly({EDA_13})
  
  # Modelos
  
  # Sin Balancear
  
  # Modelo 1
  output$sin_balancear_plot <- renderPlotly({sin_balancear_plot})
  
  output$sin_balancear_importance_plot <- renderPlotly({sin_balancear_importance_plot})
  
  output$caret_confusion_hotel <- renderPrint({
    print(caret_confusion_hotel)
  })
  
  output$sin_balancear_oob_plot <- renderPlotly({sin_balancear_oob_plot})
  
  
  # Modelo 2
  output$sin_balancear_top_plot <- renderPlotly({sin_balancear_top_plot})
  
  output$sin_balancear_top_importance_plot <- renderPlotly({sin_balancear_top_importance_plot})
  
  output$caret_confusion_hotel_top5 <- renderPrint({
    print(caret_confusion_hotel_top5)
  })
  
  output$sin_balancear_top_oob_plot <- renderPlotly({sin_balancear_top_oob_plot})
  
  # Upsample
  
  # Modelo 3
  output$upsampling_plot <- renderPlotly({upsampling_plot})
  
  output$upsample_importance_plot <- renderPlotly({upsample_importance_plot})
  
  output$caret_confusion_hotel_upsample <- renderPrint({
    caret_confusion_hotel_upsample
  })
  
  output$upsample_oob_plot <- renderPlotly({upsample_oob_plot})
  
  
  # Modelo 4
  output$upsampling_top_plot <- renderPlotly({upsampling_top_plot})
  
  output$upsample_top_importance_plot <- renderPlotly({upsample_top_importance_plot})
  
  output$caret_confusion_hotel_upsample_top5 <- renderPrint({
    caret_confusion_hotel_upsample_top5
  })
  
  output$upsample_top_oob_plot <- renderPlotly({upsample_top_oob_plot})
  
  # Downsample
  
  # Modelo 5
  output$downsampling_plot <- renderPlotly({downsampling_plot})
  
  output$downsample_importance_plot <- renderPlotly({downsample_importance_plot})
  
  output$caret_confusion_hotel_downsample <- renderPrint({
    caret_confusion_hotel_downsample
  })
  
  output$downsample_oob_plot <- renderPlotly({downsample_oob_plot})
  
  # Modelo 6
  output$downsampling_top_plot <- renderPlotly({downsampling_top_plot})
  
  output$downsample_top_importance_plot <- renderPlotly({downsample_top_importance_plot})
  
  output$caret_confusion_hotel_downsample_top5 <- renderPrint({
    caret_confusion_hotel_downsample_top5
  })
  
  output$downsample_top_oob_plot <- renderPlotly({downsample_top_oob_plot})
  
  
  # Comparación de modelos
  
  output$interactive_accuracy <- renderPlotly({interactive_accuracy})
  output$interactive_precision <- renderPlotly({interactive_precision})
  output$interactive_recall <- renderPlotly({interactive_recall})
  output$interactive_f1 <- renderPlotly({interactive_f1})
  
  
  # Predicción Interactiva
  predict_booking_status <- reactive({
    user_input <- data.frame(
      lead.time = input$lead_time,
      average.price = input$average_price,
      special.requests = input$special_requests,
      day = input$day,
      month = input$month
    )
    predict(hotel_model_upsample_top, user_input)
  })
  
  observeEvent(input$predict_btn, {
    #   output$prediction_result <- renderText({
    #     paste("El estado de la reserva predicho es:", predict_booking_status())
    #   })
    # Mostrar el resultado
    output$prediction_result <- renderText({
      ifelse (predict_booking_status() == "Not_Canceled",
              "😊 La predicción es que no cancelan el hospedaje.",
              "😞 La predicción es que cancelan el hospedaje."
              
      )
    })
    output$imagen <- renderUI({
      if (predict_booking_status() == "Not_Canceled") {
        img(src = "Viaje.jpg", height = "300px")
      } else {
        img(src = "Cancelado.jpg", height = "300px")
      }
    })
    # Calcular el ingreso esperado basado en la predicción
    expected_revenue <- ifelse(
      predict_booking_status() == "Not_Canceled",
      input$average_price * input$Dias,           # Sin cancelación: Ganancia completa
      (input$average_price * input$Dias) * 0.40   # Con cancelación: 40% de la ganancia
    )
    # Mostrar el análisis económico
    output$economic_analysis <- renderText({
      paste("Ingreso esperado basado en la predicción:", 
            round(expected_revenue, 2), "USD")
    })
  })
  
  
  observeEvent(input$reset_btn, {
    
    updateNumericInput(session, "lead_time", value = 30)
    updateNumericInput(session, "average_price", value = 100)
    updateNumericInput(session, "special_requests", value = 0)
    updateNumericInput(session, "day", value = 15)
    updateNumericInput(session, "month", value = 6)
    output$imagen <- renderUI({
      NULL  # Eliminar la imagen
    })
    
    # Limpiar los resultados de la predicción
    output$prediction_result <- renderText("")
    output$economic_analysis <- renderText("")
    
    output$prediction_result <- renderText({
      "El estado de la reserva predicho aparecerá aquí."
    })
    output$economic_analysis <- renderText("")
  })
}

# Run the app
shinyApp(ui = ui, server = server)