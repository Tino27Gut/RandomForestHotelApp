# 🌳 Random Forest Hotel Predictive Analysis 🏨

##### Hotel Booking Cancellation Predictive Analytics App - R Project in [Python and R Data Science Diploma]

## 🔎 What You'll Find
This project implements a predictive analysis of hotel booking cancellations using Random Forest.
The analytical application is divided into the following sections:

1. **Theory** *(Teoría)*: Theoretical foundations of the Random Forest algorithm and its applications
2. **EDA**: Exploratory data analysis with interactive visualizations
3. **Models** *(Modelos)*: Implementation and evaluation of different Random Forest models
4. **Model Comparison** *(Comparación de Modelos)*: Comparative evaluation of different configurations' performance
5. **Interactive** *(Interactivo)*: Real-time prediction interactive module

## 💡 Main Insights
- Most influential variables in predictions were [lead.time], [average.price], [day], [month], and [special.requests]
- Clients are more likely to cancel their reservations when there is a considerable time period between booking and arrival date [lead.time]
- Cancellations are most common for rooms priced around $110/day [average.price]
- Bookings are more likely to be cancelled if made in the middle of the month or middle of the year [day], [month]
- The higher the number of special requests, the less likely clients are to cancel their reservation [special.requests]
- The best Random Forest model implementation achieved 96% accuracy on the test set *(data balanced with upsampling technique and model trained with top 5 most influential variables)*

## 🏗️ Project Structure
- 📁 01_data: Zipped raw data
- 📁 02_rfmodels: Zipped .rds files containing trained models for faster deployment
- 📁 www: Static resources
- 📱 app.R: Main Shiny application

## ⚠️ Considerations
- To run the app on your local machine, unzip the raw data and models in the [01_data] and [02_rfmodels] folders and install necessary R dependencies.
- The application's interface is in Spanish, as this project was developed for a Spanish-language course.

## 🎬 Section Demos

### Theory
[Space for Theory section GIF]

### EDA
[Space for EDA section GIF]

### Models
[Space for Models section GIF]

### Model Comparison
[Space for Model Comparison GIF]

### Interactive
[Space for Interactive section GIF]

## 👥 Contributions
- [Maricel Flamini](https://ar.linkedin.com/in/maricel-flamini-19a433222) was a key contributor to this project

## 🔗 Sources
- **Dataset**: [Hotel Booking Cancellation Prediction](https://www.kaggle.com/datasets/youssefaboelwafa/hotel-booking-cancellation-prediction/data)

