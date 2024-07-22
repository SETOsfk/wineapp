[Wine Recommender and Score Predictor](https://g1fccn-sertan0afak.shinyapps.io/wine_app/)

This Shiny application helps users find wines that match their preferences and predicts the score of their wine based on specific features. The app uses machine learning models and data visualization techniques to provide insights into wine selection and scoring.
I've used ["21st Century Bordeaux Wine"](https://www.kaggle.com/datasets/mexwell/21st-century-bordeaux-wine-dataset/code) dataset from kaggle. You can the analysis at "analysis" section if you want detailed analysis you can check my [kaggle](https://www.kaggle.com/sertanafak).


This is my first MLOPS project. If you have any questions or suggestions please feel free to contact. 
[Email](setosfk@outlook.com)


Features

	•	Wine Finder: Select up to 2 main words that describe your wine, and find wines that are similar based on these characteristics.
	•	Score Predictor: Predict the score of your wine based on selected features, with explanations for the predictions.
	•	Word Cloud: Visualize the most frequently used words by wine enthusiasts for wines scoring above 90 and below 89.
	•	Value Boxes: Display the most used main words by critics.
	•	Interactive Explanations: Understand why the system chose a particular score with interactive plots.
 Usage

Wine Finder

	1.	Select Columns: Choose up to 2 main descriptive words for your wine from the dropdown menu.
	2.	Find Wine: Click the “Find Wine” button to get a list of similar wines based on your selection.

Score Predictor

	1.	Select Columns: Choose the features of your wine from the dropdown menu.
	2.	Predict Score: Click the “Predict Score” button to get the predicted score and explanation of the prediction.
	3.	Understanding Predictions: View the explanation plot to understand the factors influencing the predicted score.

Word Cloud

	1.	Frequency and Number of Words: Adjust the sliders to set the minimum frequency and maximum number of words for the word cloud.
	2.	Above 90 Score Words: Click the “Most used main words from wine enthusiasts for above 90 score wines” button.
	3.	Below 89 Score Words: Click the “Most used main words from wine enthusiasts for below 89 score wines” button.
	4.	View Word Cloud: The word cloud will be displayed based on your settings.

Most Used Main Words by Critics

	•	View value boxes showing the most used main words by critics for “Finish”, “Fruit”, and “Great”.

Explanation and Information

	•	Why system choose this?: View the interactive plot explaining the prediction.
	•	How to understand critics?: Read additional information from the included markdown file.

Data

	•	Model: Pre-trained random forest model saved as model.rds.
	•	Words: Data on wordcloud usage saved as words.rds.
	•	Data Features: Data with reduced features saved as X_reduced.rds.
	•	KNN: KNN Model for "Find Your Wine" section, saved as knn.rds.

