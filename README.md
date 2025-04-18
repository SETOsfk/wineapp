# 🍷 Wine Quality Explorer App

This repository contains an **interactive Shiny web application** designed to explore wine quality datasets through **dynamic visualizations**, **statistical summaries**, and **machine learning models**. Built using **R** and **Shiny**, the app helps users analyze the factors influencing wine quality in an intuitive, visual way.

---

## 🚀 Features

- 📊 **Interactive visualizations** with `ggplot2` and `plotly`
- 🔍 **Exploratory data analysis** on physicochemical attributes
- 🧪 **Machine learning models** (classification/prediction of wine quality)
- 🎛️ Filter and compare wines by chemical properties
- 📈 Correlation heatmaps, scatter plots, and boxplots
- 💡 Suitable for both red and white wine datasets

---

## 🧰 Technologies Used

- **R**, **Shiny**, **tidyverse**
- `ggplot2`, `plotly`, `caret`, `shinyWidgets`, `dplyr`, `data.table`

---

## 📦 Folder Structure

```
wineapp/
├── app.R               # Main Shiny app script
├── data/               # Wine datasets (.rmd files)
└── README.md
```

---

## ▶️ Getting Started

1. Clone this repository:
   ```bash
   git clone https://github.com/SETOsfk/wineapp.git
   ```
2. Open `app.R` in **RStudio**
3. Install dependencies if needed:
   ```r
   install.packages(c("shiny", "tidyverse", "plotly", "caret", "shinyWidgets", "data.table"))
   ```
4. Run the app:
   ```r
   shiny::runApp()
   ```

---

## 📊 Dataset

The app uses the **Wine Quality Dataset** available on UCI Machine Learning Repository, including physicochemical tests and quality scores for both **red and white** wines.

- [Wine Quality Data (UCI)](https://archive.ics.uci.edu/ml/datasets/wine+quality)

---

## 📄 License

This project is open-source and licensed under the MIT License.  
See the [LICENSE](LICENSE) file for details.

---

## 👤 Author

**Sertan Şafak**  
📧 [LinkedIn](https://www.linkedin.com/in/sertan-%C5%9Fafak-990a2a172/) • 🌐 [GitHub](https://github.com/SETOsfk)

---
