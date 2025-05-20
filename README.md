# 🏙️ Boston 311 Service Equity Analysis

![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)
![R Version](https://img.shields.io/badge/R-v4.2.0+-blue.svg)
![Made With](https://img.shields.io/badge/Made%20With-R%20&%20ggplot2-green.svg)

---

## 📊 Project Overview

This project investigates **equity in public service response times** across Boston neighborhoods using Boston 311 service request data (2015–2019). Using exploratory data analysis, statistical modeling (OLS, stepwise, LASSO), and visual storytelling, the goal is to understand whether **response disparities** exist along lines of **race, income, education, complexity**, or **geographic region**.

---

## 🔍 Key Insights

- **Income-based disparity**: Higher-income areas consistently received faster responses for complex requests.
- **Minority group impact**: Neighborhoods with >75% minority population had **notably slower** service.
- **Seasonality & region interaction**: South Boston had slower service especially in winter.
- **Statistical models** confirmed demographic and request-level factors significantly impacted response times.

---

## 📂 Project Structure

```

📁 Boston311-Equity-Analysis/
├── Final\_Group\_Analysis.R                # Full source code
├── \*.csv                                 # Cleaned & aggregated analysis outputs
├── \*.png                                 # Final presentation-quality charts
├── regression\_results.txt                # Summary of OLS regression
├── stepwise\_regression\_results.txt       # Stepwise selection summary
├── lasso\_coefficients.txt                # LASSO model coefficients
├── README.md                             # Project overview (this file)

````

---

## 🛠️ Technologies Used

- **R 4.2.0+**
- `tidyverse`, `ggplot2`, `broom`, `glmnet`, `caret`, `patchwork`, `lubridate`, `Metrics`
- Data: [Boston 311 Public Service Requests](https://data.boston.gov/)

---

## 🧪 Analytical Methods

### ✅ OLS Regression
Used log-transformed response time with interaction effects to identify demographic, request, and seasonality effects.

### ✅ Stepwise Regression
Selected significant predictors based on AIC criteria.

### ✅ LASSO Regularization
Optimized model with penalty tuning for sparse and interpretable predictors.

---

## 🧠 Skills Demonstrated

- **Advanced Data Wrangling**: 311 records + neighborhood-level demographic merging
- **Feature Engineering**: Request complexity, region classification, socio-economic grouping
- **Regression Modeling**: Linear, interaction, stepwise, and LASSO
- **Visual Storytelling**: Custom, publication-quality plots with ggplot2
- **Equity-Oriented Analysis**: Designed to uncover structural disparity, not just performance

---

## ▶️ How to Run

1. Clone the repo:
```bash
git clone https://github.com/yourusername/Boston311-Equity-Analysis.git
cd Boston311-Equity-Analysis
````

2. Open `Final_Group_Analysis.R` in **RStudio**

3. Make sure you have the required packages installed:

```R
install.packages(c("tidyverse", "lubridate", "ggplot2", "scales", "broom",
                   "glmnet", "caret", "Metrics", "patchwork", "mice", "stargazer"))
```

4. Run the file to:

   * Clean & join the data
   * Analyze equity patterns
   * Fit and compare regression models
   * Export results + visualizations

---

## 📄 License

This project is licensed under the [MIT License](LICENSE).

---

## 🙋‍♂️ Contact

Built with ❤️ by Hittanshu Bhanderi
Connect via [LinkedIn](https://www.linkedin.com/in/hittanshubhanderi/)

---
