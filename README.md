*Statistical-Analysis-in-R-Software*
# Heart Disease Risk Assessment

Heart disease is a significant global health issue, and early detection is crucial for effective management and treatment. This project combines five distinct datasets—Cleveland, Hungarian, Switzerland, Long Beach VA, and Statlog (Heart)—into a comprehensive collection aimed at improving research on coronary artery disease (CAD).

The dataset comprises 1,190 instances with 11 common features, making it the largest heart disease dataset available for research purposes. Our analysis focuses on predicting the likelihood of heart disease in individuals based on various medical and demographic factors. By identifying patterns and statistical significance among these factors, we aim to facilitate early detection and preventive care for heart disease.
Data Description
Dataset Characteristics

 Categorical Features: 0, 1, 2
    Class: 0, 1 (Heart Disease: Yes or No)
    Number of Instances: 1,190
    Features: 11 common attributes across datasets

Objectives

The primary objective of this project is to analyze the combined datasets to improve clinical diagnosis and early treatment of heart disease by predicting individual risk based on various factors.
Methodology
Analytical Techniques

Our analysis employs logistic regression, ANOVA, and ANCOVA to explore the key factors influencing heart disease risk.
Logistic Regression

Key Factors: The analysis revealed that the following variables significantly influence the likelihood of heart disease:
        Age
        Sex
        Chest pain type
        Cholesterol levels
        Fasting blood sugar
        Maximum heart rate achieved
        Exercise-induced angina
        ST depression (oldpeak)
        ST slope

ANOVA and ANCOVA

Chest Pain Type: ANOVA results showed that chest pain type significantly affects maximum heart rate, with age as a covariate demonstrating its impact.
    Cholesterol Levels: ANCOVA analysis indicated that sex significantly influences cholesterol levels, especially when age is considered as a covariate.

Key Findings

Sex as a Predictor: Males exhibit a significantly higher risk of heart disease compared to females, making sex a potent predictor.
    Crucial Indicators: Factors such as chest pain type, cholesterol levels, fasting blood sugar, and maximum heart rate are critical indicators of heart disease risk.

Implications

The findings from this analysis provide actionable insights for risk assessment and personalized healthcare strategies. Understanding these key factors can aid in developing preventive interventions and optimizing treatment plans for individuals at risk of heart disease.
Conclusion

This project highlights the importance of analyzing comprehensive datasets to improve heart disease risk assessment. The integration of various datasets allows for a more thorough understanding of the factors influencing heart disease, ultimately facilitating better clinical outcomes through early detection and tailored interventions.

