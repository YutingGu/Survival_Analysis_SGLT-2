# Survival Analysis on SGLT2 Inhibitors for Secondary Prevention of Myocardial Infarction
### Overview
This repository contains the code and analysis for a coursework project that investigates the role of Sodium-glucose Cotransporter-2 (SGLT-2) inhibitors in the secondary prevention of myocardial infarction (MI). The focus of this project is on the survival analysis of MI survivors over a five-year period, examining the effect of SGLT-2 inhibitors on reducing the risk of a secondary MI.

Myocardial infarction (MI) is a leading cause of mortality and morbidity worldwide. In the UK alone, over a million people live as MI survivors, many of whom are at significant risk of experiencing a second MI. Sodium-glucose Cotransporter-2 (SGLT-2) inhibitors, originally designed for managing blood glucose levels in diabetic patients, have shown promising results in reducing cardiovascular events, including secondary MI.

### Objectives
This project specifically aims to assess the efficacy of SGLT-2 inhibitors in reducing the risk of secondary MI, using survival analysis techniques such as Cox proportional hazards regression, Kaplan-Meier estimates, and time-split analyses.
The main objective of this project is to determine whether SGLT-2 inhibitors can lower the incidence of secondary MI, focusing on differences in treatment effects across gender and time.

1. **Medication adherence:** Logistic regression was used to identify factors influencing adherence to SGLT-2 inhibitors.
2. **Survival analysis:** Cox regression models were employed to investigate the impact of SGLT-2 inhibitors on secondary MI risk, with a particular focus on time-to-event analysis.
3. **Blood pressure outcomes:** The effect of SGLT-2 inhibitors on systolic blood pressure was evaluated through linear regression and t-tests.

### Key Findings
- **Survival Analysis:** The survival analysis revealed that SGLT-2 inhibitors significantly reduced the risk of secondary MI in male participants, particularly after the first 1.5 years of treatment. The results suggest that the protective effect of SGLT-2 inhibitors becomes more pronounced over time.
- **Gender Differences:** While male participants showed a significant reduction in MI risk, the analysis did not find a statistically significant effect for female participants. This may be attributed to the smaller number of female participants in the study.
- **Blood Pressure:** Participants taking SGLT-2 inhibitors experienced a significant reduction in systolic blood pressure compared to those receiving usual care, further supporting the use of these inhibitors for comprehensive cardiovascular management.

### Data
Data were obtained from a randomised controlled trial involving 1,988 MI survivors across four regions in the UK. The trial included both a treatment group receiving SGLT-2 inhibitors and a control group receiving usual care, with follow-up data collected over five years. Data used in this project can be found in [mi_sglt2.csv](mi_sglt2.csv)

### Methodology
The project utilised survival analysis methods to analyse time-to-event data:

- **Cox Proportional Hazards Regression:** To assess the effect of SGLT-2 inhibitors on the incidence of secondary MI over time, while controlling for baseline BMI and stratifying by gender.
- **Kaplan-Meier Estimates:** To visualise survival probabilities and compare the cumulative hazard functions between treatment groups.
Time-Split Analysis: A time-split Cox regression was performed to evaluate how the effect of SGLT-2 inhibitors varied over different time periods.

Here is the [R Scripts](mini-project.R) for all analysis and modelling.

### Conclusion
This project demonstrates the potential of SGLT-2 inhibitors to reduce secondary MI risk in male survivors, particularly after long-term use. However, further research is required to confirm these benefits in female patients and to explore additional factors influencing medication adherence.

### Analysis Report
Full analysis report can be found in [here](Project_Report.pdf).
