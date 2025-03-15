# MCDA-Rshiny-Nigeria
Multi-criteria decision analysis tool **(MCDA)** to support strengthening community health workforce using a blended approach, including data extractions, spatial optimization, and R Shiny dashboard. 

# 1. Preaparing datasets for analysis

## 1.4 State-level Malaria indicators
Survey datasets from Nigeria Malaria Indicator Survey 2021 (MIS-VIII) were accessed through DHS program. Following datasets were used:
| Dataset |	Unit of analysis | Indicators |
| --- | --- | --- |
| Household Data - Household Recode (HR) |	Household	|	Household ownership of ITNs ‘hh_ITN’ |
| Household Listing Data - Household Member Recode (PR) |	Household member |	Parasitaemia (via microscopy) in children 6-59 months ‘pr_micro_chld’ |
| Children's Data - Children's Recode (KR) | Children of women born in the last 5 years (0-59 months)	|	Children under age 5 years with fever in the 2 weeks preceding the survey ‘kr_fever’ <br />	Advice or treatment was sought the same or next day ‘kr_fev_day’ |

## 1.5 LGA-level Malaria prevalence data
We used updated dataset on percentage of children under 5 years of age who have PF parasitaemia with RDT test at LGA-level by Malaria Atlas Project
