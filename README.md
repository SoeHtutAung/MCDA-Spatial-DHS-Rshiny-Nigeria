# MCDA-Spatial-DHS-Rshiny-Nigeria
Multi-criteria decision analysis tool **(MCDA)**, using R Shiny app, to support strengthening community health workforce using a blended approach, including data extractions, spatial optimization, and R Shiny dashboard. This project used both ***public and private*** datasets.

# 1. Step-wise approach used in the project
At first step, information on disease burden, population distribution and driving distance from the capital city is used to support selection of states. At next step, the dashboard allows customized ward selection where user can select different set of parameters and apply different level of importance on each parameter to calculate vulnerability score of each ward. Through this process, number of community health workers (CHWs) needed could be estimated and priority list of wards for CHW expansion could be generated. In each ward, spatial optimization model is used to quantify exact number of CHWs needed and identify where to assign them.
<img src="https://github.com/user-attachments/assets/3fa64035-ab5f-4b66-b231-b3e8acd8cb83" title="process" width="600"> <br/>

# 2. Creating ward-level datasets
To enable this approach, various spatial datasets and survey datasets are used to generate parameters at granular (ward) level.
## 2.1 ETL
Refer to *Chapter 3.4 Data sources and extraction* of `docs/MSc project report_20250311.pdf` for data sources and extraction methods. Raw data files are kept in `data/` folder, but they are not uploaded in this public repository.
## 2.2 Preparing dataset
Refer to *Chapter 3.4 Data manipulation* of `docs/MSc project report_20250311.pdf` for data management steps. Scripts for ETL processed are kept in `scripts/` folder. From spatial and other datasets, ward-level final datasets are created and kept as shape file in `data/shp/NGA_wards_dashboard`. Other outputs from data processing are saved in `outputs/` folder.
### Travel time to nearest health facility
<img src="https://github.com/user-attachments/assets/05444eae-da48-480d-8330-f97634a5bf04" title="travel time" width="600"> <br/>
Percentage of population (left) and population size (right) at wards, who are living beyond 1 hour distance from the nearest PHC facilities (GRID3 – Health facilities list, WorldPop – Population surface, and MAP – Friction surface)
### Population weighted malaria indicators at ward-level
<img src="https://github.com/user-attachments/assets/cebcd834-8a62-4d2f-9723-039d3efe1e7d" title="malaria" width="600"> <br/>
Estimated Malaria incidence rate (left) and mortality rate (right) at wards in 2022 (MAP - Malaria surfaces)

# 3. Creating interactive dashboard (RShiny App)
Using `data/process.R` necessary packages and datasets are loaded. Scripts for dashboard are kept in `shiny_dashboard/` folder. 
### Page 1: Scenario setting
<img src="https://github.com/user-attachments/assets/b212bd80-6c91-4285-b2e1-d16868b40661" title="scenario setting" width="600"> <br/>
Screenshots of **‘Scenario setting’** page with defined parameters and selected states. (In ‘Adjust parameters of vulnerability score’ box at top left corner, there are three tabs for three parameters: ‘Malaria’ - Disease burden, ‘Intervention’ – Intervention coverage and ‘Accessibility’ – Accessibility to PHC facilities. At each tab, selected indicators and their importance levels are displayed. In bubble plot, different indicators can be displayed on x and y axis.)
### Page 2: Ward selection
<img src="https://github.com/user-attachments/assets/72b7e605-f344-498f-ae44-293613e6e502" title="ward selection" width="600"> <br/>
Screenshots of **‘Ward selection’** page with set parameters. (In ‘Ward parameters’ box at top left corner, there are 4 options to select regarding median travel time to nearest PHC facilities to filter wards. While tuning the parameters, summary information about each state and selected CHW expansion strategy could be seen simultaneously in the ‘Summary information’ box at right bottom corner)
### Page 3: View map
<img src="https://github.com/user-attachments/assets/a4288d98-abaf-4921-b44e-b5cb7808a191" title="view page" width="500"> <img src="https://github.com/user-attachments/assets/9c49f742-3a88-4b0d-95ec-d3908db08cf5" title="view page more" width="400"> <br/>
Screenshot of **'View map'** page, displaying viewing options at right upper corner
### Page 4: Details
<img src="https://github.com/user-attachments/assets/4d4a47f7-cb1a-4503-b4ee-e98c07c4ac98" title="details" width="600"> <br/>
Screenshots of **‘Details’** page, displaying number of CHWs needed at prioritized wards and download options for Delta state.
### Page 5: Spatial optimization model
<img src="https://github.com/user-attachments/assets/2ffc365d-8194-4c88-bdaf-25416c6c90cb" title="optimization" width="600"> <br/>
Screenshot of **'Optimization'**, displaying location and allocation of CHWs needed at prioritized wards (Red dots – PHC facilities, Blue dots – Optimized locations of CHWs)


> [!IMPORTANT]
> :one: Datasets are not uploaded on GitHub. <br/>
> :two: README file is included in each folder to describe the contents. <br/>
> :three: Shiny App is not uploaded on posit, so there is no link to the dashboard and only sample screenshots are included.
