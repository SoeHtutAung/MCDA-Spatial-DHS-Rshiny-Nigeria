# MCDA-Spatial-DHS-Rshiny-Nigeria
Multi-criteria decision analysis tool **(MCDA)** to support strengthening community health workforce using a blended approach, including data extractions, spatial optimization, and R Shiny dashboard. This project used both ***public and private*** datasets.

# 1. Creating ward-level datasets
## 1.1 Data extraction
Refer to *Chapter 3.4 Data sources and extraction* of `docs/MSc project report_20250311.pdf` for data sources and extraction methods. Raw data files are kept in `data/` folder, but they are not uploaded in this public repository.
## 1.2 Preparing dataset
Refer to *Chapter 3.4 Data manipulation* of `docs/MSc project report_20250311.pdf` for data management steps. Scripts for ETL processed are kept in `scripts/` folder. From spatial and other datasets, ward-level final datasets are created and kept as shape file in `data/shp/NGA_wards_dashboard`. Other outputs from data processing are saved in `outputs/` folder.

# 2. Creating interactive dashboard (RShiny App)
Using `data/process.R` necessary packages and datasets are loaded. Scripts for dashboard are kept in `shiny_dashboard/` folder. 

> [!IMPORTANT]
> :one: Datasets are not uploaded on GitHub. <br/>
> :two: README file is included in each folder to describe the contents. <br/>
> :three: Shiny App is not uploaded on posit, so there is no link to the dashboard and only sample screenshots are included.
