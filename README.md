# Spatial Functional Kriging

This repository presents a spatial functional data analysis using kriging-based methods.
The project focuses on predicting meteorological variables in regions with limited observation coverage, with applications to temperature and PM10 concentration estimation.

The analysis implements:
- Ordinary Kriging for Functional Data (OKFD)
- Functional cokriging using wind speed as a secondary variable

---

## Overview

In regions where meteorological observation stations are sparse or data availability is limited, direct access to environmental information can be challenging.
This project explores how spatial functional data analysis can be used to address this issue from a methodological perspective.

Two main tasks are considered:
1. **Temperature prediction** in North Korea using Ordinary Kriging for Functional Data (OKFD)
2. **PM10 concentration prediction** using functional cokriging, where wind speed curves are treated as functional secondary variables

The emphasis of this repository is on methodology rather than operational forecasting.


## Data

All datasets used in this project are publicly available and were obtained from the Korea Meteorological Administration (KMA).

- **Temperature data**: 27 observation stations in North Korea (3-hour intervals)
- **Wind speed and PM10 data**: 23 observation stations in South Korea (hourly)
- **Location data**: Latitude, longitude, and elevation of observation sites

---

## Disclaimer

This repository contains work originally conducted as part of a graduate-level course project in spatial and functional data analysis.

