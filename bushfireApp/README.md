# bushfireApp

**bushfireApp** is an R package designed to explore **global temperature anomalies** (°C) relative to the 1861–1890 baseline.

It includes:

- 🧭 A **cleaned dataset** `temp_data`  
- 📊 An **interactive Shiny app** for visualizing warming trends  
- 📚 **Documentation and vignette** to support interpretation  

> **Data source:** [Our World in Data](https://ourworldindata.org/grapher/temperature-anomaly) – Annual temperature anomalies relative to the pre-industrial period (Met Office Hadley Centre, *HadCRUT5*).

---

## 🧩 Installation

You can install the development version of **bushfireApp** from GitHub with:


```r
# install.packages("remotes")   # if not yet installed
remotes::install_github("ETC5523-2025/assignment-4-packages-and-shiny-apps-yluu0123")
```

## 🚀 Quick Start
After installation, load the package and launch the interactive app:

```r
library(bushfireApp)

# Launch the Shiny app

run_my_app()
```

This will open a browser window where you can:

- Select a region (e.g., World, Australia, Northern Hemisphere)

- Adjust the year range interactively

- View temperature anomaly trends and inspect underlying data
