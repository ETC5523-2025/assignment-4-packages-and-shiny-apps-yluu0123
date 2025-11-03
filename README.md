# ETC5523 Assignment 4 — bushfireApp

**Author:** Yao Lu  
**Unit:** ETC5523 Communicating with Data (Monash University)  
**Semester:** 2025  

---

## 🌐 Live pkgdown site

👉 [Click here to view the bushfireApp documentation website](https://etc5523-2025.github.io/assignment-4-packages-and-shiny-apps-yluu0123/)

---

## 📦 About the package

**bushfireApp** is an R package designed to explore **global temperature anomalies (°C)**  
relative to the 1861–1890 baseline.

It includes:
- 🌡️ An interactive **Shiny app** for visualising warming trends  
- 📊 A cleaned dataset `temp_data`  
- 📖 Documentation and vignette to support interpretation  

Data source:  
[Our World in Data](https://ourworldindata.org/co2-and-greenhouse-gas-emissions) —  
Annual temperature anomalies relative to the pre-industrial period (Met Office Hadley Centre, *HadCRUT5*).

---

## ⚙️ Installation

You can install the development version from GitHub:

```r
# install.packages("pak")
pak::pak("ETC5523-2025/assignment-4-packages-and-shiny-apps-yluu0123")
