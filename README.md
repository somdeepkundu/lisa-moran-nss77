

---

## 📌 Spatial Analysis of Jhum and Non-Jhum Cultivation in India (NSS 77th Round)

This repository contains an R-based spatial analysis pipeline to calculate and visualize **Local Indicators of Spatial Association (LISA)** and **Moran's I** statistics for land-use variables derived from the **NSS 77th Round** – Situation Assessment Survey of Agricultural Households in Rural India (2018).

### 🔍 Objectives
- Analyze spatial clustering patterns of **jhum (shifting)** and **non-jhum crop cultivation**
- Generate **LISA cluster maps** to identify High-High, Low-Low, and outlier zones
- Compute **Moran’s I** statistics to assess global spatial autocorrelation

### 📁 Folder Structure
```
data/          # Shapefiles and source datasets
scripts/       # R scripts for processing and visualization
figures/       # Output maps (PNG)
results/       # CSVs or final spatial outputs (optional)
```

### 🛠️ Features
- Cleaned pipeline using `sf`, `spdep`, and `tmap`
- LISA cluster classification using z-score and significance thresholds
- Styled outputs with custom legend and layout positioning
- Moran's I values printed for both variables

### 📜 Citation
If using this workflow or adapting it in academic contexts, please cite:

```
Kundu, S. (2025). Spatial Analysis of Jhum and Non-Jhum Cultivation using NSSO 77th Round Data. TD630 Coursework, CTARA, IIT Bombay.
```
![image](https://github.com/user-attachments/assets/e19dfce8-04da-482a-a84d-07e30ff2d4dc)
![image](https://github.com/user-attachments/assets/356594f3-6ff1-4f67-80b3-38ddadef2049)

