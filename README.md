# ✈️ Risk Radar: Clustering Airports by Claim Patterns to Spotlight High-Risk Hubs

This project analyzes **Transportation Security Administration (TSA) passenger claim data (2003–2015)** to cluster airports based on claim characteristics and identify **high-risk hubs**. By applying multiple clustering algorithms (K-Means, K-Prototypes, DBSCAN, BIRCH), we uncover distinct airport risk profiles that highlight inefficiencies in claim handling and opportunities for improved risk management.

> **Highlight:** Using K-Means, we achieved a **silhouette score of 0.875** with four distinct clusters (High, Medium, Medium-Low, Low risk). Airports in Cluster 1 exhibited very high-value claims (avg. ≈ $28,695) with >95% denials, while Cluster 4 showed consistently low-value claims that were fully approved.

---

## 📁 Repository Structure

```
Risk-Radar-Clustering-Airports-by-Claim-Patterns-to-Spotlight-High-Risk-Hubs/
├─ data/                           # TSA claims dataset (2003–2015, cleaned)
├─ notebooks/
│   ├─ preprocessing.ipynb          # Data cleaning, feature engineering
│   ├─ exploration.ipynb            # Exploratory data analysis (EDA)
│   ├─ clustering_models.ipynb      # K-Means, K-Prototypes, DBSCAN, BIRCH
│   └─ visualization.ipynb          # Cluster profiling, plots
├─ results/
│   ├─ cluster_summary.csv          # Cluster assignments
│   └─ figures/                     # Elbow plots, silhouettes, heatmaps
└─ README.md
```

---

## 🔧 Environment & Dependencies

Use Python 3.9–3.11 with Jupyter. Install requirements with:

```bash
pip install pandas numpy scikit-learn matplotlib seaborn plotly kmodes scipy
```

---

## 📊 Data

- **Source:** U.S. Transportation Security Administration (TSA) passenger claim database (2002–2017).  
- **Study Window:** 2003–2015 (118,925 claims after cleaning).  
- **Features Used:**
  - Claim details: `Claim Amount`, `Close Amount`, `Disposition`
  - Contextual: `Claim Type`, `Claim Site`, `Item`
  - Metadata: `Airport Code`, `Airport Name`, `Incident/Received Date`

Preprocessing steps:
- Removed incomplete years (2002, 2016–2017) and duplicates
- Converted monetary columns to numeric, cleaned special characters
- Encoded categorical variables (one-hot/frequency encoding)
- Scaled numerical variables for distance-based clustering

---

## 🧠 Methodology

1. **Data Cleaning & Feature Engineering**
   - Removed nulls, duplicates, redundant fields
   - Created new temporal features (incident month/year, reporting delays)

2. **Exploratory Data Analysis (EDA)**
   - Frequency stats: Most common claim type = *Passenger Property Loss*  
   - Most affected claim site = *Checked Baggage* (≈82% of claims)  
   - Most frequent disposition = *Denied* (≈53% of claims)  

3. **Clustering Models**
   - **K-Means:** Optimal k=4 (Elbow + Silhouette), best performance (score=0.875)  
   - **K-Prototypes:** Mixed categorical + numerical, silhouette=0.452  
   - **DBSCAN:** Captured dense clusters, silhouette=0.843  
   - **BIRCH:** Efficient hierarchical, silhouette=0.865  

4. **Evaluation**
   - Silhouette coefficient (cohesion + separation)  
   - Inertia and within-cluster sum of squares  
   - Statistical tests (ANOVA, Kruskal-Wallis) confirmed significant differences in claim/close amounts across clusters

---

## ✅ Results

**K-Means (Best Model):** Four distinct clusters

| Cluster | Risk Level   | Avg Claim ($) | Avg Close ($) | Disposition Pattern            | Share of Claims |
|---------|--------------|---------------|---------------|--------------------------------|-----------------|
| 1       | High Risk    | 28,695        | 199           | 95% Denied, few Settled        | 0.29%           |
| 2       | Medium Risk  | 677           | ~0            | Denied, mostly baggage-related | 52.8%           |
| 3       | Medium-Low   | 495           | 244           | Mostly Settled/Resolved        | 20.5%           |
| 4       | Low Risk     | 140           | 139           | Fully Approved                 | 26.4%           |

📌 **Insights:**
- **Checked baggage** is the most problematic claim site across clusters  
- **Seasonality:** August/July (peak claims), February (lowest)  
- **High-value claims** are systematically denied, suggesting risk management bias  
- **Low-value claims** are approved quickly, showing efficiency for minor cases  

---

## 📈 Visualizations

- **Elbow plot & silhouette analysis** (K selection)  
- **Cluster profiles:** by claim site, type, disposition, temporal trends  
- **Correlation heatmap:** weak relation between claim and close amounts  

---

## 🗺️ Roadmap / Future Work

- Use **updated TSA/insurance datasets** beyond 2015  
- Aggregate clusters at the **airport level** (some appear in multiple clusters by month)  
- Apply **mixed-method clustering** combining temporal + categorical data  
- Explore **anomaly detection** for extreme claim spikes  
- Integrate **traffic volume & regional features** for deeper insights  

---

## 📚 References

- TSA Claims Data (2002–2017)  
- Jain et al. (1999, 2000), “Data Clustering”  
- Bishop (1995), *Neural Networks for Pattern Recognition*  
- Kelly & Wang (2020), *A Data Set for Modeling Claims Processes — TSA Claims Data*  
- Related work on airport claims, baggage mishandling, and risk management  

---

## ⚖️ License

No explicit license provided. If open-sourcing, consider MIT or Apache-2.0.

---

## 🙏 Acknowledgments

Developed by **Bismack Tokoli**, Department of Data Science, Florida Polytechnic University.  
Dataset courtesy of the **U.S. Transportation Security Administration (TSA)**.
