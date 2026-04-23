 # 🍽️ Yelp Data Analytics Dashboard

An interactive data analytics dashboard built on Yelp’s large-scale dataset to explore restaurant trends, customer behavior, and regional insights. This project combines data engineering, feature engineering, and visualization to transform raw data into actionable insights.

---

## 📌 Overview

This project processes and analyzes ~10GB of Yelp data, including business details, user reviews, and check-in activity. The goal is to provide an intuitive dashboard that allows users to explore:

- Restaurant ratings and distributions  
- Cuisine trends across regions  
- Pricing patterns ($–$$$$)  
- Customer engagement and traffic (via check-ins)  
- “Vibe” analysis (e.g., trendy, casual, upscale)  

---

## 🛠️ Tech Stack

- **Languages:** Python, R  
- **Libraries:** pandas, NumPy, matplotlib  
- **Visualization:** Plotly / Shiny (interactive dashboard)  
- **Data Processing:** Regex, time-series parsing  
- **Tools:** Jupyter Notebook, Git  

---

## 🧱 Data Pipeline

1. **Data Ingestion**
   - Imported Yelp dataset (~10GB)  
   - Converted JSON files into structured CSV format  

2. **Data Cleaning & Transformation**
   - Filtered dataset to focus on restaurant-related entries  
   - Handled missing and inconsistent values  
   - Normalized nested attributes  

3. **Feature Engineering**
   - Categorized cuisines using regex-based mapping  
   - Derived price ranges and business attributes  
   - Parsed check-in timestamps to estimate traffic patterns  
   - Created “vibe” classifications from business metadata  

4. **Data Optimization**
   - Reduced dataset size by dropping unused columns  
   - Implemented filtering and caching to improve dashboard performance  

---

## 📊 Key Features

- **Interactive Map Visualization**
  - Displays restaurant locations with dynamic filters  
  - Color-coded by rating, cuisine, price, or vibe  

- **Dynamic Filtering System**
  - Filter by cuisine, price range, noise level, and more  
  - Real-time updates across all visualizations  

- **Traffic & Engagement Analysis**
  - Uses check-in data to estimate busy vs. quiet periods  

- **Comparative Insights**
  - Analyze how attributes (e.g., outdoor seating, alcohol, noise) impact ratings  

---

## 🚀 How to Run

1. Clone the repository:
   ```bash
   git clone https://github.com/your-username/yelp_dashboard.git
   cd yelp-dashboard
