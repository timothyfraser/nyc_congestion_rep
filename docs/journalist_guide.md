# Data Journalist Guide: Using NYC Congestion Pricing Data

This guide helps data journalists understand and use the key data files from the NYC Congestion Pricing policy evaluation study.

## ⭐ Quick Summary: att.csv

**`att.csv` is the simplest file to start with** - it contains the key findings summarized in one place: Average Treatment Effects (ATT) for each geographic area plus an overall combined estimate.

### File Location

```
descriptives/att.csv
```

### What It Contains

A comprehensive table with **multiple rows** showing Average Treatment Effects (ATT) at different geographic levels:

**Main Geographic Areas:**
- **CRZ** (Congestion Relief Zone) - Model M9
- **NYC** (5 Boroughs) - Model M6  
- **CBSA** (Metro Area) - Model M3
- **Overall** (Combined across all areas) - Model Combined

**County-Specific Estimates:**
- One row per county (e.g., Bronx, Queens, Kings, New York, Richmond, Nassau, Suffolk, and other counties in the metro area) - Model Mixed
- Each county uses the most appropriate model: Bronx, Queens, Kings, Richmond use NYC model; Manhattan (New York) uses CRZ model; others use CBSA model

**Regional Aggregations:**
- **Long Island** (Kings, Queens, Nassau, Suffolk combined) - Model Mixed
- **Long Island (Nassau & Suffolk)** (just Nassau and Suffolk counties) - Model M3

### Understanding the Columns

| Column | Description | Units/Format |
|--------|-------------|--------------|
| `area` | Geographic area | `CRZ`, `NYC`, `CBSA`, `Overall`, county names (e.g., `Bronx`, `Queens`, `Kings`, `New York`, `Richmond`, `Nassau`, `Suffolk`), or regional aggregations (`Long Island`, `Long Island (Nassau & Suffolk)`) |
| `model` | Model identifier | `M9` (CRZ), `M6` (NYC), `M3` (CBSA), `Combined` (Overall), or `Mixed` (county-specific or regional aggregations using best model per area) |
| `att` | **Average Treatment Effect** - Change in PM2.5 | μg/m³ (negative = reduction) |
| `se_att` | Standard error of ATT | μg/m³ |
| `stars` | Statistical significance | `***`, `**`, `*`, `.`, or empty |
| `yhat1` | Average Predicted PM2.5 WITH policy  | μg/m³ |
| `yhatse1` | Standard error of yhat1 | μg/m³ |
| `yhat0` | Average Predicted PM2.5 WITHOUT policy - counterfactual | μg/m³ |
| `yhatse0` | Standard error of yhat0 | μg/m³ |

### Quick Interpretation

- **`att`**: The main finding - how much PM2.5 changed. Negative = reduction (good!)
- **`stars`**: Statistical significance (`***` = very strong evidence, `**` = strong, `*` = moderate)
- **`yhat0` vs `yhat1`**: Compare what happened (yhat1) vs what would have happened without policy (yhat0)
- **Percentage reduction**: Calculate as `(att / yhat0) × 100` or `((yhat1 - yhat0) / yhat0) × 100`

---

## 🎯 Detailed Data: qi_by_sensordate.csv

**`qi_by_sensordate.csv` is the main statistics** for *detailed*, sensor-by-sensor analysis. This file contains sensor-date level observations with both predicted treatment effects and observed values, giving you the most granular view of how air quality changed at each monitoring location on each day.

Note: Both the counterfactual (`yhat0`) and the treated (`yhat1`) are predictions from models. This means that there is prediction error. The `yhat1` numbers will not align perfectly with the observed values during this period. However, our models estimated average treatement effects of the policy, and so the difference between `yhat1` and `yhat0` will still capture the expected *change* in emissions, with reasonable error. 

> If you have questions about this or our modeling, **please do not hesitate to contact the research team.**
> It is very important that estimates of the congestion pricing policy are appropriately conservative and take into account modeling error. We are happy to clarify on how to interpret these effects and error margins.

### Why Use qi_by_sensordate.csv?

- **Granularity**: Data at the sensor-date level (not aggregated)
- **Complete Picture**: Includes both predicted effects AND observed values
- **Multiple Models**: Contains results from the best models for each geographic area (M3 for CBSA, M6 for NYC, M9 for CRZ)
- **Ready for Analysis**: Includes percentage changes, standard errors, and background pollution levels

### File Location

```
descriptives/qi_by_sensordate.csv
```

### Understanding the Columns

| Column | Description | Units/Format |
|--------|-------------|--------------|
| `id` | Unique identifier for sensor-date pair (for simulation purposes) | integer |
| `model` | Model identifier | `M3` (CBSA), `M6` (NYC), `M9` (CRZ) |
| `date` | Date of observation | YYYY-MM-DD |
| `aqs_id_full` | Unique sensor/monitor identifier | character |
| `treated` | Whether this date is in the treatment period | logical (TRUE/FALSE) |
| `area` | Geographic area | `cbsa`, `nyc`, or `crz` |
| `diff` | **Treatment effect** - Mean difference between treated and counterfactual predictions | μg/m³ (PM2.5) |
| `sediff` | Standard error of the difference | μg/m³ |
| `yhat1` | **Treated predicted value** - What the model predicts happened WITH the policy | μg/m³ |
| `yhat0` | **Counterfactual predicted value** - What would have happened WITHOUT the policy | μg/m³ |
| `se1` | Standard error of treated prediction | μg/m³ |
| `se0` | Standard error of counterfactual prediction | μg/m³ |
| `percentchg` | **Percentage change** - (diff / yhat0) × 100 | percentage |
| `observed` | **Observed PM2.5 concentration** - Actual measured value | μg/m³ |
| `bgmean` | **Background PM2.5** - Non-transportation related pollution estimate | μg/m³ |

### Key Interpretations

- **`diff`**: Negative values = pollution reduction (good!). More negative = bigger reduction.
- **`percentchg`**: Percentage reduction in pollution. A value of -.10 means 10% reduction.
- **`yhat0` vs `yhat1`**: Compare what happened (yhat1) vs what would have happened without the policy (yhat0).
- **`observed`**: The actual measured PM2.5 value - compare this to predictions to see model fit.

---

## effects.csv: Aggregated Summary Data

**Note**: [`effects.csv`](/descriptives/effects.csv) contains **aggregated** treatment effects (overall, per area, per sensor, per week, per month). For sensor-date level data with observed values, use [`qi_by_sensordate.csv`](#-primary-data-source-qi_by_sensordatecsv) instead.

## What is effects.csv?

`effects.csv` contains **Average Treatment Effects on the Treated (ATT)** - aggregated estimates of how much PM2.5 air pollution changed in New York City after the Congestion Relief Zone policy went into effect on January 6, 2025. This file summarizes effects at different levels (overall, by area, by sensor, by week, by month).

**Key finding**: Negative values mean **reductions** in pollution (good news!). The more negative, the greater the reduction.


## Understanding the Columns

### Core Columns (Always Present)

| Column | Description | Example Values |
|--------|-------------|----------------|
| `model` | Which statistical model was used | `cbsa3`, `nyc2`, `crz1` |
| `type` | How effects are aggregated | `overall`, `per_area`, `per_sensor`, `per_week`, `per_month` |
| `att` | **Average Treatment Effect** - Change in PM2.5 (μg/m³) | `-2.5`, `-1.8`, `-0.3` |
| `se_att` | **Standard Error** - Uncertainty around the estimate | `0.15`, `0.42`, `1.2` |
| `stars` | Statistical significance | `***` (p<0.001), `**` (p<0.01), `*` (p<0.05), `.` (p<0.1), `` (not significant) |
| `spec` | Model specification complexity | `1` (Basic), `2` (+Weather), `3` (+Demographics) |

### Conditional Columns (Present Based on `type`)

| Column | When Present | Description | Example Values |
|--------|--------------|-------------|----------------|
| `area` | `type == "per_area"` | Geographic area | `cbsa`, `nyc`, `crz` |
| `week` | `type == "per_week"` | Week number (1-22) | `1`, `5`, `10`, `20` |
| `month` | `type == "per_month"` | Month number (1-12) | `1` (Jan), `6` (Jun) |
| `aqs_id_full` | `type == "per_sensor"` | Unique sensor identifier | `840999999997`, `360610001` |

## Understanding Model Names

Models are named by **geographic scope** + **specification level**:

### Geographic Scope

- **`cbsa`** = New York City Metropolitan Area (Core-Based Statistical Area) - largest area
- **`nyc`** = New York City 5 Boroughs (Bronx, Brooklyn, Manhattan, Queens, Staten Island)
- **`crz`** = Congestion Relief Zone - the area where the policy applies (smallest, most relevant)

### Specification Levels

- **`1`** = Basic model (treatment + time controls + background pollution + distance to roads)
- **`2`** = Adds weather controls (temperature, humidity, wind, precipitation, cloud cover)
- **`3`** = Adds demographic controls (population density, income, race/ethnicity) - **most complete**

> **IMPORTANT**: Use `crz3` for the most reliable estimate within the Congestion Relief Zone, or `nyc3` for NYC-wide effects, or `cbsa3` for NYC metro area effects (eg. all the way to New Jersey).

## Understanding Effect Types

The `type` column tells you how the effects are aggregated:

| Type | Description | Use Case |
|------|-------------|----------|
| `overall` | Single average effect across entire period | "What was the overall impact?" |
| `per_area` | Effects broken down by geographic area | "Did effects differ by area?" |
| `per_sensor` | Effects for each monitoring station | "Which locations saw biggest changes?" |
| `per_week` | Effects for each week | "Did effects change over time?" |
| `per_month` | Effects for each month | "Monthly trend analysis" |

---

## Interpreting Results

### What does ATT mean?

**Average Treatment Effect on the Treated (ATT)** answers: "What was the average change in PM2.5 pollution in areas affected by the policy, compared to what would have happened without the policy?"

### Units

- **`att`** is measured in **micrograms per cubic meter (μg/m³)** of PM2.5
- **Negative values = reductions** (good!)
- **Positive values = increases** (rare, but possible)

### Statistical Significance

The `stars` column indicates how confident we are the effect is real (not due to chance):

| Stars | Meaning | Interpretation |
|-------|---------|---------------|
| `***` | p < 0.001 | Very strong evidence (99.9% confident) |
| `**` | p < 0.01 | Strong evidence (99% confident) |
| `*` | p < 0.05 | Moderate evidence (95% confident) |
| `.` | p < 0.1 | Weak evidence (90% confident) |
| (none) | p ≥ 0.1 | Not statistically significant |

### Converting to Air Quality Index (AQI)

To convert PM2.5 changes to AQI, you need baseline concentrations. The study uses the `pm25_aqi()` function (see `descriptives/00_functions.R`), but for rough estimates:

- **1 μg/m³ reduction** ≈ **1-2 point AQI improvement** (depends on baseline level)
- Reductions are most meaningful when baseline AQI is moderate to high

### Example Interpretation

```
model: crz3
type: overall
att: -3.05
se_att: 0.15
stars: ***
```

> **Interpretation**: 
> - In the Congestion Relief Zone, PM2.5 decreased by an average of **3.05 μg/m³** after the policy
> - This reduction is **statistically significant** (p < 0.001)
> - We're 95% confident the true effect is between -XX and -YY μg/m³
> - This is a **meaningful reduction**.

---

## Common Pitfalls to Avoid

1. **Don't confuse negative values**: Negative `att` values are **good** - they mean pollution decreased
2. **Check significance**: Effects without stars (`***`, `**`, `*`) may not be reliable
3. **Use appropriate model**: `crz3` is best for CRZ effects, `nyc3` for NYC-wide, `csba3` for entire NYC metro area
4. **Consider confidence intervals**: Always report uncertainty, not just point estimates
5. **Don't over-interpret small effects**: A -0.1 μg/m³ change may be statistically significant but not practically meaningful

---

## Getting Help

- See [`descriptives/README.md`](../descriptives/README.md) for script documentation
- Check the main project [README](../README.md) for project overview
- Review `descriptives/04_effects.R` to see how researchers use this data

---

## Data Period

- **Study period**: January 2024 - June 2025
- **Treatment period**: January 6, 2025 - June, 2025
- **Pre-treatment period**: January 2024 - January 5, 2025

## Units Reference

- **PM2.5**: Measured in micrograms per cubic meter (μg/m³)
- **AQI**: Air Quality Index (0-500 scale)
- **Time**: All dates/times in UTC unless otherwise noted

---


## Citation

If using this data in a publication, please cite:

> Fraser, T., Park, Y.G., Lu, D. et al. A first look into congestion pricing in the United States: PM2.5 impacts after six months of New York City cordon pricing. *npj Clean Air* 1, 39 (2025). https://doi.org/10.1038/s44407-025-00037-2
