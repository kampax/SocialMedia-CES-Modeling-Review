# Database metadata

This file documents the contents, structure, and coding criteria of the tables used in the systematic review, *Spatial modeling of Cultural Ecosystem Services from social media data*. The tables are linked by `ID` (article identifier) and follow the PRISMA screening workflow:

```text
csv files/savedrecs.csv (627 WoS+Scopus records, PRISMA screening)
        |  Relevant = yes -> Spatial = yes -> Includes models = yes -> Final extraction = yes
        v
Data_extraction.csv (68 articles; one row per predictor variable)
        |
        +-- Validacion_CES and variables groups.csv (68 articles; one row per validated CES)
        +-- EvaluationMetrics.csv (68 articles; one row per reported evaluation metric)
        +-- Complementary_Data.csv (68 articles; one row per article, complementary/ethical variables)
```

`ID` is consistent across all files.

## 1. `csv files/savedrecs.csv`

**Purpose:** Combined Web of Science and Scopus bibliographic database, exported and unified by `1) Make scopus database compatible with WoS.R`, with PRISMA screening columns added for the article selection process. It contains the initial 627 records from which the 68 fully extracted articles were derived.

- **Rows:** 627 bibliographic records, one per candidate article.

### Standard bibliographic columns

These include `Fuente` (record source: `wos` or `scopus`), `scopus`/`wos` (database-presence flags used to identify duplicates), `Publication Type`, `Authors`, `Article Title`, `Source Title`, `Language`, `Document Type`, `Author Keywords`, `Keywords Plus`, `Abstract`, `Journal Abbreviation`, `Journal ISO Abbreviation`, `Publication Year`, `Volume`, `Issue`, `Supplement`, `DOI`, `DOI Link`, `WoS Categories`, `Web of Science Index`, `Research Areas`, and `IDS Number`.

### PRISMA screening columns

| Column | Description | Values |
|---|---|---|
| `Cargo` | Reviewer role assigned to the record. | Reviewer labels |
| `Relevant` | First filter: does the article address CES and social-media data in general terms? | `yes` / `no` |
| `Spatial` | Second filter: does the study apply a spatial approach? | `yes` / `no` / `not evaluated` |
| `Includes models` | Third filter: does the study apply spatial CES modeling rather than only spatial description? | `yes` / `no` / `not evaluated` |
| `Final extraction` | Final filter: was the article confirmed for full data extraction? | `yes` / `no` |
| `Exclusion reason` | Free-text reason for exclusion at the screening stage. | Free text |

## 2. `Data_extraction.csv`

**Purpose:** Main data-extraction table, with one row for each original or derived predictor variable used in the models reported by each article.

- **Rows:** 2,241 variables from 68 unique articles (`ID`), with approximately 33 extracted variables per article on average.

### Article-level columns

These columns are repeated for every row belonging to the same `ID`:

| Column | Description |
|---|---|
| `ID`, `Authors`, `Article Title`, `DOI`, `DOI Link` | Article identification. |
| `Supply/demand CES` | Whether the study models CES supply, demand, both, or does not specify it. |
| `CES services type original` | CES terminology used in the original article. |
| `Initial variables` / `Final Variables` | Candidate variables initially considered and variables retained in the final model. |
| `Most contributing variable` | Variable identified by the authors as having the greatest importance or contribution. |
| `Models used` / `Best Model` | Statistical or machine-learning models used and the best-performing model reported. |
| `Presence/Abundance model` | Response type: `Presence`, `Abundance`, or both. |
| `Original pixel size` / `Pixel size resampled` | Original and, where applicable, resampled spatial resolution. |
| `Temporality` | Years covered by the social-media data. |
| `Social media source data` | Source platform or platforms, such as `Flickr`, `Instagram`, `Sina Weibo`, or `Panoramio`. |
| `Type of social media data` | Content type, including geolocated photos, text, geolocation, sign-in data, and hashtags. |
| `Spatial cover` | Study scale: `Local`, `Regional`, `National`, or `Continental`. |
| `Protect area` | Whether the study area includes a protected area. |
| `Country`, `Area name` | Geographic location of the study area. |
| `Ecosystem type 1/2/3` | Up to three ecosystem types present in the study area. |
| `Evaluate changes over time` | Whether temporal changes in CES are evaluated. |
| `Metrics` | Reported model-evaluation metric or metrics. |
| `Order` | Auxiliary ordering field. |

### Variable-level columns

| Column | Description |
|---|---|
| `Variables Originales` | Variable name as reported in the original article. |
| `Variables Standardized` | Standardized variable name used to group synonyms across studies. |
| `Type` | Measure or transformation type, such as `Abundance`, `Area`, `Count`, `Density`, `Distance`, `Diversity`, `Index`, `Mean`, `Presence`, `Richness`, or `Percentage`. |
| `Level1` | Broad category: `Anthropic elements`, `Natural elements`, `Land management`, or `Remote sensing data`. |
| `Level2` | Intermediate category, including accessibility, cultural heritage, environmental quality, infrastructure, land use and land cover, management, natural environment, protected areas, recreational opportunities, remote sensing data, and statistical data. |
| `Level3` | More specific descriptive category. |
| `CES` | Standardized CES associated with the variable. |
| `IPBES` | IPBES category associated with the CES. |

The `Level1 -> Level2 -> Level3` hierarchy feeds the Sankey diagrams and predictor-variable counts in the analysis script.

## 3. `Validacion_CES and variables groups.csv`

**Purpose:** Validates and standardizes the classification of Cultural Ecosystem Services reported by each article. It records whether each CES definition is clear and whether the CES is modeled separately from other services.

- **Rows:** 240 individual CES records from 68 unique articles (`ID`), with between 1 and 30 CES records per article.

### Column dictionary

| Column | Description | Values / format |
|---|---|---|
| `ID` | Article identifier shared with `Data_extraction.csv`. | Integer |
| `Title`, `DOI`, `DOI link` | Article reference. | Text |
| `original CES` | CES name used by the original authors. | Free text |
| `CES ` | Standardized CES name used for cross-study comparison. | 17 categories |
| `IPBES categories` | Higher-level IPBES category for the standardized CES. | 5 categories or blank for `Not cultural` |
| `Codigo IPBES` | IPBES code associated with the category. | `15`, `16`, `17`, `18`, or combinations |
| `Definition` | CES definition extracted from the article. | Free text |
| `well defined` | Whether the CES is clearly and operationally defined. | `yes` / `no` / `does not provide definition` |
| `disaggregated` | Whether the CES is reported or modeled separately. | `yes` / `no` |

The standardized CES categories are `Artistic inspiration`, `Cultural heritage`, `Enjoy the fauna and flora`, `Enjoy the landscape`, `Environmental education`, `Intrinsic`, `Maintenance of options`, `Non-material NCP`, `Not cultural`, `Psychological experiences`, `Recreation`, `Recreation (e.g. hunting, fishing)`, `Research`, `Rural tourism`, `Sense of belonging`, `Social relations`, and `Spiritual significance`.

`Not cultural` identifies ecosystem services that are not CES and are excluded from CES counts. `Non-material NCP` is a generic or cross-cutting category and is renamed `General CES` in the figures.

### IPBES categories

| Code | Category |
|---|---|
| 15 | Learning and inspiration |
| 16 | Physical and psychological experiences |
| 17 | Supporting identities |
| 18 | Maintenance of options |
| 15, 16, 17, 18 | Non-material NCP (cross-cutting; `General CES` in figures) |

The graph order is `Physical and psychological experiences`, `Supporting identities`, `Learning and inspiration`, `Maintenance of options`, and `General CES`.

### Validation criteria

- **`well defined = yes`**: the article explicitly explains what the CES includes or excludes and how it is identified in the data.
- **`well defined = no`**: the CES is mentioned or used without a clear operational definition.
- **`well defined = does not provide definition`**: no definition is attempted.
- **`disaggregated = yes`**: the CES is analyzed or modeled separately.
- **`disaggregated = no`**: it is aggregated with other CES or services.

## 4. `EvaluationMetrics.csv`

**Purpose:** Records one row per reported model-evaluation metric, optionally disaggregated by CES.

- **Rows:** 336 metrics from 68 unique articles (`ID`).

| Column | Description |
|---|---|
| `ID` | Article identifier. |
| `Authors` | Abbreviated article authors. |
| `Article Title` | Article title. |
| `CES services type original` | Original CES terminology associated with the metric. |
| `Models.used` | Statistical or machine-learning model associated with the metric. |
| `Metrics` | Reported evaluation metric, such as `AUC-ROC`, `R²`, `RMSE`, `AIC`, `Kappa`, or `Moran's I`. |
| `Metrics values` / `Metrics values2` | Numeric value or second comparable value when a range or multiple results are reported. |

## 5. `Complementary_Data.csv`

**Purpose:** One row per article with complementary information about conceptual values, methodological practice, and ethical considerations.

- **Rows:** 68, one per article and unique `ID`.

| Column | Description |
|---|---|
| `ID`, `Title`, `DOI`, `Authors` | Article identification. |
| `Data extraction` | Methods used to extract or process social-media data, including geolocated photos, check-ins, filtering, text analysis, manual or AI labeling, sentiment analysis, and text mining. |
| `Metrics variable` | Statistical methods used to select or validate predictor variables, including correlations, VIF, AIC, expert knowledge, OLS, and related combinations. |
| `Relational` | Whether relational CES values are considered. |
| `Intrinsec` | Whether intrinsic values are considered. |
| `Instrumental` | Whether instrumental values are considered. |
| `Ethical considerations` | Whether the article explicitly addresses ethics, privacy, or consent in the use of social-media data. |
| `Transparency` | Whether the methodology is documented transparently. |
| `Accessibility` | Whether the data and/or code are publicly accessible. |
| `Replicability` | Whether enough methodological detail is provided for replication. |
| `Bundles` | Whether CES bundles are analyzed jointly rather than separately. |

`Relational`, `Intrinsec`, and `Instrumental` describe the IPBES typology of nature's values. `Ethical considerations` through `Bundles` provide a cross-study assessment of methodological good practices.
