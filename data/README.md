# Data

`data/raw/` is not in the repo. Download **BordeauxWines.csv** (29 MB) from Kaggle —
[jessiedong1/bordeaux-wine-reviews-from-2000-to-2016](https://www.kaggle.com/datasets/jessiedong1/bordeaux-wine-reviews-from-2000-to-2016) — and put it in `data/raw/`:

```bash
kaggle datasets download -d jessiedong1/bordeaux-wine-reviews-from-2000-to-2016 -p data/raw --unzip
```

The dataset accompanies Dong, Guo, Rajana & Chen (2020), *Beverages* 6(1):5 ([doi:10.3390/beverages6010005](https://doi.org/10.3390/beverages6010005)):
14,349 Wine Spectator reviews (vintages 2000–2016) turned into 985 binary descriptors by the Computational Wine Wheel.
Kaggle lists the licence as **Unknown**, so the raw file is not redistributed here.

`data/reference/` is hand-built and versioned — every row cites its source:

| File | What | Source |
|---|---|---|
| `descriptors.csv` | 985 descriptors → 26 CWW families, EN/TR labels | column order of the raw file; TR labels hand-written |
| `appellations.csv` | appellation → bank, style, typical grapes | Bordeaux appellation rules (bordeaux.com / Wikipedia) |
| `encoding_fixes.csv` | 20 mojibake repairs (`ChÃ¢teau` → `Château`) | derived from the raw names |
| `cheeses.csv`, `pairing_rules.csv` | 19 cheeses, 6 pairing rules | CIVB (bordeaux.com), Wine Folly, Aydın Gastronomy 4(1) 2020 |
| `turkish_wines.csv` | 23 Turkish wines: grapes, body, oak, region | degustasyon.net tasting notes, kalpak.net |
| `provinces.csv` | 81 province centroids | github.com/caglarsarikaya/turkey-geolocations |
