"""Load and clean the 21st-century Bordeaux review dataset.

Source: Dong, Guo, Rajana & Chen (2020), *Beverages* 6(1):5 — 14,349 Wine Spectator
reviews (vintages 2000–2016) turned into 985 binary descriptors by the
Computational Wine Wheel (CWW). Kaggle: jessiedong1/bordeaux-wine-reviews-from-2000-to-2016.

Everything here is deterministic and mirrored line-for-line in ``R/wine_data.R``.
"""
from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[2]
REF = ROOT / "data" / "reference"
RAW_DEFAULT = ROOT / "data" / "raw" / "BordeauxWines.csv"
META = ["Wine", "Year", "Score", "Price"]

# Tokens that on their own cannot be a producer name ("Château Margaux Margaux":
# the first "Margaux" is part of the producer, the second is the appellation).
GENERIC = {"château", "chateau", "clos", "domaine", "vieux", "le", "la", "les", "l'", "de", "du", "des",
           "d'", "cru", "grand", "petit", "enclos", "mas", "vignobles", "cuvée"}


@dataclass
class WineData:
    meta: pd.DataFrame          # one row per wine: name, producer, appellation, style, bank, year, score, price...
    X: np.ndarray               # (n_wines, n_descriptors) uint8 matrix
    descriptors: pd.DataFrame   # attribute, family, labels — aligned with X columns

    @property
    def y(self) -> np.ndarray:
        return (self.meta["score"].to_numpy() >= 90).astype(int)


def fix_encoding(s: str, table: pd.DataFrame | None = None) -> str:
    """Undo the UTF-8 → cp1252 double encoding in wine names ("ChÃ¢teau" → "Château")."""
    if table is None:
        table = load_encoding_table()
    for broken, fixed in table.itertuples(index=False):
        s = s.replace(broken, fixed)
    return re.sub(r"\s+", " ", s).strip()


def load_encoding_table() -> pd.DataFrame:
    return pd.read_csv(REF / "encoding_fixes.csv", dtype=str, keep_default_na=False)


def load_appellations() -> pd.DataFrame:
    return pd.read_csv(REF / "appellations.csv")


def _appellation_regex(app: pd.DataFrame) -> re.Pattern:
    pats = sorted(app["pattern"].unique(), key=len, reverse=True)
    alts = []
    for p in pats:
        esc = re.escape(p)
        if p == "Cadillac":            # the sweet AOC, not "Cadillac Côtes de Bordeaux"
            esc += r"(?! Côtes)"
        alts.append(esc)
    return re.compile(r"(?<![\w-])(" + "|".join(alts) + r")(?![\w-])")


def parse_name(name: str, app: pd.DataFrame, rx: re.Pattern | None = None) -> dict:
    """Split "Château X <Appellation> [White] [cuvée]" into its parts.

    Rule: take the first appellation match whose preceding text is a real producer
    name (not only generic words such as "Château" or "Clos des").
    """
    rx = rx or _appellation_regex(app)
    lookup = app.drop_duplicates("pattern").set_index("pattern")
    for m in rx.finditer(name):
        prefix = name[: m.start()].strip()
        tokens = [t.lower() for t in re.split(r"\s+", prefix) if t]
        if not tokens or all(t in GENERIC for t in tokens):
            continue
        pat = m.group(1)
        row = lookup.loc[pat]
        rest = name[m.end():].strip()
        style = row["style"]
        if rest.startswith("White"):
            style, rest = "white", rest[len("White"):].strip()
        if re.search(r"\b(Rosé|Clairet)\b", name):
            style = "rosé"
        return {"producer": prefix, "appellation": row["appellation"], "bank": row["bank"],
                "style": style, "cuvee": rest, "main_grapes": row["main_grapes"] if style != "white"
                else "Sauvignon Blanc, Sémillon, Muscadelle"}
    return {"producer": name, "appellation": None, "bank": None, "style": None, "cuvee": "", "main_grapes": None}


def parse_price(s: str) -> float:
    m = re.search(r"\d+(?:\.\d+)?", str(s))
    return float(m.group(0)) if m else np.nan


def load(raw_path: str | Path = RAW_DEFAULT) -> WineData:
    raw = pd.read_csv(raw_path, encoding="utf-8-sig")
    desc = pd.read_csv(REF / "descriptors.csv", keep_default_na=False)
    attr_cols = list(raw.columns[4:])
    if attr_cols != desc["attribute"].tolist():
        raise ValueError("descriptor columns do not match data/reference/descriptors.csv")

    enc = load_encoding_table()
    app = load_appellations()
    rx = _appellation_regex(app)
    names = raw["Wine"].map(lambda s: fix_encoding(s, enc))
    cache = {n: parse_name(n, app, rx) for n in names.unique()}
    parsed = pd.DataFrame([cache[n] for n in names])

    meta = pd.DataFrame({
        "wine_id": np.arange(len(raw)),
        "name": names,
        "year": raw["Year"].astype(int),
        "score": raw["Score"].astype(int),
        "price_usd": raw["Price"].map(parse_price),
    })
    meta = pd.concat([meta, parsed], axis=1)
    # the same wine (producer + appellation + cuvée) across vintages → one group for grouped CV
    meta["label"] = names
    meta["group"] = pd.factorize(names)[0]
    X = raw[attr_cols].to_numpy(dtype=np.uint8)
    return WineData(meta=meta, X=X, descriptors=desc)


def quality_report(d: WineData) -> dict:
    """Data-quality checks reported in the README (every number there comes from here)."""
    X, m = d.X, d.meta
    used = X.sum(axis=0)
    return {
        "n_wines": int(len(m)),
        "n_labels": int(m["label"].nunique()),
        "n_producers": int(m["producer"].nunique()),
        "vintages": [int(m["year"].min()), int(m["year"].max())],
        "share_90plus": round(float((m["score"] >= 90).mean()), 4),
        "n_descriptors": int(X.shape[1]),
        "descriptors_never_used": int((used == 0).sum()),
        "descriptors_used_lt5": int((used < 5).sum()),
        "wines_without_descriptors": int((X.sum(axis=1) == 0).sum()),
        "median_descriptors_per_wine": float(np.median(X.sum(axis=1))),
        "names_with_encoding_damage": None,  # filled by run_analysis (needs the raw names)
        "price_missing_share": round(float(m["price_usd"].isna().mean()), 4),
        "appellation_unparsed": int(m["appellation"].isna().sum()),
        "style_counts": {k: int(v) for k, v in m["style"].value_counts(dropna=False).items()},
        "duplicate_name_year": int(m.duplicated(["name", "year"]).sum()),
    }
