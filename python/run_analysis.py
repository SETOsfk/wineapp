"""Re-analysis of the 21st-century Bordeaux dataset — every number in the README comes from here.

    python python/run_analysis.py            # ~6 min on a laptop; writes results/
"""
from __future__ import annotations

import json
import time
from pathlib import Path

import numpy as np
import pandas as pd
from scipy.stats import spearmanr
from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import LogisticRegression, Ridge
from sklearn.metrics import accuracy_score, mean_absolute_error, roc_auc_score
from sklearn.model_selection import StratifiedKFold

from wine.data import ROOT, load, quality_report
from wine.models import SEED, evaluate, feature_sets, model_zoo, splits, summarize

OUT = ROOT / "results"
OUT.mkdir(exist_ok=True)


def log(*a):
    print(time.strftime("%H:%M:%S"), *a, flush=True)


def main() -> None:
    d = load()
    X, y, meta, desc = d.X, d.y, d.meta, d.descriptors
    raw_names = pd.read_csv(ROOT / "data" / "raw" / "BordeauxWines.csv", encoding="utf-8-sig", usecols=["Wine"])["Wine"]
    q = quality_report(d)
    q["names_with_encoding_damage"] = int(raw_names.str.contains("Ã|Â", regex=True).sum())
    fs = feature_sets(desc, X)
    q["descriptors_used"] = int(len(fs["all"]))
    q["descriptors_sensory"] = int(len(fs["sensory"]))
    q["descriptors_descriptive"] = int(len(fs["descriptive"]))
    log("quality", q)

    zoo = model_zoo()
    rows, fold_ids = [], {}
    plan = []
    for design in ("random", "grouped", "temporal"):
        for fset in ("all", "sensory", "descriptive"):
            for m in ("majority", "naive_bayes", "logistic", "linear_svm"):
                plan.append((design, fset, m))
    plan += [("random", "all", "random_forest"), ("random", "all", "gradient_boosting"),
             ("random", "sensory", "random_forest"), ("random", "sensory", "gradient_boosting"),
             ("grouped", "all", "gradient_boosting")]
    for design, fset, m in plan:
        t = time.time()
        res = evaluate(X, y, meta, fs[fset], zoo[m], design, fold_ids=fold_ids)
        s = summarize(res)
        row = {"design": design, "features": fset, "model": m, "n_folds": len(res)}
        for k, v in s.items():
            row[k] = round(v["mean"], 4)
            row[k + "_sd"] = round(v["sd"], 4)
        rows.append(row)
        log(design, fset, m, row["accuracy"], row["roc_auc"], f"{time.time() - t:.0f}s")
    comp = pd.DataFrame(rows)
    comp.to_csv(OUT / "model_comparison.csv", index=False)

    folds = pd.DataFrame({"wine_id": meta["wine_id"], "random_fold": fold_ids["random"],
                          "grouped_fold": fold_ids["grouped"],
                          "temporal_test": (meta["year"] > 2011).astype(int)})
    folds.to_csv(OUT / "folds.csv", index=False)

    # ---- the 2024 pipeline, reproduced: top-45 RF importances chosen on ALL rows, then CV ----
    cols = fs["all"]
    rf_sel = RandomForestClassifier(n_estimators=100, random_state=SEED, n_jobs=-1).fit(X[:, cols], y)
    top45 = cols[np.argsort(rf_sel.feature_importances_)[::-1][:45]]
    cv = StratifiedKFold(5, shuffle=True, random_state=SEED)
    leak, nested = [], []
    for tr, te in cv.split(X, y):
        m1 = RandomForestClassifier(n_estimators=300, random_state=SEED, n_jobs=-1).fit(X[np.ix_(tr, top45)], y[tr])
        p1 = m1.predict_proba(X[np.ix_(te, top45)])[:, 1]
        leak.append((accuracy_score(y[te], p1 >= .5), roc_auc_score(y[te], p1)))
        sel = RandomForestClassifier(n_estimators=100, random_state=SEED, n_jobs=-1).fit(X[np.ix_(tr, cols)], y[tr])
        tk = cols[np.argsort(sel.feature_importances_)[::-1][:45]]
        m2 = RandomForestClassifier(n_estimators=300, random_state=SEED, n_jobs=-1).fit(X[np.ix_(tr, tk)], y[tr])
        p2 = m2.predict_proba(X[np.ix_(te, tk)])[:, 1]
        nested.append((accuracy_score(y[te], p2 >= .5), roc_auc_score(y[te], p2)))
    leak, nested = np.array(leak), np.array(nested)
    legacy = {"selection_outside_cv": {"accuracy": round(leak[:, 0].mean(), 4), "roc_auc": round(leak[:, 1].mean(), 4)},
              "selection_inside_cv": {"accuracy": round(nested[:, 0].mean(), 4), "roc_auc": round(nested[:, 1].mean(), 4)},
              "top45_selected_on_all_rows": [desc.loc[i, "attribute"] for i in top45]}
    log("legacy", legacy["selection_outside_cv"], legacy["selection_inside_cv"])

    # ---- coefficients (full fit) + fold stability ----
    coef_rows = []
    for fset in ("all", "sensory"):
        c = fs[fset]
        full = LogisticRegression(C=0.5, max_iter=3000).fit(X[:, c], y)
        per_fold = []
        for _, tr, _te in splits(meta, y, "grouped"):
            per_fold.append(LogisticRegression(C=0.5, max_iter=3000).fit(X[np.ix_(tr, c)], y[tr]).coef_[0])
        per_fold = np.array(per_fold)
        for j, col in enumerate(c):
            coef_rows.append({"features": fset, "attribute": desc.loc[col, "attribute"], "family": desc.loc[col, "family"],
                              "n_reviews": int(X[:, col].sum()), "coef": float(full.coef_[0][j]),
                              "coef_fold_sd": float(per_fold[:, j].std(ddof=1)),
                              "sign_stable": bool(np.all(np.sign(per_fold[:, j]) == np.sign(full.coef_[0][j]))),
                              "intercept": float(full.intercept_[0])})
    coefs = pd.DataFrame(coef_rows)
    coefs.to_csv(OUT / "coefficients.csv", index=False)

    # ---- out-of-fold probabilities (grouped CV) for the app + calibration ----
    oof = {}
    for fset in ("all", "sensory"):
        p = np.zeros(len(y))
        for _, tr, te in splits(meta, y, "grouped"):
            mdl = LogisticRegression(C=0.5, max_iter=3000).fit(X[np.ix_(tr, fs[fset])], y[tr])
            p[te] = mdl.predict_proba(X[np.ix_(te, fs[fset])])[:, 1]
        oof[fset] = p
    pd.DataFrame({"wine_id": meta["wine_id"], "p90_all": oof["all"].round(4),
                  "p90_sensory": oof["sensory"].round(4)}).to_csv(OUT / "oof_predictions.csv", index=False)
    bins = np.linspace(0, 1, 11)
    calib = []
    for fset in ("all", "sensory"):
        idx = np.clip(np.digitize(oof[fset], bins) - 1, 0, 9)
        for b in range(10):
            s = idx == b
            if s.sum():
                calib.append({"features": fset, "bin": b, "mean_pred": round(float(oof[fset][s].mean()), 4),
                              "observed": round(float(y[s].mean()), 4), "n": int(s.sum())})

    # ---- where does the model fail? by style ----
    by_style = []
    for st, g in meta.groupby("style"):
        ii = g.index.to_numpy()
        pred = oof["all"][ii] >= 0.5
        by_style.append({"style": st, "n": int(len(ii)), "share_90plus": round(float(y[ii].mean()), 4),
                         "accuracy_all": round(float((pred == y[ii]).mean()), 4),
                         "accuracy_sensory": round(float(((oof["sensory"][ii] >= .5) == y[ii]).mean()), 4),
                         "majority_baseline": round(float(max(y[ii].mean(), 1 - y[ii].mean())), 4)})

    # ---- score regression (grouped CV) ----
    reg = {}
    score = meta["score"].to_numpy().astype(float)
    for fset in ("all", "sensory"):
        pred = np.zeros(len(y))
        for _, tr, te in splits(meta, y, "grouped"):
            pred[te] = Ridge(alpha=30.0).fit(X[np.ix_(tr, fs[fset])], score[tr]).predict(X[np.ix_(te, fs[fset])])
        reg[fset] = {"mae": round(mean_absolute_error(score, pred), 3),
                     "spearman": round(float(spearmanr(score, pred).statistic), 4)}
    base = np.zeros(len(y))
    for _, tr, te in splits(meta, y, "grouped"):
        base[te] = score[tr].mean()
    reg["mean_baseline"] = {"mae": round(mean_absolute_error(score, base), 3)}

    # ---- price ----
    pr = meta.dropna(subset=["price_usd"])
    price = {"n_priced": int(len(pr)),
             "spearman_price_score": round(float(spearmanr(pr["price_usd"], pr["score"]).statistic), 4),
             "median_price_by_band": {band: float(pr[(pr["score"] >= lo) & (pr["score"] <= hi)]["price_usd"].median())
                                      for band, (lo, hi) in {"<=85": (0, 85), "86-89": (86, 89), "90-93": (90, 93),
                                                             "94+": (94, 100)}.items()}}

    metrics = {"quality": q, "legacy_2024_pipeline": legacy, "calibration": calib, "by_style": by_style,
               "regression": reg, "price": price, "literature": {
                   "dong_2020_svm_accuracy_all_wines": 0.8697,
                   "source": "Dong et al. (2020) Beverages 6(1):5, doi:10.3390/beverages6010005"}}
    (OUT / "metrics.json").write_text(json.dumps(metrics, indent=1, ensure_ascii=False))
    log("done")


if __name__ == "__main__":
    main()
