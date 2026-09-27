"""Export everything the static web app needs into web/data/wine.json (+ README figures).

    python python/build_web.py      # after run_analysis.py
"""
from __future__ import annotations

import json
import math

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn.linear_model import LogisticRegression

from wine.data import ROOT, load
from wine.models import feature_sets
from wine.pairing import equivalents, load_reference, native_alternative, pairing_rule, profile_from_wine, cheeses_for

RES = ROOT / "results"
WEB = ROOT / "web" / "data"
FIG = ROOT / "docs"
WEB.mkdir(parents=True, exist_ok=True)
FIG.mkdir(exist_ok=True)


def main() -> None:
    d = load()
    X, y, meta, desc = d.X, d.y, d.meta, d.descriptors
    fs = feature_sets(desc, X)
    ref = load_reference()
    comp = pd.read_csv(RES / "model_comparison.csv")
    coefs = pd.read_csv(RES / "coefficients.csv")
    oof = pd.read_csv(RES / "oof_predictions.csv")
    metrics = json.loads((RES / "metrics.json").read_text())

    # the app's "build a profile" model: sensory-only logistic regression on all rows
    sens = fs["sensory"]
    lr = LogisticRegression(C=0.5, max_iter=3000).fit(X[:, sens], y)
    coef = {int(c): float(w) for c, w in zip(sens, lr.coef_[0])}
    used = fs["all"]
    n_docs = len(y)
    df_ = X.sum(axis=0)
    dlist = []
    for i in used:
        r = desc.loc[i]
        dlist.append({"a": r["attribute"], "f": r["family"], "en": r["label_en"], "tr": r["label_tr"] or r["label_en"],
                      "n": int(df_[i]), "idf": round(math.log(n_docs / df_[i]), 3), "s": int(r["family"] != "descriptive"),
                      "c": round(coef[i], 3) if i in coef else None})
    pos = {int(c): k for k, c in enumerate(used)}

    names = meta["label"].unique().tolist()
    name_idx = {n: k for k, n in enumerate(names)}
    apps = sorted(meta["appellation"].unique().tolist())
    styles = ["red", "white", "sweet", "rosé"]
    tr_ids = ref["turkish"]["id"].tolist()
    rules = ref["rules"]["rule_id"].tolist()

    w = {k: [] for k in ("l", "y", "s", "p", "ap", "st", "pr", "at", "ru", "eq", "na")}
    attr_names = desc["attribute"].to_numpy()
    for i, row in meta.iterrows():
        at = np.nonzero(X[i])[0]
        prof = profile_from_wine(row["style"], row["bank"], row["main_grapes"], set(attr_names[at]))
        w["l"].append(name_idx[row["label"]])
        w["y"].append(int(row["year"]))
        w["s"].append(int(row["score"]))
        w["p"].append(None if pd.isna(row["price_usd"]) else int(row["price_usd"]))
        w["ap"].append(apps.index(row["appellation"]))
        w["st"].append(styles.index(row["style"]))
        w["pr"].append(round(float(oof.loc[i, "p90_sensory"]), 3))
        w["at"].append([pos[int(a)] for a in at])
        w["ru"].append(rules.index(pairing_rule(prof)))
        w["eq"].append([[tr_ids.index(e["id"]), e["score"]] for e in equivalents(prof, ref)])
        w["na"].append(1 if native_alternative(prof) else 0)

    appl = pd.read_csv(ROOT / "data" / "reference" / "appellations.csv").drop_duplicates("appellation")
    top = coefs[(coefs["n_reviews"] >= 30)]
    out = {
        "generated": pd.Timestamp.now().strftime("%Y-%m-%d"),
        "quality": metrics["quality"],
        "names": names, "appellations": apps, "styles": styles,
        "appellation_info": {r.appellation: {"bank": r.bank, "grapes": r.main_grapes} for r in appl.itertuples()},
        "wines": w, "desc": dlist, "model": {"intercept": round(float(lr.intercept_[0]), 4)},
        "cheeses": ref["cheeses"].to_dict("records"), "rules": ref["rules"].to_dict("records"),
        "turkish": ref["turkish"].to_dict("records"),
        "provinces": pd.read_csv(ROOT / "data" / "reference" / "provinces.csv")[["province", "lat", "lon"]].values.tolist(),
        "families": {r.family: [r.family_en, r.family_tr] for r in desc.drop_duplicates("family").itertuples()},
        "results": {
            "comparison": comp[["design", "features", "model", "accuracy", "accuracy_sd", "balanced_accuracy",
                                "roc_auc", "roc_auc_sd", "f1_90plus"]].to_dict("records"),
            "legacy": {k: v for k, v in metrics["legacy_2024_pipeline"].items() if k != "top45_selected_on_all_rows"},
            "calibration": metrics["calibration"], "by_style": metrics["by_style"],
            "regression": metrics["regression"], "price": metrics["price"], "literature": metrics["literature"],
            "top": {f: {"pos": top[top.features == f].nlargest(12, "coef")[["attribute", "coef", "n_reviews"]].to_dict("records"),
                        "neg": top[top.features == f].nsmallest(12, "coef")[["attribute", "coef", "n_reviews"]].to_dict("records")}
                    for f in ("all", "sensory")},
        },
    }
    (WEB / "wine.json").write_text(json.dumps(out, ensure_ascii=False, separators=(",", ":")))
    print("wine.json", round((WEB / "wine.json").stat().st_size / 1e6, 2), "MB")

    # ---- two README figures ----
    plt.rcParams.update({"font.size": 10, "axes.spines.top": False, "axes.spines.right": False})
    g = comp[(comp.design == "grouped") & (comp.model == "logistic")].set_index("features")
    fig, ax = plt.subplots(figsize=(6.4, 2.4))
    order = ["all", "descriptive", "sensory"]
    lab = {"all": "All 616 descriptors", "descriptive": "Descriptive/praise terms only", "sensory": "Flavour & structure only"}
    ys = list(range(len(order)))[::-1]
    vals = [g.loc[o, "roc_auc"] for o in order]
    ax.hlines(ys, 0.5, vals, color="#d8d4cc", lw=2)
    ax.plot(vals, ys, "o", color="#7a1f33", ms=8)
    for yy, v in zip(ys, vals):
        ax.text(v + 0.008, yy, f"{v:.3f}", va="center")
    ax.set_yticks(ys, [lab[o] for o in order])
    ax.set_xlim(0.5, 1.0)
    ax.set_xlabel("ROC AUC, 5-fold CV grouped by wine (0.5 = coin flip)")
    ax.set_title("Praise words predict a 90+ score better than flavours", loc="left", fontsize=10.5)
    fig.tight_layout()
    fig.savefig(FIG / "ablation.png", dpi=160)

    s = coefs[(coefs.features == "sensory") & (coefs.n_reviews >= 30)]
    s = pd.concat([s.nsmallest(8, "coef"), s.nlargest(8, "coef")]).sort_values("coef")
    fig, ax = plt.subplots(figsize=(6.4, 4.2))
    ax.barh(s["attribute"].str.capitalize(), s["coef"], color=np.where(s["coef"] > 0, "#7a1f33", "#8a8f98"), height=0.6)
    ax.axvline(0, color="#333", lw=1)
    ax.set_xlabel("Log-odds of a 90+ score (sensory-only logistic regression)")
    ax.set_title("Flavour words that move the odds", loc="left", fontsize=11)
    fig.tight_layout()
    fig.savefig(FIG / "sensory_words.png", dpi=160)
    print("figures written")


if __name__ == "__main__":
    main()
