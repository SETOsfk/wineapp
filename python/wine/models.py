"""Model zoo and evaluation designs.

Three ways to split the data, because "how good is the model" depends on the question:
  random    stratified 5-fold CV — what the literature reports (Dong et al. 2020: SVM 86.97 %)
  grouped   the same wine (all vintages of a label) never sits in train and test at once
  temporal  train on vintages 2000–2011, test on 2012–2016 (a model is used on future vintages)
"""
from __future__ import annotations

import numpy as np
import pandas as pd
from sklearn.base import clone
from sklearn.dummy import DummyClassifier
from sklearn.ensemble import HistGradientBoostingClassifier, RandomForestClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (accuracy_score, balanced_accuracy_score, brier_score_loss, f1_score,
                             roc_auc_score)
from sklearn.model_selection import StratifiedGroupKFold, StratifiedKFold
from sklearn.naive_bayes import BernoulliNB
from sklearn.svm import LinearSVC

SEED = 20260926
TEMPORAL_CUTOFF = 2011   # train <= 2011, test >= 2012


def feature_sets(descriptors: pd.DataFrame, X: np.ndarray) -> dict[str, np.ndarray]:
    """Column indices for the ablation. Descriptors never used in any review are dropped
    (unsupervised filter, no label information → no leakage)."""
    used = X.sum(axis=0) > 0
    fam = descriptors["family"].to_numpy()
    return {
        "all": np.where(used)[0],
        "sensory": np.where(used & (fam != "descriptive"))[0],
        "descriptive": np.where(used & (fam == "descriptive"))[0],
    }


def model_zoo() -> dict:
    return {
        "majority": DummyClassifier(strategy="most_frequent"),
        "naive_bayes": BernoulliNB(alpha=1.0),
        "logistic": LogisticRegression(C=0.5, max_iter=2000),
        "linear_svm": LinearSVC(C=0.05, max_iter=5000),
        "random_forest": RandomForestClassifier(n_estimators=400, min_samples_leaf=2, n_jobs=-1,
                                                random_state=SEED),
        "gradient_boosting": HistGradientBoostingClassifier(max_iter=400, learning_rate=0.06,
                                                            max_leaf_nodes=31, l2_regularization=1.0,
                                                            random_state=SEED),
    }


def splits(meta: pd.DataFrame, y: np.ndarray, design: str, n_splits: int = 5):
    """Yield (fold, train_idx, test_idx)."""
    idx = np.arange(len(y))
    if design == "random":
        cv = StratifiedKFold(n_splits=n_splits, shuffle=True, random_state=SEED)
        for k, (tr, te) in enumerate(cv.split(idx, y)):
            yield k, tr, te
    elif design == "grouped":
        cv = StratifiedGroupKFold(n_splits=n_splits, shuffle=True, random_state=SEED)
        for k, (tr, te) in enumerate(cv.split(idx, y, groups=meta["group"].to_numpy())):
            yield k, tr, te
    elif design == "temporal":
        yr = meta["year"].to_numpy()
        yield 0, idx[yr <= TEMPORAL_CUTOFF], idx[yr > TEMPORAL_CUTOFF]
    else:
        raise ValueError(design)


def _scores(model, X) -> np.ndarray:
    if hasattr(model, "predict_proba"):
        return model.predict_proba(X)[:, 1]
    return model.decision_function(X)


def metrics(y_true, y_pred, y_score, proba: bool) -> dict:
    out = {
        "accuracy": accuracy_score(y_true, y_pred),
        "balanced_accuracy": balanced_accuracy_score(y_true, y_pred),
        "f1_90plus": f1_score(y_true, y_pred, zero_division=0),
        "roc_auc": roc_auc_score(y_true, y_score) if len(np.unique(y_score)) > 1 else 0.5,
    }
    if proba:
        out["brier"] = brier_score_loss(y_true, y_score)
    return out


def evaluate(X: np.ndarray, y: np.ndarray, meta: pd.DataFrame, cols: np.ndarray, model, design: str,
             fold_ids: dict | None = None) -> pd.DataFrame:
    rows = []
    for k, tr, te in splits(meta, y, design):
        m = clone(model)
        m.fit(X[np.ix_(tr, cols)], y[tr])
        Xte = X[np.ix_(te, cols)]
        pred = m.predict(Xte)
        score = _scores(m, Xte)
        r = metrics(y[te], pred, score, proba=hasattr(m, "predict_proba"))
        r.update(fold=k, n_test=len(te))
        rows.append(r)
        if fold_ids is not None:
            fold_ids.setdefault(design, np.full(len(y), -1))[te] = k
    return pd.DataFrame(rows)


def summarize(df: pd.DataFrame) -> dict:
    keys = [c for c in df.columns if c not in ("fold", "n_test")]
    return {k: {"mean": float(df[k].mean()), "sd": float(df[k].std(ddof=1)) if len(df) > 1 else 0.0}
            for k in keys}
