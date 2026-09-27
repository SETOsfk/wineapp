"""Cheese pairing and Turkish-equivalent matching.

Both are transparent rules, not models: every suggestion carries the rule that fired
and the source behind it (data/reference/*.csv). Mirrored in R/wine_rules.R.
"""
from __future__ import annotations

import math
import re
from dataclasses import dataclass

import pandas as pd

from .data import REF

OAK = {"OAK", "VANILLA", "TOAST", "CEDAR", "WOOD", "ROASTED VANILLA", "SANDALWOOD", "ALDER"}
BODY_ORDER = {"light": 0, "medium": 1, "full": 2}


@dataclass
class Profile:
    style: str | None          # red / white / sweet / rosé
    bank: str | None           # left / right / generic / sweet
    grapes: dict               # grape -> weight (sums to 1)
    body: str | None           # light / medium / full / None
    tannin: str | None         # low / medium / high / None
    oak: bool | None


def load_reference() -> dict[str, pd.DataFrame]:
    return {
        "cheeses": pd.read_csv(REF / "cheeses.csv", keep_default_na=False),
        "rules": pd.read_csv(REF / "pairing_rules.csv", keep_default_na=False),
        "turkish": pd.read_csv(REF / "turkish_wines.csv", keep_default_na=False, dtype={"oak_months": str}),
    }


def grape_weights(spec: str) -> dict:
    """"A;B;C" → weights 1, 1/2, 1/3 (listed order = dominance, normalised);
    "A:80;B:20" → explicit shares. Also accepts comma-separated lists."""
    parts = [p.strip() for p in re.split(r"[;,]", spec or "") if p.strip()]
    parts = [re.sub(r"\s*\(.*\)$", "", p) for p in parts]          # drop "(noble rot)"
    w = {}
    for rank, p in enumerate(parts, start=1):
        if ":" in p:
            g, share = p.split(":")
            w[g.strip()] = float(share)
        else:
            w[p] = 1.0 / rank
    tot = sum(w.values()) or 1.0
    return {g: v / tot for g, v in w.items()}


def profile_from_wine(style, bank, main_grapes, attrs: set[str]) -> Profile:
    body = ("full" if "FULL-BODIED" in attrs else "light" if "LIGHT-BODIED" in attrs
            else "medium" if "MEDIUM-BODIED" in attrs else None)
    tannin = ("high" if "TANNINS_HIGH" in attrs else "low" if "TANNINS_LOW" in attrs
              else "medium" if ({"TANNINS_MEDIUM", "TANNINS_MED"} & attrs) else None)
    return Profile(style=style, bank=bank, grapes=grape_weights(main_grapes or ""), body=body,
                   tannin=tannin, oak=bool(OAK & attrs))


def pairing_rule(p: Profile) -> str:
    if p.style == "sweet":
        return "sweet"
    if p.style == "white":
        return "white_dry"
    if p.style == "rosé":
        return "rose"
    if p.style == "red":
        full = p.body == "full" or p.tannin == "high" or (p.bank == "left" and p.body != "light")
        return "red_full" if full else "red_fruity"
    return "fallback"


def cheeses_for(p: Profile, ref: dict) -> dict:
    rule = ref["rules"].set_index("rule_id").loc[pairing_rule(p)]
    ids = rule["cheese_ids"].split(";")
    ch = ref["cheeses"].set_index("id").loc[ids].reset_index()
    return {"rule": pairing_rule(p), "rationale_tr": rule["rationale_tr"], "rationale_en": rule["rationale_en"],
            "source_name": rule["source_name"], "source_url": rule["source_url"],
            "turkish": ch[ch["is_turkish"] == 1]["id"].tolist(), "international": ch[ch["is_turkish"] == 0]["id"].tolist()}


def _cosine(a: dict, b: dict) -> float:
    num = sum(a[g] * b.get(g, 0.0) for g in a)
    den = math.sqrt(sum(v * v for v in a.values())) * math.sqrt(sum(v * v for v in b.values()))
    return num / den if den else 0.0


def _body_sim(a, b) -> float:
    if a is None or b is None or a == "" or b == "":
        return 0.5
    return 1.0 - abs(BODY_ORDER[a] - BODY_ORDER[b]) / 2


def _oak_of(months: str):
    if months is None or months == "":
        return None
    m = re.findall(r"\d+", str(months))
    return bool(m) and max(int(x) for x in m) > 0


def equivalents(p: Profile, ref: dict, k: int = 3) -> list[dict]:
    """Rank Turkish wines of the same style by grape overlap (60 %), body (25 %) and oak (15 %)."""
    tr = ref["turkish"]
    out = []
    for r in tr.itertuples(index=False):
        if r.style != p.style:
            continue
        g = _cosine(p.grapes, grape_weights(r.grapes))
        if g == 0:                       # no shared grape → shown separately as a native-grape idea
            continue
        b = _body_sim(p.body, r.body)
        t_oak = _oak_of(r.oak_months)
        o = 0.5 if (p.oak is None or t_oak is None) else float(p.oak == t_oak)
        out.append({"id": r.id, "score": round(0.60 * g + 0.25 * b + 0.15 * o, 4),
                    "grape": round(g, 3), "body": b, "oak": o})
    out.sort(key=lambda d: (-d["score"], d["id"]))
    return out[:k]


def native_alternative(p: Profile) -> str | None:
    """A native-grape blend with the same structural role split (Boğazkere = tannic backbone,
    Öküzgözü = fruit and acidity; Wikipedia 'Boğazkere'). Only offered for fuller reds."""
    if p.style == "red" and (p.body == "full" or p.tannin == "high" or p.bank == "left"):
        return "kayra_buzbag_rezerv"
    return None
