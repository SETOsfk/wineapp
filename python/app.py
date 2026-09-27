"""Streamlit version of the explorer.   streamlit run python/app.py
Reads the same web/data/wine.json the static site uses (build it with python/build_web.py)."""
import json
import math
from pathlib import Path

import numpy as np
import requests
import streamlit as st

ROOT = Path(__file__).resolve().parents[1]
st.set_page_config(page_title="Bordeaux → Türkiye", page_icon="🍷", layout="wide")


@st.cache_data
def load():
    d = json.loads((ROOT / "web" / "data" / "wine.json").read_text())
    w = d["wines"]
    n, m = len(w["l"]), len(d["desc"])
    sens = np.array([x["s"] for x in d["desc"]], bool)
    idf = np.array([x["idf"] for x in d["desc"]])
    M = np.zeros((n, m), np.float32)                     # dense is fine: 14k × 616
    for i, at in enumerate(w["at"]):
        M[i, at] = 1
    V = M * np.where(sens, idf, 0)                       # IDF-weighted, flavour & structure only
    V /= np.linalg.norm(V, axis=1, keepdims=True) + 1e-9
    return d, V


d, V = load()
w = d["wines"]
tr = st.sidebar.radio("Dil / Language", ["Türkçe", "English"]) == "Türkçe"
T = (lambda a, b: a if tr else b)
lab = lambda x: x["tr"] if tr else x["en"]

st.title(T("90 puanlık bir Bordeaux'yu ne yapar?", "What makes a 90-point Bordeaux?"))
st.caption(T("14.349 Wine Spectator incelemesi · peynir eşleşmesi · Türk muadilleri · yakındaki satış noktaları",
             "14,349 Wine Spectator reviews · cheese pairing · Turkish equivalents · shops nearby"))


def wine_card(i):
    name = d["names"][w["l"][i]]
    app, style = d["appellations"][w["ap"][i]], d["styles"][w["st"][i]]
    st.subheader(f"{name} · {w['y'][i]}")
    c1, c2, c3 = st.columns(3)
    c1.metric(T("Puan", "Score"), w["s"][i], "90+" if w["s"][i] >= 90 else "≤89")
    c2.metric(T("Model (görmeden)", "Model (unseen)"), f"{w['pr'][i]:.0%}")
    c3.metric(T("Çıkış fiyatı", "Release price"), f"${w['p'][i]}" if w["p"][i] else "—")
    st.write(f"{app} · {style} · " + (d["appellation_info"].get(app, {}).get("grapes") or ""))
    st.write(" · ".join(lab(d["desc"][a]) for a in w["at"][i]))

    sims = V @ V[i]
    order = [j for j in np.argsort(-sims) if w["l"][j] != w["l"][i] and w["st"][j] == w["st"][i]]
    seen, rows = set(), []
    for j in order:
        if w["l"][j] in seen:
            continue
        seen.add(w["l"][j]); rows.append({T("Şarap", "Wine"): d["names"][w["l"][j]], T("Yıl", "Year"): w["y"][j],
                                          T("Puan", "Score"): w["s"][j], T("Benzerlik", "Similarity"): f"{sims[j]:.0%}"})
        if len(rows) == 5:
            break
    st.markdown("#### " + T("Tadı en çok benzeyenler", "Most similar in taste"))
    st.dataframe(rows, hide_index=True, width="stretch")

    rule = d["rules"][w["ru"][i]]
    st.markdown("#### " + T("Yanına peynir", "Cheese to pair"))
    st.write((rule["rationale_tr"] if tr else rule["rationale_en"]) + f" — [{rule['source_name']}]({rule['source_url']})")
    ch = {c["id"]: c for c in d["cheeses"]}
    st.write(", ".join((ch[c]["name_tr"] if tr else ch[c]["name_en"]) for c in rule["cheese_ids"].split(";")))

    st.markdown("#### " + T("Türkiye'deki muadilleri", "Turkish equivalents"))
    for k, score in w["eq"][i]:
        t_ = d["turkish"][k]
        st.write(f"**{t_['producer']} — {t_['wine']}** ({score:.0%}) · {t_['grapes'].replace(';', ', ')} · "
                 f"{t_['location_tr'] if tr else t_['location_en']} · [{T('kaynak', 'source')}]({t_['source_url']})")


def haversine(a, b):
    r = math.pi / 180
    h = math.sin((b[0] - a[0]) * r / 2) ** 2 + math.cos(a[0] * r) * math.cos(b[0] * r) * math.sin((b[1] - a[1]) * r / 2) ** 2
    return 12742 * math.asin(math.sqrt(h))


@st.cache_data(ttl=3600, show_spinner=False)
def shops(lat, lon):
    for radius in (3000, 10000, 30000):
        q = (f'[out:json][timeout:20];(nwr["shop"~"^(wine|alcohol|beverages)$"](around:{radius},{lat},{lon});'
             f'nwr["craft"="winery"](around:{radius},{lat},{lon}););out center 80;')
        els = requests.post("https://overpass-api.de/api/interpreter", data={"data": q}, timeout=30).json()["elements"]
        out = []
        for e in els:
            la, lo = e.get("lat") or e.get("center", {}).get("lat"), e.get("lon") or e.get("center", {}).get("lon")
            tg = e.get("tags", {})
            out.append({"km": round(haversine((lat, lon), (la, lo)), 1), "name": tg.get("name", "—"),
                        "type": tg.get("shop") or tg.get("craft"), "osm": f"https://www.openstreetmap.org/{e['type']}/{e['id']}"})
        if len(out) >= 5 or radius == 30000:
            return sorted(out, key=lambda x: x["km"])[:12]


tab1, tab2, tab3 = st.tabs([T("Adıyla ara", "By name"), T("Tadıyla ara", "By taste"), T("Nereden alırım?", "Where to buy?")])
with tab1:
    label_i = st.selectbox(T("Şarap", "Wine"), range(len(d["names"])), format_func=lambda k: d["names"][k],
                           index=d["names"].index("Château Margaux Margaux"))
    rows = sorted([i for i, l in enumerate(w["l"]) if l == label_i], key=lambda i: -w["y"][i])
    i = st.radio(T("Rekolte", "Vintage"), rows, format_func=lambda i: f"{w['y'][i]} · {w['s'][i]}", horizontal=True)
    wine_card(i)
with tab2:
    choices = [k for k, x in enumerate(d["desc"]) if x["s"] and x["c"] is not None and x["n"] >= 60]
    picked = st.multiselect(T("Sevdiğin tatlar", "Flavours you like"), choices, format_func=lambda k: lab(d["desc"][k]), max_selections=8)
    if picked:
        p = 1 / (1 + math.exp(-(d["model"]["intercept"] + sum(d["desc"][k]["c"] for k in picked))))
        st.metric(T("90+ olasılığı", "Chance of 90+"), f"{p:.0%}")
        q = np.zeros(V.shape[1]); q[picked] = [d["desc"][k]["idf"] for k in picked]
        sims = V @ (q / np.linalg.norm(q))
        top = np.argsort(-sims)[:10]
        st.dataframe([{T("Şarap", "Wine"): d["names"][w["l"][j]], T("Yıl", "Year"): w["y"][j], T("Puan", "Score"): w["s"][j]}
                      for j in top], hide_index=True, width="stretch")
with tab3:
    st.caption(T("OpenStreetMap'teki şarap/içki satış noktaları. 22.00–06.00 arası perakende satış ve 18 yaş altına satış yasaktır (4250 s. Kanun md. 6).",
                 "Wine/liquor shops from OpenStreetMap. Retail sales are banned 22:00–06:00 and to under-18s (Law 4250, art. 6)."))
    prov = st.selectbox(T("İl", "Province"), d["provinces"], format_func=lambda p: p[0], index=33)
    if st.button(T("Yakındaki satış noktalarını bul", "Find shops nearby")):
        try:
            st.dataframe(shops(prov[1], prov[2]), hide_index=True, width="stretch",
                         column_config={"osm": st.column_config.LinkColumn("OpenStreetMap")})
        except requests.RequestException:
            st.error(T("OpenStreetMap şu an yanıt vermedi.", "OpenStreetMap did not answer."))
    st.caption("© OpenStreetMap contributors (ODbL)")
