"""Smallest checks that fail if the parsing or the rules break.  pytest python/tests"""
import pandas as pd

from wine.data import _appellation_regex, fix_encoding, load_appellations, parse_name
from wine.pairing import Profile, equivalents, grape_weights, load_reference, native_alternative, pairing_rule

APP = load_appellations()
RX = _appellation_regex(APP)


def test_encoding():
    assert fix_encoding("ChÃ¢teau L'Ã‰vangile MÃ©doc") == "Château L'Évangile Médoc"


def test_parse_name_edge_cases():
    p = parse_name("Château Margaux Margaux", APP, RX)                      # producer shares the AOC word
    assert (p["producer"], p["appellation"], p["style"]) == ("Château Margaux", "Margaux", "red")
    p = parse_name("Grand Enclos du Château de Cérons Graves", APP, RX)       # generic-only prefix skipped
    assert p["appellation"] == "Graves"
    p = parse_name("Château de Fieuzal Pessac-Léognan White L'Abeille de Fieuzal", APP, RX)
    assert (p["style"], p["cuvee"]) == ("white", "L'Abeille de Fieuzal")
    assert parse_name("Château X Cadillac Côtes de Bordeaux", APP, RX)["style"] == "red"   # not the sweet AOC
    assert parse_name("Château Y Cadillac", APP, RX)["style"] == "sweet"


def test_rules():
    assert grape_weights("A:80;B:20") == {"A": 0.8, "B": 0.2}
    assert abs(sum(grape_weights("A;B;C").values()) - 1) < 1e-9
    left = Profile("red", "left", grape_weights("Cabernet Sauvignon, Merlot, Cabernet Franc, Petit Verdot"), "full", "high", True)
    assert pairing_rule(left) == "red_full" and native_alternative(left) == "kayra_buzbag_rezerv"
    assert pairing_rule(Profile("sweet", "sweet", {}, None, None, None)) == "sweet"
    top = equivalents(left, load_reference())
    assert top[0]["id"] in {"chateau_kalpak", "urla_tempus"}                  # Bordeaux blends rank first
    sweet = Profile("sweet", "sweet", grape_weights("Sémillon, Sauvignon Blanc, Muscadelle"), "full", None, None)
    assert equivalents(sweet, load_reference())[0]["id"] == "gurbuz_semillon_lh"
