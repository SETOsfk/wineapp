"""One-off helper that wrote data/reference/descriptors.csv.

The Computational Wine Wheel (CWW) columns arrive in category blocks. The block
boundaries below were read off the column order of BordeauxWines.csv (see README,
"Data"); Turkish labels were written by hand for every descriptor that appears in
at least 20 reviews (sensory) or 100 reviews (descriptive terms).
"""
import csv
import sys
from pathlib import Path

import pandas as pd

BLOCKS = [(0, 22, "citrus"), (23, 50, "berry"), (51, 59, "fruit_general"), (60, 117, "tree_fruit"),
          (118, 177, "dried_fruit"), (178, 190, "candied"), (191, 200, "yeast_dairy"), (201, 205, "animal"),
          (206, 244, "floral_herbaceous"), (245, 288, "spice"), (289, 292, "tannin"), (293, 317, "body"),
          (318, 320, "acidity"), (321, 325, "finish"), (326, 757, "descriptive"), (758, 770, "meat"),
          (771, 799, "herbs"), (800, 814, "vegetal"), (815, 835, "tea_tobacco"), (836, 850, "nuts"),
          (851, 890, "sweet"), (891, 899, "oak"), (900, 903, "medicinal"), (904, 929, "roasted"),
          (930, 960, "earth"), (961, 984, "chemical")]

TR = {
 "BLOOD ORANGE": "Kan portakalı", "CITRUS": "Narenciye", "CLEMENTINE": "Klementin", "LIME": "Misket limonu",
 "GRAPEFRUIT": "Greyfurt", "ORANGE": "Portakal", "LEMON": "Limon", "TANGERINE": "Mandalina",
 "BERRY": "Orman meyvesi", "BLACK CURRANT": "Siyah frenk üzümü", "BLACKBERRY": "Böğürtlen",
 "BLUEBERRY": "Yaban mersini", "BOYSENBERRY": "Boysen böğürtleni", "RASPBERRY": "Ahududu",
 "CURRANT": "Frenk üzümü", "GOOSEBERRY": "Bektaşi üzümü", "LOGANBERRY": "Logan böğürtleni",
 "STRAWBERRY": "Çilek", "BLACK FRUIT": "Siyah meyve", "DARK FRUIT": "Koyu meyve", "FRUIT": "Meyve",
 "APPLE": "Elma", "APRICOT": "Kayısı", "BLACK CHERRY": "Siyah kiraz", "CHERRY": "Kiraz", "GRAPE": "Üzüm",
 "NECTARINE": "Nektarin", "PERSIMMON": "Trabzon hurması", "YELLOW APPLE": "Sarı elma", "PLUM": "Erik",
 "DAMSON": "Mürdüm eriği", "PEAR": "Armut", "PEACH": "Şeftali", "MELON": "Kavun", "PINEAPPLE": "Ananas",
 "MANGO": "Mango", "PLUM SKIN": "Erik kabuğu", "POMEGRANATE": "Nar", "QUINCE": "Ayva",
 "TOASTED COCONUT": "Kavrulmuş hindistancevizi", "BLACK FIG": "Siyah incir", "BRAISED FIG": "Pişmiş incir",
 "CRUSHED FIG": "Ezilmiş incir", "FIG": "İncir", "FRUIT CAKE": "Meyveli kek", "JAM": "Reçel",
 "PRUNE": "Kuru erik", "RAISIN": "Kuru üzüm", "KIRSCH": "Kirsch (kiraz likörü)", "BRIOCHE": "Brioş",
 "CREAM": "Krema", "BOTRYTIS": "Botritis (soylu küf)", "LEATHER": "Deri", "BRIAR": "Çalılık",
 "CHAMOMILE": "Papatya", "CHIVE": "Frenk soğanı", "CLOVE": "Karanfil", "DRIED FLOWERS": "Kuru çiçek",
 "FENNEL": "Rezene", "FLORAL": "Çiçeksi", "HONEYSUCKLE": "Hanımeli", "LEAF": "Yaprak", "LILAC": "Leylak",
 "RHUBARB": "Ravent", "ROSE": "Gül", "TARRAGON": "Tarhun", "VERBENA": "Limon otu", "VIOLET": "Menekşe",
 "ANISE": "Anason", "SPICE": "Baharat", "BLACK LICORICE": "Siyah meyankökü", "CINNAMON": "Tarçın",
 "FLEUR DE SEL": "Deniz tuzu", "GINGER": "Zencefil", "PEPPER": "Biber", "INDIAN SPICES": "Hint baharatları",
 "LICORICE": "Meyankökü", "MULLED SPICE": "Sıcak şarap baharatı", "PASTIS": "Pastis (anason)",
 "ROASTED VANILLA": "Kavrulmuş vanilya", "TANNINS_HIGH": "Yüksek tanen", "TANNINS_MEDIUM": "Orta tanen",
 "TANNINS_MED": "Orta tanen", "TANNINS_LOW": "Düşük tanen", "CORE": "Yoğun çekirdek", "DARK RUBY": "Koyu yakut",
 "DENSE": "Yoğun", "FRAME": "Sağlam çatı", "FULL-BODIED": "Dolgun gövde", "LIGHT-BODIED": "Hafif gövde",
 "MEDIUM-BODIED": "Orta gövde", "RED": "Kırmızı", "ROUND": "Yuvarlak", "SOLID": "Sağlam",
 "WINE_WEIGHT": "Ağırlık", "WHITE": "Beyaz", "YELLOW": "Sarı", "WELL-STRUCTURED": "İyi yapılanmış",
 "CONCENTRATED": "Konsantre", "ACIDITY_LOW": "Düşük asidite", "FINISH": "Bitiş", "LONG FINISH": "Uzun bitiş",
 "EXCELLENT FINISH": "Mükemmel bitiş", "WEAK FINISH": "Zayıf bitiş", "BEEF": "Sığır eti", "GAME": "Av eti",
 "MEAT": "Et", "BRAMBLE": "Böğürtlen çalısı", "HERBS": "Otlar", "MINT": "Nane", "MESQUITE": "Mesquite dumanı",
 "SAGE": "Adaçayı", "ASPARAGUS": "Kuşkonmaz", "BLACK OLIVE": "Siyah zeytin", "OLIVE": "Zeytin",
 "BLACK TEA": "Siyah çay", "GREEN TEA": "Yeşil çay", "HAY/STRAW": "Saman", "MADURO TOBACCO": "Maduro tütün",
 "TEA": "Çay", "TOBACCO": "Tütün", "ALMOND": "Badem", "CHESTNUT": "Kestane", "HAZELNUT": "Fındık",
 "MACADAMIA NUT": "Makadamya", "CHOCOLATE": "Çikolata", "COCOA": "Kakao", "GANACHE": "Ganaj",
 "BUTTER": "Tereyağı", "CARAMEL": "Karamel", "HONEY": "Bal", "ESPRESSO": "Espresso",
 "LINZER TORTE": "Linzer tart", "MARZIPAN": "Badem ezmesi", "MERINGUE": "Beze", "MOCHA": "Moka",
 "PIE": "Turta", "PIECRUST": "Turta hamuru", "TART": "Tart", "TOFFEE": "Tofi", "ALDER": "Kızılağaç",
 "CEDAR": "Sedir", "OAK": "Meşe", "VANILLA": "Vanilya", "SANDALWOOD": "Sandal ağacı", "WOOD": "Ahşap",
 "QUININE": "Kinin", "COFFEE": "Kahve", "CHARCOAL": "Kömür", "CIGAR": "Puro", "COLA": "Kola",
 "ROASTED": "Kavrulmuş", "SMOKE": "Duman", "TOAST": "Kızarmış ekmek", "CHALK": "Tebeşir",
 "LOAM": "Tınlı toprak", "EARTHY": "Topraksı", "CRUSHED ROCK": "Ezilmiş taş", "DUSTY": "Tozlu",
 "IRON": "Demir", "PENCIL LEAD": "Kurşun kalem ucu", "HUMUS": "Humus toprağı", "MINERAL": "Mineral",
 "MUSHROOM": "Mantar", "PEBBLE": "Çakıl", "STONE": "Taş", "TAR": "Katran", "MENTHOL": "Mentol",
 # descriptive terms (>= 100 reviews)
 "ACCENTS": "Vurgular", "BEAUTY": "Güzellik", "ALLURING": "Cezbedici", "AMPLE": "Bol", "INTENSE": "Yoğun",
 "APPROACHABLE": "Kolay içimli", "ATTRACTIVE": "Çekici", "AUSTERE": "Sert", "BALANCE": "Denge",
 "BEAM": "Parıltı", "GREAT": "Harika", "BIG": "Büyük", "BITTER": "Acı", "BRIGHT": "Canlı", "BRISK": "Diri",
 "BROAD": "Geniş", "CARESS": "Okşayıcı", "CHARACTER": "Karakter", "CLEAN": "Temiz", "CONSISTENT": "Tutarlı",
 "CRISP": "Ferah", "CUT": "Keskinlik", "DARK": "Koyu", "DEFINED": "Belirgin", "DELICATE": "Narin",
 "DELICIOUS": "Lezzetli", "DELIVERS": "Tatmin edici", "DEPTH": "Derinlik", "DRIVE": "Enerji",
 "ELEGANT": "Zarif", "ENTICING": "Davetkâr", "FINE": "İnce", "FIRM": "Sıkı", "FLATTERING": "Hoşa giden",
 "FLAVORS": "Aromalar", "PERSIST": "Kalıcı", "FLESH": "Etli doku", "FOCUSED": "Odaklı", "FRESH": "Taze",
 "FULL": "Dolgun", "GENTLE": "Nazik", "GLIDE": "Akıcı", "GOOD": "İyi", "GORGEOUS": "Muhteşem",
 "GRIP": "Kavrayış", "HONEST": "Dürüst", "IMPRESSES": "Etkileyici", "INCENSE": "Tütsü",
 "INTEGRATE": "Bütünleşik", "INVITING": "Davetkâr", "JUICINESS": "Sululuk", "LACED": "İşlenmiş",
 "LAYER": "Katman", "LENGTH": "Uzunluk", "LINGER": "Kalıcı tat", "LIVELY": "Canlı", "POWER": "Güç",
 "LONG": "Uzun", "LOVELY": "Hoş", "LUSH": "Bereketli", "MODERN": "Modern", "MODEST": "Mütevazı",
 "MOUTHWATERING": "Ağız sulandıran", "MUSCLE": "Kaslı", "NEEDS TIME": "Zamana ihtiyaç duyar",
 "NICE": "Güzel", "OPEN": "Açık", "PERFUMY": "Parfümsü", "PLEASANT": "Hoş", "PLUMP": "Etli",
 "PLUSH": "Pelüş doku", "POLISH": "Cilalı", "PRETTY": "Sevimli", "PURE": "Saf", "YOUNG": "Genç",
 "RACY": "Diri asitli", "RANGE": "Çeşitlilik", "REFINED": "Rafine", "RICH": "Zengin", "RIPE": "Olgun",
 "SANGUINE": "Demirimsi", "SAVORY": "Tuzlu-umami", "STYLE": "Stil", "SERIOUS": "Ciddi", "SILKY": "İpeksi",
 "SLEEK": "Pürüzsüz", "SOFT": "Yumuşak", "STRENGTH": "Kuvvet", "SUBTLE": "İncelikli", "SUPPLE": "Esnek",
 "SWEET": "Tatlı", "TANGY": "Ekşimsi", "TASTY": "Tadı yerinde", "TAUT": "Gergin", "TEXTURE": "Doku",
 "THICK": "Kalın doku", "TIGHT": "Kapalı", "VELVET": "Kadife", "WARM": "Sıcak", "WELL DONE": "İyi yapılmış",
 "WONDERFUL": "Şahane", "ZEST": "Kabuk",
}
EN_SPECIAL = {"TANNINS_HIGH": "High tannins", "TANNINS_MEDIUM": "Medium tannins", "TANNINS_MED": "Medium tannins",
              "TANNINS_LOW": "Low tannins", "ACIDITY_LOW": "Low acidity", "ACIDITY_HIGH": "High acidity",
              "ACIDITY_MEDIUM": "Medium acidity", "WINE_WEIGHT": "Weight", "HAY/STRAW": "Hay / straw",
              "WET WOOL,WET DOG": "Wet wool / wet dog"}

FAMILY_LABELS = {
 "citrus": ("Citrus", "Narenciye"), "berry": ("Berries", "Orman meyveleri"),
 "fruit_general": ("Fruit (general)", "Meyve (genel)"), "tree_fruit": ("Tree & stone fruit", "Ağaç meyveleri"),
 "dried_fruit": ("Dried & cooked fruit", "Kuru ve pişmiş meyve"), "candied": ("Candied & kirsch", "Şekerleme"),
 "yeast_dairy": ("Yeast & dairy", "Maya ve süt"), "animal": ("Botrytis & leather", "Botritis ve deri"),
 "floral_herbaceous": ("Floral & leafy", "Çiçeksi ve yapraksı"), "spice": ("Spice", "Baharat"),
 "tannin": ("Tannins", "Tanen"), "body": ("Body & structure", "Gövde ve yapı"), "acidity": ("Acidity", "Asidite"),
 "finish": ("Finish", "Bitiş"), "descriptive": ("Descriptive terms", "Betimleyici terimler"),
 "meat": ("Meat & savoury", "Et ve tuzlu"), "herbs": ("Herbs", "Otlar"), "vegetal": ("Vegetal & olive", "Sebzemsi ve zeytin"),
 "tea_tobacco": ("Tea & tobacco", "Çay ve tütün"), "nuts": ("Nuts", "Kuruyemiş"),
 "sweet": ("Chocolate, honey & pastry", "Çikolata, bal ve tatlı"), "oak": ("Oak & wood", "Meşe ve ahşap"),
 "medicinal": ("Medicinal", "Tıbbi"), "roasted": ("Coffee, toast & smoke", "Kahve, kavrulmuş ve duman"),
 "earth": ("Earth & mineral", "Toprak ve mineral"), "chemical": ("Tar, menthol & faults", "Katran, mentol ve kusurlar"),
}


def en_label(a: str) -> str:
    return EN_SPECIAL.get(a, a.capitalize() if a.isupper() else a)


def main(csv_path: str) -> None:
    cols = list(pd.read_csv(csv_path, encoding="utf-8-sig", nrows=0).columns[4:])
    assert len(cols) == 985, len(cols)
    fam = {}
    for a, b, k in BLOCKS:
        for i in range(a, b + 1):
            fam[cols[i]] = k
    out = Path(__file__).resolve().parents[1] / "data" / "reference" / "descriptors.csv"
    with open(out, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, lineterminator="\n")
        w.writerow(["attribute", "position", "family", "family_en", "family_tr", "label_en", "label_tr"])
        for i, c in enumerate(cols):
            k = fam[c]
            w.writerow([c, i, k, FAMILY_LABELS[k][0], FAMILY_LABELS[k][1], en_label(c), TR.get(c, "")])
    print("wrote", out)


if __name__ == "__main__":
    main(sys.argv[1])
