# ============================================================
# ZH FELADATVÁLASZTÁS ALAPJA
# A korrelációs feladat biztosan bekerül.
# A másik biztos feladatot és a random feladatokat később adjuk hozzá.
# ============================================================

set.seed(seed)

feladat_szoveg <- paste0(
  "# ============================================================\n",
  "# Humánbiológia II. – Alkalmazott biometria ZH\n",
  "# Neptun: ", neptun, "\n",
  "# ZH: ", ZH, "\n",
  "# Seed: ", seed, "\n",
  "# ============================================================\n\n"
)

# ============================================================
# 1. KÖTELEZŐ FELADAT: KORRELÁCIÓ ÉS REGRESSZIÓ
# ============================================================

general_korrelacio_feladat <- function(seed) {
  
  set.seed(seed + 101)
  
  korrelacio_temak <- list(
    
    elettani = list(
      bevezetok = c(
        "Egy egyetemi humánbiológiai kutatás során a hallgatók alapvető élettani jellemzőit vizsgálták. A kutatók arra voltak kíváncsiak, hogy bizonyos, egyszerűen mérhető fiziológiai mutatók között kimutatható-e statisztikailag értelmezhető kapcsolat.",
        "Egy oktatási célú felmérésben két tanszék hallgatóinak néhány alapvető élettani mérőszámát rögzítették. A cél annak megvizsgálása volt, hogy a kiválasztott két változó között megfigyelhető-e lineáris kapcsolat.",
        "Egy humánbiológiai gyakorlat keretében a hallgatók anonim módon rögzített élettani adatait elemezték. A vizsgálat során két különböző tanszék hallgatóinak adatait hasonlították össze."
      ),
      valtozok = c(
        "nyugalmi pulzus",
        "terhelés utáni pulzus",
        "reggeli éberségi pontszám",
        "napi alvásidő",
        "heti sportolással töltött idő",
        "reakcióidő",
        "koncentrációs teszt pontszáma",
        "testtömegindex",
        "testzsír százalék",
        "kézszorító erő",
        "tüdőkapacitás",
        "lépésszám",
        "napi folyadékbevitel",
        "fáradtsági pontszám",
        "stresszérzet pontszáma"
      )
    ),
    
    viselkedes = list(
      bevezetok = c(
        "Egy viselkedésbiológiai jellegű egyetemi felmérésben a hallgatók napi szokásait és önértékelésen alapuló teljesítménymutatóit vizsgálták. A kutatók arra voltak kíváncsiak, hogy bizonyos viselkedéses jellemzők együtt változnak-e.",
        "Egy hallgatói életmódkutatás során a résztvevők több, mindennapi viselkedéshez kapcsolódó adatot adtak meg. A vizsgálat célja az volt, hogy feltárják, van-e kapcsolat a napi szokások és a szubjektív teljesítménymutatók között.",
        "Egy egyetemi kérdőíves vizsgálatban a hallgatók tanulási, pihenési és szabadidős szokásait mérték fel. A kutatók azt feltételezték, hogy egyes viselkedéses változók között lineáris kapcsolat figyelhető meg."
      ),
      valtozok = c(
        "napi képernyőidő",
        "napi tanulási idő",
        "napi közösségi média használat",
        "napi zenehallgatás ideje",
        "napi séta időtartama",
        "heti sportalkalmak száma",
        "kávéfogyasztás mennyisége",
        "energiaital fogyasztás gyakorisága",
        "alvásminőség pontszáma",
        "hangulat pontszáma",
        "motivációs pontszám",
        "vizsgaszorongás pontszáma",
        "életelégedettségi pontszám",
        "tanulmányi önértékelés pontszáma",
        "reggeli aktivitási pontszám"
      )
    ),
    
    antropometria = list(
      bevezetok = c(
        "Egy antropometriai gyakorlat során két tanszék hallgatóinak egyszerű testméreti adatait rögzítették. A vizsgálat célja az volt, hogy a hallgatók megtanulják, miként lehet két mennyiségi változó kapcsolatát korrelációval és regresszióval elemezni.",
        "Egy humánbiológiai mérés során a résztvevők különböző testméreti és testösszetételi adatait gyűjtötték össze. A kutatók arra voltak kíváncsiak, hogy a kiválasztott változók között milyen irányú és erősségű kapcsolat figyelhető meg.",
        "Egy oktatási célú adatfelvételben a hallgatók antropometriai változóit használták fel statisztikai elemzésre. A két tanszék adatai lehetőséget adnak arra, hogy a kapcsolat erősségét és a regressziós paramétereket külön-külön is vizsgálják."
      ),
      valtozok = c(
        "testmagasság",
        "testtömeg",
        "alkar hossza",
        "felkar kerülete",
        "combkerület",
        "vállszélesség",
        "lépéshossz",
        "ülőmagasság",
        "tenyérhossz",
        "lábméret",
        "testtömegindex",
        "derékkerület",
        "csípőkerület",
        "kézszorító erő",
        "testzsír százalék"
      )
    )
  )
  
  tanszek_parok <- list(
    c("Biológia Tanszék", "Sporttudomány Tanszék"),
    c("Pszichológia Tanszék", "Óvodapedagógia Tanszék"),
    c("Földrajz Tanszék", "Környezettudomány Tanszék"),
    c("Kémia Tanszék", "Biológia Tanszék"),
    c("Testnevelés Tanszék", "Egészségtudományi Tanszék"),
    c("Pedagógia Tanszék", "Pszichológia Tanszék"),
    c("Élelmiszertudományi Tanszék", "Egészségtudományi Tanszék"),
    c("Informatika Tanszék", "Sporttudomány Tanszék"),
    c("Természettudományi Tanszék", "Biológia Tanszék"),
    c("Rekreáció Tanszék", "Testnevelés Tanszék")
  )
  
  tema_nev <- sample(names(korrelacio_temak), 1)
  tema <- korrelacio_temak[[tema_nev]]
  
  bevezeto_kor <- sample(tema$bevezetok, 1)
  
  valtozok_kor <- sample(tema$valtozok, 2, replace = FALSE)
  x_nev <- valtozok_kor[1]
  y_nev <- valtozok_kor[2]
  
  tanszekek_kor <- sample(tanszek_parok, 1)[[1]]
  tanszek1 <- tanszekek_kor[1]
  tanszek2 <- tanszekek_kor[2]
  
  n1 <- sample(22:35, 1)
  n2 <- sample(22:35, 1)
  
  # X változó generálása témakör szerint
  if (tema_nev == "elettani") {
    X1 <- round(runif(n1, 3, 10), 1)
    X2 <- round(runif(n2, 3, 10), 1)
  } else if (tema_nev == "viselkedes") {
    X1 <- round(runif(n1, 0, 12), 1)
    X2 <- round(runif(n2, 0, 12), 1)
  } else {
    X1 <- round(runif(n1, 140, 200), 1)
    X2 <- round(runif(n2, 140, 200), 1)
  }
  
  # Kapcsolat típusa tanszékenként:
  # lehet pozitív, negatív vagy semleges/nem szignifikáns jellegű
  kapcsolat_tipusok <- c("pozitiv", "negativ", "semleges")
  kapcsolat1 <- sample(kapcsolat_tipusok, 1)
  kapcsolat2 <- sample(kapcsolat_tipusok, 1)
  
  general_Y <- function(X, kapcsolat) {
    
    n <- length(X)
    Xs <- as.numeric(scale(X))
    
    if (kapcsolat == "pozitiv") {
      Y <- 5 + runif(1, 0.8, 1.6) * Xs + rnorm(n, 0, runif(1, 0.5, 1.0))
      
    } else if (kapcsolat == "negativ") {
      Y <- 5 + runif(1, -1.6, -0.8) * Xs + rnorm(n, 0, runif(1, 0.5, 1.0))
      
    } else {
      Y <- rnorm(n, mean = 5, sd = 1.8)
    }
    
    Y <- round(Y, 1)
    Y <- pmin(pmax(Y, 1), 10)
    
    return(Y)
  }
  
  Y1 <- general_Y(X1, kapcsolat1)
  Y2 <- general_Y(X2, kapcsolat2)
  
  adat_korrelacio <- data.frame(
    Tanszek = c(rep(tanszek1, n1), rep(tanszek2, n2)),
    X = c(X1, X2),
    Y = c(Y1, Y2)
  )
  
  names(adat_korrelacio) <- c("Tanszek", x_nev, y_nev)
  
  # Ez nem kerül ki automatikusan a hallgatónak, mert nem adat_ előtagú.
  megoldas_korrelacio <- data.frame(
    Tanszek = c(tanszek1, tanszek2),
    Kapcsolat_tipus = c(kapcsolat1, kapcsolat2),
    Korrelacio = c(cor(X1, Y1), cor(X2, Y2)),
    P_ertek = c(cor.test(X1, Y1)$p.value, cor.test(X2, Y2)$p.value)
  )
  
  szoveg <- paste0(
    "# Korrelációs és regressziós feladat\n",
    "#\n",
    "# ", bevezeto_kor, "\n",
    "# A vizsgálat során két különböző tanszék hallgatóit vonták be az adatfelvételbe: ",
    tanszek1, " és ", tanszek2, ". A kutatók azt szerették volna megvizsgálni, hogy a(z) ",
    x_nev, " és a(z) ", y_nev, " között kimutatható-e statisztikai kapcsolat. ",
    "A két tanszék adatait külön-külön kell elemezni, majd az eredményeket röviden össze kell hasonlítani.\n",
    "#\n",
    "# Az adatok az adat_korrelacio nevű data.frame-ben találhatók.\n",
    "#\n",
    "# ", tanszek1, "\n",
    "# ", toupper(x_nev), ": ", paste(X1, collapse = ", "), "\n",
    "# ", toupper(y_nev), ": ", paste(Y1, collapse = ", "), "\n",
    "#\n",
    "# ", tanszek2, "\n",
    "# ", toupper(x_nev), ": ", paste(X2, collapse = ", "), "\n",
    "# ", toupper(y_nev), ": ", paste(Y2, collapse = ", "), "\n",
    "#\n",
    "# Feladatok:\n",
    "# - Olvassa le az adatokat. (1 pont)\n",
    "# - Állítsa fel a H0 és H1 hipotéziseket. (2 pont)\n",
    "# - Határozza meg a korrelációt mindkét tanszéken. (1 pont)\n",
    "# - Készítsen ábrát regressziós egyenessel. (2 pont)\n",
    "# - Vizsgálja a regressziós paraméterek megbízhatóságát. (2 pont)\n",
    "# - Ábrázolja mindkét változó tanszékenkénti értékeit közös boxplot ábrán. (1 pont)\n",
    "# - Az eredmények alapján melyik hipotézist fogadná el? Értelmezze röviden az eredményeket biológiai vagy viselkedésbiológiai szempontból. (1 pont)\n\n"
  )
  
  list(
    adat = adat_korrelacio,
    szoveg = szoveg,
    megoldas = megoldas_korrelacio
  )
}

# ============================================================
# KÖTELEZŐ FELADAT: ADATMÁTRIX, SZŰRÉS ÉS BOXPLOT
# Ezt később a korrelációs feladat mellé fix feladatként tesszük be.
# ============================================================

general_adatmatrix_feladat <- function(seed) {
  
  set.seed(seed + 202)
  
  adatmatrix_temak <- list(
    
    hallgatoi_eletmod = list(
      bevezetok = c(
        "Egy humánbiológiai kutatás során a hallgatók életmódbeli jellemzőit vizsgálták. A kutatók arra voltak kíváncsiak, hogy a különböző hallgatói csoportok között milyen eltérések figyelhetők meg az alvás, a képernyőhasználat és az aktivitás szempontjából. Az adatállomány célja, hogy gyakorlati példán keresztül lehessen vizsgálni a szűréseket, a csoportosítást és az ábrázolást.",
        "Egy egyetemi életmódfelmérés során a résztvevők napi rutinjára, aktivitására és pihenési szokásaira vonatkozó adatokat rögzítettek. A vizsgálat célja az volt, hogy feltárják, a különböző hallgatói csoportokban hogyan alakulnak az életmódbeli jellemzők.",
        "Egy oktatási célú humánbiológiai adatfelvétel során mesterséges adatállományt hoztak létre hallgatói életmódváltozókból. Az adatbázis alkalmas arra, hogy a hallgatók gyakorolják a kategóriás és folytonos változók kezelését, valamint a szűrt adatok grafikus megjelenítését."
      ),
      kat1_nev = "Csoport",
      kat1 = c("Elsőéves", "Másodéves", "Harmadéves", "Sportoló", "Nem sportoló"),
      kat2_nev = "Napi_aktivitas",
      kat2 = c("alacsony", "közepes", "magas", "nagyon magas"),
      kat3_nev = "Lakhatasi_tipus",
      kat3 = c("kollégium", "albérlet", "otthon lakik", "bejáró"),
      num1_nev = "Alvásidő",
      num1_atlag = 7.1,
      num1_szoras = 1.2,
      num2_nev = "Képernyőidő",
      num2_atlag = 5.4,
      num2_szoras = 1.8,
      num3_nev = "Napi_lépésszám",
      num3_atlag = 7600,
      num3_szoras = 2100
    ),
    
    tanulasi_terheles = list(
      bevezetok = c(
        "Egy egyetemi kutatás során azt vizsgálták, hogy a hallgatók tanulási terhelése hogyan függ össze a pihenéssel és a mindennapi aktivitással. A kutatók több hallgatói csoportot különítettek el, és olyan változókat rögzítettek, amelyek alkalmasak lehetnek szűrési és ábrázolási feladatok gyakorlására.",
        "Egy oktatási felmérésben a hallgatók tanulási szokásait és napi rutinját vizsgálták. A cél az volt, hogy kiderüljön, a különböző tanulmányi státuszú hallgatók között milyen eltérések figyelhetők meg a tanulási idő, pihenés és képernyőhasználat területén.",
        "Egy hallgatói teljesítményhez kapcsolódó adatgyűjtésben a tanulásra fordított idő, a pihenés és a napi aktivitás változóit modellezték. Az adatállomány lehetőséget ad arra, hogy a hallgatók több feltétel alapján szűrjenek, majd az eredményeket boxplot ábrákon jelenítsék meg."
      ),
      kat1_nev = "Évfolyam",
      kat1 = c("Elsőéves", "Másodéves", "Harmadéves", "MSc első év", "MSc második év"),
      kat2_nev = "Tanulási_terhelés",
      kat2 = c("alacsony", "közepes", "magas", "nagyon magas"),
      kat3_nev = "Munkarend",
      kat3 = c("nappali", "levelező", "részidős", "gyakorlaton lévő"),
      num1_nev = "Tanulási_idő",
      num1_atlag = 4.2,
      num1_szoras = 1.5,
      num2_nev = "Alvásidő",
      num2_atlag = 6.8,
      num2_szoras = 1.1,
      num3_nev = "Koncentrációs_pont",
      num3_atlag = 68,
      num3_szoras = 14
    ),
    
    sport_es_regeneracio = list(
      bevezetok = c(
        "Egy sportélettani jellegű hallgatói vizsgálatban a kutatók a fizikai aktivitás, a regeneráció és a terhelhetőség kapcsolatát vizsgálták. A résztvevőket több kategóriába sorolták, majd folytonos változóként különböző életmódbeli és teljesítményhez kapcsolódó értékeket rögzítettek.",
        "Egy rekreációs és humánbiológiai felmérés során a hallgatók sportolási szokásait, pihenését és napi aktivitását modellezték. A kutatók célja az volt, hogy megvizsgálják, a különböző aktivitási csoportokban milyen eltérések mutathatók ki.",
        "Egy egyetemi sportegészségügyi adatfelvételben a hallgatók terhelési és regenerációs mutatóit vizsgálták. Az adatállomány alkalmas arra, hogy különböző szűrési feltételek alapján részhalmazokat hozzanak létre, majd ezeket grafikus formában értelmezzék."
      ),
      kat1_nev = "Sporttipus",
      kat1 = c("állóképességi", "erősport", "labdajáték", "nem sportoló", "rekreációs sportoló"),
      kat2_nev = "Edzésgyakoriság",
      kat2 = c("ritka", "heti 1-2", "heti 3-4", "napi"),
      kat3_nev = "Regenerácio",
      kat3 = c("gyenge", "közepes", "jó", "kiváló"),
      num1_nev = "Alvásidő",
      num1_atlag = 7.3,
      num1_szoras = 1.0,
      num2_nev = "Heti_edzésidő",
      num2_atlag = 5.8,
      num2_szoras = 2.4,
      num3_nev = "Nyugalmi_pulzus",
      num3_atlag = 68,
      num3_szoras = 8
    ),
    
    mentalis_jolet = list(
      bevezetok = c(
        "Egy hallgatói mentális jóllétet vizsgáló kutatásban a résztvevők napi szokásait, terhelését és szubjektív közérzetét rögzítették. A kutatók célja az volt, hogy megvizsgálják, bizonyos csoportokban hogyan alakulnak a pihenéshez, stresszhez és teljesítményhez kapcsolódó értékek.",
        "Egy pszichológiai és humánbiológiai határterülethez kapcsolódó felmérés során a hallgatók stresszérzetét, hangulatát és életmódbeli tényezőit modellezték. Az adatállomány alkalmas arra, hogy a hallgatók gyakorolják a szűrést és a dobozdiagramok értelmezését.",
        "Egy egyetemi jóllétkutatás során a hallgatói populáció különböző alcsoportjait vizsgálták. A kutatók több kategóriás és folytonos változót használtak annak érdekében, hogy a csoportok közötti eltérések vizuálisan is értékelhetők legyenek."
      ),
      kat1_nev = "Hallgatói_csoport",
      kat1 = c("elsőéves", "felsőbb éves", "kollégista", "bejáró", "dolgozó hallgató"),
      kat2_nev = "Stressz_kategória",
      kat2 = c("alacsony", "közepes", "magas", "nagyon magas"),
      kat3_nev = "Szabadidő_tipus",
      kat3 = c("sport", "zene", "olvasás", "közösségi program", "digitális szabadidő"),
      num1_nev = "Hangulat_pont",
      num1_atlag = 6.4,
      num1_szoras = 1.6,
      num2_nev = "Alvasido",
      num2_atlag = 6.9,
      num2_szoras = 1.3,
      num3_nev = "Kepernyőidő",
      num3_atlag = 5.8,
      num3_szoras = 2.0
    )
  )
  
  tema_nev <- sample(names(adatmatrix_temak), 1)
  tema <- adatmatrix_temak[[tema_nev]]
  
  bevezeto <- sample(tema$bevezetok, 1)
  
  n <- sample(seq(4000, 8000, by = 200), 1)
  
  kat1 <- sample(tema$kat1, n, replace = TRUE)
  kat2 <- sample(tema$kat2, n, replace = TRUE)
  kat3 <- sample(tema$kat3, n, replace = TRUE)
  
  num1 <- round(rnorm(n, tema$num1_atlag, tema$num1_szoras), 2)
  num2 <- round(rnorm(n, tema$num2_atlag, tema$num2_szoras), 2)
  num3 <- round(rnorm(n, tema$num3_atlag, tema$num3_szoras), 2)
  
  num1[num1 < 0] <- 0
  num2[num2 < 0] <- 0
  num3[num3 < 0] <- 0
  
  adat <- data.frame(kat1, kat2, kat3, num1, num2, num3)
  names(adat) <- c(
    tema$kat1_nev,
    tema$kat2_nev,
    tema$kat3_nev,
    tema$num1_nev,
    tema$num2_nev,
    tema$num3_nev
  )
  
  # Random, de biztosan létező szűrési feltételek
  felt1_kat1 <- sample(tema$kat1, 1)
  felt1_kat2 <- sample(tema$kat2, 1)
  
  felt2_kat1 <- sample(tema$kat1, 1)
  felt2_kat2 <- sample(tema$kat2, 1)
  felt2_kat3 <- sample(tema$kat3, 1)
  
  num1_hatar <- round(quantile(adat[[tema$num1_nev]], probs = sample(c(0.25, 0.35, 0.40), 1)), 2)
  num2_hatar <- round(quantile(adat[[tema$num2_nev]], probs = sample(c(0.60, 0.65, 0.75), 1)), 2)
  
# ============================================================
# VÁLTOZÓK KIVÁLASZTÁSA LOGIKÁVAL
# ============================================================

kat_nevek <- c(tema$kat1_nev, tema$kat2_nev, tema$kat3_nev)
num_nevek <- c(tema$num1_nev, tema$num2_nev, tema$num3_nev)

# --- FŐ BOXPLOT (itt még bármi lehet)
boxplot_fo_num <- sample(num_nevek, 1)
boxplot_fo_kat <- sample(kat_nevek, 1)

# ============================================================
# 1. SZŰRÉS (kat1 és kat2 alapján történik)
# → ezért boxplot csak kat3 szerint lehet
# ============================================================

szures1_kat_nevek <- c(tema$kat1_nev, tema$kat2_nev)

boxplot_szurt1_kat <- setdiff(kat_nevek, szures1_kat_nevek)
boxplot_szurt1_kat <- sample(boxplot_szurt1_kat, 1)

boxplot_szurt1_num <- sample(num_nevek, 1)

# ============================================================
# 2. SZŰRÉS (itt NE szűrj mindhárom kategóriára!)
# → válassz ki 2 kategóriát a 3-ból
# ============================================================

szures2_kat_nevek <- sample(kat_nevek, 2, replace = FALSE)

boxplot_szurt2_kat <- setdiff(kat_nevek, szures2_kat_nevek)
boxplot_szurt2_kat <- sample(boxplot_szurt2_kat, 1)

# A konkrét szűrési értékek is ehhez igazodjanak
felt2_kat1_nev <- szures2_kat_nevek[1]
felt2_kat2_nev <- szures2_kat_nevek[2]

felt2_kat1_ertek <- sample(adat[[felt2_kat1_nev]], 1)
felt2_kat2_ertek <- sample(adat[[felt2_kat2_nev]], 1)

boxplot_szurt2_num <- sample(num_nevek, 1)
  
  szoveg <- paste0(
    "# Adatmátrix, szűrés és boxplot feladat\n",
    "#\n",
    "# ", bevezeto, "\n",
    "# Az adatállomány már előre létrehozásra került, az adat_adatmatrix nevű data.frame-ben található.\n",
    "# Az adatbázis ", n, " megfigyelést tartalmaz. A változók között három kategóriás és három folytonos változó szerepel.\n",
    "#\n",
    "# Kategóriás változók:\n",
    "# - ", tema$kat1_nev, ": ", paste(tema$kat1, collapse = ", "), "\n",
    "# - ", tema$kat2_nev, ": ", paste(tema$kat2, collapse = ", "), "\n",
    "# - ", tema$kat3_nev, ": ", paste(tema$kat3, collapse = ", "), "\n",
    "#\n",
    "# Folytonos változók:\n",
    "# - ", tema$num1_nev, ": normális eloszlás, átlag = ", tema$num1_atlag, ", szórás = ", tema$num1_szoras, "\n",
    "# - ", tema$num2_nev, ": normális eloszlás, átlag = ", tema$num2_atlag, ", szórás = ", tema$num2_szoras, "\n",
    "# - ", tema$num3_nev, ": normális eloszlás, átlag = ", tema$num3_atlag, ", szórás = ", tema$num3_szoras, "\n",
    "#\n",
    "# Feladatok:\n",
    "# - Készítsen boxplot ábrát a(z) ", boxplot_fo_num, " változóra a(z) ", boxplot_fo_kat, " kategóriái szerint. (2 pont)\n",
    "# - Szűrje ki az adatokat az alábbi feltételek szerint: ",
    tema$kat1_nev, " = ", felt1_kat1, ", ",
    tema$kat2_nev, " = ", felt1_kat2, ". ",
    "Az így kapott adatokból készítsen boxplot ábrát a(z) ", boxplot_szurt1_num, " változóról a(z) ", boxplot_szurt1_kat, " kategóriái szerint. (3 pont)\n",
    "# - Határozza meg a boxplot ábrán ábrázolt kategóriák gyakoriságát. (1 pont)\n",
    "# - Szűrje ki az adatokat az alábbi feltételek szerint: ",
    felt2_kat1_nev, " = ", felt2_kat1_ertek, ", ",
    felt2_kat2_nev, " = ", felt2_kat2_ertek, ", ",
    tema$num1_nev, " < ", num1_hatar, ", ",
    tema$num2_nev, " > ", num2_hatar, ". ",
    "Az így kapott adatokból készítsen boxplot ábrát a(z) ", boxplot_szurt2_num, " változóról a(z) ", boxplot_szurt2_kat, " kategóriái szerint. (3 pont)\n",
    "# - Határozza meg a boxplot ábrán ábrázolt kategóriák gyakoriságát. (1 pont)\n\n"
  )
  
  list(
    adat = adat,
    szoveg = szoveg
  )
}

# ============================================================
# TESZTVÁLASZTÁS FELADAT – EGYMINTÁS PRÓBÁK
# Két adatsor, eltérő mintaszám, normális eloszlás.
# A két adatsort külön-külön kell referenciaértékhez hasonlítani.
# ============================================================

general_egymintas_feladat <- function(seed) {
  
  set.seed(seed + 303)
  
  mu_tipus <- sample(c("azonos_mu", "kulon_mu"), 1)
  
  n1 <- sample(18:28, 1)
  n2 <- sample(29:40, 1)
  
  if (sample(c(TRUE, FALSE), 1)) {
    temp <- n1
    n1 <- n2
    n2 <- temp
  }
  
  # ------------------------------------------------------------
  # AZONOS MU TÉMÁK
  # ------------------------------------------------------------
  
  azonos_mu_temak <- list(
    
    nyugalmi_pulzus = list(
      csoport1 = "Sporttudomány szakos hallgatók",
      csoport2 = "Biológia szakos hallgatók",
      valtozo = "nyugalmi pulzus",
      egyseg = "ütés/perc",
      mu = sample(66:74, 1),
      sd = runif(1, 4, 7),
      bevezeto = "Egy élettani jellegű egyetemi vizsgálat során két hallgatói csoport nyugalmi pulzusát mérték. A kutatók arra voltak kíváncsiak, hogy a két csoport pulzusértékei külön-külön megfelelnek-e a szakirodalomban megadott, egészséges fiatal felnőttekre jellemző átlagos értéknek. A két csoportból eltérő számú hallgatót vontak be a mérésbe."
    ),
    
    alvasido = list(
      csoport1 = "Kollégista hallgatók",
      csoport2 = "Bejáró hallgatók",
      valtozo = "napi alvásidő",
      egyseg = "óra",
      mu = round(runif(1, 6.8, 7.5), 1),
      sd = runif(1, 0.7, 1.2),
      bevezeto = "Egy hallgatói életmódvizsgálat során két különböző lakhatási helyzetű csoport napi alvásidejét rögzítették. A kutatók azt szerették volna megvizsgálni, hogy az egyes csoportok átlagos alvásideje megfelel-e az ajánlott napi értéknek. A mérések önbevalláson alapultak, de az adatokat oktatási célú statisztikai elemzésre használták fel."
    ),
    
    reakcioido = list(
      csoport1 = "Reggeli mérésen résztvevő hallgatók",
      csoport2 = "Délutáni mérésen résztvevő hallgatók",
      valtozo = "reakcióidő",
      egyseg = "ms",
      mu = sample(seq(240, 280, by = 5), 1),
      sd = runif(1, 18, 35),
      bevezeto = "Egy kognitív teljesítményt vizsgáló gyakorlaton hallgatók reakcióidejét mérték egyszerű vizuális ingerre. A kutatók két külön mérési időpontban gyűjtöttek adatokat, majd azt vizsgálták, hogy az egyes minták átlagos reakcióideje eltér-e a szakirodalmi referenciaértéktől."
    ),
    
    lepesszam = list(
      csoport1 = "Elsőéves hallgatók",
      csoport2 = "Felsőbb éves hallgatók",
      valtozo = "napi lépésszám",
      egyseg = "lépés",
      mu = sample(c(7000, 8000, 9000, 10000), 1),
      sd = runif(1, 900, 1800),
      bevezeto = "Egy fizikai aktivitással kapcsolatos felmérés során két hallgatói csoport napi lépésszámát modellezték. A cél annak megállapítása volt, hogy az egyes csoportok átlagos napi aktivitása eléri-e az ajánlott, szakirodalomban gyakran használt referenciaértéket."
    ),
    
    testhomerseklet = list(
      csoport1 = "Délelőtti mérésen résztvevők",
      csoport2 = "Délutáni mérésen résztvevők",
      valtozo = "testhőmérséklet",
      egyseg = "°C",
      mu = round(runif(1, 36.5, 36.9), 1),
      sd = runif(1, 0.15, 0.35),
      bevezeto = "Egy alapvető élettani gyakorlat során két hallgatói mintában testhőmérsékletet mértek. A kutatók arra voltak kíváncsiak, hogy az egyes minták átlagos testhőmérséklete eltér-e a normál élettani értékként használt referenciaátlagtól."
    ),
    
    vercukor = list(
      csoport1 = "Reggeli előtt mért hallgatók",
      csoport2 = "Késő délelőtt mért hallgatók",
      valtozo = "éhomi vércukorszint",
      egyseg = "mmol/L",
      mu = round(runif(1, 4.8, 5.5), 1),
      sd = runif(1, 0.25, 0.55),
      bevezeto = "Egy egészségügyi jellegű oktatási adatfelvételben két hallgatói csoport vércukorszintjét modellezték. A cél annak vizsgálata volt, hogy a minták átlagos értékei megfelelnek-e a normál tartomány középértékeként megadott referenciaértéknek."
    ),
    
    pocok_testtomeg = list(
      csoport1 = "A pocokpopuláció",
      csoport2 = "B pocokpopuláció",
      valtozo = "testtömeg",
      egyseg = "g",
      mu = round(runif(1, 18, 28), 1),
      sd = runif(1, 1.5, 3.2),
      bevezeto = "Egy zoológiai terepi vizsgálat során két pocokpopuláció egyedeinek testtömegét mérték. A kutatók arra voltak kíváncsiak, hogy a két populációból származó minták külön-külön megfelelnek-e a fajra vonatkozó szakirodalmi átlagos testtömegnek."
    )
  )
  
  # ------------------------------------------------------------
  # KÜLÖN MU TÉMÁK
  # ------------------------------------------------------------
  
  kulon_mu_temak <- list(
    
    testmagassag = list(
      csoport1 = "Fiúk",
      csoport2 = "Lányok",
      valtozo = "testmagasság",
      egyseg = "cm",
      mu1 = sample(172:180, 1),
      mu2 = sample(160:168, 1),
      sd1 = runif(1, 5, 8),
      sd2 = runif(1, 5, 8),
      bevezeto = "Egy auxológiai vizsgálat során fiúk és lányok testmagasságát mérték. A kutatók arra voltak kíváncsiak, hogy a vizsgált minták átlagos testmagassága megfelel-e az adott nemre és életkori csoportra vonatkozó referenciaértékeknek. A két csoportból eltérő elemszámú mintát vettek."
    ),
    
    testtomeg = list(
      csoport1 = "Férfi hallgatók",
      csoport2 = "Női hallgatók",
      valtozo = "testtömeg",
      egyseg = "kg",
      mu1 = sample(72:82, 1),
      mu2 = sample(58:68, 1),
      sd1 = runif(1, 6, 10),
      sd2 = runif(1, 5, 9),
      bevezeto = "Egy humánbiológiai mérési gyakorlat során férfi és női hallgatók testtömegét vizsgálták. A cél annak megállapítása volt, hogy a két minta külön-külön megfelel-e a nemekre jellemző átlagos referenciaértékeknek."
    ),
    
    kezszorito_ero = list(
      csoport1 = "Férfi hallgatók",
      csoport2 = "Női hallgatók",
      valtozo = "kézszorító erő",
      egyseg = "kg",
      mu1 = sample(38:48, 1),
      mu2 = sample(24:34, 1),
      sd1 = runif(1, 4, 7),
      sd2 = runif(1, 3, 6),
      bevezeto = "Egy sportélettani gyakorlat során férfi és női hallgatók kézszorító erejét mérték. A kutatók azt vizsgálták, hogy a mért értékek külön-külön megfelelnek-e a nemekre jellemző szakirodalmi referenciaértékeknek."
    ),
    
    vo2max = list(
      csoport1 = "Sportoló hallgatók",
      csoport2 = "Nem sportoló hallgatók",
      valtozo = "becsült VO2 max",
      egyseg = "ml/kg/min",
      mu1 = sample(48:58, 1),
      mu2 = sample(32:42, 1),
      sd1 = runif(1, 4, 7),
      sd2 = runif(1, 4, 7),
      bevezeto = "Egy sportélettani vizsgálat során sportoló és nem sportoló hallgatók becsült aerob kapacitását vizsgálták. A kutatók arra voltak kíváncsiak, hogy az egyes csoportok átlagos értékei megfelelnek-e a csoportokra jellemző referenciaátlagoknak."
    ),
    
    pulzus_csoport = list(
      csoport1 = "Sportoló hallgatók",
      csoport2 = "Nem sportoló hallgatók",
      valtozo = "nyugalmi pulzus",
      egyseg = "ütés/perc",
      mu1 = sample(55:64, 1),
      mu2 = sample(68:76, 1),
      sd1 = runif(1, 4, 7),
      sd2 = runif(1, 5, 8),
      bevezeto = "Egy élettani felmérésben sportoló és nem sportoló hallgatók nyugalmi pulzusát rögzítették. A vizsgálat célja annak eldöntése volt, hogy a két csoport külön-külön megfelel-e a saját csoportjára jellemző elméleti átlagértéknek."
    ),
    
    testzsir = list(
      csoport1 = "Férfi hallgatók",
      csoport2 = "Női hallgatók",
      valtozo = "testzsír százalék",
      egyseg = "%",
      mu1 = sample(14:20, 1),
      mu2 = sample(22:30, 1),
      sd1 = runif(1, 3, 5),
      sd2 = runif(1, 3, 6),
      bevezeto = "Egy testösszetételt vizsgáló humánbiológiai gyakorlat során férfi és női hallgatók testzsír százalékát modellezték. A cél annak megállapítása volt, hogy a minták átlagai megfelelnek-e az adott csoportra jellemző referenciaértékeknek."
    ),
    
    reakcioido_csoport = list(
      csoport1 = "Rendszeresen sportoló hallgatók",
      csoport2 = "Keveset mozgó hallgatók",
      valtozo = "reakcióidő",
      egyseg = "ms",
      mu1 = sample(seq(220, 250, by = 5), 1),
      mu2 = sample(seq(250, 290, by = 5), 1),
      sd1 = runif(1, 15, 28),
      sd2 = runif(1, 18, 35),
      bevezeto = "Egy kognitív és életmódbeli vizsgálat során két eltérő aktivitású hallgatói csoport reakcióidejét mérték. A kutatók azt vizsgálták, hogy az egyes csoportok átlagos reakcióideje megfelel-e a saját csoportra meghatározott referenciaértéknek."
    )
  )
  
  # ------------------------------------------------------------
  # TÉMA KIVÁLASZTÁSA ÉS ADATGENERÁLÁS
  # ------------------------------------------------------------
  
  if (mu_tipus == "azonos_mu") {
    
    tema <- sample(azonos_mu_temak, 1)[[1]]
    
    mu1 <- tema$mu
    mu2 <- tema$mu
    
    elteres1 <- sample(c(-1, 1), 1) * runif(1, 0.15, 0.65)
    elteres2 <- sample(c(-1, 1), 1) * runif(1, 0.15, 0.65)
    
    atlag1 <- mu1 + elteres1 * tema$sd
    atlag2 <- mu2 + elteres2 * tema$sd
    
    A <- round(rnorm(n1, mean = atlag1, sd = tema$sd), 1)
    B <- round(rnorm(n2, mean = atlag2, sd = tema$sd), 1)
    
    csoport1 <- tema$csoport1
    csoport2 <- tema$csoport2
    valtozo <- tema$valtozo
    egyseg <- tema$egyseg
    
    bevezeto <- tema$bevezeto
    
    referencia_szoveg <- paste0(
      "# Mindkét csoport esetében ugyanaz a referenciaérték használandó.\n",
      "# Referenciaérték: ", mu1, " ", egyseg, "\n"
    )
    
  } else {
    
    tema <- sample(kulon_mu_temak, 1)[[1]]
    
    mu1 <- tema$mu1
    mu2 <- tema$mu2
    
    elteres1 <- sample(c(-1, 1), 1) * runif(1, 0.15, 0.65)
    elteres2 <- sample(c(-1, 1), 1) * runif(1, 0.15, 0.65)
    
    atlag1 <- mu1 + elteres1 * tema$sd1
    atlag2 <- mu2 + elteres2 * tema$sd2
    
    A <- round(rnorm(n1, mean = atlag1, sd = tema$sd1), 1)
    B <- round(rnorm(n2, mean = atlag2, sd = tema$sd2), 1)
    
    csoport1 <- tema$csoport1
    csoport2 <- tema$csoport2
    valtozo <- tema$valtozo
    egyseg <- tema$egyseg
    
    bevezeto <- tema$bevezeto
    
    referencia_szoveg <- paste0(
      "# A két csoporthoz eltérő referenciaérték tartozik.\n",
      "# ", csoport1, " referenciaértéke: ", mu1, " ", egyseg, "\n",
      "# ", csoport2, " referenciaértéke: ", mu2, " ", egyseg, "\n"
    )
  }
  
  adat_teszt <- data.frame(
    Csoport = c(rep(csoport1, n1), rep(csoport2, n2)),
    Ertek = c(A, B)
  )
  
  # ------------------------------------------------------------
  # SZÖVEG
  # ------------------------------------------------------------
  
  szoveg <- paste0(
    "# Statisztikai próba választása referenciaérték alapján\n",
    "#\n",
    "# ", bevezeto, "\n",
    "# A vizsgált változó: ", valtozo, " (", egyseg, ").\n",
    "# Az adatok az adat_teszt nevű data.frame-ben találhatók.\n",
    "# A két minta elemszáma eltérő: ", csoport1, " n = ", n1, ", ", csoport2, " n = ", n2, ".\n",
    "#\n",
    referencia_szoveg,
    "#\n",
    "# ", csoport1, ":\n",
    "# ", paste(A, collapse = ", "), "\n",
    "#\n",
    "# ", csoport2, ":\n",
    "# ", paste(B, collapse = ", "), "\n",
    "#\n",
    "# Feladatok:\n",
    "# - Ábrázolja a két adatsort közös boxplot ábrán. (1 pont)\n",
    "# - Vizsgálja meg mindkét adatsor normalitását. (2 pont)\n",
    "# - Fogalmazza meg a megfelelő H0 és H1 hipotéziseket mindkét mintára. (2 pont)\n",
    "# - Válassza ki és végezze el a megfelelő statisztikai próbát mindkét adatsorra. (3 pont)\n",
    "# - Hasonlítsa össze, melyik csoport tér el jobban a saját referenciaértékétől. (1 pont)\n",
    "# - Értelmezze az eredményeket biológiai szempontból. (1 pont)\n\n"
  )
  
  megoldas <- data.frame(
    Csoport = c(csoport1, csoport2),
    Mu = c(mu1, mu2),
    Atlag = c(mean(A), mean(B)),
    Shapiro_p = c(shapiro.test(A)$p.value, shapiro.test(B)$p.value),
    T_p = c(t.test(A, mu = mu1)$p.value, t.test(B, mu = mu2)$p.value)
  )
  
  list(
    adat = adat_teszt,
    szoveg = szoveg,
    tipus = "egymintas_referencia",
    mu_tipus = mu_tipus,
    megoldas = megoldas
  )
}

# ============================================================
# TESZTVÁLASZTÁS FELADAT – KÉTMINTÁS PRÓBA / MANN–WHITNEY–WILCOXON
# 2×2 adatsor:
# K1 csoport: A és B minta, azonos elemszámmal
# K2 csoport: A és B minta, azonos elemszámmal
# K1 és K2 elemszáma eltérő
# A és B minták függetlenek, NEM párosítottak
# ============================================================

general_ketmintas_feladat <- function(seed) {
  
  set.seed(seed + 404)
  
  # ------------------------------------------------------------
  # Témák
  # ------------------------------------------------------------
  
  ketmintas_temak <- list(
    
    pulzus = list(
      csoport1 = "Sporttudomány szakos hallgatók",
      csoport2 = "Biológia szakos hallgatók",
      mintaA = "délelőtti mérés",
      mintaB = "délutáni mérés",
      valtozo = "nyugalmi pulzus",
      egyseg = "ütés/perc",
      minv = 48,
      maxv = 95,
      alap_atlag = 68,
      alap_sd = 7,
      bevezeto = paste0(
        "Egy élettani jellegű egyetemi vizsgálat során hallgatók nyugalmi pulzusát mérték. ",
        "A kutatók arra voltak kíváncsiak, hogy két különböző hallgatói csoportban eltérnek-e egymástól ",
        "a különböző időpontokban, de egymástól független személyeken mért pulzusértékek. ",
        "A mérések során az A és B minták nem ugyanazoktól a hallgatóktól származnak, ezért az összehasonlításokat ",
        "független mintákra vonatkozó próbával kell elvégezni."
      )
    ),
    
    reakcioido = list(
      csoport1 = "Kipihent hallgatók",
      csoport2 = "Alváshiányos hallgatók",
      mintaA = "egyszerű vizuális inger",
      mintaB = "összetett vizuális inger",
      valtozo = "reakcióidő",
      egyseg = "ms",
      minv = 180,
      maxv = 420,
      alap_atlag = 270,
      alap_sd = 35,
      bevezeto = paste0(
        "Egy kognitív teljesítményt vizsgáló kutatásban hallgatók reakcióidejét mérték. ",
        "A vizsgálat célja annak megállapítása volt, hogy az egyszerűbb és összetettebb ingerhelyzetekhez tartozó ",
        "reakcióidők között kimutatható-e eltérés két különböző hallgatói csoportban. ",
        "Az A és B minták független részmintákból származnak, tehát nem ugyanazon személyek ismételt mérései."
      )
    ),
    
    alvasido = list(
      csoport1 = "Kollégista hallgatók",
      csoport2 = "Bejáró hallgatók",
      mintaA = "vizsgaidőszakon kívüli hét",
      mintaB = "vizsgaidőszak hete",
      valtozo = "napi alvásidő",
      egyseg = "óra",
      minv = 3.5,
      maxv = 10,
      alap_atlag = 7,
      alap_sd = 1.1,
      bevezeto = paste0(
        "Egy hallgatói életmódfelmérésben a napi alvásidő alakulását vizsgálták. ",
        "A kutatók két különböző hallgatói csoportban hasonlították össze a vizsgaidőszakon kívüli és a vizsgaidőszakban ",
        "gyűjtött, egymástól független mintákat. ",
        "A cél annak eldöntése volt, hogy az A és B minták eloszlása, illetve középértéke eltér-e egymástól."
      )
    ),
    
    testzsir = list(
      csoport1 = "Rendszeresen sportoló hallgatók",
      csoport2 = "Keveset mozgó hallgatók",
      mintaA = "A mérőcsoport",
      mintaB = "B mérőcsoport",
      valtozo = "testzsír százalék",
      egyseg = "%",
      minv = 8,
      maxv = 38,
      alap_atlag = 22,
      alap_sd = 5,
      bevezeto = paste0(
        "Egy testösszetétellel kapcsolatos humánbiológiai gyakorlat során hallgatók testzsír százalékát becsülték. ",
        "A vizsgálatban két hallgatói csoport szerepelt, és mindkét csoportban két egymástól független mintát hasonlítottak össze. ",
        "A kutatók azt szerették volna eldönteni, hogy az A és B minták alapján kimutatható-e statisztikailag igazolható eltérés."
      )
    ),
    
    kezszorito = list(
      csoport1 = "Férfi hallgatók",
      csoport2 = "Női hallgatók",
      mintaA = "domináns kézzel mért minta",
      mintaB = "nem domináns kézzel mért minta",
      valtozo = "kézszorító erő",
      egyseg = "kg",
      minv = 12,
      maxv = 65,
      alap_atlag = 36,
      alap_sd = 8,
      bevezeto = paste0(
        "Egy sportélettani mérés során hallgatók kézszorító erejét vizsgálták. ",
        "A kutatók két csoportban elemezték, hogy az A és B független minták között kimutatható-e eltérés. ",
        "A mérések nem ugyanazon személyek két kezéről származnak, hanem külön részmintákból, ezért nem különbségen alapuló próbát kell alkalmazni."
      )
    ),
    
    lepesszam = list(
      csoport1 = "Elsőéves hallgatók",
      csoport2 = "Felsőbb éves hallgatók",
      mintaA = "hétköznapi napokon mért minta",
      mintaB = "hétvégi napokon mért minta",
      valtozo = "napi lépésszám",
      egyseg = "lépés",
      minv = 1000,
      maxv = 18000,
      alap_atlag = 8000,
      alap_sd = 2200,
      bevezeto = paste0(
        "Egy fizikai aktivitással kapcsolatos felmérésben a hallgatók napi lépésszámát vizsgálták. ",
        "A kutatók két hallgatói csoportban hasonlították össze a hétköznapi és hétvégi napokon mért, ",
        "egymástól független mintákat. ",
        "A cél annak eldöntése volt, hogy az A és B minták között statisztikailag kimutatható-e eltérés."
      )
    ),
    
    nyugdijas_aktivitas = list(
      csoport1 = "városi nyugdíjasotthon lakói",
      csoport2 = "kistelepülési nyugdíjasotthon lakói",
      mintaA = "délelőtti aktivitási csoport",
      mintaB = "délutáni aktivitási csoport",
      valtozo = "napi séta időtartama",
      egyseg = "perc",
      minv = 0,
      maxv = 140,
      alap_atlag = 55,
      alap_sd = 22,
      bevezeto = paste0(
        "Egy idősödéssel kapcsolatos humánbiológiai és életmódbeli felmérés során nyugdíjasotthonok lakóinak napi sétával töltött idejét vizsgálták. ",
        "A kutatók két intézménytípusban hasonlították össze az A és B független mintákat. ",
        "Az adatok alapján el kell dönteni, hogy a minták közötti eltérés vizsgálható-e kétmintás t-próbával, vagy nemparaméteres próba szükséges."
      )
    )
  )
  
  tema <- sample(ketmintas_temak, 1)[[1]]
  
  # ------------------------------------------------------------
  # Elemszámok
  # K1-en belül A és B azonos n
  # K2-n belül A és B azonos n
  # K1 és K2 eltérő n
  # ------------------------------------------------------------
  
  n_K1 <- sample(18:28, 1)
  n_K2 <- sample(30:42, 1)
  
  if (sample(c(TRUE, FALSE), 1)) {
    temp <- n_K1
    n_K1 <- n_K2
    n_K2 <- temp
  }
  
  # ------------------------------------------------------------
  # Normalitási helyzet
  # Lehet:
  # - mindkét csoport normális: kétmintás t-próba mindkettőre
  # - K1 nem normális: K1 MWW, K2 t-próba
  # - K2 nem normális: K1 t-próba, K2 MWW
  # - mindkettő nem normális: MWW mindkettőre
  # ------------------------------------------------------------
  
  normalitas_tipus <- sample(c(
    "mind_normalis",
    "K1_nemnormalis",
    "K2_nemnormalis",
    "mind_nemnormalis"
  ), 1)
  
  K1_normalis <- normalitas_tipus %in% c("mind_normalis", "K2_nemnormalis")
  K2_normalis <- normalitas_tipus %in% c("mind_normalis", "K1_nemnormalis")
  
  # ------------------------------------------------------------
  # Adatgeneráló segédfüggvények
  # ------------------------------------------------------------
  
  gen_normalis <- function(n, atlag, sd, minv, maxv) {
    x <- rnorm(n, mean = atlag, sd = sd)
    x[x < minv] <- minv
    x[x > maxv] <- maxv
    round(x, 1)
  }
  
  gen_nemnormalis <- function(n, atlag, sd, minv, maxv) {
    # jobbra ferde, outlier-jellegű eloszlás
    x <- rgamma(n, shape = 2.2, scale = sd / 1.4)
    x <- x - mean(x) + atlag
    
    # néhány szélső érték
    out_n <- max(1, round(n * 0.10))
    idx <- sample(seq_len(n), out_n)
    x[idx] <- x[idx] + runif(out_n, 1.8 * sd, 3.2 * sd)
    
    x[x < minv] <- minv
    x[x > maxv] <- maxv
    round(x, 1)
  }
  
  gen_par <- function(n, normalis, alap_atlag, alap_sd, minv, maxv) {
    
    kulonbseg <- sample(c(-0.7, -0.4, 0, 0.4, 0.7), 1) * alap_sd
    
    atlag_A <- alap_atlag + runif(1, -0.4 * alap_sd, 0.4 * alap_sd)
    atlag_B <- atlag_A + kulonbseg
    
    if (normalis) {
      A <- gen_normalis(n, atlag_A, alap_sd, minv, maxv)
      B <- gen_normalis(n, atlag_B, alap_sd, minv, maxv)
    } else {
      A <- gen_nemnormalis(n, atlag_A, alap_sd, minv, maxv)
      B <- gen_nemnormalis(n, atlag_B, alap_sd, minv, maxv)
    }
    
    list(A = A, B = B)
  }
  
  K1 <- gen_par(
    n = n_K1,
    normalis = K1_normalis,
    alap_atlag = tema$alap_atlag,
    alap_sd = tema$alap_sd,
    minv = tema$minv,
    maxv = tema$maxv
  )
  
  K2 <- gen_par(
    n = n_K2,
    normalis = K2_normalis,
    alap_atlag = tema$alap_atlag + runif(1, -0.3 * tema$alap_sd, 0.3 * tema$alap_sd),
    alap_sd = tema$alap_sd,
    minv = tema$minv,
    maxv = tema$maxv
  )
  
  K1_A <- K1$A
  K1_B <- K1$B
  K2_A <- K2$A
  K2_B <- K2$B
  
  # ------------------------------------------------------------
  # Data.frame
  # ------------------------------------------------------------
  
  adat_teszt <- data.frame(
    Csoport = c(
      rep("K1", length(K1_A) + length(K1_B)),
      rep("K2", length(K2_A) + length(K2_B))
    ),
    Minta = c(
      rep("A", length(K1_A)),
      rep("B", length(K1_B)),
      rep("A", length(K2_A)),
      rep("B", length(K2_B))
    ),
    Ertek = c(K1_A, K1_B, K2_A, K2_B)
  )
  
  # ------------------------------------------------------------
  # Feladatszöveg
  # ------------------------------------------------------------
  
  szoveg <- paste0(
    "# Statisztikai próba választása két független minta összehasonlításához\n",
    "#\n",
    "# ", tema$bevezeto, "\n",
    "# A vizsgált változó: ", tema$valtozo, " (", tema$egyseg, ").\n",
    "# Az adatok az adat_ketmintas nevű data.frame-ben találhatók.\n",
    "#\n",
    "# Fontos: az A és B minták egymástól függetlenek. Nem ugyanazon személyek ismételt mérései.\n",
    "# Ezért különbségen alapuló vagy párosított próbát itt nem szabad alkalmazni.\n",
    "#\n",
    "# A K1 csoportban az A és B minta elemszáma azonos: n = ", n_K1, ".\n",
    "# A K2 csoportban az A és B minta elemszáma azonos: n = ", n_K2, ".\n",
    "# A K1 és K2 csoport elemszáma eltérő.\n",
    "#\n",
    "# Csoportok:\n",
    "# K1: ", tema$csoport1, "\n",
    "# K2: ", tema$csoport2, "\n",
    "#\n",
    "# Minták jelentése:\n",
    "# A minta: ", tema$mintaA, "\n",
    "# B minta: ", tema$mintaB, "\n",
    "#\n",
    "# K1 csoport\n",
    "# A minta: ", paste(K1_A, collapse = ", "), "\n",
    "# B minta: ", paste(K1_B, collapse = ", "), "\n",
    "#\n",
    "# K2 csoport\n",
    "# A minta: ", paste(K2_A, collapse = ", "), "\n",
    "# B minta: ", paste(K2_B, collapse = ", "), "\n",
    "#\n",
    "# Feladatok:\n",
    "# - Olvassa le az adatokat, és ellenőrizze a csoportok, minták elemszámát. (1 pont)\n",
    "# - Vizsgálja meg mind a négy adatsor normalitását. (2 pont)\n",
    "# - Fogalmazza meg a megfelelő null- és alternatív hipotéziseket a K1 és K2 csoport összehasonlításához. (2 pont)\n",
    "# - Hasonlítsa össze a K1 csoportban az A és B mintát a megfelelő statisztikai próbával. Indokolja a próba választását. (2 pont)\n",
    "# - Hasonlítsa össze a K2 csoportban az A és B mintát a megfelelő statisztikai próbával. Indokolja a próba választását. (2 pont)\n",
    "# - Ábrázolja mind a négy adatsort egy közös boxplot ábrán, és röviden értelmezze az eredményeket. (1 pont)\n\n"
  )
  
  # ------------------------------------------------------------
  # Tanári megoldási segédlet
  # Nem kerül ki automatikusan a hallgatónak, mert nem adat_ előtagú
  # ------------------------------------------------------------
  
  megoldas <- data.frame(
    Csoport = c("K1", "K2"),
    Normalitas_generalt = c(K1_normalis, K2_normalis),
    Javasolt_proba = c(
      ifelse(K1_normalis, "kétmintás t-próba", "Mann–Whitney–Wilcoxon próba"),
      ifelse(K2_normalis, "kétmintás t-próba", "Mann–Whitney–Wilcoxon próba")
    ),
    Shapiro_A_p = c(shapiro.test(K1_A)$p.value, shapiro.test(K2_A)$p.value),
    Shapiro_B_p = c(shapiro.test(K1_B)$p.value, shapiro.test(K2_B)$p.value),
    T_teszt_p = c(t.test(K1_A, K1_B)$p.value, t.test(K2_A, K2_B)$p.value),
    MWW_p = c(wilcox.test(K1_A, K1_B)$p.value, wilcox.test(K2_A, K2_B)$p.value)
  )
  
  list(
    adat = adat_teszt,
    szoveg = szoveg,
    tipus = "ketmintas_vagy_mww",
    normalitas_tipus = normalitas_tipus,
    megoldas = megoldas
  )
}

# ============================================================
# FELADATLISTA ÖSSZEÁLLÍTÁSA
# ============================================================

# Két kötelező feladat
fix_feladatok <- c("korrelacio", "adatmatrix")

# Nem fix / választható feladatok
# Most még csak az egymintás referenciaértékes feladat van benne,
# ezért ezt fogja kiválasztani.
random_feladatok <- c("egymintas","ketmintas")

# Jelenleg 1 random feladatot választunk.
# Később, ha több választható feladat lesz, csak ide kell őket beírni.
valasztott_random <- sample(random_feladatok, 1, replace = FALSE)

# Végső feladatlista
vegso_feladatok <- c(fix_feladatok, valasztott_random)

# A feladatok sorrendjét összekeverjük
vegso_feladatok <- sample(vegso_feladatok, length(vegso_feladatok), replace = FALSE)

# ============================================================
# FELADATOK GENERÁLÁSA
# ============================================================

feladat_sorszam <- 1

for (feladat in vegso_feladatok) {
  
  if (feladat == "korrelacio") {
    
    kor <- general_korrelacio_feladat(seed + feladat_sorszam)
    adat_korrelacio <- kor$adat
    
    feladat_szoveg <- paste0(
      feladat_szoveg,
      "# ", feladat_sorszam, ". feladat\n",
      kor$szoveg
    )
    
    megoldas_korrelacio <- kor$megoldas
  }
  
  if (feladat == "adatmatrix") {
    
    adatmatrix <- general_adatmatrix_feladat(seed + feladat_sorszam)
    adat_adatmatrix <- adatmatrix$adat
    
    feladat_szoveg <- paste0(
      feladat_szoveg,
      "# ", feladat_sorszam, ". feladat\n",
      adatmatrix$szoveg
    )
  }
  
  if (feladat == "egymintas") {
    
    egymintas <- general_egymintas_feladat(seed + feladat_sorszam)
    adat_teszt <- egymintas$adat
    
    feladat_szoveg <- paste0(
      feladat_szoveg,
      "# ", feladat_sorszam, ". feladat\n",
      egymintas$szoveg
    )
    
    megoldas_egymintas <- egymintas$megoldas
  }
  
  if (feladat == "ketmintas") {
    
    ketmintas <- general_ketmintas_feladat(seed + feladat_sorszam)
    adat_teszt <- ketmintas$adat
    
    feladat_szoveg <- paste0(
      feladat_szoveg,
      "# ", feladat_sorszam, ". feladat\n",
      ketmintas$szoveg
    )
    
    megoldas_ketmintas <- ketmintas$megoldas
  }
  
  feladat_sorszam <- feladat_sorszam + 1
}
