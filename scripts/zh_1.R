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
      num1_nev = "Alvasido",
      num1_atlag = 7.1,
      num1_szoras = 1.2,
      num2_nev = "Kepernyoido",
      num2_atlag = 5.4,
      num2_szoras = 1.8,
      num3_nev = "Napi_lepesszam",
      num3_atlag = 7600,
      num3_szoras = 2100
    ),
    
    tanulasi_terheles = list(
      bevezetok = c(
        "Egy egyetemi kutatás során azt vizsgálták, hogy a hallgatók tanulási terhelése hogyan függ össze a pihenéssel és a mindennapi aktivitással. A kutatók több hallgatói csoportot különítettek el, és olyan változókat rögzítettek, amelyek alkalmasak lehetnek szűrési és ábrázolási feladatok gyakorlására.",
        "Egy oktatási felmérésben a hallgatók tanulási szokásait és napi rutinját vizsgálták. A cél az volt, hogy kiderüljön, a különböző tanulmányi státuszú hallgatók között milyen eltérések figyelhetők meg a tanulási idő, pihenés és képernyőhasználat területén.",
        "Egy hallgatói teljesítményhez kapcsolódó adatgyűjtésben a tanulásra fordított idő, a pihenés és a napi aktivitás változóit modellezték. Az adatállomány lehetőséget ad arra, hogy a hallgatók több feltétel alapján szűrjenek, majd az eredményeket boxplot ábrákon jelenítsék meg."
      ),
      kat1_nev = "Evfolyam",
      kat1 = c("Elsőéves", "Másodéves", "Harmadéves", "MSc első év", "MSc második év"),
      kat2_nev = "Tanulasi_terheles",
      kat2 = c("alacsony", "közepes", "magas", "nagyon magas"),
      kat3_nev = "Munkarend",
      kat3 = c("nappali", "levelező", "részidős", "gyakorlaton lévő"),
      num1_nev = "Tanulasi_ido",
      num1_atlag = 4.2,
      num1_szoras = 1.5,
      num2_nev = "Alvasido",
      num2_atlag = 6.8,
      num2_szoras = 1.1,
      num3_nev = "Koncentracios_pont",
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
      kat2_nev = "Edzesgyakorisag",
      kat2 = c("ritka", "heti 1-2", "heti 3-4", "napi"),
      kat3_nev = "Regeneracio",
      kat3 = c("gyenge", "közepes", "jó", "kiváló"),
      num1_nev = "Alvasido",
      num1_atlag = 7.3,
      num1_szoras = 1.0,
      num2_nev = "Heti_edzesido",
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
      kat1_nev = "Hallgatoi_csoport",
      kat1 = c("elsőéves", "felsőbb éves", "kollégista", "bejáró", "dolgozó hallgató"),
      kat2_nev = "Stressz_kategoria",
      kat2 = c("alacsony", "közepes", "magas", "nagyon magas"),
      kat3_nev = "Szabadido_tipus",
      kat3 = c("sport", "zene", "olvasás", "közösségi program", "digitális szabadidő"),
      num1_nev = "Hangulat_pont",
      num1_atlag = 6.4,
      num1_szoras = 1.6,
      num2_nev = "Alvasido",
      num2_atlag = 6.9,
      num2_szoras = 1.3,
      num3_nev = "Kepernyoido",
      num3_atlag = 5.8,
      num3_szoras = 2.0
    )
  )
  
  tema_nev <- sample(names(adatmatrix_temak), 1)
  tema <- adatmatrix_temak[[tema_nev]]
  
  bevezeto <- sample(tema$bevezetok, 1)
  
  n <- sample(seq(1600, 2600, by = 100), 1)
  
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
  
  # Ábrázolandó változók random kiválasztása
  boxplot_fo_num <- sample(c(tema$num1_nev, tema$num2_nev, tema$num3_nev), 1)
  boxplot_fo_kat <- sample(c(tema$kat1_nev, tema$kat2_nev, tema$kat3_nev), 1)
  
  boxplot_szurt1_num <- sample(c(tema$num1_nev, tema$num2_nev, tema$num3_nev), 1)
  boxplot_szurt2_num <- sample(c(tema$num1_nev, tema$num2_nev, tema$num3_nev), 1)
  
  szoveg <- paste0(
    "# Adatmátrix, szűrés és boxplot feladat\n",
    "#\n",
    "# ", bevezeto, "\n",
    "# Az adatállomány már előre létrehozásra került, az adatmatrix_feladat nevű data.frame-ben található.\n",
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
    "# - Ellenőrizze az adatmatrix_feladat szerkezetét, oszlopneveit és első néhány sorát. (2 pont)\n",
    "# - Készítsen boxplot ábrát a(z) ", boxplot_fo_num, " változóra a(z) ", boxplot_fo_kat, " kategóriái szerint. (2 pont)\n",
    "# - Szűrje ki az adatokat az alábbi feltételek szerint: ",
    tema$kat1_nev, " = ", felt1_kat1, ", ",
    tema$kat2_nev, " = ", felt1_kat2, ". ",
    "Az így kapott adatokból készítsen boxplot ábrát a(z) ", boxplot_szurt1_num, " változóról. (3 pont)\n",
    "# - Szűrje ki az adatokat az alábbi feltételek szerint: ",
    tema$kat1_nev, " = ", felt2_kat1, ", ",
    tema$kat2_nev, " = ", felt2_kat2, ", ",
    tema$kat3_nev, " = ", felt2_kat3, ", ",
    tema$num1_nev, " < ", num1_hatar, ", ",
    tema$num2_nev, " > ", num2_hatar, ". ",
    "Az így kapott adatokból készítsen boxplot ábrát a(z) ", boxplot_szurt2_num, " változóról. (3 pont)\n\n"
  )
  
  list(
    adat = adat,
    szoveg = szoveg
  )
}

# ============================================================
# FELADATLISTA ÖSSZEÁLLÍTÁSA
# ============================================================

# Ez biztosan bekerül
fix_feladatok <- c("korrelacio")

# Ide később kerül majd a második fix feladat
masodik_fix_feladat <- c("adatmatrix")

# Ide később kerülnek a választható random feladatok
random_feladatok <- character(0)

# Későbbi végleges logika majd ilyen lesz:
# fix_feladatok <- c("korrelacio", "masodik_fix")
# random_feladatok <- c("tesztvalasztas", "adatmatrix", "szures", "diverzitas", "regresszio")
# valasztott_random <- sample(random_feladatok, 2, replace = FALSE)
# vegso_feladatok <- sample(c(fix_feladatok, valasztott_random), 4)

vegso_feladatok <- fix_feladatok

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
    
    # Tanári célra, de nem kerül ki automatikusan, mert nem adat_ előtagú
    megoldas_korrelacio <- kor$megoldas
  }
  
  feladat_sorszam <- feladat_sorszam + 1
}
