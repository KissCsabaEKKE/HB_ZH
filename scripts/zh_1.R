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
# FELADATLISTA ÖSSZEÁLLÍTÁSA
# ============================================================

# Ez biztosan bekerül
fix_feladatok <- c("korrelacio")

# Ide később kerül majd a második fix feladat
masodik_fix_feladat <- NULL

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
