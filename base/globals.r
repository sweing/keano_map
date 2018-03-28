folders = list(
    tmp         = "./tmp",
    ocoData     = "../daytona/data/oco2/v8rData",
    dataFolder  = "../daytona/data",
)


# ----------------------------------------------
# MAKE 1° GRID + ID
# ----------------------------------------------
grid = expand.grid(lats = c(-90:90), lons = c(-180:180))
grid$id = 1:nrow(grid)
# ----------------------------------------------

countryGroups = list(
    EU28 = c("AUT", "BEL", "DEU", "DNK", "ESP", "FIN", "FRA", "GBR", 
             "GRC", "IRL", "ITA", "LUX", "NLD", "PRT", "SWE", "BGR", 
             "HRV", "CYP", "CZE", "EST", "HUN",
             "LVA", "LTU", "POL", "ROU", "SVK", "SVN", "MLT"),
    Europe = c("RUS", "LIE", "PAK", "AFG", "UKR", "YEM", "SAU", 
               "IRQ", "TUR", "LBY", "EGY", "ISR", "Gaza", "JOR", "ARM", 
               "ISL", "LBR", "SRB", "SYR", "LBN", "DJI", "GEO", "DZA", 
               "MAR", "IRN", "TKM", "NOR"),
    Easia = c("JPN", "TWN", "CHN", "MNG", "IND", "TJK", "KAZ", "THA", "KOR", 
              "PHL", "MMR", "PRK", "IDN", "NPL"),
    Oceania = c("AUS", "NZL", "VUT", "PNG"),
    Africa = c("SOM", "MOZ", "ZMB", "TGO", "UGA", "CIV", "NGA", "GHA", "SWZ", 
               "GMB", "ZWE", "COD", "SDN", "TCD", "BFA", "MLI"),
    Sam = c("BRA", "ARG", "BOL", "PER", "COL", "URY", "PRY", "ECU", "CHL", "GUY", "NIC"),
    Nam = c("CAN", "USA", "MEX", "DOM", "HND")
)

#"SDS" "MDG" "KEN" "ETH" "ERI" "CNM" "CYN" "MDA" "BLR" "ZAF" "BWA" "NAM" "AGO" "CAF" "NER" "GIN"
#"MRT" "SAH" "BDI" "RWA" "HRV" "SVN" "BEN" "VEN" "MYS" "PSE" "TUN" "FRA" "LAO" "LKA" "KGZ" "OMN"
#"QAT" "BHR" "AZE" "MWI" "TZA" "GRL" "ALB" "MKD" "KOS" "BGD" "SOL" "LSO" "CHE" "BEL" "SUR" "VNM"
#"ARE" "CMR" "TLS" "BTN" "CUB" "POL" "COG" "KAB" "ATG" "KWT" "BIH" "CPV" "HTI" "BHS" "PYF" "SLB"
#"KHM" "WSM" "GNB" "SEN" "GTM" "FJI" "MNE" "PAN" "SHN" "MNP" "DMA" "SPM" "ASM" "LCA" "SYC" "GGY"
#"SGS" "GNQ" "GAB" "SLV" "BLZ" "MLT" "NCL" "KNM" "FLK" "JAM" "SLE" "TTO" "KNA" "HKG" "PRI" "KIR"
#"ATF" "NRU" "CUW" "IMN" "BRN" "MUS" "CRI" "COM" "VCT" "CYM" "USG" "WSB" "TON" "ESB" "TCA" "VIR"
#"MAF" "AIA" "ALA" "AND" "COK" "STP" "GUM" "SMR" "FRO" "MDV" "PLW" "JEY" "KAS" "FSM" "HMD" "ABW"
#"NIU" "MSR" "BRB"