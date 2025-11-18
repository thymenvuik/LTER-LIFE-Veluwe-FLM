# Use a custom SufficientLight table, this should keep the same dimensions
custom_light <- data.frame(
  ShadeClass = 1:5,
  `0` = c(1, 0.8, 0.6, 0.4, 0.2),
  `1` = c(0.9, 0.7, 0.5, 0.3, 0.1),
  `2` = c(0.8, 0.6, 0.4, 0.2, 0.0),
  `3` = c(0.7, 0.5, 0.3, 0.1, 0.0),
  `4` = c(0.6, 0.4, 0.2, 0.0, 0.0),
  `5` = c(0.5, 0.3, 0.1, 0.0, 0.0)
)

#use a custom fire table, this can include up to 5 fire severities
custom_fire <- data.frame(
  Severity = 1:3,
  WoodLitterReduct = c(0.5, 0.0, 0.0),
  LitterReduct = c(0.5, 0.85, 1.0)
)

#use of custom MinRelativeBiomas, class 4 can already be 100% if less shade classes are required
custom_MinRelativeBiomass <- setNames(
  data.frame(
    ShadeClass = 1:5,
    c("25%", "45%", "56%", "70%", "90%"),
    c("25%", "45%", "56%", "70%", "90%")
  ),
  c("ShadeClass", "101", "102")
)

#use of custom EcoregionParameters, this should be the same amount as the amount of ecoregions used in ecoregion.txt and ecoregion.tif
custom_EcoregionParameters <- data.frame(
  EcoregionID = c(101, 102),
  AET = c(600, 600)
)

#use of a custom harvest table, this can include as many harvest interventions as requested
#also used for harvest disturbance extension
custom_harvest <- data.frame(
  Name = c("SelectiveHarvest", "Thinning"),
  WoodLitterReduct = c(0.7, 0.9),
  LitterReduct = c(0.2, 0.3),
  CohortWoodRemoval = c(0.6, 0.5),
  CohortLeafRemoval = c(0.1, 0.2)
)


#create biomass-succession input
generate_biomass <- function(
    timestep = 10,
    seedingalgorithm = "WardSeedDispersal",
    SufficientLight = NULL,
    FireReductionParameters = NULL,
    HarvestReductionParameters = NULL,
    MinRelativeBiomass = NULL,
    EcoregionParameters = NULL,
    expected_ecoregions = c("101", "102"),
    filename
) {
  library(glue)
  
  # Validate seeding algorithm
  valid_algorithms <- c("WardSeedDispersal", "NoDispersal", "UniversalDispersal")
  if (!(seedingalgorithm %in% valid_algorithms)) {
    stop("Invalid seeding algorithm. Choose one of: WardSeedDispersal, NoDispersal, UniversalDispersal")
  }
  
  #validate SufficientLight to have 5 rows and 6 columns
  if (!is.null(SufficientLight)) {
    if (nrow(SufficientLight) != 5 || ncol(SufficientLight) != 7) {
      stop("SufficientLight must have exactly 5 rows and 6 shade columns plus 1 ShadeClass column.")
    }
  }
  
  #validate FireReductionParameters to have up to 5 rows
  if (!is.null(FireReductionParameters)) {
    if (nrow(FireReductionParameters) > 5) {
      stop("FireReductionParameters must not have more than 5 rows.")
    }
  }
  
  # Default SufficientLight
  default_light <- data.frame(
    ShadeClass = 1:5,
    `0` = c(1.0, 1.0, 1.0, 1.0, 0.1),
    `1` = c(0.5, 1.0, 1.0, 1.0, 0.5),
    `2` = c(0.25, 0.5, 1.0, 1.0, 1.0),
    `3` = c(0.0, 0.25, 0.5, 1.0, 1.0),
    `4` = c(0.0, 0.0, 0.25, 0.5, 1.0),
    `5` = c(0.0, 0.0, 0.0, 0.25, 1.0)
  )
  
  #define what to include for SufficientLight, default or user defined
  light_table <- if (is.null(SufficientLight)) default_light else SufficientLight
  light_header <- "SufficientLight\n>> Spp Shade\tProbability\n>> Class\tby Actual Shade\n>> ----------\t--------------------\n>>\t\t0\t1\t2\t3\t4\t5"
  light_rows <- apply(light_table, 1, function(row) {
    paste0("\t", row[1], "\t", paste(format(row[-1], nsmall = 2), collapse = "\t"))
  })
  light_block <- paste(c(light_header, light_rows), collapse = "\n")
  
  # Default FireReductionParameters
  default_fire <- data.frame(
    Severity = 1:3,
    WoodLitterReduct = c(0.0, 0.0, 0.0),
    LitterReduct = c(0.5, 0.75, 1.0)
  )
  
  #define what to include for FireReductionParameters, default or user defined
  fire_table <- if (is.null(FireReductionParameters)) default_fire else FireReductionParameters
  fire_header <- "FireReductionParameters\n>>\tSeverity\tWoodLitter\tLitter\t\n>>\tFire\t\tReduct\t\tReduct"
  fire_rows <- apply(fire_table, 1, function(row) {
    paste0("\t", row["Severity"], "\t\t", row["WoodLitterReduct"], "\t\t", row["LitterReduct"])
  })
  fire_block <- paste(c(fire_header, fire_rows), collapse = "\n")
  
  # Default HarvestReductionParameters
  default_harvest <- data.frame(
    Name = c("MaxAgeClearcut", "PatchCutting"),
    WoodLitterReduct = c(0.5, 1.0),
    LitterReduct = c(0.15, 1.0),
    CohortWoodRemoval = c(0.8, 1.0),
    CohortLeafRemoval = c(0.0, 0.0)
  )
  
  #define what to include for HarvestReductionParameters, default or user defined
  harvest_table <- if (is.null(HarvestReductionParameters)) default_harvest else HarvestReductionParameters
  harvest_header <- "HarvestReductionParameters\n>>\tName\t\tWoodLitter\tLitter\tCohort\t\tCohort\n>>\t\t\tReduct\t\tReduct\tWoodRemoval\tLeafRemoval"
  harvest_rows <- apply(harvest_table, 1, function(row) {
    paste0("\t", row["Name"], "\t", row["WoodLitterReduct"], "\t\t", row["LitterReduct"], "\t", row["CohortWoodRemoval"], "\t\t", row["CohortLeafRemoval"])
  })
  harvest_block <- paste(c(harvest_header, harvest_rows), collapse = "\n")
  
  # Default MinRelativeBiomass
  default_biomass <- setNames(
    data.frame(
      ShadeClass = 1:5,
      c("25%", "45%", "56%", "70%", "90%"),
      c("25%", "45%", "56%", "70%", "90%")
    ),
    c("ShadeClass", "101", "102")
  )
  
  #define what to include for MinRelativeBiomass, default or user defined
  biomass_table <- if (is.null(MinRelativeBiomass)) default_biomass else MinRelativeBiomass
  biomass_cols <- colnames(biomass_table)[-1]
  if (!setequal(biomass_cols, as.character(expected_ecoregions))) {
    stop("MinRelativeBiomass columns must match expected ecoregion IDs.")
  }
  biomass_header <- glue("
MinRelativeBiomass
>> Shade\tPercent Max Biomass
>> Class\tby Ecoregions
>> ----------\t--------------------\n\t        {paste(biomass_cols, collapse = '\t')}")
  biomass_rows <- apply(biomass_table, 1, function(row) {
    paste0("\t", row[1], "\t", paste(row[-1], collapse = "\t"))
  })
  biomass_block <- paste(c(biomass_header, biomass_rows), collapse = "\n")
  
  # Default EcoregionParameters
  default_ecoregion <- data.frame(
    EcoregionID = c(101, 102),
    AET = c(600, 600)
  )
  
  #define what to include for EcoregionParameters, default or user defined
  ecoregion_table <- if (is.null(EcoregionParameters)) default_ecoregion else EcoregionParameters
  if (!setequal(ecoregion_table$EcoregionID, expected_ecoregions)) {
    stop("EcoregionParameters rows must match expected ecoregion IDs.")
  }
  ecoregion_block <- paste(
    c("EcoregionParameters\n>>\tAET (mm)",
      apply(ecoregion_table, 1, function(row) paste(row[1], row[2], sep = "\t"))),
    collapse = "\n"
  )
  
  # Main text
  text <- glue("
LandisData  \"Biomass Succession\"

>>------------------
>> REQUIRED INPUTS
>>------------------

Timestep            {timestep}

SeedingAlgorithm        {seedingalgorithm}

InitialCommunities          ./biomass-succession_InitialCommunities.csv
InitialCommunitiesMap       initial-communities.tif
ClimateConfigFile       ./biomass-succession_ClimateGenerator.txt

>> CalibrateMode        yes

>>----------------------------
>> LIFE HISTORY PARAMETERS
>>----------------------------

{biomass_block}

{light_block}

SpeciesDataFile     SpeciesData.csv

{ecoregion_block}

SpeciesEcoregionDataFile   SppEcoregionData.csv 

{fire_block}

{harvest_block}
")
  
  writeLines(text, filename)
}

#run the function
cat(generate_biomass(filename = "biomass-succession.txt"))



