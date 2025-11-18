#this file makes the scenario file for any landis run

generate_scenario <- function(duration = 50, cellLength = 100,
                            succession, succession_filename,
                            disturbance = NULL, disturbance_filename = NULL,
                            output = NULL, output_filename = NULL,
                            reproducibility = "no",
                            randomOrder = "no",
                            filename) {
  library(glue)
  library(purrr)
  
  # Disturbance block (supports multiple, one or none)
  disturbance_block <- ""
  if (!is.null(disturbance) && !is.null(disturbance_filename)) {
    disturbance_lines <- map2_chr(disturbance, disturbance_filename, ~ glue("    \"{.x}\" { .y }")) #makes sure disturbance and initialization file are both included
    random_order_line <- if (tolower(randomOrder) == "yes") { #tolower can be deleted if we have 2 options in the final NaaVRE
      "DisturbancesRandomOrder  yes     << optional"
    } else {
      ">>   DisturbancesRandomOrder  yes    << optional"
    }
    
    disturbance_block <- glue("
>> --------------------------
>> DISTURBANCE EXTENSIONS
>> -------------------------

>>  Disturbance Extension   Initialization File
>>  --------------------    -------------------
{paste(disturbance_lines, collapse = '\\n')}

{random_order_line}
                                << Commented (default) is \"no\"
")
  }
  
  # Output block (supports multiple, one or none)
  output_block <- ""
  if (!is.null(output) && !is.null(output_filename)) {
    output_lines <- map2_chr(output, output_filename, ~ glue("      \"{.x}\"        {.y}")) #makes sure disturbance and initialization file are both included
    output_block <- glue("
>> ------------------------
>> OUTPUT EXTENSIONS
>> ----------------------

>>  Output Extension        Initialization File
>>  ----------------        -------------------
{paste(output_lines, collapse = '\\n')}
")
  }
  
  # RandomNumberSeed line, yes for people who want to do reproducibility tests
  seed_line <- if (tolower(reproducibility) == "yes") {
    "RandomNumberSeed  147  << optional parameter; uncomment for reproducibility tests"
  } else {
    ">> RandomNumberSeed  147  << optional parameter; uncomment for reproducibility tests"
  }
  
  # Main text
  text <- glue("
LandisData  \"Scenario\"


>>-------------------
>> REQUIRED INPUTS
>>-------------------

Duration    {duration}

Species     Core_species_data.txt

Ecoregions      ./ecoregions.txt
EcoregionsMap   ./ecoregions.tif

CellLength      {cellLength} << meters, 100 x 100 m = 1 ha

>> -----------------------
>> SUCCESSION EXTENSIONS
>> -----------------------

>>  Succession Extension     Initialization File
>>  --------------------     -------------------
    \"{succession}\"    {succession_filename}

{disturbance_block}
{output_block}

{seed_line}
")
  
  writeLines(text, filename) #export scenario
}

#example prompts
generate_scenario(
  succession = "Biomass-succession",
  succession_filename = "Biomass.txt",
  filename = "scenario.txt"
)

generate_scenario(
  duration = 60,
  cellLength = 100,
  succession = "NECN-succession",
  succession_filename = "NECN.txt",
  disturbance = c("Biomass harvest"),
  disturbance_filename = c("Biomass-harvest.txt"),
  output = c("Output Biomass", "Community output", "wildlife suitability output"),
  output_filename = c("output-biomass.txt", "Community-output.txt", "Suitability-output.txt"),
  filename = "scenario.txt"
)

