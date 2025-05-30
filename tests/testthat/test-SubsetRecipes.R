library(testthat) 

test_that("Get Indication Subset Definition Ids", {
  
  cohortDefinitionSet <- data.frame(
    targetIds = c(1, 2, 3),                 # Target IDs
    cohortName = c("Cohort A",              # Cohort Names
                   "Cohort B",
                   "Cohort C"),
    stringsAsFactors = FALSE
  )
  
  indicationSubsetIds <- c(1001,1002,1003)
  # Attach the "indicationSubsetDefinitions" as an attribute
  attr(cohortDefinitionSet, "indicationSubsetDefinitions") <- indicationSubsetIds
  
  # Use the function to extract the indication subset IDs
  getIndicationSubsetIds <- CohortGenerator::getIndicationSubsetDefinitionIds(cohortDefinitionSet)
  expect_equal(indicationSubsetIds, getIndicationSubsetIds)
  
  
})


test_that("Add Indication Subset Definition",{
  
  cohort1 <- 1
  cohort2 <- 2
  cohort3 <- 3
  cohortDefinitionSet <- data.frame(
    cohortId = c(cohort1,cohort2,cohort3),                      # Sample cohort IDs
    cohortName = c("Cohort A",                  # Cohort names
                   "Cohort B",
                   "Cohort C"),
    sql = c("SELECT * FROM cohort1",            # Placeholder SQL
            "SELECT * FROM cohort2",
            "SELECT * FROM cohort3"),
    json = c("{}","{}","{}"),                   # Placeholder JSON
    stringsAsFactors = FALSE
  )
  
  # Optional: Attach an initial empty attribute for indication definitions
  attr(cohortDefinitionSet, "indicationSubsetDefinitions") <- NULL
  
  targetCohortIds <- c(cohort1, cohort2)                   # Cohort IDs for the target population
  indicationCohortIds <- c(cohort3)                  # Indication cohort required for inclusion
  definitionId <- 1001                         # Subset definition ID
  
  # Step 3: Call the function to add an indication subset definition
  updatedCohortDefinitionSet <- CohortGenerator::addIndicationSubsetDefinition(
    cohortDefinitionSet = cohortDefinitionSet,
    targetCohortIds = targetCohortIds,
    indicationCohortIds = indicationCohortIds,
    definitionId = definitionId,
    name = "Indication Subset Example",
    cohortCombinationOperator = "all",
    lookbackWindowStart = -365,
    lookbackWindowEnd = 0,
    ageMin = 18,
    ageMax = 65,
    requiredPriorObservationTime = 180,
    requiredFollowUpTime = 30
  )
  
  #Is cohort id 1 the parent of 2001
  expect_equal(updatedCohortDefinitionSet$subsetParent[updatedCohortDefinitionSet$cohortId == 2001],cohort1)  
  
  #Is cohort id 2 the parent of 2001
  expect_equal(updatedCohortDefinitionSet$subsetParent[updatedCohortDefinitionSet$cohortId == 3001],cohort2)  
  
  
  #checkingJson
  subsetJsonFor2001 <- updatedCohortDefinitionSet$json[updatedCohortDefinitionSet$cohortId == 2001]
  parsedJsonFor2001 <- jsonlite::fromJSON(subsetJsonFor2001) # Convert JSON string to R object
  expect_equal(parsedJsonFor2001$targetCohortId, cohort1)
  
  subsetJsonFor3001 <- updatedCohortDefinitionSet$json[updatedCohortDefinitionSet$cohortId == 3001]
  parsedJsonFor3001 <- jsonlite::fromJSON(subsetJsonFor3001)
  expect_equal(parsedJsonFor3001$targetCohortId,cohort2)

})

test_that("Get Restricted Subset Definition Ids ",{
  
  cohortDefinitionSet <- data.frame(
    cohortId = c(1, 2, 3),                      # Sample cohort IDs
    cohortName = c("Cohort A",                  # Cohort names
                   "Cohort B",
                   "Cohort C"),
    stringsAsFactors = FALSE
  )
  indicationSubsetDefinition <- c(1001, 1002, 1003)
  # Set the `indicationSubsetDefinitions` attribute
  attr(cohortDefinitionSet, "indicationSubsetDefinitions") <- indicationSubsetDefinition
  
  result <- CohortGenerator::getRestrictionSubsetDefinitionIds(cohortDefinitionSet)
  expect_equal(result, indicationSubsetDefinition)   # The returned values should match
  
})

test_that("Add Restricted Subset Definition",{
  
  cohortDefinitionSet <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("Cohort A", "Cohort B", "Cohort C"),
    sql = c("SELECT * FROM cohort1", "SELECT * FROM cohort2", "SELECT * FROM cohort3"),
    json = c("{}","{}","{}"),
    stringsAsFactors = FALSE
  )
  
  # Attach an empty restrictionSubsetDefinitions attribute
  attr(cohortDefinitionSet, "restrictionSubsetDefinitions") <- NULL
  
  
  # Test 1: Basic subset creation ----------------------------------------
  
  # Inputs for the subset
  targetCohortIds <- c(1)    # Target cohort is Cohort A (cohortId 1)
  definitionId <- 2001       # Subset definition ID
  
  updatedCohortDefinitionSet <- CohortGenerator::addRestrictionSubsetDefinition(
    cohortDefinitionSet = cohortDefinitionSet,
    targetCohortIds = targetCohortIds,
    definitionId = definitionId,
    name = "Restriction Subset Example",
    subsetCohortNameTemplate = "@baseCohortName - @subsetDefinitionName",
    ageMin = 18,
    ageMax = 65,
    requiredPriorObservationTime = 180,
    requiredFollowUpTime = 30
  )
  
  expect_equal(attr(updatedCohortDefinitionSet, "restrictionSubsetDefinitions"), c(2001))
  expect_true(3001 %in% updatedCohortDefinitionSet$cohortId) # Subset cohortId is 2001
  expect_equal(updatedCohortDefinitionSet$subsetParent[updatedCohortDefinitionSet$cohortId == 3001],targetCohortIds)  
  subsetJsonFor3001 <- updatedCohortDefinitionSet$json[updatedCohortDefinitionSet$cohortId == 3001]
  parsedJsonFor3001 <- jsonlite::fromJSON(subsetJsonFor3001)
  expect_equal(parsedJsonFor3001$targetCohortId,targetCohortIds)
  
})