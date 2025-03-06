#########################################
#### NCAIR Presentation - 2025 ##########
# Putting the R in Institutional Research
# Dr. Thomas Kirnbauer & Dr. Kyle Fassett
########################################

#### Set up R ####
# Install packages the first time you use them on your computer
install.packages("sjlabelled")
install.packages("tidyverse")

# load the packages (every time you open R )
library(sjlabelled) # to work with SPSS files and labels
library(tidyverse) # to do piping %>% or |>

#### Data ####
# read/load the dataframe from your desktop- we'll call the dataframe "df" for shorthand
# you'll need to change the file path to your desktop, and make sure the slashes are forward / instead of backward \
# highlight the code and click the Run button, or ctrl+enter (or cmd+return on a Mac)

# use this for spss files & plugin your own pathway and datafile name
# sample data from: sjlabelled package: 'efcs'
# Lamura G, Döhner H, Kofahl C, editors. Family carers of older people in Europe: a six-country comparative study. Münster: LIT, 2008.
df<-read_spss("patway/data_example.sav")


#### Codebook #####
install.packages("codebook")
library(codebook)

# Run the one line of code, which will pop open a new screen.
# Then, you need to copy in your data path in that new pop up screen
# in the pop-up, you'll Un-comment the 'Sav' etc. Line of code in the pop-up window and paste the path to your data
# click "knit" button in the R console
# click the dropdown Codebook, save as excel. Done!
codebook::new_codebook_rmd()

#### Reports ####
#### Example 1. Create the Report with Summary Tools
install.packages("summarytools")
library(summarytools) #

#  Convert factor responses to labeled/character responses - we only need this when working with SPSS Files
dflabel <- df |>
  sjlabelled::as_label()


# click the pop-out button the Viewer window, click print, and save as PDF.
# can also highlight and copy/paste into excel
dflabel %>%              # we take our dataframe and
  dfSummary() %>%   # feed that df into the summary table view and
  stview()         # feed it into the dataframe summary action


# We can also add a grouping variable via group_by()
dflabel %>%
  group_by(e16sex) %>%
  dfSummary() %>%
  stview()



#### Example 2: Create the Report with gt (Grammar of Tables = gt)
install.packages("gt")
install.packages("gtsummary")
library(gt)
library(gtsummary)


# feed the dataframe into the gt function & add a grouping variable
dflabel |>
  select(c161sex, c172code, c12hour, c83cop2) |> # select variables
  filter(!is.na(c161sex) & !is.na(c172code)) |>   # Drop rows with NA in either grouping variable
  tbl_strata(
    strata = c161sex, # outer grouping variable
    ~ .x |>
      tbl_summary(
        by = c172code,  # inner grouping variable
        missing = "no",
        statistic = list(
          all_continuous() ~ "{mean} ({sd})",
          all_categorical() ~ "{n} ({p}%)"
        ),
        label = NULL
      ) |>
      add_p(
        test = list(
          all_continuous() ~ "aov", # anova since we have more than 2 groups
          all_categorical() ~ "chisq.test"
        )
      ) |>
      modify_header(label = "**Characteristic**") |> # label
      bold_labels()
  ) |>
  as_gt() |>
  tab_options(table.font.size = px(13)) |>
  opt_table_font(font = "Calibri") |>
  gt::tab_header(title = md("**Table of Items by Sex & Education**")) |> # header
  tab_source_note(source_note = c("-Data: sjlabelled::efc",
                                  "-Missing data listwise removed")) # footnotes



### END ###