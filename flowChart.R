library(DiagrammeR)
library(glue)
library(DiagrammeRsvg)
library(rsvg)

n1 <- nrow(cancer)
exclude1_1 <- cancer %>% filter(is.na(AgeDx)) %>% nrow() # Missing age at diagnosis
exclude2_1 <- cancer %>% filter(!is.na(AgeDx),
                              FollowUpYears <= 0) %>% nrow()  # Did not reach follow-up period
include1_1 <- n1 - exclude1_1 - exclude2_1

n1 <- comma(n1)
exclude1_1 <- comma(exclude1_1)
exclude2_1 <- comma(exclude2_1)
include1_1 <- comma(include1_1)

# Data2
exclude1_2 <- 3
exclude2_2 <- 37350
include1_2 <- nrow(us_sir_general_cl_df) - exclude1_2 - exclude2_2
n2 <- nrow(us_sir_general_cl_df) + exclude1_2

n2 <- comma(n2)
exclude1_2 <- comma(exclude1_2)
exclude2_2 <- comma(exclude2_2)
include1_2 <- comma(include1_2)

white_alive <- comma(us_sir_general %>% filter(race_eth == "White", `Cause of Death` == "Alive") %>% pull(Count))
white_cvd <- comma(us_sir_general %>% filter(race_eth == "White", `Cause of Death` == "CVD Death") %>% pull(Count))
white_other <- comma(us_sir_general %>% filter(race_eth == "White", `Cause of Death` == "Other Causes of Death") %>% pull(Count))

black_alive <- comma(us_sir_general %>% filter(race_eth == "Black", `Cause of Death` == "Alive") %>% pull(Count))
black_cvd <- comma(us_sir_general %>% filter(race_eth == "Black", `Cause of Death` == "CVD Death") %>% pull(Count))
black_other <- comma(us_sir_general %>% filter(race_eth == "Black", `Cause of Death` == "Other Causes of Death") %>% pull(Count))

us_hispanic_alive <- comma(us_sir_general %>% filter(race_eth == "US Hispanic", `Cause of Death` == "Alive") %>% pull(Count))
us_hispanic_cvd <- comma(us_sir_general %>% filter(race_eth == "US Hispanic", `Cause of Death` == "CVD Death") %>% pull(Count))
us_hispanic_other <- comma(us_sir_general %>% filter(race_eth == "US Hispanic", `Cause of Death` == "Other Causes of Death") %>% pull(Count))

aanhpi_alive <- comma(us_sir_general %>% filter(race_eth == "AANHPI & AIAN", `Cause of Death` == "Alive") %>% pull(Count)) 
aanhpi_cvd <- comma(us_sir_general %>% filter(race_eth == "AANHPI & AIAN", `Cause of Death` == "CVD Death") %>% pull(Count)) 
aanhpi_other <- comma(us_sir_general %>% filter(race_eth == "AANHPI & AIAN", `Cause of Death` == "Other Causes of Death") %>% pull(Count)) 

pr_alive <- comma(pr_sir_general %>% filter(`Cause of Death` == "Alive") %>% pull(Count))
pr_cvd <- comma(pr_sir_general %>% filter(`Cause of Death` == "CVD Death") %>% pull(Count))
pr_other <- comma(pr_sir_general %>% filter(`Cause of Death` == "Other Causes of Death") %>% pull(Count))

flowchart_svg <- grViz(
  glue("digraph my_flowchart {{ 
      graph[splines = ortho]
      node [fontname = Helvetica, shape = box, width = 2.5, height = 0.75, fontsize = 8]

      // PRCCR label as a separate node
      prccr_label[label = \"Puerto Rico Central Cancer Registry\", shape = plaintext, fontsize = 12];

      // Puerto Rico Data
      node1_1[label = <Total available women 18+<br/>diagnosed with primary invasive BC<br/>(n = {n1})>]

      blank2_1[label = '', width = 0.01, height = 0.01]
      excluded2_1[label = <Excluded because of not meeting minimum of<br/>12-month post-diagnosis (n = {exclude2_1})>]
      node1_1 -> blank2_1[dir = none];
      blank2_1 -> excluded2_1[minlen = 2];
      {{ rank = same; blank2_1 excluded2_1 }}

      node2_1[label = <Included for analysis<br/>(n = {include1_1})>]
      
      node3[label = < PR Hispanic<br/>
      Alive (n = {pr_alive})<br/>
      CVD death (n = {pr_cvd})<br/>
      Other causes of death (n = {pr_other})>]
      node2_1 -> node3;
      blank2_1 -> node2_1;

      // Position PRCCR label above PR data
      prccr_label -> node1_1 [style=invis]

      // SEER
       // PRCCR label as a separate node
      seer_label[label = \"SEER Research Data, 17 Registries\", shape = plaintext, fontsize = 12];
      
      node1_2[label = <Total available women 18+<br/>diagnosed with primary invasive BC<br/>(n = {n2})>]
      // Position SEER label above SEER data
      seer_label -> node1_2 [style=invis]
      
      blank2_2[label = '', width = 0.01, height = 0.01]
      excluded2_2[label = <Excluded because of not meeting minimum of<br/>12-month post-diagnosis (n = {exclude2_2})>]
      node1_2 -> blank2_2[dir = none];
      blank2_2 -> excluded2_2[minlen = 2];
      {{ rank = same; blank2_2 excluded2_2 }}

      node2_2[label = <Included for analysis<br/>(n = {include1_2})>]
      blank2_2 -> node2_2;

     // SEER Data formatted like PR Data
      node4_1[label = < US Hispanic<br/>
      Alive (n = {us_hispanic_alive})<br/>
      CVD death (n = {us_hispanic_cvd})<br/>
      Other causes of death (n = {us_hispanic_other})>]

      node4_2[label = < White<br/>
      Alive (n = {white_alive})<br/>
      CVD death (n = {white_cvd})<br/>
      Other causes of death (n = {white_other})>]

      node4_3[label = < Black<br/>
      Alive (n = {black_alive})<br/>
      CVD death (n = {black_cvd})<br/>
      Other causes of death (n = {black_other})>]

      node4_4[label = < AANHPI and AIAN<br/>
      Alive (n = {aanhpi_alive})<br/>
      CVD death (n = {aanhpi_cvd})<br/>
      Other causes of death (n = {aanhpi_other})>]

      // Connect Dataset 2 to SEER boxes
      node2_2 -> node4_1;
      node2_2 -> node4_2;
      node2_2 -> node4_3;
      node2_2 -> node4_4;

      // Keep all SEER boxes in the same rank
      {{ rank = same; node4_1 node4_2 node4_3 node4_4 }}
      // Layout adjustment to keep everything side by side
      {{ rank = same; node1_1 node1_2 }}
      {{ rank = same; node2_1 node2_2 }}
       {{ rank = same; node3 }}
      
  }}")
) %>% export_svg()

# Save as PNG
rsvg_png(charToRaw(flowchart_svg), file = "~/Downloads/Statistical Analysis/Outputs/Graphs/Flowchart.png", width = 2000, height = 1500)
