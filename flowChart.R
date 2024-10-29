library(DiagrammeR)
library(glue)

# Create the flowchart
grViz("
digraph flowchart {

  # Define graph attributes
  graph [layout = dot, rankdir = TB]
  
  # Define node attributes
  node [shape = box, style = filled, color = lightgray]
  
  A [label = '2004-2015 patients\\n with a first primary breast cancer diagnosis\\n (ICD-10:C50.x)\\n (n = 511,210)']
  B [label = 'Breast cancer patients without CT/RT\\n (n = 131,306)']


 blank1[label = '', width = 0.01, height = 0.01]
        excluded1[label = <Excluded because of<br/>exclusion criteria (n={exclude1})>]
        
        node1 -> blank1[dir = none];
        blank1 -> excluded1[minlen = 2];
        {{ rank = same; blank1 excluded1 }}
        
        
L1 [label = 'SMR Calculation', shape = plaintext, fontsize = 16, width = 0, height = 0]
    
  # SMR calculation
  subgraph cluster_SMR {
    style=filled;
    color='#90EE90';
    C [label = 'Overall cohort\\n (n = 131,306)', fillcolor = white]
    D [label = 'Tumor resection cohort\\n (n = 117,012)', fillcolor = white]
    E [label = 'No resection cohort\\n (n = 14,294)', fillcolor = white]
  }
  
  F [label = 'Resection selection cohort\\n (n = 106,326)', fillcolor = lightgray]
  L2 [label = 'Competing risk regression', shape = plaintext, fontsize = 16, width = 0, height = 0]

  # Competing risk regression
  subgraph cluster_CompetingRisk {
    style=filled;
    color=lightblue;
    G [label = 'Tumor resection sub-cohort\\n (unmatched)\\n (n = 97,496)', fillcolor = white]
    H [label = 'No resection sub-cohort\\n (unmatched)\\n (n = 8830)', fillcolor = white]
    I [label = 'Tumor resection sub-cohort\\n (matched)\\n (n = 8739)', fillcolor = white]
    J [label = 'No resection sub-cohort\\n (matched)\\n (n = 8739)', fillcolor = white]
  }

  # Define the edges
  A -> B
  B -> C
  C -> D
  L1 -> D [style=invis]
  C -> E
  D -> F
  E -> F
  F -> G
  L2 -> I [style=invis]
  F -> H
  G -> I
  H -> J

}
  
")


n <- 1000
exclude1 <- 100
exclude2 <- 50
include1 <- n - exclude1 - exclude2

grViz(
  glue("digraph my_flowchart {{ 
      graph[splines = ortho]
      node [fontname = Helvetica, shape = box, width = 4, height = 1]
      
        node1[label = <Total available patients<br/>(n = {n})>]
                
        blank1[label = '', width = 0.01, height = 0.01]
        excluded1[label = <Excluded because of<br/>exclusion criteria (n={exclude1})>]
        
        node1 -> blank1[dir = none];
        blank1 -> excluded1[minlen = 2];
        {{ rank = same; blank1 excluded1 }}
        
        blank2[label = '', width = 0.01, height = 0.01]
        excluded2[label = <Excluded because of missing values (n={exclude2})>]
        
        blank1 -> blank2[dir = none];
        blank2 -> excluded2[minlen = 2];
        {{ rank = same; blank2 excluded2 }}
        
        node2[label = <Included for analysis<br/>(n={include1})>]
        blank2 -> node2;
        
        node3[label = <Data linked with<br/>external dataset>]
        node2 -> node3;
     }}")
)
