acm_cah_ui <- function() {
  tagList(
    # ===== Hero Section - Orange Theme =====
    div(
      style = "background: linear-gradient(135deg, #ed8936 0%, #dd6b20 100%);
               padding: 25px 20px;
               border-radius: 10px;
               margin-bottom: 25px;
               box-shadow: 0 4px 15px rgba(237, 137, 54, 0.25);",
      h3("HAC (Hierarchical Agglomerative Clustering)",
         style = "margin: 0 0 8px 0;
                  font-weight: 700;
                  font-size: 1.8em;
                  color: white;
                  text-align: center;"),
      p("Designed for qualitative variables, uses an agglomerative (bottom-up) hierarchical approach that progressively merges similar categorical variables",
        style = "text-align: center;
                 font-size: 0.95em;
                 color: rgba(255,255,255,0.95);
                 margin: 0;")
    ),

    # ===== Model Overview + Elbow Plot =====
    fluidRow(
      # Model Overview
      column(
        width = 4,
        div(
          style = "display: flex; flex-direction: column; justify-content: center; height: 450px;",

          div(
            style = "padding: 25px;",

            div(
              style = "text-align: center; margin-bottom: 20px;",
              div(style = "font-size: 40px; margin-bottom: 10px;", "📋"),
              h4("Model Overview", style = "margin: 0; color: #2d3748; font-weight: 700; font-size: 1.3em;")
            ),

            tags$head(
              tags$style(HTML("
                #acm_cah_print {
                  background-color: white !important;
                  color: #2d3748 !important;
                  font-family: 'Consolas', 'Monaco', monospace !important;
                  font-size: 13px !important;
                  padding: 20px !important;
                  border-radius: 8px !important;
                  border: 1px solid #e2e8f0 !important;
                  line-height: 1.8 !important;
                  box-shadow: 0 2px 8px rgba(0,0,0,0.06) !important;
                }
              "))
            ),
            verbatimTextOutput("acm_cah_print")
          )
        )
      ),
      # Elbow Plot
      column(
        width = 8,
        div(
          style = "background: white;
                   padding: 20px;
                   border-radius: 10px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.06);
                   border-left: 4px solid #ed8936;
                   height: 450px;",
          div(
            style = "display: flex; align-items: center; gap: 10px; margin-bottom: 15px;",
            div(style = "font-size: 22px;", "📈"),
            h4("Elbow Method", style = "margin: 0; color: #2d3748; font-weight: 600; font-size: 1.2em;")
          ),
          plotOutput("acm_cah_elbow", height = "380px")
        )
      )
    ),

    tags$div(style = "height: 20px;"),

    # ===== Dendrogram + Heatmap =====
    fluidRow(
      column(
        width = 6,
        div(
          style = "background: white;
                   padding: 20px;
                   border-radius: 10px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.06);
                   border-left: 4px solid #ed8936;
                   height: 100%;",
          div(
            style = "display: flex; align-items: center; gap: 10px; margin-bottom: 15px;",
            div(style = "font-size: 22px;", "🌳"),
            h4("Hierarchical Dendrogram", style = "margin: 0; color: #2d3748; font-weight: 600; font-size: 1.2em;")
          ),
          plotOutput("acm_cah_dendrogram", height = "450px")
        )
      ),

      column(
        width = 6,
        div(
          style = "background: white;
                   padding: 20px;
                   border-radius: 10px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.06);
                   border-left: 4px solid #ed8936;
                   height: 100%;",
          div(
            style = "display: flex; align-items: center; gap: 10px; margin-bottom: 15px;",
            div(style = "font-size: 22px;", "🔥"),
            h4("Distance Heatmap", style = "margin: 0; color: #2d3748; font-weight: 600; font-size: 1.2em;")
          ),
          plotOutput("acm_cah_heatmap", height = "450px")
        )
      )
    ),

    tags$div(style = "height: 20px;"),

    # ===== NEW: Carte factorielle ACM =====
    div(
      style = "background: white;
               padding: 20px;
               border-radius: 10px;
               box-shadow: 0 2px 10px rgba(0,0,0,0.06);
               border-left: 4px solid #ed8936;
               margin-bottom: 20px;",
      div(
        style = "display: flex; align-items: center; gap: 10px; margin-bottom: 15px;",
        div(style = "font-size: 22px;", "🗺️"),
        h4("Factorial Map (ACM only)",
           style = "margin: 0; color: #2d3748; font-weight: 600; font-size: 1.2em;")
      ),
      plotOutput("acm_cah_factorial_map", height = "550px")
    ),

    # ===== NEW: Scree plot + Contributions =====
    fluidRow(
      # Scree plot
      column(
        width = 6,
        div(
          style = "background: white;
                   padding: 20px;
                   border-radius: 10px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.06);
                   border-left: 4px solid #ed8936;
                   height: 100%;",
          div(
            style = "display: flex; align-items: center; gap: 10px; margin-bottom: 15px;",
            div(style = "font-size: 22px;", "📊"),
            h4("Scree Plot (ACM only)",
               style = "margin: 0; color: #2d3748; font-weight: 600; font-size: 1.2em;")
          ),
          plotOutput("acm_cah_scree", height = "400px")
        )
      ),

      # Contributions
      column(
        width = 6,
        div(
          style = "background: white;
                   padding: 20px;
                   border-radius: 10px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.06);
                   border-left: 4px solid #ed8936;
                   height: 100%;",
          div(
            style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
            div(style = "font-size: 22px;", "📈"),
            h4("Contributions (ACM only)",
               style = "margin: 0; color: #2d3748; font-weight: 600; font-size: 1.2em;")
          ),
          # Slider pour choisir la dimension
          div(
            style = "margin-bottom: 10px;",
            sliderInput(
              "acm_dim",
              "Select Dimension:",
              min = 1,
              max = 5,
              value = 1,
              step = 1,
              width = "100%"
            )
          ),
          plotOutput("acm_cah_contrib", height = "340px")
        )
      )
    ),

    tags$div(style = "height: 20px;"),

    # ===== Modalities Analysis =====
    div(
      style = "background: white;
               padding: 25px;
               border-radius: 10px;
               box-shadow: 0 2px 10px rgba(0,0,0,0.06);
               margin-bottom: 20px;
               border-left: 4px solid #ed8936;",

      div(
        style = "display: flex; align-items: center; gap: 10px; margin-bottom: 20px;",
        div(style = "font-size: 24px;", "📊"),
        h4("Modalities Analysis", style = "margin: 0; color: #2d3748; font-weight: 700; font-size: 1.3em;")
      ),

      # Summary Text
      div(
        style = "background: #f8f9fa;
                 padding: 15px;
                 border-radius: 6px;
                 border: 1px solid #e2e8f0;
                 margin-bottom: 20px;
                 font-size: 0.9em;",
        verbatimTextOutput("acm_cah_summary_text")
      ),

      # Modalities Table
      div(
        style = "margin-bottom: 25px;",
        h5(
          style = "color: #ed8936;
                   font-weight: 600;
                   margin-bottom: 12px;
                   padding-bottom: 8px;
                   border-bottom: 2px solid #e2e8f0;
                   font-size: 1.05em;",
          "🎯 Modality Cluster Assignments"
        ),
        div(
          style = "overflow-x: auto; font-size: 0.9em;",
          tableOutput("acm_cah_modalities")
        )
      ),

      # Quality Metrics
      div(
        h5(
          style = "color: #ed8936;
                   font-weight: 600;
                   margin-bottom: 12px;
                   padding-bottom: 8px;
                   border-bottom: 2px solid #e2e8f0;
                   font-size: 1.05em;",
          "📈 Cluster Quality Metrics"
        ),
        div(
          style = "overflow-x: auto; font-size: 0.9em;",
          tableOutput("acm_cah_quality")
        )
      )
    ),

    # ===== Illustrative Variables =====
    div(
      style = "background: white;
               padding: 25px;
               border-radius: 10px;
               box-shadow: 0 2px 10px rgba(0,0,0,0.06);
               margin-bottom: 30px;
               border-left: 4px solid #ed8936;",

      div(
        style = "display: flex; align-items: center; gap: 10px; margin-bottom: 20px;",
        div(style = "font-size: 24px;", "🎯"),
        h4("Illustrative Variables", style = "margin: 0; color: #2d3748; font-weight: 700; font-size: 1.3em;")
      ),

      fluidRow(
        # Table on the left
        column(
          width = 5,
          div(
            h5(
              style = "color: #ed8936;
                       font-weight: 600;
                       margin-bottom: 12px;
                       padding-bottom: 8px;
                       border-bottom: 2px solid #e2e8f0;
                       font-size: 1.05em;",
              "📊 Variable Summary"
            ),
            div(
              style = "overflow-x: auto; font-size: 0.9em;",
              tableOutput("acm_cah_illu_table")
            )
          )
        ),

        # Plot on the right
        column(
          width = 7,
          div(
            h5(
              style = "color: #ed8936;
                       font-weight: 600;
                       margin-bottom: 12px;
                       padding-bottom: 8px;
                       border-bottom: 2px solid #e2e8f0;
                       font-size: 1.05em;",
              "📊 Illustrative Analysis"
            ),
            plotOutput("acm_cah_illu_plot", height = "500px")
          )
        )
      )
    )
  )
}
