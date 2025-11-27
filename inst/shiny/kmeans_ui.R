kmeans_ui <- function() {
  tagList(
    # ===== Hero Section - Blue Theme =====
    div(
      style = "background: linear-gradient(135deg, #4299e1 0%, #3182ce 100%);
               padding: 25px 20px;
               border-radius: 10px;
               margin-bottom: 25px;
               box-shadow: 0 4px 15px rgba(66, 153, 225, 0.25);",
      h3("K-Means Method",
         style = "margin: 0 0 8px 0;
                  font-weight: 700;
                  font-size: 1.8em;
                  color: white;
                  text-align: center;"),
      p("Clusters quantitative variables using a reallocation algorithm that iteratively assigns variables to clusters based on their correlations",
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
                #kmeans_print {
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
            verbatimTextOutput("kmeans_print")
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
                   border-left: 4px solid #4299e1;
                   height: 450px;",
          div(
            style = "display: flex; align-items: center; gap: 10px; margin-bottom: 15px;",
            div(style = "font-size: 22px;", "📈"),
            h4("Elbow Method", style = "margin: 0; color: #2d3748; font-weight: 600; font-size: 1.2em;")
          ),
          plotOutput("kmeans_elbow", height = "380px")
        )
      )
    ),

    tags$div(style = "height: 20px;"),

    # ===== Cluster Composition + Inertia Evolution =====
    fluidRow(
      column(
        width = 6,
        div(
          style = "background: white;
                   padding: 20px;
                   border-radius: 10px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.06);
                   border-left: 4px solid #4299e1;
                   height: 100%;",
          div(
            style = "display: flex; align-items: center; gap: 10px; margin-bottom: 15px;",
            div(style = "font-size: 22px;", "🎯"),
            h4("Cluster Composition", style = "margin: 0; color: #2d3748; font-weight: 600; font-size: 1.2em;")
          ),
          div(
            style = "overflow-x: auto; font-size: 0.9em;",
            tableOutput("kmeans_composition")
          )
        )
      ),

      column(
        width = 6,
        div(
          style = "background: white;
                   padding: 20px;
                   border-radius: 10px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.06);
                   border-left: 4px solid #4299e1;
                   height: 100%;",
          div(
            style = "display: flex; align-items: center; gap: 10px; margin-bottom: 15px;",
            div(style = "font-size: 22px;", "📊"),
            h4("Inertia Evolution", style = "margin: 0; color: #2d3748; font-weight: 600; font-size: 1.2em;")
          ),
          plotOutput("kmeans_inertia", height = "350px")
        )
      )
    ),

    tags$div(style = "height: 20px;"),

    # ===== Cluster Analysis =====
    div(
      style = "background: white;
               padding: 25px;
               border-radius: 10px;
               box-shadow: 0 2px 10px rgba(0,0,0,0.06);
               margin-bottom: 20px;
               border-left: 4px solid #4299e1;",

      div(
        style = "display: flex; align-items: center; gap: 10px; margin-bottom: 20px;",
        div(style = "font-size: 24px;", "📊"),
        h4("Cluster Analysis", style = "margin: 0; color: #2d3748; font-weight: 700; font-size: 1.3em;")
      ),

      # Summary Text
      div(
        style = "background: #f8f9fa;
                 padding: 15px;
                 border-radius: 6px;
                 border: 1px solid #e2e8f0;
                 margin-bottom: 20px;
                 font-size: 0.9em;",
        verbatimTextOutput("kmeans_summary_text")
      ),

      # Cluster Summary Table (Full Width)
      div(
        style = "margin-bottom: 25px;",
        h5(
          style = "color: #4299e1;
                   font-weight: 600;
                   margin-bottom: 12px;
                   padding-bottom: 8px;
                   border-bottom: 2px solid #e2e8f0;
                   font-size: 1.05em;",
          "📐 Cluster Summary Statistics"
        ),
        div(
          style = "overflow-x: auto; font-size: 0.9em;",
          tableOutput("kmeans_cluster_summary")
        )
      ),

      # Variable Assignments
      div(
        h5(
          style = "color: #4299e1;
                   font-weight: 600;
                   margin-bottom: 12px;
                   padding-bottom: 8px;
                   border-bottom: 2px solid #e2e8f0;
                   font-size: 1.05em;",
          "🎯 Variable Cluster Assignments"
        ),
        div(
          style = "overflow-x: auto; font-size: 0.9em;",
          tableOutput("kmeans_assignments")
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
               border-left: 4px solid #4299e1;",

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
              style = "color: #4299e1;
                       font-weight: 600;
                       margin-bottom: 12px;
                       padding-bottom: 8px;
                       border-bottom: 2px solid #e2e8f0;
                       font-size: 1.05em;",
              "📊 Variable Summary"
            ),
            div(
              style = "overflow-x: auto; font-size: 0.9em;",
              tableOutput("kmeans_illu_table")
            )
          )
        ),

        # PCA Circle on the right
        column(
          width = 7,
          div(
            h5(
              style = "color: #4299e1;
                       font-weight: 600;
                       margin-bottom: 12px;
                       padding-bottom: 8px;
                       border-bottom: 2px solid #e2e8f0;
                       font-size: 1.05em;",
              "⭕ Correlation Circle"
            ),
            plotOutput("kmeans_illu_plot", height = "500px")
          )
        )
      )
    )
  )
}
