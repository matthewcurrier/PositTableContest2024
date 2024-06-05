my_tbl_theme <- function(gt_object, ...) {
  
  gt_object |> 
    opt_align_table_header(align = "left") |> 
    tab_options(
      column_labels.border.top.color = "black",
      column_labels.border.top.width = px(3),
      column_labels.border.bottom.color = "black",
      quarto.use_bootstrap = FALSE,
      
      heading.title.font.size = px(28),
      heading.subtitle.font.size = px(20),
      
      table.border.top.width = px(0),
    ) |>    
    tab_style(
      style = cell_borders(
        sides = c("top"),
        color = "red",
        weight = px(0),
        style = "solid"
      ),
      locations = cells_title()
    ) |> 
    tab_style(
      style = cell_text(
        color = "black", 
        weight = "bold",
        transform = "uppercase"),
      locations = list(
        cells_column_spanners(everything()),
        cells_column_labels(everything())
      )) 
}