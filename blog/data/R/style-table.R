# Format a data frame as a table using the blog palette.
# Example: style_table(head(data))
style_table <- function(data, text_size = 4, font_family = "serif") {
  stopifnot(is.data.frame(data), nrow(data) > 0, ncol(data) > 0)
  fields <- names(data)
  formatted <- lapply(data, function(column) {
    if (inherits(column, "Date")) {
      format(column, "%Y-%m-%d")
    } else if (is.numeric(column)) {
      scales::comma(column)
    } else {
      as.character(column)
    }
  })
  cells <- tibble::tibble(
    field = factor(rep(fields, each = nrow(data)), levels = fields),
    row = rep(seq_len(nrow(data)), times = length(fields)),
    value = unlist(formatted, use.names = FALSE)
  )
  headers <- tibble::tibble(
    field = factor(fields, levels = fields), value = fields
  )

  ggplot2::ggplot(cells, ggplot2::aes(x = field, y = -row)) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = row %% 2 == 0),
      color = "#F5F3EF", linewidth = 0.6
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = value),
      family = font_family, color = "#283438", size = text_size
    ) +
    ggplot2::geom_tile(
      data = headers, ggplot2::aes(x = field, y = 0),
      inherit.aes = FALSE, fill = "#24434A", color = "#F5F3EF"
    ) +
    ggplot2::geom_text(
      data = headers, ggplot2::aes(x = field, y = 0, label = value),
      inherit.aes = FALSE, family = font_family,
      fontface = "bold", color = "white", size = text_size
    ) +
    ggplot2::scale_fill_manual(
      values = c("FALSE" = "#F5F3EF", "TRUE" = "#E3E9E7")
    ) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.background = ggplot2::element_rect(fill = "#F5F3EF", color = NA),
      plot.margin = ggplot2::margin(8, 8, 8, 8)
    )
}
