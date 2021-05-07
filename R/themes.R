## Default Theme ===============================================================

theme_DataInt <- function(...) {
    theme_minimal() %+replace% 
        theme(
            plot.background = element_rect(fill = "#ffffff", colour = NA),
            text = element_text(family = "Roboto", colour = "#505050"),
            title = element_text(family = "Open Sans SemiBold", colour = "#505050"),
            plot.subtitle = element_text(size = 12, hjust = 0.5, family = "Open Sans",
                                         margin = margin(t = 0, b = 12, unit = "pt")),
            plot.title = element_text(hjust = 0.5, size = 14, family = "Open Sans Extrabold",
                                      margin = margin(t = 14, b = 14, unit = "pt")),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            panel.grid.major = element_line(color = "#cccccc", size = 0.2),
            panel.grid.minor = element_line(color = "#e6e6e6", size = 0.1),
            axis.title = element_text(size = 12, margin = margin(30, 10, 30, 10),
                                      colour = "#696969"),
            axis.text = element_text(size = 10, colour = "#696969", margin = margin(30, 10, 30, 10)),
            plot.caption = element_markdown(family = "Roboto", colour = "#828282",
                                            size = 9, lineheight = 1.25, hjust = 0, margin = margin(10, 0, 10, 0)),
            legend.position = "bottom",
            ...
        )
}

## Maps theme ==================================================================

theme_DataInt_maps <- function(...) {
    theme_minimal() %+replace% 
        theme(
            plot.background = element_rect(fill = "#ffffff", colour = NA),
            text = element_text(family = "Roboto", colour = "#505050"),
            title = element_text(family = "Open Sans", colour = "#505050"),
            plot.subtitle = element_text(size = 12, hjust = 0.5, family = "Roboto",
                                         margin = margin(t = 0, b = 12, unit = "pt")),
            plot.title = element_text(hjust = 0.5, size = 14, family = "Open Sans Extrabold",
                                      margin = margin(t = 14, b = 14, unit = "pt")),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
            panel.grid.minor = element_blank(),
            plot.caption = element_text(family = "Roboto", colour = "#828282",
                                        size = 9, lineheight = 1, hjust = 1, margin = margin(10, 0, 10, 0)),
            legend.position = "bottom",
            ...
        )
}
