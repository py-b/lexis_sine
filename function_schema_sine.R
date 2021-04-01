library(tibble)
library(ggplot2)
library(ggthemes)
library(dplyr)


# DATA --------------------------------------------------------------------

annees_full <- 1994:2033

couleurs <- setNames(
  c("#666666", "#1B9E77", "#D95F02", "#7570B3", "#E7298A",
    "#66A61E", "#A6761D", "#1F78B4", "#E6AB02", "#E31A1C"),
  as.character(seq(1994, 2033, by = 4))
)

ggplot_themes <-
  c("base", "bw", "calc", "classic", "clean", "economist", "excel",
    "gdocs", "fivethirtyeight", "grey", "hc", "light", "linedraw", "minimal",
    "pander", "solarized", "tufte")

data_full <-
  tibble(
    annee = annees_full,
    age = rep(
      c(0, 5, NA, 3),
      length.out = max(annees_full) - min(annees_full) + 1
    ),
    cohorte = as.character(annee - age)
  ) %>%
  filter(!is.na(age), cohorte != "1990")


# GRAPH --------------------------------------------------------------

schema_sine <- function(
  annee1,
  annee2,
  cohorte_proj1, # première cohorte non commencée
  cohorte_drop = NULL, # pour masquer une cohorte sur le graphique
  theme = "minimal" # ?ggplot2::theme_minimal
) {

  if (missing(cohorte_proj1)) cohorte_proj1 <- 2100

  ## toutes années de toutes cohortes présentes dans années sélectionnées
  cohortes <-
    data_full %>%
    filter(annee %>% between(annee1, annee2)) %>%
    `[[`("cohorte") %>%
    unique()
  data_graph <- data_full %>% filter(cohorte %in% cohortes)

  ## ajout points fictifs en 2100 pour prolongation des traits obliques
  data_graph <-
    bind_rows(
      data_graph,
      tibble(
        cohorte = cohortes,
        annee = 2100,
        age = annee - as.double(cohorte)
      )
    ) %>%
    mutate(projet = as.numeric(cohorte) >= cohorte_proj1)

  if (!is.null(cohorte_drop))
    data_graph <-
      data_graph %>%
      filter(!cohorte %in% as.character(cohorte_drop))

  ## plot

  # initialisation
  ggplot(
    data = data_graph,
    mapping = aes(x = annee)
  ) +
  # zoom sur années sélectionnées
  coord_cartesian(
    xlim = c(annee1 + 0.3, annee2 - 0.3),
    ylim = c(-0.3, 7.1)
  ) +
  # barres verticales
  geom_bar(
    mapping = aes(weight = age, fill = cohorte),
    position = "identity",
    width = 0.05,
    show.legend = FALSE
  ) +
  # traits obliques (cohorte en cours ou finie)
  geom_line(
    data = data_graph %>% filter(!projet),
    mapping = aes(y = age, colour = cohorte),
    linetype = "solid",
    size = .3
  ) +
  # traits obliques (cohorte projet)
  geom_line(
    data = data_graph %>% filter(projet),
    mapping = aes(y = age, colour = cohorte),
    linetype = "dashed"
  ) +
  # texte "(projet)"
  geom_text(
    data = data_graph %>% filter(projet, age == 0),
    mapping = aes(y = age - 0.4, colour = cohorte),
    label = "(projet)",
    size = 3,
    show.legend = FALSE
  ) +
  # interrogation entreprise (carré)
  geom_point(
    mapping = aes(y = age, colour = cohorte),
    size = 5,
    shape = 15
  ) +
  # interrogation entreprise (point interrogation)
  geom_point(
    mapping = aes(y = age),
    size = 3,
    shape = "?",
    colour = "white"
  ) +
  # axe x
  scale_x_continuous(
    name = "",
    breaks = annee1:annee2,
    minor_breaks = NULL
  ) +
  # axe y
  scale_y_continuous(
    name = "",
    breaks = c(0, 3, 5),
    minor_breaks = 0:7,
    labels = c("Année\nde la\ncréation", "3 ans", "5 ans")
  ) +
  # couleur séries (barres verticales)
  scale_fill_manual(
    values = couleurs[cohortes]
  ) +
  # couleur séries (autres couches)
  scale_colour_manual(
    values = couleurs[cohortes],
    name = "Cohorte"
  ) +
  # apparence générale (ggtheme)
  eval(parse(text = paste0("theme_", theme, "()"))) +
  # cosmétique axes et légende
  theme(
    axis.text.x = element_text(angle = 50, hjust = 1, vjust = 0.5),
    legend.key.size = unit(0.6, "cm"),
    legend.key.width = unit(1, "cm"),
    axis.line = element_line(
      colour = "gray19",
      arrow = arrow(angle = 30, unit(0.07, "inches"))
    )
  ) +
  # note de pied de page
  labs(
    caption = "? = interrogation des entreprises"
  )

}
