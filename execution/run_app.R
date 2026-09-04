# execution/run_app.R
# Lance l'application Shiny depuis la racine projet
# version du 20.07.26

# Vérification des packages requis ------
required_packages <- c("shiny", "bslib", "yaml", "DT", "processx", "here", "dplyr", "jsonlite", "readODS")

cat("Vérification des packages requis...\n")
missing_packages <- character(0)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
    cat("  ✗", pkg, "- MANQUANT\n")
  } else {
    cat("  ✓", pkg, "\n")
  }
}

if (length(missing_packages) > 0) {
  cat("\n❌ Packages manquants détectés!\n\n")
  cat("Installez-les avec la commande suivante:\n")
  cat(paste0('install.packages(c("', paste(missing_packages, collapse = '", "'), '"))\n\n'))
  
  response <- readline(prompt = "Voulez-vous installer automatiquement ces packages? (o/n): ")
  
  if (tolower(substr(response, 1, 1)) == "o") {
    cat("\nInstallation en cours...\n")
    install.packages(missing_packages, repos = "https://cloud.r-project.org")
    cat("\n✓ Installation terminée!\n\n")
  } else {
    stop("Installation annulée. Veuillez installer les packages manuellement.")
  }
}

cat("\n✓ Tous les packages sont disponibles\n\n")

# Vérifier que les fichiers requis existent -------
cat("Vérification de la structure du projet...\n")

required_files <- c(
  file.path("execution", "app.R"),
  file.path("engine", "load_config.R"),
  file.path("engine", "validate_config.R"),
  file.path("study", "config", "config.yml")
)

all_exist <- TRUE
for (file in required_files) {
  if (file.exists(file)) {
    cat("  ✓", file, "\n")
  } else {
    cat("  ✗", file, "- INTROUVABLE\n")
    all_exist <- FALSE
  }
}

if (!all_exist) {
  cat("\n❌ Certains fichiers requis sont manquants.\n")
  cat("Assurez-vous d'être dans le répertoire racine du projet.\n\n")
  stop("Fichiers manquants détectés.")
}

cat("\n✓ Structure du projet validée\n\n")

# Créer les répertoires nécessaires ------
cat("Création des répertoires de travail...\n")
dirs_to_create <- c(
  "study/config/runs",
  "study/outputs/logs"
)

for (dir in dirs_to_create) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    cat("  ✓ Créé:", dir, "\n")
  } else {
    cat("  ✓ Existe:", dir, "\n")
  }
}

# Lancer l'application ------
tryCatch({
  shiny::runApp(
    appDir = "execution",
    launch.browser = TRUE,
    host = "0.0.0.0",
    port = 3838
  )
}, error = function(e) {
  cat("\n❌ Erreur lors du lancement de l'application:\n")
  cat(e$message, "\n\n")
  stop("Lancement échoué.")
})
