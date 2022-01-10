##
## Note: Many of these functions below were modified from the `reticulate` package
## But we've added a few as well and a fix for Windows.
##

print_py_config  <- function() {
    cat("R reticulate configuration\n")
    print(reticulate::py_config())
    cat(sprintf("R, Python tensorflow version: %s, %s\n",
                utils::packageVersion("tensorflow"),
                pypkg_version("tensorflow")))
    cat(sprintf("Python numpy version: %s\n",
                pypkg_version("numpy")))
    cat(sprintf("Python scipy version: %s\n",
                pypkg_version("scipy")))
    cat(sprintf("Python pillow version: %s\n",
                pypkg_version("PIL")))

}

pypkg_version  <- function(pkg) {
  pkg  <- reticulate::import(pkg)
  reticulate::py_get_attr(pkg, "__version__")
}

get_arch  <- function() {
  arch  <- Sys.info()["machine"]
  if (arch == "x86-64") arch <- "x86_64"
  arch
}
get_os <- function() {
  tolower(Sys.info()["sysname"])
}

installation_target_tag  <- function() {
  arch <- get_arch()
  os  <- get_os()
  paste0(os, "_", arch)
}

known_install_target  <- function() {
  arch <- get_arch()
  os  <- get_os()
  tag  <- paste0(os, "_", arch)
  tag %in% names(miniconda_installers)
}

tested_installation_combos  <- data.frame(
  os = c("darwin", "darwin", "windows", "linux"),
  arch = c("x86_64", "arm64", "x86_64", "x86_64"),
  python = rep("3.9.5", 4L),
  numpy = rep("1.19.5", 4L),
  scipy = rep("1.7.0", 4L),
  tensorflow = c("2.4.1", rep("2.5.0", 3L)),
  pillow = rep("8.2.0", 4L),
  installer = c(
    "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-MacOSX-x86_64.sh",
    "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-MacOSX-arm64.sh",
    "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-Windows-x86_64.exe",
    "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-Linux-x86_64.sh")
)

row.names(tested_installation_combos) <- with(tested_installation_combos, paste0(os, "_", arch))
miniconda_installer_url  <- function() {
  tag  <- installation_target_tag()
  result  <- tested_installation_combos[tag, ]$installer
}

install_miniconda <- function(path = reticulate::miniconda_path(),
                              update = TRUE,
                              force = FALSE) {
  tag  <- installation_target_tag()
  tested_combo <- tested_installation_combos[tag, ]
  if (is.na(tested_combo$os)) {
    stop(sprintf("install_miniconda: Unknown target %s.", tag))
  }

  reticulate:::check_forbidden_install("Miniconda")

  if (grepl(" ", path, fixed = TRUE))
    stop("cannot install Miniconda into a path containing spaces")

  # TODO: what behavior when miniconda is already installed?
  # fail? validate installed and matches request? reinstall?
  reticulate:::install_miniconda_preflight(path, force)

  # download the installer
  url <- miniconda_installer_url()
  installer <- reticulate:::miniconda_installer_download(url)

  # run the installer
  message("* Installing Miniconda -- please wait a moment ...")
  reticulate:::miniconda_installer_run(installer = installer, update = update, path = path)

  # validate the install succeeded
  ok <- reticulate:::miniconda_exists(path) && reticulate:::miniconda_test(path)
  if (!ok)
    stop("Miniconda installation failed [unknown reason]")

  # update to latest version if requested
  if (update)
    reticulate::miniconda_update(path)

  conda <- reticulate:::miniconda_conda(path)
  # create r-reticulate environment
  # just basic stuff for now
  py_pkgs <- c(
      paste0("python==", tested_combo$python),
      paste0("pillow==", tested_combo$pillow),
      paste0("numpy==", tested_combo$numpy),
      paste0("scipy==", tested_combo$scipy)
  )

  reticulate::conda_create(envname = "r-reticulate",
                            packages = py_pkgs,
                            conda = conda,
                            python_version = tested_combo$python
                            )
  message(sprintf("* Miniconda has been successfully installed at %s.", shQuote(path)))
  path

}

## This is needed only on Windows else pip will complain about SSL not being there.
conda_env_paths <- function(prefix, env = "r-reticulate") {
    if (!is.null(env)) {
        prefix <- file.path(prefix, "envs", env)
    }
    p <- c(
        normalizePath(file.path(prefix, "Library/mingw-w64/bin"), winslash="\\", mustWork = FALSE),
        normalizePath(file.path(prefix, "Library/usr/bin"), winslash="\\", mustWork = FALSE),
        normalizePath(file.path(prefix, "Library/bin"), winslash="\\", mustWork = FALSE),
        normalizePath(file.path(prefix, "Scripts"), winslash="\\", mustWork = FALSE),
        normalizePath(file.path(prefix, "bin"), winslash="\\", mustWork = FALSE)
    )
    paste(p, collapse = ";")
}


## See https://developer.apple.com/metal/tensorflow-plugin/
install_tensorflow  <- function() {
  tag  <- installation_target_tag()
  tested_combo <- tested_installation_combos[tag, ]
  if (is.na(tested_combo$os)) {
    stop(sprintf("install_miniconda: Unknown target %s.", tag))
  }

  miniconda_path <- reticulate::miniconda_path()
  conda  <- reticulate:::miniconda_conda(miniconda_path)

  if (tag == "darwin_arm64") {
    ## Install into r-reticulate environment
    args <- c("install", "-y", "-n r-reticulate", "-c apple", "tensorflow-deps")
    status <- system2(conda , args)
    if (status != 0)
      stop(sprintf("miniconda installation of tensorflow-deps failed [exit code %i]", status))

    python <- file.path(reticulate::miniconda_path(), "envs/r-reticulate/bin/python")
    args  <- c("-m pip", "install", "tensorflow-macos",
               paste0("pillow==", tested_combo$pillow),
               paste0("scipy==", tested_combo$scipy),
               paste0("numpy==", tested_combo$numpy))

    status <- system2(python, args)
    if (status != 0) {
      stop(sprintf("miniconda installation of tensorflow-macos failed [exit code %i]", status))
    } else {
      message("* Tensorflow for Mac M1 has been successfully installed.")
    }
  } else if (tag == "windows_x86_64") {
    ## Install into r-reticulate environment
    ## Save old path first before modifying
    old_path <- Sys.getenv("PATH")
    Sys.setenv(PATH = paste(conda_env_paths(miniconda_path), old_path, sep = ";"))
    tryCatch({
      reticulate::conda_install(
        envname = "r-reticulate",
        packages = paste0("tensorflow==", tested_combo$tensorflow),
        pip = TRUE,
        python_version = tested_combo$python)
      message("* Tensorflow for Windows x86_64 has been successfully installed.")
    },
    error = function(e) {
      message("* Tensorflow for Windows x86_64 could not be installed.")
    })
    ## Set old path back
    Sys.setenv(PATH = old_path)
  } else if (tag == "darwin_x86_64") {
    ## create r-reticulate environment and install into it
    args <- c("install", "-y", "-n r-reticulate",
              paste0("tensorflow==", tested_combo$tensorflow),
              paste0("pillow==", tested_combo$pillow),
              paste0("scipy==", tested_combo$scipy),
              paste0("numpy==", tested_combo$numpy))

    status <- system2(conda , args)

    if (status != 0) {
      stop(sprintf("miniconda installation of tensorflow failed [exit code %i]", status))
    } else {
      message("* Tensorflow for Mac x86_64 has been successfully installed.")
    }
  } else { ## linux
    tensorflow::install_tensorflow(
      version = tested_combo$tensorflow,
      conda_python_version = tested_combo$python
    )
  }
}


#' Install miniconda, create environment, and install tensorflow into it
install_miniconda_and_tensorflow <- function() {
  install_miniconda()
  install_tensorflow()
}
