# Global variable declarations for R CMD check

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".data"))
}
