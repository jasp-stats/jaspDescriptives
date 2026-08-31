# Ensure deterministic timezone and locale for plotting and time-based labels in CI
Sys.setenv(TZ = "UTC")
# Set a neutral C locale to avoid locale-dependent label differences
# Note: On some systems, "C" may be the right choice; adapt if needed.
tryCatch({
  Sys.setlocale(category = "LC_ALL", locale = "C")
}, error = function(e) {
  # If setting locale fails on some CI images, we continue (non-fatal)
  message("Warning: could not set locale to C: ", conditionMessage(e))
})
