import Config

# Disable tzdata auto-update in production.
# The bundled data shipped with the tzdata package is used instead.
# This prevents :tzdata_release_updater from crashing on network timeouts
# when the host has no outbound internet access or IANA is unreachable.
config :tzdata, :autoupdate, :disabled
