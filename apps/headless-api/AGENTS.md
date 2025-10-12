# Headless API Agent Notes

This service exists purely as a thin HTTP wrapper for the Futon runtime. No
business logic or data modelling should live here – application behaviour
belongs in the feature apps (e.g. `apps/basic-chat-demo` for protocol flows or
`apps/open-world-ingest` for NL processing and persistence). Whenever you need a
new capability, add it to the owning app first and call into it from the API.
Keep handlers limited to request parsing, delegation, and response formatting
so the surface stays hygienic and easy to audit.
