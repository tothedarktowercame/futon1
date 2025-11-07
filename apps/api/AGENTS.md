# API module architecture

## Overview

The `apps/api` module exposes the Futon pipeline over HTTP. It shares all
domain logic (`app.*` namespaces) with the in-process client via the
`apps/graph-memory` library so that both interfaces stay thin and consistent.

```
apps/api               apps/graph-memory
└─ src/api             └─ src/app
   ├─ server.clj          ├─ slash.clj      ; shared slash adapter
   └─ handlers/           ├─ command_service.clj
      graph.clj           │     ▲
      me.clj              │     │ shared domain logic
      turns.clj           │     │
      types.clj           │     │
                          └─ store_manager.clj ; persistence wiring
```

* **`app.command-service`** – single implementation of entity, relation,
  profile, and type operations. Both the HTTP handlers and the demo/client slash
  commands depend on this namespace.
* **`api.handlers.*`** – thin HTTP adapters. They accept a Ring-style request,
  pull headers/query parameters, and delegate to `app.command-service`,
  returning JSON-friendly maps.
* **`app.slash`** – CLI adapter that parses `/slash` commands, calls the shared
  service, and prints human-readable responses. (The demo CLI uses it directly.)
* **`app.store-manager`** – common persistence/configuration layer used by both
  transports to manage XTDB/Datascript state and profile documents. The manager
  advertises the current data root via the `basic-chat.data-root` system
  property so auxiliary modules (e.g. `basic-chat/v6` ingest) can place their
  data alongside the active profile directory.

## Guiding principles

1. **One implementation, multiple adapters.** Any new command or behavior
   should be implemented in `app.command-service` once, then exposed via HTTP
   and the CLI by adding minimal adapter code.
2. **Adapters stay thin.** HTTP handlers and slash commands must avoid business
   logic. They should validate inputs, call the shared service, and format the
   response.
3. **Shared tests.** When adding commands, include coverage in
   `apps/graph-memory/test/app/slash_test.clj` (for CLI output) and extend API
   tests if the HTTP surface changes.
4. **Protocol reuse.** The default protocol remains `basic-chat/v6`. Changes to
   processing should happen in the ingest/open-world modules, not in the
   adapters.

Following this structure keeps behavior in sync between interactive sessions
and automated clients while avoiding duplicate implementations.
