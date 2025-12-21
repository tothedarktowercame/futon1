# Storage/Hydration Notes

## XTDB Touchpoints

- `app.xt`: wraps `xtdb.api/start-node`, `stop!`, `submit!`, `entity`, `q`.
- `app.store`: writes entity/relation docs to XT alongside Datascript, calls `xt/ensure-node!` and `xt/put-entity!` from `app.xt`.
- `graph_memory/types_registry`: loads `:type/*` docs from XT with `(xt/q '{:find [(pull ?t [*])] :where [[?t :type/id _]]})` and caches them; writes back via `xt/submit!` when merging aliases.
- `graph_memory/me_profile`, `app.command_service`, `app.focus`, `app.header`: query XT for salience, neighbors, profile summaries.
- `apps/api` handlers (`me`, `turns`, `types`, etc.) enrich request context with `:xt/db` and call the above command-service helpers.
- `apps/demo`: interactive CLI; slash/bang commands ultimately route through `app.command_service`/`app.store`, so every `/tail`, `/ego`, `!rel` hit the same XT node.
- Scripts such as `app.scripts.salience-demo` demonstrate persistence/rehydration by adding an entity, stopping, restarting, and showing the `:entity/seen-count` survive.

## Datascript ↔️ XTDB Hydration

1. On boot, `store-manager/start!` spins up Datascript (`graph_memory/main` schema) and starts XTDB via `app.xt/start!` (config `apps/graph-memory/resources/xtdb.edn`, data dir from `config.edn` or `$BASIC_CHAT_DATA_DIR`).
2. `app.store/hydrate!` replays XT documents into Datascript:
   - reads identity + version nodes (`:entity/id`, `:entity.version/*`) so `:entity/seen-count`, `:entity/last-seen`, pins, etc., are present in-memory.
   - writes rel docs (when present) so slash commands and focus headers can query them without hitting XT directly.
3. `graph_memory/types_registry/load-cache!` queries XT for `:type/id` docs and builds a cache with alias/parent info. `/types` renders this cache via Datascript, so those type stats persist across runs even if there are zero relation docs.
4. Bang commands (`!entity`, `!rel`, `!type merge`) update Datascript and immediately mirror changes to XT via `xt/submit!`. Next boot, hydration replays them back.

## Why `/tail` Can Be Empty While `/types` Isn’t

- `/types` reflects `graph_memory.types_registry`, which is hydrated from every `:type/*` doc stored in XTDB. Even a brand new profile includes the seeded `:me`, `:person`, etc., doc set, so `/types` always returns data.
- `/tail` only lists relation docs (hyperedges). If no `:relation/id` documents have been written yet (e.g., you haven’t run `!rel`), the endpoint legitimately returns “(none)” even though the XT node has other data (entities, types, profile metadata).

## Inspecting the Store Manually

In CIDER or `clojure -M:repl` (from `apps/graph-memory` so XT deps are on the classpath):

```clojure
(require '[app.xt :as xt]
         '[xtdb.api :as xtdb]
         '[graph-memory.types_registry :as types])

(xt/start! "resources/xtdb.edn" {:data-dir "../demo/data/default"})

(let [db (xtdb/db (xt/node))]
  ;; List entities (identity nodes)
  (clojure.pprint/pprint
   (xtdb/q db '{:find [(pull ?e [:entity/id :entity/name :entity/seen-count :entity/last-seen :entity/pinned?])]
               :where [[?e :entity/id _]]}))
  ;; List relation docs if any
  (clojure.pprint/pprint
   (xtdb/q db '{:find [(pull ?r [:relation/id :relation/type :relation/src :relation/dst :relation/last-seen])]
               :where [[?r :relation/id _]]})))

;; Type registry contents
(let [cache (types/load-cache!)]
  (println "types:" (count (:docs cache)))
  (doseq [doc (take 5 (:docs cache))]
    (clojure.pprint/pprint doc)))


(xt/stop!)
```

From the repo root you can run the same snippet via the new helper alias:

```
clojure -M:storage/inspect # optional XTDB_INSPECT_DATA=/path/to/data
```

See `apps/graph-memory/test/app/xt_storage_snippet_test.clj` for an automated version of this snippet (`cd apps/graph-memory && clojure -M:test`).

You can also point the salience demo at a directory (`SAL_DEMO_DIR=/path/to/data clojure -M -m app.scripts.salience-demo`) to see `:entity/seen-count` persist across starts.
