# XT stories

Recipes that make the XT mirror feel friendly from a REPL session. All examples below assume
`(require '[app.xt :as xt] '[app.xt.recipes :as recipes])` and that the XT node is running
(`(xt/start! "resources/xtdb.edn" {:data-dir "data/dev-xt"})`).

## Count and list entities

```clojure
(recipes/counts)
;; => {:entities 72, :relations 211}

(recipes/list-entities {:limit 5})
;; => [{:id #uuid"..." :name "Serena" :type :person :seen-count 9 :last-seen 1709608123000}
;;     {:id #uuid"..." :name "Futon v5" :type :project :seen-count 7 :pinned? true}
;;     ...]

(recipes/list-entities {:type :place :sort :name :limit 3})
;; => sorted subset scoped to `:place` entities.
```

## Inspect a single ego network

```clojure
(def serena (-> (recipes/list-entities {:sort :seen}) first :id))
(recipes/entity-relations serena {:limit 4})
;; => [{:id #uuid"..." :type :located-in :direction :outgoing
;;      :src {:name "Serena" ...}
;;      :dst {:name "Minneapolis" ...}}
;;     {:id #uuid"..." :type :with :direction :incoming
;;      :src {:name "Isabella" ...}
;;      :dst {:name "Serena" ...}}]

(recipes/entity-relations serena {:direction :incoming :limit 2})
;; only reverse edges
```

## Salience survives process restarts

Run the scripted story to prove that `:entity/seen-count` and `:entity/last-seen` persist through
an XT restart:

```bash
cd apps/graph-memory
clojure -M -m app.scripts.salience-demo
```

The script seeds an entity named "Serena", restarts XTDB against the same data directory, and
bumps the salience again so the next run shows a higher `:entity/seen-count`. Point
`SAL_DEMO_DIR` at a scratch directory if you want to watch the counter climb across multiple
shell sessions.

## Cleaning up

Call `(xt/stop!)` when you are finished or use `(xt/restart! <cfg> {:data-dir ...})` to point at a
fresh RocksDB directory.
