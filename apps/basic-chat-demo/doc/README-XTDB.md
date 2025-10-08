# XTDB quick reference

This note collects a few common XTDB tasks for the basic-chat demo. It assumes
you are running inside `apps/basic-chat-demo/`.

## 1. Start a REPL with the same datastore

```bash
BASIC_CHAT_DATA_DIR=data \
BASIC_CHAT_XTDB_RESOURCE=resources/xtdb-test.edn \
clojure -M:repl
```

At the prompt, require the XT helper and start the node (the store automatically
reuses the configured RocksDB path and keeps logging quiet via the SLF4J NOP backend):

```clojure
(require '[app.xt :as xt])
(xt/start! (or (System/getenv "BASIC_CHAT_XTDB_RESOURCE") "resources/xtdb-test.edn")
          {:data-dir (System/getenv "BASIC_CHAT_DATA_DIR")})
```

## 2. Inspect entities mentioning Pat

```clojure
(require '[xtdb.api :as xt])

(def db (xt/db))

(xt/q db '{:find  [(pull ?e [:entity/id :entity/name :entity/type :entity/seen-count :entity/last-seen])]
          :where [[?e :entity/name "Pat"]]})
```

The result is a vector containing an entity map. You can transform the pull to
see linked relations:

```clojure
(xt/q db '{:find  [(pull ?r [:relation/id :relation/type :relation/src :relation/dst :relation/last-seen])]
          :where [[?r :relation/src [:entity/id #uuid "b65e9a08-09b1-46e1-af13-d1f33e730ead"]]]})
```

Replace the UUID with the one returned in the previous step.

## 3. Count all stored entities or relations

```clojure
(xt/q db '{:find  [(count ?e)]
          :where [[?e :entity/id _]]})

(xt/q db '{:find  [(count ?r)]
          :where [[?r :relation/id _]]})
```

To get both counts at once:

```clojure
(xt/q db '{:find  [(count ?e) (count ?r)]
          :where [[?e :entity/id _]
                  [?r :relation/id _]]})
```

## 4. Shut the node down

Always close the node when the session ends:

```clojure
(xt/stop!)
```

This keeps RocksDB from holding open file locks between runs.
