# futon1a Design Sketch

A ground-up rewrite of futon1 organized around layered invariants.
The principle: **each layer is a gate; failures surface at the lowest failing layer.**

## Directory Structure

```
futon1a/
├── src/
│   ├── futon1a/
│   │   ├── core/
│   │   │   ├── xtdb.clj          # Layer 0: Durability
│   │   │   ├── identity.clj      # Layer 1: Identity
│   │   │   ├── entity.clj        # Layer 2: Integrity (entities)
│   │   │   ├── relation.clj      # Layer 2: Integrity (relations)
│   │   │   └── rehydrate.clj     # Layer 2: Integrity (startup)
│   │   │
│   │   ├── auth/
│   │   │   └── penholder.clj     # Layer 3: Authorization
│   │   │
│   │   ├── model/
│   │   │   ├── schema.clj        # Layer 4: Model schemas
│   │   │   ├── media.clj         # Layer 4: Media-specific rules
│   │   │   ├── pattern.clj       # Layer 4: Pattern language rules
│   │   │   └── validate.clj      # Layer 4: Model validation
│   │   │
│   │   ├── api/
│   │   │   ├── server.clj        # HTTP server setup
│   │   │   ├── handlers.clj      # Route handlers
│   │   │   └── errors.clj        # Error hierarchy & responses
│   │   │
│   │   └── diag/
│   │       ├── health.clj        # Health checks per layer
│   │       └── metrics.clj       # Observability
│   │
│   └── futon1a.clj               # Entry point
│
├── test/
│   ├── futon1a/
│   │   ├── core_test.clj         # Layer 0-2 invariant tests
│   │   ├── auth_test.clj         # Layer 3 tests
│   │   ├── model_test.clj        # Layer 4 tests
│   │   └── integration_test.clj  # End-to-end
│   │
│   └── invariants/
│       ├── durability_test.clj   # Prove writes are durable
│       ├── identity_test.clj     # Prove IDs are unique
│       ├── integrity_test.clj    # Prove rehydration is complete
│       └── error_test.clj        # Prove errors surface correctly
│
└── resources/
    └── schema.edn                # Datascript schema (if needed)
```

---

## Layer 0: Durability (`core/xtdb.clj`)

The foundation. Every write is confirmed durable before returning.

```clojure
(ns futon1a.core.xtdb
  "Layer 0: Durability guarantees.

   Invariants:
   - put! returns ONLY after XTDB confirms the write
   - get! returns ONLY data that is durably stored
   - No fire-and-forget, no async-without-callback
   - All failures throw, no silent logging")

(defn put!
  "Write entity to XTDB. Blocks until durable. Throws on failure."
  [node doc]
  (let [tx (xt/submit-tx node [[::xt/put doc]])]
    (xt/await-tx node tx)  ;; BLOCK until confirmed
    (when-not (xt/tx-committed? node tx)
      (throw (ex-info "XTDB write failed"
                      {:layer :durability
                       :op :put
                       :doc-id (:xt/id doc)})))
    doc))

(defn get!
  "Read entity from XTDB. Returns nil if not found, throws on error."
  [node id]
  (try
    (xt/entity (xt/db node) id)
    (catch Exception e
      (throw (ex-info "XTDB read failed"
                      {:layer :durability
                       :op :get
                       :id id}
                      e)))))

(defn delete!
  "Delete entity from XTDB. Blocks until confirmed."
  [node id]
  (let [tx (xt/submit-tx node [[::xt/delete id]])]
    (xt/await-tx node tx)
    (when-not (xt/tx-committed? node tx)
      (throw (ex-info "XTDB delete failed"
                      {:layer :durability
                       :op :delete
                       :id id})))))

(defn healthy?
  "Check XTDB is responsive and writable."
  [node]
  (try
    (let [probe-id (UUID/randomUUID)
          probe {:xt/id probe-id :probe/timestamp (System/currentTimeMillis)}]
      (put! node probe)
      (delete! node probe-id)
      true)
    (catch Exception _ false)))
```

---

## Layer 1: Identity (`core/identity.clj`)

UUID and external ID management. One entity per identity, period.

```clojure
(ns futon1a.core.identity
  "Layer 1: Identity uniqueness.

   Invariants:
   - Every entity has exactly one UUID
   - (source, external-id) maps to exactly one UUID
   - Duplicate external IDs are rejected BEFORE write
   - UUID collisions are impossible (random) or detected (deterministic)")

(defn uuid-for-external
  "Look up existing UUID for (source, external-id) pair.
   Returns nil if no mapping exists."
  [node source external-id]
  (let [result (xt/q (xt/db node)
                     '{:find [?id]
                       :where [[?e :entity/source ?src]
                               [?e :entity/external-id ?ext]
                               [?e :xt/id ?id]]
                       :in [?src ?ext]}
                     source external-id)]
    (when (> (count result) 1)
      (throw (ex-info "Duplicate external ID detected"
                      {:layer :identity
                       :source source
                       :external-id external-id
                       :duplicates (mapv first result)})))
    (ffirst result)))

(defn generate-uuid
  "Generate a UUID for a new entity.
   If external-id provided, checks for existing mapping first."
  [node {:keys [source external-id]}]
  (if external-id
    ;; Deterministic: check for existing, or create new
    (or (uuid-for-external node source external-id)
        (UUID/randomUUID))
    ;; Random: always unique
    (UUID/randomUUID)))

(defn assert-unique!
  "Verify no entity exists with this (source, external-id).
   Throws if duplicate would be created."
  [node source external-id]
  (when-let [existing (uuid-for-external node source external-id)]
    (throw (ex-info "Entity already exists for external ID"
                    {:layer :identity
                     :source source
                     :external-id external-id
                     :existing-id existing}))))
```

---

## Layer 2: Integrity - Entities (`core/entity.clj`)

CRUD with referential integrity and schema validation.

```clojure
(ns futon1a.core.entity
  "Layer 2: Entity integrity.

   Invariants:
   - Entities have required fields (id, type)
   - External IDs are unique per source (via Layer 1)
   - Writes are durable (via Layer 0)
   - Read-after-write always succeeds")

(defn create!
  "Create a new entity. Enforces identity uniqueness and durability."
  [node entity-spec]
  (let [{:keys [source external-id type]} entity-spec
        ;; Layer 1: Check uniqueness
        _ (when external-id
            (identity/assert-unique! node source external-id))
        ;; Generate ID
        id (identity/generate-uuid node entity-spec)
        ;; Build document
        doc (-> entity-spec
                (assoc :xt/id id
                       :entity/id id
                       :entity/type type
                       :entity/created-at (Instant/now))
                (cond-> external-id
                  (assoc :entity/external-id external-id
                         :entity/source source)))]
    ;; Layer 0: Durable write
    (xtdb/put! node doc)
    ;; Verify read-after-write
    (let [stored (xtdb/get! node id)]
      (when-not stored
        (throw (ex-info "Read-after-write failed"
                        {:layer :integrity
                         :op :create
                         :id id})))
      stored)))

(defn update!
  "Update existing entity. Entity must exist."
  [node id updates]
  (let [existing (xtdb/get! node id)]
    (when-not existing
      (throw (ex-info "Entity not found"
                      {:layer :integrity
                       :op :update
                       :id id})))
    (let [doc (merge existing updates {:entity/updated-at (Instant/now)})]
      (xtdb/put! node doc)
      (xtdb/get! node id))))

(defn get-by-external
  "Fetch entity by external ID. Returns nil if not found.
   Throws if multiple entities match (Layer 1 violation)."
  [node source external-id]
  (when-let [id (identity/uuid-for-external node source external-id)]
    (xtdb/get! node id)))
```

---

## Layer 2: Integrity - Rehydration (`core/rehydrate.clj`)

Startup verification. All-or-nothing loading.

```clojure
(ns futon1a.core.rehydrate
  "Layer 2: Rehydration integrity.

   Invariants:
   - Startup succeeds ONLY if all entities load correctly
   - No silent stubs or partial loads
   - Failure is loud and blocks startup
   - Reports exactly what failed and why")

(defn verify-entity
  "Verify a single entity loaded correctly. Returns {:ok true} or {:ok false :reason ...}"
  [node entity]
  (let [id (:xt/id entity)]
    (cond
      (nil? id)
      {:ok false :reason :missing-id :entity entity}

      (nil? (:entity/type entity))
      {:ok false :reason :missing-type :id id}

      :else
      {:ok true :id id})))

(defn verify-relation
  "Verify a relation's endpoints exist."
  [node relation]
  (let [{:keys [relation/src relation/dst]} relation]
    (cond
      (nil? (xtdb/get! node src))
      {:ok false :reason :missing-src :src src :relation relation}

      (nil? (xtdb/get! node dst))
      {:ok false :reason :missing-dst :dst dst :relation relation}

      :else
      {:ok true})))

(defn load-all!
  "Load all entities from XTDB. Returns {:ok true :count N} or throws."
  [node]
  (let [all-entities (xt/q (xt/db node)
                           '{:find [(pull ?e [*])]
                             :where [[?e :entity/id _]]})
        results (map #(verify-entity node (first %)) all-entities)
        failures (filter #(false? (:ok %)) results)]
    (when (seq failures)
      (throw (ex-info "Rehydration failed: invalid entities"
                      {:layer :integrity
                       :op :rehydrate
                       :failures failures
                       :failure-count (count failures)
                       :total-count (count all-entities)})))
    {:ok true
     :entity-count (count all-entities)}))

(defn startup!
  "Full startup sequence. Throws if any layer fails."
  [node]
  (println "[futon1a] Starting up...")

  ;; Layer 0: Verify XTDB is healthy
  (when-not (xtdb/healthy? node)
    (throw (ex-info "XTDB not healthy" {:layer :durability})))
  (println "[futon1a] Layer 0 (durability): OK")

  ;; Layer 2: Load and verify all entities
  (let [result (load-all! node)]
    (println (format "[futon1a] Layer 2 (integrity): OK - %d entities"
                     (:entity-count result))))

  ;; TODO: Layer 3, 4 checks

  (println "[futon1a] Startup complete")
  {:ok true})
```

---

## API Error Hierarchy (`api/errors.clj`)

Errors map to the layer where they occurred.

```clojure
(ns futon1a.api.errors
  "Error hierarchy matching invariant layers.

   Each layer has distinct HTTP status codes and error shapes.
   Clients can immediately identify which layer failed.")

(def error-codes
  {:durability   {:status 503 :code "STORAGE_UNAVAILABLE"}
   :identity     {:status 409 :code "DUPLICATE_ENTITY"}
   :integrity    {:status 500 :code "INTEGRITY_VIOLATION"}
   :auth         {:status 403 :code "NOT_AUTHORIZED"}
   :validation   {:status 400 :code "VALIDATION_FAILED"}
   :not-found    {:status 404 :code "NOT_FOUND"}})

(defn layer-error->response
  "Convert an ex-info with :layer key to HTTP response."
  [ex]
  (let [data (ex-data ex)
        layer (:layer data)
        {:keys [status code]} (get error-codes layer {:status 500 :code "UNKNOWN"})]
    {:status status
     :body {:error code
            :layer (name layer)
            :message (ex-message ex)
            :details (dissoc data :layer)}}))

(defn wrap-errors
  "Ring middleware to catch layer errors and convert to responses."
  [handler]
  (fn [request]
    (try
      (handler request)
      (catch clojure.lang.ExceptionInfo e
        (if (:layer (ex-data e))
          (layer-error->response e)
          (throw e))))))
```

---

## Handler Example (`api/handlers.clj`)

Clean handlers that let layer errors bubble up.

```clojure
(ns futon1a.api.handlers
  "API handlers. Thin wrappers that let layer errors surface.")

(defn create-entity
  "POST /entity - Create new entity."
  [{:keys [node body]}]
  ;; No try/catch here - let layer errors bubble to middleware
  (let [entity (entity/create! node body)]
    {:status 201
     :body entity}))

(defn get-entity
  "GET /entity/:id - Fetch entity by ID."
  [{:keys [node path-params]}]
  (let [id (parse-uuid (:id path-params))]
    (if-let [entity (xtdb/get! node id)]
      {:status 200 :body entity}
      {:status 404 :body {:error "NOT_FOUND" :id id}})))

(defn get-by-external
  "GET /entity/external/:source/:external-id"
  [{:keys [node path-params]}]
  (let [{:keys [source external-id]} path-params]
    (if-let [entity (entity/get-by-external node source external-id)]
      {:status 200 :body entity}
      {:status 404 :body {:error "NOT_FOUND"
                          :source source
                          :external-id external-id}})))
```

---

## Invariant Tests (`test/invariants/`)

Tests that PROVE each invariant holds.

```clojure
;; durability_test.clj
(deftest write-is-durable
  (let [node (test-node)
        entity {:type "test" :name "Alice"}
        created (entity/create! node entity)]
    ;; Restart node (simulate crash)
    (let [node2 (restart-node node)]
      ;; Data must survive
      (is (= created (xtdb/get! node2 (:xt/id created)))))))

(deftest failed-write-throws
  (let [node (broken-node)]  ;; Node that rejects writes
    (is (thrown-with-msg? ExceptionInfo #"XTDB write failed"
          (entity/create! node {:type "test"})))))

;; identity_test.clj
(deftest external-id-is-unique
  (let [node (test-node)]
    (entity/create! node {:type "user" :source "github" :external-id "123"})
    (is (thrown-with-msg? ExceptionInfo #"Entity already exists"
          (entity/create! node {:type "user" :source "github" :external-id "123"})))))

(deftest duplicate-detection-on-lookup
  (let [node (test-node-with-duplicates)]  ;; Pre-seeded with duplicates
    (is (thrown-with-msg? ExceptionInfo #"Duplicate external ID"
          (identity/uuid-for-external node "github" "123")))))

;; integrity_test.clj
(deftest rehydration-fails-on-invalid-entity
  (let [node (test-node-with-corrupt-entity)]
    (is (thrown-with-msg? ExceptionInfo #"Rehydration failed"
          (rehydrate/startup! node)))))

;; error_test.clj
(deftest durability-error-is-503
  (let [resp (api-call (broken-node) :post "/entity" {:type "test"})]
    (is (= 503 (:status resp)))
    (is (= "STORAGE_UNAVAILABLE" (get-in resp [:body :error])))))

(deftest duplicate-error-is-409
  (let [node (test-node)]
    (api-call node :post "/entity" {:type "user" :source "gh" :external-id "1"})
    (let [resp (api-call node :post "/entity" {:type "user" :source "gh" :external-id "1"})]
      (is (= 409 (:status resp)))
      (is (= "DUPLICATE_ENTITY" (get-in resp [:body :error]))))))
```

---

## Migration Path

1. **Build futon1a** with passing invariant tests
2. **Run both** futon1 and futon1a in parallel (different ports)
3. **Migrate data** via XTDB (same underlying store, or export/import)
4. **Switch clients** one by one
5. **Retire futon1** when all clients migrated

---

## Current State: What futon1 Now Enforces (after Codex fixes)

### Enforced in Code

1. **Durable write verification**: writes must land in XTDB before "success" returned
2. **No silent success**: failed invariants throw; UI gets an error
3. **Transactional lyrics upsert**: lyrics entity + relation written as one event (`:media/lyrics-upsert`)
4. **External-id vs UUID discipline**: custom string IDs rejected; external IDs map to UUIDs; conflicts error
5. **Penholder enforcement**: write events must have authorized penholder for model
6. **Startup invariant check**: invariants verified at boot; failures surfaced immediately
7. **Volatile vs durable divergence warning** at startup
8. **Type count baseline** and "nondecreasing" warning at startup

### Discussed But Not Fully Enforced Yet

1. **Persistence is core**: always stop-the-line if durability fails (beyond warnings)
2. **Read-after-write content hash verification** before UI success (stronger than existence)
3. **Session-local journaling** of user-typed lyrics as recovery fallback
4. **Single chokepoint** for all persistence writes (no bypasses or fallbacks)
5. **Stronger per-type census** with monotonic baseline (durable vs volatile comparisons)
6. **Guard at shutdown**: run durable census, emit failure if it regresses
7. **Explicit proof-commit entity** in XTDB after restores/large imports
8. **Hot-reload and rehydrate sanity checks** (ensure XTDB and Datascript stay in lockstep)

---

## Key Differences: futon1 (current) vs futon1a (proposed)

| Aspect | futon1 (after fixes) | futon1a |
|--------|---------------------|---------|
| Durability | Verified, but warnings possible | Hard failure, no warnings - success or throw |
| External ID | Conflicts error | Same, plus ambiguity detection on lookup |
| Rehydration | Startup check, warnings | All-or-nothing, blocks startup on any failure |
| Errors | Improved, still some scatter | Strict layer hierarchy, correct layer always surfaces |
| Invariants | Enforced at boot, some optional | All mandatory, no optional flag |
| Datascript | Primary with XTDB mirror | XTDB-only (cache added later if needed) |
| Chokepoint | Multiple paths exist | Single write path, no bypasses |
| Read-after-write | Existence check | Content hash verification |
| Shutdown guard | Warning on regression | Hard failure on regression |

---

## Open Questions

1. **Datascript**: Keep as read cache, or XTDB-only?
   - Pro cache: Fast queries
   - Con cache: Two stores = sync bugs
   - Option: Start XTDB-only, add cache later if needed

2. **Event log**: Keep append-only log, or rely on XTDB history?
   - XTDB has built-in bitemporal history
   - May not need separate event log

3. **Profiles**: Keep multi-profile support, or single store?
   - Simplify first, add profiles later if needed

4. **Compatibility**: Wire-compatible with futon1 API, or clean break?
   - Recommend: Same REST shapes, but stricter error codes
