## Summary

During pattern ingests, Futon1 logs repeated messages:

```
[store] skipped relation hydration for 12 item(s):
  c1fdbec593bb76ac9a562876254d225be5c292ce
  04e22ab3aabced990fb6f93dd9ee64bd80b0216f
  22a5c51aebc4b4c6cf77d902227d4a829daf5e7a
  fe7c2326177afc08ce1a65f3667f22dba401de09
  ffc373d40b021a429f01c7ede4c2f6c6d140ed6f
  3967014103315e6424a7edefe2bbb9dfe81cf660
  f8faa65d43bab7525ad7a608d4965b5bdabf89e3
  ffcd940c7ba12e1e34c3654cc61345a572092cc1
  d7f9f28b2dca0b241a1a16d56b1239940ae35ab6
  fbd05ea877ccb0d518ea43e415892f6368f4ef5d
  69d7bc39f0cf8ddf4382c135d5ed2c2f4c2168d4
  6b36ae96c7264582638d772a2d780b2ea6ae4b99
```

These appear to be dangling relations in open-world ingest (missing endpoints
or relation targets that no longer exist).

## Why it matters

Hydration skips indicate graph inconsistency. This undermines trust in the
open-world ingest and should be cleaned up or repaired to keep invariants
honest.

## Next steps

- Resolve each relation id to confirm missing endpoints.
- Decide whether to delete or repair them.
- Add a guard so bad relations are logged with more context (src/dst/type).
