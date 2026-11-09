# Migration Guide: Encrypting the Blink Search Key-Value Store

The Blink Search **Key Value** backend used to store values as plaintext in
SQLite. Values are now encrypted at rest with **AES-256-GCM**, using the same
master password and payload format as the Emacs password vault
(`SECRET_VAULT_PASSWORD`). Existing rows are migrated in place, automatically,
the first time the store is opened with a configured password.

- Code: `lib/exhub/blink_search/backends/key_value_store.ex`
- Primitives: `lib/exhub/crypto/aes_gcm.ex` (shared with
  `Exhub.ResponseHandlers.ExhubVault`)
- Default database: `~/.emacs.d/blink-search-kv.db`
- Default table: `blink_search_kv`

## What Changes

| Aspect | Before | After |
|--------|--------|-------|
| Value column | plaintext | `enc:v1:` + `Base64(iv ‖ tag ‖ ciphertext)` |
| Key column | plaintext | unchanged (needed for `LIKE` substring search) |
| `set key value` | stores `value` | encrypts first; confirmation message shows only the key |
| bare `key` | copies stored text | decrypts, then copies the plaintext |
| Logs / Messages | value echoed on `set` | values are never printed, logged, or returned in errors |

Payload details: 12-byte random IV, 16-byte GCM tag, key =
`SHA-256(master password)`. A fresh IV is used per write, so re-setting the same
value produces different ciphertext. Decryption fails if the password changed or
the row was tampered with (GCM authentication).

## Prerequisites

1. `SECRET_VAULT_PASSWORD` must be set **for the running ExHub VM**.

   ```sh
   # Vault-based shell (preferred)
   ENVLESS_ROOT=~/GTD envless list | grep -i secret_vault   # key exists?
   # or plain environment variable, however you normally launch ExHub
   ```

   ExHub reads it at VM boot (`runtime.exs` → `config :exhub, :secret_vault`).
   After changing it, rotate/restart through your normal mechanism or call the
   `exhub_reload_keys` MCP tool (zero downtime) — never kill the beam to pick it
   up.

2. The password that encrypts the data is the **current** one. There is no
   re-keying tool: if you change the master password later, values written
   under the old password can no longer be decrypted (see
   [Rotating the master password](#rotating-the-master-password)).

## Behaviour When No Password Is Configured

Encryption is never silently downgraded:

| Operation | No `SECRET_VAULT_PASSWORD` |
|-----------|----------------------------|
| `set key value` | **Refused.** Nothing is written; a message reports the missing password. |
| `del key` | Works (no decryption involved). |
| bare `key` (legacy plaintext row) | Works — plaintext fallback is kept for un-migrated rows. |
| bare `key` (encrypted row) | Reports “Cannot decrypt …: master password is not configured”. |
| Migration | Deferred. No marker is written, so it runs on the first open after a password becomes available. |

## How the Migration Works

It is **automatic, lazy, idempotent, and transactional** — no CLI step and no
downtime.

1. Every database open calls `ensure_schema/2` (search and actions alike).
2. If a password is configured and the marker row is absent, the migration runs:
   - `BEGIN IMMEDIATE` (takes the write lock, so concurrent opens serialise),
   - selects up to **100** rows at a time whose value does not start with
     `enc:v1:` (`NULL` values are skipped — nothing to protect),
   - encrypts each one with a parameterised `UPDATE`,
   - repeats until no plaintext rows remain,
   - writes `encryption_version = 1` for the table into
     `blink_search_kv_meta` and `COMMIT`s.
3. Any failure (lock, I/O, unconfigured password) `ROLLBACK`s the whole batch,
   leaves the marker absent, logs a **single warning with no row data**, and the
   migration is retried on the next open. A failed migration never breaks
   searching or `set`/`del`.
4. Once the marker exists, later opens cost exactly one small indexed `SELECT`
   and nothing per keystroke. Already-encrypted rows are never re-encrypted.

Expected log line (in the `*exhub*` buffer):

```
[Blink-Search] Encrypted 42 legacy key-value row(s) in blink_search_kv
```

Only the row **count** is logged — never keys or values.

## Recommended Procedure

> ⚠️ Do not dump the database before or after migration. The pre-migration file
> is plaintext; treat every copy as a secret.

### 1. Back up the database (plaintext!)

Do this while nothing is searching (quit any active blink-search session so the
file is not in use):

```sh
cp ~/.emacs.d/blink-search-kv.db ~/.emacs.d/blink-search-kv.db.bak-$(date +%Y%m%d%H%M)
```

Record the current row count so you can verify the migration is complete:

```sh
sqlite3 ~/.emacs.d/blink-search-kv.db \
  "SELECT COUNT(*) FROM blink_search_kv;"          # rows before
```

### 2. Deploy the new code

Pick one of:

- **Zero-downtime (running prod release):**

  ```sh
  MIX_ENV=prod mix release --overwrite
  _build/prod/rel/exhub/bin/exhub rpc "IO.puts(Exhub.HotReload.reload_and_summarize())"
  ```

  (`exhub_hot_reload` MCP tool does the same.) Expect `N reloaded, 0 errors`.

- **Next natural restart:** nothing extra — the migration runs on first use.

### 3. Confirm the password is visible to the VM

```sh
_build/prod/rel/exhub/bin/exhub rpc "Exhub.Crypto.AesGcm.configured?()"
# => true
```

### 4. Trigger the migration

Any Blink Search opening the Key Value backend does it. To trigger it without
the UI (returns only a count, never keys):

```sh
_build/prod/rel/exhub/bin/exhub rpc \
  'length(Exhub.BlinkSearch.Backends.KeyValueStore.search_match("", %{}))'
```

Then check the `*exhub*` buffer for the `Encrypted N legacy key-value row(s)`
line.

### 5. Verify (no secret values are read)

```sh
DB=~/.emacs.d/blink-search-kv.db

# total rows — must match the count taken in step 1
sqlite3 "$DB" "SELECT COUNT(*) FROM blink_search_kv;"

# un-migrated plaintext rows — must be 0
sqlite3 "$DB" "SELECT COUNT(*) FROM blink_search_kv WHERE value NOT LIKE 'enc:v1:%';"

# per-table marker — expect blink_search_kv|1
sqlite3 "$DB" "SELECT * FROM blink_search_kv_meta;"

# sanity: nothing in the value column is still readable text
sqlite3 "$DB" "SELECT COUNT(*) FROM blink_search_kv WHERE value LIKE '%password%';"
```

Finally, fetch one key you know from Blink Search and confirm the value lands on
the kill ring intact. Then **delete the plaintext backup** (or move it into your
secret vault) once you are satisfied:

```sh
rm ~/.emacs.d/blink-search-kv.db.bak-YYYYMMDDHHMM
```

If your configured table differs from `blink_search_kv` (pushed from Emacs via
`update`), use that table name in the queries above; the marker row is keyed by
table name.

## Rollback

The format is self-describing: a value starting with `enc:v1:` is decrypted, and
anything else is treated as legacy plaintext. That makes rollback simple.

1. Restore the backup:

   ```sh
   mv ~/.emacs.d/blink-search-kv.db.bak-YYYYMMDDHHMM ~/.emacs.d/blink-search-kv.db
   ```

2. Deploy the previous code version (release + hot reload, or next restart).

The `blink_search_kv_meta` table is harmless: older code ignores it and never
removes it. Values written by the new code after migration are lost if you
restore the backup — take a fresh backup before rolling back if that matters.

## Rotating the Master Password

There is no bulk re-key command. Values are bound to the password that was
active when they were written:

- After a rotation, existing keys fail with
  `Cannot decrypt key <key>: decryption failed (wrong password or tampered data)`.
- Re-write each value once under the new password (`set key value` from Blink
  Search). The write path always encrypts with the current password.
- If a rotation is planned, either rotate **and** re-enter values in the same
  session, or keep the old password until the store has been re-written.
- Rotating also invalidates org-mode vault links encrypted with the old
  password — plan both together.

## Shared Databases and Other Tools

- The old Python/snails tooling (and any other program reading the same
  `blink_search_kv` / `kvstore` table) will see ciphertext after migration. If
  you still use such a tool, point it at a different database or upgrade it.
- Keys remain plaintext by design (substring lookup uses SQL `LIKE`), so key
  names such as `compass-redis-pass` still reveal *what* is stored even though
  the value does not.
- `NULL` values in legacy tables are left untouched by the migration.

## Troubleshooting

| Symptom | Cause | Action |
|---------|-------|--------|
| `Cannot store key X: master password is not configured` | `SECRET_VAULT_PASSWORD` unset for the VM | Set it and reload keys (`exhub_reload_keys`), then re-run `set`. |
| `Cannot decrypt key X: master password is not configured` | Password not visible to the VM | Same as above. |
| `Cannot decrypt key X: decryption failed (wrong password or tampered data)` | Password rotated, or DB restored from another profile | Re-enter the value under the current password, or restore the matching password. |
| `Cannot decrypt key X: invalid base64` / `ciphertext is too short` | Row was hand-edited or truncated | Re-enter the value. |
| `Key not found: X` although the row exists | Legacy `NULL` value | `set X <value>` to populate it. |
| `Encrypted 0 legacy key-value row(s)` never appears and values stay plaintext | No master password configured | Verify with `Exhub.Crypto.AesGcm.configured?()`; set the password. |
| Warning `Key-value encryption migration failed; will retry on next open` | Write lock held by another process, or the file/dir is read-only | Close other tools using the DB / fix permissions; the next open retries automatically. |
| Migration seems stuck at 0 rows | The table name in the pushed config differs from the one you queried | Check `value` counts in that specific table and its marker row. |

To force a retry after fixing an environment issue, the next open is enough; to
re-run deliberately (safe — encrypted rows are skipped):

```sh
sqlite3 ~/.emacs.d/blink-search-kv.db \
  "DELETE FROM blink_search_kv_meta WHERE table_name = 'blink_search_kv';"
```

## Security Notes

- Protects data **at rest**: backups, cloud-synced copies, a stolen disk, or a
  copied `.db` file. It does not protect against a live process with access to
  your environment or an unlocked Emacs (decrypted values go to the kill ring by
  design).
- The master password is never written to the database; only the IV, tag, and
  ciphertext are stored.
- GCM tags detect tampering. Modified rows fail closed (error message, no copy)
  rather than returning garbage.
- Migration and runtime logs contain row counts only — never keys or values.