# exhub-vault — Org-mode Password Vault

## Overview

Exhub-vault provides an Emacs org-mode password vault feature integrated with the Exhub Elixir backend. Secrets are encrypted with **AES-256-GCM** on the server side and stored as org links in your org files, allowing you to manage sensitive information (API keys, passwords, tokens, etc.) directly within org-mode.

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│  Emacs (org-mode)                                       │
│                                                         │
│  [[exhub-vault:BASE64_CIPHERTEXT][My GitHub Token]]     │
│                                                         │
│  C-c v i  → insert secret                               │
│  C-c v c  → decrypt & copy                              │
│  C-c v s  → decrypt & show                              │
│  C-c C-o  → follow link (decrypt & copy)                │
└──────────────────────┬──────────────────────────────────┘
                       │ WebSocket
                       ▼
┌─────────────────────────────────────────────────────────┐
│  Exhub (Elixir)                                         │
│                                                         │
│  Exhub.ResponseHandlers.ExhubVault                      │
│  ├─ encrypt: AES-256-GCM (SHA-256 of master password)   │
│  └─ decrypt: AES-256-GCM                                │
│                                                         │
│  Key source: SECRET_VAULT_PASSWORD (same as SecretVault)│
└─────────────────────────────────────────────────────────┘
```

## Setup

### Prerequisites

- `SECRET_VAULT_PASSWORD` must be set (same password used by SecretVault for API keys)
- Exhub backend running and WebSocket connected

### Emacs Configuration

```elisp
(add-to-list 'load-path (expand-file-name "site-lisp/exhub" user-emacs-directory))
(require 'exhub-vault)

;; Enable vault keybindings globally
(exhub-vault-mode 1)

;; Or only in org-mode buffers
(add-hook 'org-mode-hook #'exhub-vault-mode)
```

### Exhub Backend Configuration

No additional configuration is needed. The vault reuses the existing `SECRET_VAULT_PASSWORD`:

```bash
export SECRET_VAULT_PASSWORD="your-master-password"
```

## Usage

### Insert a Secret

`M-x exhub-vault-insert-secret` or `C-c v i`

1. Enter a description (e.g., "GitHub Personal Access Token")
2. Enter the secret value (input is hidden)
3. An encrypted org link is inserted at point:

```
[[exhub-vault:RB3mVpLgrk8hN8xxCjtF8am2CBxD9faM6Gv7JP9e9thM][GitHub Personal Access Token]]
```

### Decrypt and Copy to Clipboard

- **On a vault link**: Move point to the link and press `C-c C-o` (org-open-at-point) or `C-c v c`
- **On selected text**: Select the base64 ciphertext and press `C-c v c`

The decrypted value is copied to the kill ring (clipboard).

### Decrypt and Show

`M-x exhub-vault-decrypt-and-show` or `C-c v s`

Displays the decrypted secret in the minibuffer without copying to clipboard.

### Example Org File

```org
* Secrets
** Development
[[exhub-vault:aBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789abcdef][Gitee AI API Key]]
[[exhub-vault:ZyXwVuTsRqPoNmLkJiHgFeDcBa0987654321fedcba][OpenAI API Key]]

** Production
[[exhub-vault:AbCdEf0123456789GhIjKlMnOpQrStUvWxYz][Production DB Password]]
[[exhub-vault:Zy9876543210XwVuTsRqPoNmLkJiHgFeDcBa][AWS Access Key]]
```

## Keybindings

| Key       | Command                        | Description                                      |
|-----------|--------------------------------|--------------------------------------------------|
| `C-c v i` | `exhub-vault-insert-secret`    | Insert an encrypted secret as an org link        |
| `C-c v c` | `exhub-vault-decrypt-and-copy` | Decrypt vault link/region and copy to clipboard  |
| `C-c v s` | `exhub-vault-decrypt-and-show` | Decrypt vault link/region and show in minibuffer |
| `C-c C-o` | `org-open-at-point`            | On vault links: decrypt and copy to clipboard    |

## Encryption Details

- **Algorithm**: AES-256-GCM (authenticated encryption)
- **Key Derivation**: SHA-256 hash of `SECRET_VAULT_PASSWORD` → 32-byte AES key
- **IV**: Random 12-byte IV generated per encryption
- **Format**: `Base64(IV || Tag || Ciphertext)`
- **Tag Length**: 16 bytes (128 bits)

The same master password is used for both SecretVault (server-side API key storage) and the org-mode vault. Secrets encrypted with one cannot be decrypted without the correct password.

## Security Notes

- The master password is never transmitted over WebSocket — only the encrypted ciphertext is sent
- Each encryption uses a unique random IV, so encrypting the same value twice produces different ciphertext
- AES-256-GCM provides both confidentiality and integrity (tamper detection)
- The org file stores only base64-encoded ciphertext — the plaintext never touches the disk in clear form
- Decrypted values are temporarily available in Emacs kill ring and minibuffer

## Components

### Elixir Backend

- **`Exhub.ResponseHandlers.ExhubVault`** — WebSocket handler for `exhub-vault` encrypt/decrypt operations
- **`Exhub.DefaultResponseHandler`** — Dispatches `exhub-vault` messages to the vault handler

### Emacs Client

- **`exhub-vault.el`** — Org-mode integration with org link support, keybindings, and vault commands
