# SecretVault Integration Guide for Exhub

This document describes how to use SecretVault for secure secret management in the Exhub project.

## Table of Contents

1. [Overview](#overview)
2. [Installation](#installation)
3. [Configuration](#configuration)
4. [Creating and Managing Secrets](#creating-and-managing-secrets)
5. [Using Secrets in Code](#using-secrets-in-code)
6. [Environment Variables](#environment-variables)
7. [Security Best Practices](#security-best-practices)
8. [CI/CD Integration](#cicd-integration)
9. [Troubleshooting](#troubleshooting)

## Overview

SecretVault is an Elixir library for securely storing and managing secrets. It encrypts secrets at rest using a master password and provides runtime access to these secrets without exposing them in the codebase or compiled artifacts.

### Why SecretVault?

- **Encrypted at rest**: Secrets are encrypted using AES-256-GCM
- **No secrets in Git**: Only encrypted `.vault_secret` files are committed
- **No compile-time leaks**: Secrets are loaded at runtime, not compiled into `app.src`
- **Environment separation**: Each `MIX_ENV` has its own isolated vault
- **Version control friendly**: Individual secrets are separate files

## Installation

SecretVault is already added to the dependencies in `mix.exs`:

```elixir
defp deps do
  [
    # ... other deps
    {:secret_vault, "~> 1.0"}
  ]
end
```

Install dependencies:

```bash
mix deps.get
```

## Configuration

### 1. Environment Variable

Set the master password for SecretVault:

```bash
export SECRET_VAULT_PASSWORD="your-secure-master-password"
```

**Important**: Choose a strong password. This password encrypts all your secrets. If you lose it, your secrets cannot be recovered.

### 2. Application Configuration

The configuration is already set up in `config/config.exs`:

```elixir
config :secret_vault, :exhub,
  default: [
    password: System.get_env("SECRET_VAULT_PASSWORD", "")
  ],
  secrets_dir: "priv/secrets"
```

### 3. Runtime Configuration

The `config/runtime.exs` file loads secrets from SecretVault at application startup. This ensures secrets are never compiled into the application.

## Creating and Managing Secrets

### Directory Structure

Secrets are stored in the following structure:

```
priv/secrets/
├── dev/
│   └── default/
│       ├── gitee_api_key.vault_secret
│       ├── openai_api_key.vault_secret
│       └── ...
├── test/
│   └── default/
└── prod/
    └── default/
```

### Creating Secrets

#### Method 1: Direct Insertion (Command Line)

Create a secret by providing the value directly:

```bash
# Set the password environment variable first!
export SECRET_VAULT_PASSWORD="your-master-password"

# Create a secret
mix scr.insert dev gitee_api_key "sk-abc123xyz789"
```

#### Method 2: Interactive Editor

Create a secret using your preferred editor:

```bash
export SECRET_VAULT_PASSWORD="your-master-password"
mix scr.create dev gitee_api_key
```

This will open your `$EDITOR` to input the secret value.

### Required Secrets for Exhub

Create the following secrets for full functionality:

```bash
# BurnCloud API (default OpenAI-compatible provider)
mix scr.insert dev burncloud_api_key "your-burncloud-api-key"

# Gitee AI API (alternative provider)
mix scr.insert dev gitee_api_key "your-gitee-ai-api-key"

# OpenAI API (for OpenAI-compatible endpoints)
mix scr.insert dev openai_api_key "your-openai-api-key"

# Gitee Cookie (for GiteeCat API access)
mix scr.insert dev gitee_cookie "your-gitee-cookie-string"

# Optional: Provider-specific keys
mix scr.insert dev siliconflow_api_key "your-siliconflow-key"
mix scr.insert dev mistral_api_key "your-mistral-key"
mix scr.insert dev codestral_api_key "your-codestral-key"
mix scr.insert dev anthropic_api_key "your-anthropic-key"
mix scr.insert dev groq_api_key "your-groq-key"
mix scr.insert dev gemini_api_key "your-gemini-key"
mix scr.insert dev cohere_api_key "your-cohere-key"
mix scr.insert dev samba_api_key "your-samba-key"
```

### Editing Secrets

Edit an existing secret:

```bash
export SECRET_VAULT_PASSWORD="your-master-password"
mix scr.edit dev gitee_api_key
```

### Viewing Secrets

To view a secret:

```bash
export SECRET_VAULT_PASSWORD="your-master-password"
mix scr.show dev gitee_api_key
```

### Listing Secrets

List all secrets in the current environment:

```bash
mix scr.list dev
```

### Deleting Secrets

Delete a secret using standard file operations:

```bash
rm priv/secrets/dev/default/gitee_api_key.vault_secret
```

Or rename/move:

```bash
mv priv/secrets/dev/default/old_key.vault_secret \
   priv/secrets/dev/default/new_key.vault_secret
```

### Auditing Secrets

Check the quality and security of your secrets:

```bash
mix scr.audit
```

This will report:
- Weak or common passwords
- Duplicate secrets
- Empty secrets
- Other security issues

## Using Secrets in Code

### In Configuration Files

Use `runtime_secret!/2` in `config/runtime.exs`:

```elixir
import Config
import SecretVault, only: [runtime_secret!: 2]

config :my_app, :api_key, runtime_secret!(:exhub, "api_key")
```

### In Application Code

Access secrets at runtime after they've been loaded:

```elixir
# If using persistent_term storage (recommended)
# First fetch the config
{:ok, config} = SecretVault.Config.fetch_from_current_env(:exhub)
# Then retrieve a specific secret
secret = SecretVault.Storage.fetch_from_persistent_term!(config, "api_key")

# Or simply use from application environment (set by runtime.exs)
Application.get_env(:exhub, :giteeai_api_key)
```

### In Modules

```elixir
defmodule MyModule do
  def do_something do
    # Access from application env (set by runtime.exs)
    api_key = Application.fetch_env!(:exhub, :giteeai_api_key)
    # ... use api_key
  end
end
```

## Environment Variables

### Required

| Variable | Description | Example |
|----------|-------------|---------|
| `SECRET_VAULT_PASSWORD` | Master password for decrypting secrets | `my-super-secret-password` |

### Optional

| Variable | Description | Default |
|----------|-------------|---------|
| `MIX_ENV` | Elixir environment | `dev` |

### Local Development Setup

Create a `.env` file (not committed to Git):

```bash
# .env
export SECRET_VAULT_PASSWORD="your-dev-password"
export MIX_ENV=dev
```

Source it before working:

```bash
source .env
mix phx.server
```

## Security Best Practices

### 1. Master Password

- Use a strong, unique password (20+ characters)
- Don't reuse passwords from other services
- Store the master password in a password manager
- Never commit the password to Git

### 2. Secret Rotation

Regularly rotate your API keys:

```bash
# Generate new key from provider dashboard
# Update the secret
mix scr.edit dev gitee_api_key
# Restart application
```

### 3. Git Hygiene

**CRITICAL**: Never commit unencrypted secrets!

The following patterns should be in `.gitignore`:

```gitignore
# Environment files
.env
.env.local
.env.*.local

# Plain text secrets (just in case)
*.key
*.secret
secrets.txt
```

**DO commit** the encrypted `.vault_secret` files:

```bash
# These SHOULD be committed
git add priv/secrets/dev/default/*.vault_secret
git commit -m "Add encrypted secrets"
```

### 4. Different Environments

Each `MIX_ENV` has its own vault:

```bash
# Development secrets
export MIX_ENV=dev
mix scr.insert dev api_key "dev-key"

# Production secrets (different password recommended)
export MIX_ENV=prod
export SECRET_VAULT_PASSWORD="prod-master-password"
mix scr.insert prod api_key "prod-key"
```

### 5. Access Control

Limit who has access to:
- The master password
- The production secrets directory
- Environment variable configuration

## CI/CD Integration

### GitHub Actions Example

```yaml
name: Deploy

on:
  push:
    branches: [main]

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Elixir
        uses: erlef/setup-beam@v1
        with:
          elixir-version: '1.16'
          otp-version: '26'
      
      - name: Install dependencies
        run: mix deps.get
      
      - name: Audit secrets
        run: mix scr.audit
        env:
          SECRET_VAULT_PASSWORD: ${{ secrets.SECRET_VAULT_PASSWORD }}
      
      - name: Build release
        run: mix release
        env:
          SECRET_VAULT_PASSWORD: ${{ secrets.SECRET_VAULT_PASSWORD }}
      
      - name: Deploy
        run: |
          # Deploy commands here
```

### Docker Example

```dockerfile
FROM elixir:1.16-alpine

WORKDIR /app

COPY mix.exs mix.lock ./
RUN mix deps.get

COPY . .

# Build the release
ARG SECRET_VAULT_PASSWORD
ENV SECRET_VAULT_PASSWORD=$SECRET_VAULT_PASSWORD
RUN mix release

# Runtime
ENV MIX_ENV=prod
CMD ["_build/prod/rel/exhub/bin/exhub", "start"]
```

Build with:

```bash
docker build --build-arg SECRET_VAULT_PASSWORD="$SECRET_VAULT_PASSWORD" .
```

## Troubleshooting

### "Secret not found" Error

**Cause**: Secret doesn't exist in the vault

**Solution**:
```bash
mix scr.list dev  # Check available secrets
mix scr.insert dev missing_secret "value"  # Create it
```

### "Invalid password" Error

**Cause**: Wrong `SECRET_VAULT_PASSWORD`

**Solution**:
- Verify the environment variable is set
- Check if the secret was created with a different password
- Recreate the secret if needed

### "Config not found" Error

**Cause**: Application config not set up correctly

**Solution**:
- Check `config/config.exs` has the secret_vault config
- Verify the OTP app name matches (`:exhub`)

### Secrets Not Loading at Runtime

**Cause**: Runtime.exs not being executed or errors in config

**Solution**:
- Check `config/runtime.exs` syntax
- Verify all referenced secrets exist
- Check application logs for errors

### Migration from Hardcoded Secrets

If you previously had secrets in `config/config.exs`:

1. Extract all secret values
2. Create them using `mix scr.insert`
3. Replace with `runtime_secret!/2` calls in `runtime.exs`
4. Remove hardcoded values from `config.exs`
5. Add `.vault_secret` files to Git
6. Update deployment documentation

### Lost Master Password

**WARNING**: If you lose the master password, your secrets are unrecoverable.

**Recovery**:
1. Regenerate all API keys from their respective providers
2. Create new secrets with a new master password
3. Update all deployments with new secrets

## Quick Reference

| Task          | Command                                             |
|---------------|-----------------------------------------------------|
| Install deps  | `mix deps.get`                                      |
| Create secret | `mix scr.create dev key_name`                       |
| Insert secret | `mix scr.insert dev key_name "value"`               |
| Edit secret   | `mix scr.edit dev key_name`                         |
| Show secret   | `mix scr.show dev key_name`                         |
| List secrets  | `mix scr.list dev`                                  |
| Audit secrets | `mix scr.audit`                                     |
| Delete secret | `rm priv/secrets/dev/default/key_name.vault_secret` |

## Additional Resources

- [SecretVault Hex Docs](https://hexdocs.pm/secret_vault)
- [SecretVault GitHub](https://github.com/SecretVault-elixir/secret_vault)
- [Elixir Runtime Configuration](https://hexdocs.pm/elixir/Config.html)
