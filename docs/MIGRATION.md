# Migration Guide: Moving from Hardcoded Secrets to SecretVault

This guide helps you migrate from the previous hardcoded secret configuration to the new SecretVault-based system.

## Pre-Migration Checklist

1. [ ] Backup your current `config/config.exs` (just in case)
2. [ ] Identify all secrets that need to be migrated
3. [ ] Prepare a secure location to temporarily store secrets during migration
4. [ ] Ensure you have `SECRET_VAULT_PASSWORD` ready (choose a strong one!)

## Migration Steps

### Step 1: Install Dependencies

```bash
mix deps.get
```

### Step 2: Set the Master Password

```bash
export SECRET_VAULT_PASSWORD="your-secure-master-password"
```

### Step 3: Extract Current Secrets

From your current `config/config.exs`, extract these values:

| Secret Name           | Config Location                                     | Description                 |
|-----------------------|-----------------------------------------------------|-----------------------------|
| `gitee_api_key`       | `gitee_api_key` or first `api_key` in gitee configs | Gitee AI API key            |
| `openai_api_key`      | `openai_api_key`                                    | OpenAI API key              |
| `gitee_cookie`        | `gitee_cat.auth.cookie`                             | Gitee authentication cookie |
| `siliconflow_api_key` | SiliconFlow LLM configs                             | SiliconFlow API key         |
| `mistral_api_key`     | Mistral LLM configs                                 | Mistral API key             |
| `codestral_api_key`   | Codestral LLM config                                | Codestral API key           |
| `anthropic_api_key`   | Anthropic LLM config                                | Anthropic API key           |
| `groq_api_key`        | Groq LLM config                                     | Groq API key                |
| `gemini_api_key`      | Gemini LLM configs                                  | Google Gemini API key       |
| `cohere_api_key`      | Cohere LLM configs                                  | Cohere API key              |
| `samba_api_key`       | Samba/QwQ LLM config                                | Samba API key               |

### Step 4: Create Secrets in SecretVault

For each secret, run:

```bash
mix scr.insert dev SECRET_NAME "SECRET_VALUE"
```

Example:

```bash
# Gitee API Key
mix scr.insert dev gitee_api_key "sk-abc123xyz789"

# OpenAI API Key
mix scr.insert dev openai_api_key "sk-def456uvw012"

# Gitee Cookie
mix scr.insert dev gitee_cookie "your-cookie-string-here"

# Continue for other providers as needed...
```

### Step 5: Interactive Setup (Alternative)

You can also use the provided setup script:

```bash
# Make the script executable
chmod +x scripts/setup_secrets.sh

# Run it
./scripts/setup_secrets.sh
```

### Step 6: Verify Secrets

List all created secrets:

```bash
mix scr.list dev
```

Verify a specific secret:

```bash
mix scr.show dev gitee_api_key
```

### Step 7: Test the Application

Start the application with the new configuration:

```bash
export SECRET_VAULT_PASSWORD="your-secure-master-password"
mix phx.server
```

Check the logs to ensure secrets are loaded correctly.

### Step 8: Clean Up Old Configuration

After confirming everything works:

1. Remove hardcoded secrets from `config/config.exs` (they are now ignored anyway)
2. The `config/runtime.exs` file already handles secret loading

### Step 9: Commit Encrypted Secrets

```bash
git add priv/secrets/
git commit -m "Add encrypted secrets via SecretVault"
```

## Rolling Back

If you need to rollback:

1. Restore your backup of `config/config.exs`
2. Delete the `priv/secrets/` directory
3. Remove `config/runtime.exs` (or restore from backup)

## Troubleshooting

### "Secret not found" errors

Ensure you created secrets for the correct environment:

```bash
MIX_ENV=dev mix scr.list dev
```

### Wrong password

If you used the wrong password, you'll need to:

1. Delete the encrypted files: `rm -rf priv/secrets/`
2. Recreate all secrets with the correct password

### Application won't start

Check the logs for:
- `SecretVault config not loaded` - Password may be missing
- `runtime_secret!` errors - Secret may not exist

## Post-Migration

### For Different Environments

Create secrets for other environments:

```bash
# Test environment
export MIX_ENV=test
mix scr.insert test gitee_api_key "test-api-key"

# Production environment (use a different password!)
export MIX_ENV=prod
export SECRET_VAULT_PASSWORD="different-strong-password"
mix scr.insert prod gitee_api_key "prod-api-key"
```

### CI/CD Updates

Update your CI/CD pipelines to set `SECRET_VAULT_PASSWORD`:

```yaml
# .github/workflows/deploy.yml
- name: Deploy
  env:
    SECRET_VAULT_PASSWORD: ${{ secrets.SECRET_VAULT_PASSWORD }}
  run: |
    mix release
    # deploy...
```

### Team Onboarding

Share with your team:

1. The master password (securely, e.g., password manager)
2. This migration guide
3. The location of `docs/SECRETS.md`

## Customizing API Base URLs

After migration, the `api_base` URLs for all LLM providers are defined in `config/runtime.exs`. The defaults are:

| Provider / Model group          | Default `api_base`                                  |
|---------------------------------|-----------------------------------------------------|
| Gitee AI models                 | `https://ai.gitee.com/v1`                           |
| SiliconFlow models              | `https://api.siliconflow.cn/v1`                     |
| Mistral models                  | `https://api.mistral.ai/v1`                         |
| Codestral                       | `https://codestral.mistral.ai/v1`                   |
| Anthropic, Groq, Gemini, Cohere | `http://127.0.0.1:9069/<provider>/v1` (local proxy) |

To change the API base URL for a specific model, open `config/runtime.exs` and replace the helper call with an explicit map. For example, to route a Gitee model through a custom proxy:

```elixir
# Before (uses the gitee_llm helper, api_base = "https://ai.gitee.com/v1")
"openai/Qwen2.5-72B-Instruct" => gitee_llm.("openai/Qwen2.5-72B-Instruct"),

# After (custom api_base)
"openai/Qwen2.5-72B-Instruct" => %{
  api_base: "http://127.0.0.1:9069/gitee/v1",
  api_key: giteeai_api_key,
  model: "openai/Qwen2.5-72B-Instruct"
},
```

Each entry in `llms_config` is a plain map with three keys — `api_base`, `api_key`, and `model` — so any field can be changed independently without affecting other models.

## Security Checklist

After migration:

- [ ] Hardcoded secrets removed from `config/config.exs`
- [ ] `.gitignore` updated to exclude plain text secrets
- [ ] Encrypted `.vault_secret` files committed to Git
- [ ] Master password stored securely (password manager)
- [ ] CI/CD updated with `SECRET_VAULT_PASSWORD`
- [ ] Team members informed of new process
- [ ] Old secrets rotated (if they were exposed)
