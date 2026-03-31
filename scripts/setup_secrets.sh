#!/bin/bash
# Setup script for Exhub secrets using SecretVault
# Usage: ./scripts/setup_secrets.sh

set -e

echo "=== Exhub SecretVault Setup ==="
echo ""

# Check if SECRET_VAULT_PASSWORD is set
if [ -z "$SECRET_VAULT_PASSWORD" ]; then
    echo "ERROR: SECRET_VAULT_PASSWORD environment variable is not set"
    echo ""
    echo "Please set it first:"
    echo "  export SECRET_VAULT_PASSWORD='your-secure-master-password'"
    echo ""
    exit 1
fi

echo "Using MIX_ENV: ${MIX_ENV:-dev}"
echo ""

ENV=${MIX_ENV:-dev}

# Function to check if a secret exists
# SecretVault stores secrets as: priv/secrets/$ENV/$PREFIX/${name}.vault_secret
secret_exists() {
    local name=$1
    local secret_file="priv/secrets/${ENV}/default/${name}.vault_secret"
    test -f "$secret_file"
}

# Function to create a secret if it doesn't exist
create_secret() {
    local name=$1
    local description=$2

    echo "Setting up: $name"
    echo "  Description: $description"

    if secret_exists "$name"; then
        echo "  ✓ Already exists (skipping)"
    else
        echo -n "  Enter value (or press Enter to skip): "
        read -s value
        echo ""  # Newline after hidden input

        if [ -z "$value" ]; then
            echo "  ⊘ Skipped (empty value)"
        else
            # Use scr.insert to create with value directly (no editor)
            mix scr.insert "$ENV" "$name" "$value"
            echo "  ✓ Created"
        fi
    fi
    echo ""
}

echo "This script will help you set up all required secrets for Exhub."
echo "Press Ctrl+C to cancel, or Enter to continue..."
read

echo "--- Required Secrets ---"
echo ""

create_secret "gitee_api_key" "Gitee AI API key (used for most LLM providers)"
create_secret "openai_api_key" "OpenAI API key"
create_secret "gitee_cookie" "Gitee cookie for GiteeCat API access"

echo "--- Optional Provider-Specific Secrets ---"
echo ""
echo "The following are optional. Press Enter to skip any of them."
echo ""

create_secret "siliconflow_api_key" "SiliconFlow API key"
create_secret "mistral_api_key" "Mistral API key"
create_secret "codestral_api_key" "Codestral API key"
create_secret "anthropic_api_key" "Anthropic API key"
create_secret "groq_api_key" "Groq API key"
create_secret "gemini_api_key" "Google Gemini API key"
create_secret "cohere_api_key" "Cohere API key"
create_secret "samba_api_key" "Samba API key"

echo "=== Setup Complete ==="
echo ""
echo "Your secrets are stored in: priv/secrets/$ENV/default/"
echo ""
echo "You can verify the secrets with:"
echo "  ls priv/secrets/$ENV/default/"
echo ""
echo "To use the application, ensure SECRET_VAULT_PASSWORD is set:"
echo "  export SECRET_VAULT_PASSWORD='your-password'"
echo ""
echo "Then start the application:"
echo "  mix phx.server"
