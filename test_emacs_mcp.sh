#!/bin/bash
# Test script for Emacs MCP Server via mcp-cli
# Usage: ./test_emacs_mcp.sh [command]

set -e

CONFIG="-c mcp_servers.json"
SERVER="emacs"

case "${1:-help}" in
  info)
    echo "=== Server Info ==="
    mcp-cli $CONFIG info $SERVER
    ;;
  list)
    echo "=== List Recent Buffers (default: 20) ==="
    MCP_NO_DAEMON=1 mcp-cli $CONFIG call $SERVER emacs_list_buffers '{}'
    ;;
  list-all)
    echo "=== List All Buffers ==="
    MCP_NO_DAEMON=1 mcp-cli $CONFIG call $SERVER emacs_list_buffers '{"limit": 0}'
    ;;
  list-filter)
    KEYWORD="${2:-}"
    LIMIT="${3:-20}"
    if [ -z "$KEYWORD" ]; then
      echo "Usage: $0 list-filter <keyword> [limit]"
      exit 1
    fi
    echo "=== List Buffers Filtered by '$KEYWORD' (limit: $LIMIT) ==="
    MCP_NO_DAEMON=1 mcp-cli $CONFIG call $SERVER emacs_list_buffers "{\"keyword\": \"$KEYWORD\", \"limit\": $LIMIT}"
    ;;
  list-details)
    echo "=== List Buffers with Details ==="
    MCP_NO_DAEMON=1 mcp-cli $CONFIG call $SERVER emacs_list_buffers '{"include_details": true}'
    ;;
  list-details-all)
    echo "=== List All Buffers with Details ==="
    MCP_NO_DAEMON=1 mcp-cli $CONFIG call $SERVER emacs_list_buffers '{"include_details": true, "limit": 0}'
    ;;
  list-details)
    echo "=== List Buffers with Details ==="
    MCP_NO_DAEMON=1 mcp-cli $CONFIG call $SERVER emacs_list_buffers '{"include_details": true}'
    ;;
  read)
    BUFFER="${2:-*scratch*}"
    echo "=== Read Buffer: $BUFFER ==="
    MCP_NO_DAEMON=1 mcp-cli $CONFIG call $SERVER emacs_read_buffer "{\"buffer_name\": \"$BUFFER\"}"
    ;;
  read-range)
    BUFFER="${2:-*scratch*}"
    START="${3:-1}"
    END="${4:-10}"
    echo "=== Read Buffer: $BUFFER (lines $START-$END) ==="
    MCP_NO_DAEMON=1 mcp-cli $CONFIG call $SERVER emacs_read_buffer "{\"buffer_name\": \"$BUFFER\", \"start_line\": $START, \"end_line\": $END}"
    ;;
  write)
    BUFFER="${2:-*scratch*}"
    CONTENT="${3:-Hello from mcp-cli!}"
    MODE="${4:-append}"
    echo "=== Write to Buffer: $BUFFER (mode: $MODE) ==="
    MCP_NO_DAEMON=1 mcp-cli $CONFIG call $SERVER emacs_write_buffer "{\"buffer_name\": \"$BUFFER\", \"content\": \"$CONTENT\", \"mode\": \"$MODE\"}"
    ;;
  close)
    BUFFER="${2:-*scratch*}"
    ACTION="${3:-discard}"
    echo "=== Close Buffer: $BUFFER (action: $ACTION) ==="
    MCP_NO_DAEMON=1 mcp-cli $CONFIG call $SERVER emacs_close_buffer "{\"buffer_name\": \"$BUFFER\", \"action\": \"$ACTION\"}"
    ;;
  schema)
    TOOL="${2:-emacs_list_buffers}"
    echo "=== Tool Schema: $TOOL ==="
    mcp-cli $CONFIG info $SERVER $TOOL
    ;;
  search)
    PATTERN="${2:-*emacs*}"
    echo "=== Search Tools: $PATTERN ==="
    mcp-cli $CONFIG grep "$PATTERN"
    ;;
  help|*)
    echo "Emacs MCP Server Test Script"
    echo ""
    echo "Usage: $0 <command> [args...]"
    echo ""
    echo "Commands:"
    echo "  info              Show server info and available tools"
    echo "  list              List recent buffers (default: 20)"
    echo "  list-all          List all buffers (no limit)"
    echo "  list-filter <keyword> [limit]  Filter buffers by keyword"
    echo "  list-details      List recent buffers with size and mode details"
    echo "  list-details-all  List all buffers with details"
    echo "  read [buffer]     Read buffer content (default: *scratch*)"
    echo "  read-range [buffer] [start] [end]  Read buffer line range"
    echo "  write [buffer] [content] [mode]    Write to buffer (mode: replace/insert/append)"
    echo "  close [buffer] [action]            Close buffer (action: save/discard/close)"
    echo "  schema [tool]     Show tool input schema"
    echo "  search [pattern]  Search tools by pattern"
    echo ""
    echo "Examples:"
    echo "  $0 list"
    echo "  $0 list-all"
    echo "  $0 list-filter el 5"
    echo "  $0 read '*scratch*'"
    echo "  $0 write '*scratch*' 'Hello World' append"
    echo "  $0 close '*scratch*' discard"
    ;;
esac
