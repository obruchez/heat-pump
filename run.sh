#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
JAR_FILE="$SCRIPT_DIR/target/scala-2.13/heat-pump-assembly-0.1.0.jar"

if [ ! -f "$JAR_FILE" ]; then
  echo "Fat JAR not found at $JAR_FILE"
  echo "Build it first with: sbt assembly"
  exit 1
fi

java -jar "$JAR_FILE" "$@"
