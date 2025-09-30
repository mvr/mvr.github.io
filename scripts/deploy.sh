#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
site_dir="$repo_root/_site"
deploy_dir="$repo_root/mvr.github.io"

if [[ ! -d "$site_dir" ]]; then
  echo "error: build directory _site does not exist" >&2
  exit 1
fi

if [[ ! -d "$deploy_dir/.git" ]]; then
  echo "error: mvr.github.io is not a git repository" >&2
  exit 1
fi

commit_msg=${1:-"Deploy $(date +'%Y-%m-%d %H:%M:%S')"}

rsync -a --delete --exclude=".git" "$site_dir/" "$deploy_dir/"
touch "$deploy_dir/.nojekyll"

cd "$deploy_dir"

changes=$(git status --porcelain)
if [[ -z "$changes" ]]; then
  echo "No changes to deploy."
  exit 0
fi

git add --all

git commit -m "$commit_msg"

git push
