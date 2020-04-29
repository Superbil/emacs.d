#!/usr/local/bin/zsh -f

set -e

sync_work="${sync_work:-$(pwd)}"
commit_message="Update for $(date '+%Y-%m-%d %H:%M') at $(hostname)"

# Pull remote, abort if fast-forward is not possible
git -C $sync_work pull --ff-only --verify-signatures

# Git add file by git_add_pathspec
if [ -e "${sync_work}/git_add_pathspec" ]; then
    git -C $sync_work add --pathspec-from-file "${sync_work}/git_add_pathspec"
fi

# Only commit when status not clear
if [ -n "$(git -C ${sync_work} status --untracked-files --porcelain)" ]; then
    git -C $sync_work commit --all --message $commit_message
fi

git -C $sync_work push
