#!/usr/local/bin/zsh -f
#
# sync.sh
# Author: superbil
#
# This scirpt will do such git commands, like sync with remote repo.
# When one command is failed, just exit.
# - git pull
# - git add
# - git commit
# - git push
#
# Usage:
#   ./sync.sh [<git_repo_folder>]
#     sync work at current folder (pwd) or <git_repo_folder>
#
set -e

sync_work="${1:-$(pwd)}"
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
