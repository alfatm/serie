#!/bin/bash
#
# Generate test git repository matching the git-graph screenshot
#
# Graph structure (bottom to top):
#   - Yellow line on the left (main trunk)
#   - Magenta/pink feature/feature branch
#   - Cyan develop branch
#   - Green master branch
#   - Red dots for merge points
#

set -e

REPO_DIR="${1:-/tmp/git-graph-test}"

rm -rf "$REPO_DIR"
mkdir -p "$REPO_DIR"
cd "$REPO_DIR"
git init --initial-branch=master
git config user.email "dev@example.com"
git config user.name "Developer"

# Counter for unique file changes - each commit gets unique file
n=0
commit() {
    local msg="$1"
    n=$((n + 1))
    mkdir -p src
    echo "// commit $n: $msg" > "src/file_$n.rs"
    git add -A
    git commit -m "$msg" --quiet
}

echo "Creating git repository with specific graph structure..."

# === Bottom of graph (oldest) ===

# 1. Initial commit
commit "initial commit"

# 2. Start side branch (will become yellow left line)
git checkout -b side-branch
commit "start branch"

# Several commits on side branch
commit "start work on side"
commit "more work on side"
commit "start work on side 2"
commit "even more work"

# 3. Merge back to master creates first merge point
git checkout master
commit "Merge branch (master side)"
git merge side-branch --no-ff -m "Merge branch side"

# 4. Side branch continues after merge
git checkout side-branch
commit "more work on side 3"
commit "start work on side 4"
commit "more work on side 5"

# 5. Master linear commits
git checkout master
commit "more work on master"
commit "increment version"

# 6. Create develop branch (cyan)
git checkout -b develop
commit "start work on develop"
commit "more work on develop"

# 7. Create another feature from master
git checkout master
git checkout -b red-feature
commit "start work on feature"
commit "more work on feature"

# 8. Merge feature to master - another merge point
git checkout master
git merge red-feature --no-ff -m "Merge branch feature"
git branch -d red-feature

# 9. Continue side branch
git checkout side-branch
commit "fixing a critical bug"
commit "start work on fix"

# 10. Another merge of side to master
git checkout master
git merge side-branch --no-ff -m "Merge branch side again"

# 11. Green branch
git checkout -b green-branch
commit "Merge branch green"

# 12. More merges
git checkout master
commit "finish work"

git merge green-branch --no-ff -m "Merge branch green back"
git branch -d green-branch

# 13. Develop continues
git checkout develop
commit "Merge branch develop work"

# 14. Merge develop to master
git checkout master
git merge develop --no-ff -m "Merge branch develop"

# 15. Side branch more work
git checkout side-branch
commit "more work on side 6"

# 16. feature/feature branch (magenta at top)
git checkout master
git checkout -b "feature/feature"
commit "feature work"

# 17. Final structure
git checkout master
commit "increment version final"

git checkout develop
commit "(develop) final work"

git checkout master
git merge develop --no-ff -m "(develop) Merge"

commit "(master) Merge"

# 18. Merge feature/feature at very top
git merge "feature/feature" --no-ff -m "(feature/feature) merge"

echo ""
echo "Done! Repository: $REPO_DIR"
echo "Commits: $(git rev-list --count HEAD)"
echo ""
echo "View graph:"
echo "  git -C $REPO_DIR log --oneline --graph --all"
echo ""
echo "Test serie:"
echo "  serie $REPO_DIR"
