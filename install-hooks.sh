#!/bin/sh

GIT_DIR=$(git rev-parse --git-dir)

echo "Installing hooks..."
# this command creates symlink to our pre-commit script
rm $GIT_DIR/hooks/pre-commit
cp ./scripts/pre-commit.sh $GIT_DIR/hooks/pre-commit
chmod +x $GIT_DIR/hooks/pre-commit
echo "Done"!