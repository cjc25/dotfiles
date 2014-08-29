#!/bin/bash

HERE="$(dirname $(readlink -f $0))"

for filename in $(ls -A $HERE) ; do
  replacement="$HERE/$filename"
  # Only look at normal files, and ignore the executables
  [[ -f $replacement ]] && [[ ! -x $replacement ]] || continue

  if [[ "xmonad.hs" == $filename ]] ; then
    # Special handling for xmonad.hs, which doesn't go in $HOME
    targetdir="$HOME/.xmonad"
  else
    targetdir="$HOME"
  fi
  target="$targetdir/$filename"
  backup="${target}.old"

  while true ; do
    echo -n "Create symlink over $target to $replacement" \
      "(backed up to $backup) (y/n/d[iff])? "
    read yesnodiff
    case ${yesnodiff:0:1} in
      d|D)
        echo
        diff -u "$replacement" "$target"
        echo
        ;;
      n|N)
        echo "Aborting replacement"
        continue 2
        ;;
      y|Y)
        break
        ;;
      *)
        echo "Please choose"
        ;;
    esac
  done

  cp "$target" "$backup"
  mkdir -p "$targetdir"
  rm "$target"
  ln -s -T "$replacement" "$target"

  echo "Done with $filename!"
  echo
done
