#! /usr/bin/env es

repos = (
  .emacs.d/
  bib
  books
  el/bibtex-fetch
  el/secret-el
  el/solarized-emacs
  embedded-topo-phases
  gitolite-admin
  hall-viscosity
  nixos-config
  phys/hall-viscosity-3heb
  phys/hall-viscosity-paper
  phys/phys-212
  phys/phys-214
  phys/defect-classification/stacking
  phys/defect-classification/aiii-notes
  phys/embedded-ti-paper
  phys/prelim
  phys/reading-log
  recategorize
)

fn git-clean {
  ~ `{ git status --porcelain >[2] /dev/null | tail -n 1 } ()
}

fn sync-git-commit repo {
  if { ! git-clean } {
    echo '# '^$repo^': dirty; please commit your changes'
    es -i
  } true {
    echo '# '^$repo^': clean'
    result 0
  }
}

fn sync-git-pull repo {
  echo -n '# '^$repo^': pull... '
  if { ! { git pull && git submodule update --init } > /dev/null >[2=1] } {
    echo 'failed'
    es -i
  } true {
    echo 'OK'
  }
}

fn sync-git-push repo {
  echo -n '# '^$repo^': push... '
  if { ! git push --porcelain > /dev/null >[2=1] } {
    echo 'failed'
    es -i
  } true {
    echo 'OK'
  }
}

fn sync-git repo {
  sync-git-commit $repo && sync-git-pull $repo && sync-git-push $repo
}

fn sync-repo repo {
  if { access -d .git } {
    sync-git $repo
  } true {
    echo '# '^$repo^': unknown repository type'
    return 1
  }
}

fn vcsh-clean repo {
  ~ `{ vcsh $repo status --porcelain >[2] /dev/null | tail -n 1 } ()
}

fn sync-vcsh-commit repo {
  if { ! vcsh-clean $repo } {
    echo '# vcsh: '^$repo^': dirty; please commit your changes'
    es -i
  } true {
    echo '# vcsh: '^$repo^': clean'
    result 0
  }
}

fn sync-vcsh-pull repo {
  echo -n '# vcsh: '^$repo^': pull... '
  if { ! vcsh $repo pull > /dev/null >[2] /dev/null } {
    echo 'failed'
    es -i
  } true {
    echo 'OK'
  }
}

fn sync-vcsh-push repo {
  echo -n '# vcsh: '^$repo^': push... '
  if { ! vcsh $repo push --porcelain > /dev/null >[2] /dev/null } {
    echo 'failed'
    es -i
  } true {
    echo 'OK'
  }
}

fn sync-vcsh {
  for (repo = `` \n { vcsh list }) {
    sync-vcsh-commit $repo && sync-vcsh-pull $repo && sync-vcsh-push $repo
  }
}

fn sync-pass-pull {
  echo -n '# pass: pull... '
  if { ! pass git pull > /dev/null >[2=1] } {
    echo 'failed'
    return 1
  }
  echo 'OK'
}

fn sync-pass-push {
  echo -n '# pass: push... '
  if { ! pass git push --porcelain > /dev/null >[2=1] } {
    echo 'failed'
    return 1
  }
  echo 'OK'
}

fn sync-pass {
  sync-pass-pull && sync-pass-push
}

fn sync-repos {
  sync-vcsh
  sync-pass
  for (repo = $repos) {
    echo ''
    fork {
      let (dest = $HOME/$repo) {
        cd $dest && sync-repo $repo
      }
    }
  }
}
