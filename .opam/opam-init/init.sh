if [ -t 0 ]; then
  test -r /home/hong/.opam/opam-init/complete.sh && . /home/hong/.opam/opam-init/complete.sh > /dev/null 2> /dev/null || true

  test -r /home/hong/.opam/opam-init/env_hook.sh && . /home/hong/.opam/opam-init/env_hook.sh > /dev/null 2> /dev/null || true
fi

test -r /home/hong/.opam/opam-init/variables.sh && . /home/hong/.opam/opam-init/variables.sh > /dev/null 2> /dev/null || true
