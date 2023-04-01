if [[ -o interactive ]]; then
  [[ ! -r /home/hong/.opam/opam-init/complete.zsh ]] || source /home/hong/.opam/opam-init/complete.zsh  > /dev/null 2> /dev/null

  [[ ! -r /home/hong/.opam/opam-init/env_hook.zsh ]] || source /home/hong/.opam/opam-init/env_hook.zsh  > /dev/null 2> /dev/null
fi

[[ ! -r /home/hong/.opam/opam-init/variables.sh ]] || source /home/hong/.opam/opam-init/variables.sh  > /dev/null 2> /dev/null
