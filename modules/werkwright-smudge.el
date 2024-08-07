(use-package smudge
  :config  
  (setq smudge-transport 'dbus)
  (setq smudge-player-status-truncate-length 70)  
  (global-smudge-remote-mode 1)
  (defun refresh-smudge-timer()
    (interactive)  
    (cancel-timer smudge-controller-timer)
    (setq smudge-controller-timer nil)
    (smudge-controller-start-player-status-timer))
  (advice-add 'smudge-controller-toggle-play :before 'refresh-smudge-timer)
  (eval-after-load 'hydra
    (defhydra hydra-spotify (:hint nil)
      "
^Search^                  ^Control^               ^Manage^
^^^^^^^^-----------------------------------------------------------------
_t_: Track               _SPC_: Play/Pause        _+_: Volume up
_m_: My Playlists        _n_  : Next Track        _-_: Volume down
_f_: Featured Playlists  _p_  : Previous Track    _x_: Mute
_u_: User Playlists      _r_  : Repeat            _d_: Device
^^                       _s_  : Shuffle           _q_: Quit
"
      ("t" smudge-track-search :exit t)
      ("m" smudge-my-playlists :exit t)
      ("f" smudge-featured-playlists :exit t)
      ("u" smudge-user-playlists :exit t)
      ("SPC" smudge-controller-toggle-play :exit nil)
      ("n" smudge-controller-next-track :exit nil)
      ("p" smudge-controller-previous-track :exit nil)
      ("r" smudge-controller-toggle-repeat :exit nil)
      ("s" smudge-controller-toggle-shuffle :exit nil)
      ("+" smudge-controller-volume-up :exit nil)
      ("-" smudge-controller-volume-down :exit nil)
      ("x" smudge-controller-volume-mute-unmute :exit nil)
      ("d" smudge-select-device :exit nil)
      ("q" quit-window "quit" :color blue))))



(provide 'werkwright-smudge)
