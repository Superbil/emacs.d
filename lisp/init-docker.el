;;; init-docker.el --- Work with Docker and its tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package docker
  :config
  (fullframe docker-images tablist-quit)
  (fullframe docker-machines tablist-quit)
  (fullframe docker-volumes tablist-quit)
  (fullframe docker-networks tablist-quit)
  (fullframe docker-containers tablist-quit))

(use-package dockerfile-mode
  :mode ("Dockerfile"))

(use-package docker-compose-mode)

(use-package docker-tramp)

(use-package docker-cli)


(provide 'init-docker)
;;; init-docker.el ends here
