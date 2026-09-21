;;; pr-review-config.el  --- configure pr-review.el
;; pr-review configuration

(use-package pr-review
  :ensure t
  :custom
  ((pr-review-search-predefined-queries . '(
                                            ("is:pr state:open archived:false sort:updated-desc org:xonasystems" . "Xona")
                                            ("is:pr archived:false author:@me is:open" . "Created")
                                            ("is:pr archived:false assignee:@me is:open" . "Assigned")
                                            ("is:pr archived:false mentions:@me is:open" . "Mentioned")
                                            ("is:pr archived:false review-requested:@me is:open" . "Review requests"))
                                        )))

(provide 'pr-review-config)
