;;; mpvi-tests.el --- Tests for mpvi.el -*- lexical-binding: t -*-

;; Copyright (C) 2023 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Unit Tests

;;; Code:

(require 'ert)
(require 'mpvi)

(ert-deftest test-mpvi-time-to-secs ()
  (should (eq   (mpvi-time-to-secs nil      ) nil))
  (should (=    (mpvi-time-to-secs 23       ) 23))
  (should (=    (mpvi-time-to-secs 23.1     ) 23.1))
  (should (=    (mpvi-time-to-secs "23"     ) 23))
  (should (=    (mpvi-time-to-secs "23.1"   ) 23.1))
  (should (=    (mpvi-time-to-secs "0:23.1" ) 23.1))
  (should (=    (mpvi-time-to-secs "10:23"  ) 623))
  (should (=    (mpvi-time-to-secs "-10:23" ) -623))
  (should (=    (mpvi-time-to-secs "1:00:23") 3623))
  (should-error (mpvi-time-to-secs t))
  (should-error (mpvi-time-to-secs "10/3"))
  (should-error (mpvi-time-to-secs '(1 2 3))))

(ert-deftest test-mpvi-secs-to-hms ()
  (should (equal (mpvi-secs-to-hms 3)          "00:03"))
  (should (equal (mpvi-secs-to-hms 3.23)       "00:03.23"))
  (should (equal (mpvi-secs-to-hms 3.23 t)     "0:00:03.23"))
  (should (equal (mpvi-secs-to-hms 11111.11)   "3:05:11.11"))
  (should (equal (mpvi-secs-to-hms 11111.11 t) "3:05:11.11")))

(ert-deftest test-mpvi-parse-link ()
  (should (equal (mpvi-parse-link "~/xxx/aaa.flv")
                 (list "~/xxx/aaa.flv" nil nil)))
  (should (equal (mpvi-parse-link "~/xxx/aaa.flv#3")
                 (list "~/xxx/aaa.flv" 3 nil)))
  (should (equal (mpvi-parse-link "~/xxx/aaa.flv#3-5")
                 (list "~/xxx/aaa.flv" 3 5)))
  (should (equal (mpvi-parse-link "~/xxx/aaa.flv#1:3-1:5")
                 (list "~/xxx/aaa.flv" 63 65)))
  (should (equal (mpvi-parse-link "~/xxx/aaa.flv#1:3")
                 (list "~/xxx/aaa.flv" 63 nil)))
  (should (equal (mpvi-parse-link "~/xxx/aaa.flv#-1:3")
                 (list "~/xxx/aaa.flv" nil 63)))
  (should-error (mpvi-parse-link "~/xxx/aaa.flv#fff:3")))

(ert-deftest test-mpvi-extract ()
  ;;(mpvi-extract nil "https://www.bilibili.com/video/BV17x411973o")
  ;;(mpvi-extract nil "https://www.bilibili.com/video/BV1Lb411Q7yq")
  ;;(mpvi-extract nil "https://www.bilibili.com/video/BV1Lb411Q7yq?p=3")
  ;;(mpvi-extract nil "https://www.douyu.com/topic/crpd?rid=9999")
  )

(provide 'mpvi-tests)

;;; mpvi-tests.el ends here
