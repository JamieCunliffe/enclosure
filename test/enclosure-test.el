;;; enclosure-test.el --- Tests for enclosure

(require 'enclosure)
(require 'el-mock)
(require 'ert)

(ert-deftest test-add-thing-at-point()
  (with-mock
   (stub enclosure--get-add-string => "(")
   (with-temp-buffer
     (insert "this is some text")
     (goto-char 10)
     (enclosure-thing-at-point)
     (should (equal (buffer-string) "this is (some) text")))))

(ert-deftest test-add-thing-at-point-non-pair()
  (with-mock
   (stub enclosure--get-add-string => "|")
   (with-temp-buffer
     (insert "this is some text")
     (goto-char 10)
     (enclosure-thing-at-point)
     (should (equal (buffer-string) "this is |some| text")))))


(ert-deftest test-add-region()
  (with-mock
   (stub enclosure--get-add-string => "(")
   (with-temp-buffer
     (insert "this is some text")
     (set-mark 6) ; i of is
     (goto-char 13) ;space after some
     (activate-mark)
     (enclosure-region)
     (should (equal (buffer-string) "this (is some) text")))))

(ert-deftest test-add-region-non-pair()
  (with-mock
   (stub enclosure--get-add-string => "|")
   (with-temp-buffer
     (insert "this is some text")
     (set-mark 6) ; i of is
     (goto-char 13) ;space after some
     (activate-mark)
     (enclosure-region)
     (should (equal (buffer-string) "this |is some| text")))))


(ert-deftest test-delete()
  (with-mock
   (stub enclosure--get-delete-string => "(")
   (with-temp-buffer
     (insert "this is (some) text")
     (goto-char 10)
     (enclosure-delete)
     (should (equal (buffer-string) "this is some text")))))

(ert-deftest test-delete-non-pair()
  (with-mock
   (stub enclosure--get-delete-string => "&")
   (with-temp-buffer
     (insert "this is &some& text")
     (goto-char 10)
     (enclosure-delete)
     (should (equal (buffer-string) "this is some text")))))

(ert-deftest test-delete-skip-pair-after()
  (with-mock
   (stub enclosure--get-delete-string => "(")
   (with-temp-buffer
     (insert "(this is (some) text)")
     (goto-char 6)
     (enclosure-delete)
     (should (equal (buffer-string) "this is (some) text")))))

(ert-deftest test-delete-skip-pair-before()
  (with-mock
   (stub enclosure--get-delete-string => "(")
   (with-temp-buffer
     (insert "(this is (some) text)")
     (goto-char 17)
     (enclosure-delete)
     (should (equal (buffer-string) "this is (some) text")))))

(ert-deftest test-change()
  (with-mock
   (stub enclosure--get-change-string => '("(" . "{"))
   (with-temp-buffer
     (insert "this is (some) text")
     (goto-char 10)
     (enclosure-change)
     (should (equal (buffer-string) "this is {some} text")))))

(ert-deftest test-change-non-pair()
  (with-mock
   (stub enclosure--get-change-string => '("(" . "|"))
   (with-temp-buffer
     (insert "this is (some) text")
     (goto-char 10)
     (enclosure-change)
     (should (equal (buffer-string) "this is |some| text")))))

(ert-deftest test-change-skip-pair-after()
  (with-mock
   (stub enclosure--get-change-string => '("(" . "|"))
   (with-temp-buffer
     (insert "(this is (some) text)")
     (goto-char 6)
     (enclosure-change)
     (should (equal (buffer-string) "|this is (some) text|")))))

(ert-deftest test-change-skip-pair-before()
  (with-mock
   (stub enclosure--get-change-string => '("(" . "|"))
   (with-temp-buffer
     (insert "(this is (some) text)")
     (goto-char 17)
     (enclosure-change)
     (should (equal (buffer-string) "|this is (some) text|")))))

;;; enclosure-test.el ends here
