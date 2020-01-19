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

(ert-deftest test-change-unbalanced-beg-pair()
  (with-mock
   (stub enclosure--get-change-string => '("(" . "${"))
   (with-temp-buffer
     (setq-local enclosure-chars (append enclosure-chars '((:beginning "${" :end "}"))))
     (insert "(this is (some) text)")
     (goto-char 12)
     (enclosure-change)
     (should (equal (buffer-string) "(this is ${some} text)")))))

(ert-deftest test-change-unbalanced-end-pair()
  (with-mock
   (stub enclosure--get-change-string => '("&%" . "@"))
   (with-temp-buffer
     (setq-local enclosure-chars (append enclosure-chars '((:beginning "&" :end "&%"))))
     (insert "(this is &some&% text)")
     (goto-char 12)
     (enclosure-change)
     (should (equal (buffer-string) "(this is @some@ text)")))))
