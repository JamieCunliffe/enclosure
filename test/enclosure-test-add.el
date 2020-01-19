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

(ert-deftest test-add-thing-at-point-unbalanced-beg-pair()
  (with-mock
   (stub enclosure--get-add-string => "${")
   (with-temp-buffer
     (setq-local enclosure-chars (append enclosure-chars '((:beginning "${" :end "}"))))
     (insert "this is some text")
     (goto-char 12)
     (enclosure-thing-at-point)
     (should (equal (buffer-string) "this is ${some} text")))))

(ert-deftest test-add-thing-at-point-unbalanced-end-pair()
  (with-mock
   (stub enclosure--get-add-string => "^%^")
   (with-temp-buffer
     (setq-local enclosure-chars (append enclosure-chars '((:beginning "^" :end "^%^"))))
     (insert "this is some text")
     (goto-char 12)
     (enclosure-thing-at-point)
     (should (equal (buffer-string) "this is ^some^%^ text")))))

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

(ert-deftest test-add-region-unbalanced-beg-pair()
  (with-mock
   (stub enclosure--get-add-string => "${")
   (with-temp-buffer
     (setq-local enclosure-chars (append enclosure-chars '((:beginning "${" :end "}"))))
     (insert "this is some text")
     (set-mark 6) ; i of is
     (goto-char 13) ;space after some
     (activate-mark)
     (enclosure-region)
     (should (equal (buffer-string) "this ${is some} text")))))

(ert-deftest test-add-region-unbalanced-end-pair()
  (with-mock
   (stub enclosure--get-add-string => "^%^")
   (with-temp-buffer
     (setq-local enclosure-chars (append enclosure-chars '((:beginning "^" :end "^%^"))))
     (insert "this is some text")
     (set-mark 6) ; i of is
     (goto-char 13) ;space after some
     (activate-mark)
     (enclosure-region)
     (should (equal (buffer-string) "this ^is some^%^ text")))))
