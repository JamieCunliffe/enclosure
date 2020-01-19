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

(ert-deftest test-delete-unbalanced-beg-pair()
  (with-mock
   (stub enclosure--get-delete-string => "${")
   (with-temp-buffer
     (setq-local enclosure-chars (append enclosure-chars '((:beginning "${" :end "}"))))
     (insert "(this is ${some} text)")
     (goto-char 12)
     (enclosure-delete)
     (should (equal (buffer-string) "(this is some text)")))))

(ert-deftest test-delete-unbalanced-end-pair()
  (with-mock
   (stub enclosure--get-delete-string => "&%")
   (with-temp-buffer
     (setq-local enclosure-chars (append enclosure-chars '((:beginning "&" :end "&%"))))
     (insert "(this is &some&% text)")
     (goto-char 12)
     (enclosure-delete)
     (should (equal (buffer-string) "(this is some text)")))))

(ert-deftest test-delete-pair-beg-contains-end()
  (with-mock
   (stub enclosure--get-delete-string => "sql\"\"\"")
   (with-temp-buffer
     (setq-local enclosure-chars (append enclosure-chars '((:beginning "sql\"\"\"" :end "\"\"\""))))
     (insert "(this is sql\"\"\"some\"\"\" text)")
     (goto-char 18)
     (enclosure-delete)
     (should (equal (buffer-string) "(this is some text)")))))

(ert-deftest test-delete-pair-end-contains-beg()
  (with-mock
   (stub enclosure--get-delete-string => "\"\"\"after")
   (with-temp-buffer
     (setq-local enclosure-chars (append enclosure-chars '((:beginning "\"\"\"" :end "\"\"\"after"))))
     (insert "(this is \"\"\"some\"\"\"after text)")
     (goto-char 15)
     (enclosure-delete)
     (should (equal (buffer-string) "(this is some text)")))))
