(ql:quickload :aggressive-murja-tests)
(unless (fiveam:run! 'murja.tests:main-suite)
  (sb-ext:exit :code 666))
