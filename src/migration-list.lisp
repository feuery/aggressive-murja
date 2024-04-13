(defpackage murja.migration-list
  (:use :cl)
  (:import-from :murja.migrations :defmigration))

(in-package :murja.migration-list)

(defmigration "001-users.up")
(defmigration "002-posts.up")
(defmigration "003-comments.up")
(defmigration "004-fixing-posts.up")
(defmigration "005-user-groups.up")
(defmigration "006-permission-table.up")
(defmigration "007-can-meta.up")
(defmigration "008-users-can-edit-self.up")
(defmigration "009-users-can-comment.up")
(defmigration "010-can-import.up")
(defmigration "011-versioned-posts.up")
(defmigration "012-version-triggers.up")
(defmigration "013-media-table.up")
(defmigration "014-tag-hidden-unlisted-validator.up")
(defmigration "015-image-post-pairing-view.up")

;; (murja.migrations:migrate)
