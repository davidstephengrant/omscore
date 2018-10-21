;=========================================================================
;  omscore - An OpenMusic toolbox for interacting with Leland Smith's SCORE music typography system
;  Copyright (C) 2018  David Stephen Grant
;
;    This file is part of omscore.
;
;    omscore is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    any later version.
;
;    omscore is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with omscore.  If not, see <https://www.gnu.org/licenses/>.
;=========================================================================

(defvar omscore)
(defpackage omscore)

(defvar omscore-test)
(defpackage omscore-test)

;;; Dependencies
(require-library "omdsg")

(in-package :om)

(defvar omscore::*lib* (find-library "omscore"))
(set-lib-release 0.01 omscore::*lib*)

;;; Load sources
(defvar omscore::*srcfiles* nil)
;(defvar omscore-test::*unittestfiles* nil)
;(defvar omscore-test::*unittests* nil)
;; Set to nil in case package is reloaded
;(setf omscore-test::*unittests* nil)

(setf omscore::*srcfiles*
      (list
       (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "batch-processing" :type "lisp")
       ))

;(setf omscore-test::*unittestfiles*
;      (list
       ;(make-pathname  :directory (append (pathname-directory *load-pathname*) (list "unittests")) :name "unittest-utils" :type "lisp")
       ;(make-pathname  :directory (append (pathname-directory *load-pathname*) (list "unittests" "functions")) :name "equivalent" :type "lisp")
;       ))

(mapc #'compile&load omscore::*srcfiles*)
;(mapc #'compile&load omscore-test::*unittestfiles*)

;;; Setup menu structure and (sub) packages
;; Syntax: ("sub package name" subpackages-list class-list function-list class-alias-list)
(fill-library 
 '(("Batch Processing" nil nil (omscore::pad-eps-bounding-box) nil)
   ;("Unit Tests" nil nil (omscore::run-unittests) nil)
   ))

;;; Documentation
;; Lib docstring for {$RESOURCES}/reference/index.html
(doc-library (concatenate 'string
                          "<p>omscore is an OpenMusic toolbox for interacting with Leland Smith's SCORE music typography system.</p>

<p>Functions and classes are loaded into the package 'omscore', and should be referenced with the prefix 'omscore::', e.g. 'omscore::write-pmx'. They can also be found in the patch menu bar or context menu (Classes/Functions > Libraries > omscore).</p>

<p>The library source code repository is on <a href=\"https://github.com/davidstephengrant/omscore\" target=\"_blank\">GitHub</a>, where the most recent releases may be downloaded. Please report any bugs/issues in the repository issue tracker.</p>

<p>Copyright (C) 2018  David Stephen Grant</p>

<p>omscore is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.</p>
<p>" (format nil "Version: ~A" (version omscore::*lib*)) "</p>"
              ) (name omscore::*lib*))

;;; Generate documentation
;; Should be commented out before commit to repo
(gen-lib-reference omscore::*lib*)

(om-print "
 ==============================
 omscore
 ==============================
 An OpenMusic toolbox for interacting with Leland Smith's SCORE music typography system
 Copyright (C) 2018  David Stephen Grant
 ==============================
" nil)