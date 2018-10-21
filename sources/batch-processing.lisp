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

(in-package :om)

(defmethod! omscore::pad-eps-bounding-box ((infile pathname) &optional (pad-size 5))
            :icon 700
  (let ((outfile (make-pathname :directory (pathname-directory infile) :name (concatenate 'string "pad_" (pathname-name infile)) :type (pathname-type infile))))
    (with-open-file (eps-stream infile :direction :input)
      (let ((data (loop for line = (read-line eps-stream nil nil)
                        while line
                        collect (let ((bb (clone (dsg::split-sequence '#\space line :remove-empty-subseqs t))))
                                  (cond ((and (string= "%%BoundingBox:" (car bb))
                                              (eq (length bb) 5))
                                         (setf (nth 1 bb) (- (parse-integer (nth 1 bb)) pad-size))
                                         (setf (nth 2 bb) (- (parse-integer (nth 2 bb)) pad-size))
                                         (setf (nth 3 bb) (+ (parse-integer (nth 3 bb)) pad-size))
                                         (setf (nth 4 bb) (+ (parse-integer (nth 4 bb)) pad-size))
                                         bb)
                                        (t bb))))))
        (om:save-data data outfile)))))

(defmethod! omscore::pad-eps-bounding-box ((infile-lst t) &optional (pad-size 5))
            :icon 700
  (loop for file in infile-lst
        collect (omscore::pad-eps-bounding-box file pad-size)))