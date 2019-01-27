(need util/string)
(need util/list)

(define (get-home-directory)
  (let ((home (getenv "HOME")))
    (if home
        home
        (let ((drive (getenv "HOMEDRIVE"))
              (path (getenv "HOMEPATH")))
          (and drive
               path
               (string-append drive path))))))

(define (get-temp-directory)
  (or (getenv "TEMP")
      (getenv "TMP")))

(define (path-separator)
  "/")

(define (path-separator-character)
  #\/)

(define (build-path path subpath)
  (cond ((starts-with? subpath (path-separator))
         (build-path path (substring subpath 1 (string-length subpath))))
        ((not (ends-with? path (path-separator)))
         (string-append path (path-separator) subpath))
        (else (string-append path subpath))))

(define *path-separator-string* "/")

(define (append-path root path)
  (cond ((string-ends-with? root *path-separator-string*)
         (string-append root path))
        ((string-starts-with? path *path-separator-string*)
         (string-append root path))
        (else
         (string-append root *path-separator-string* path))))

(define (path-base-name path)
  (let ((idx (strrchr* path "/\\")))
    (if idx
        (substring path (+ idx 1) (string-length path))
        path)))

(define (path-base-dir path)
  (let ((idx (strrchr* path "/\\")))
    (if idx
        (substring path 0 idx)
        ".")))
