(defun flatten (x)
    (labels ((rec (x acc)
                  (cond ((null x) acc)
                        ((atom x) (cons x acc))
                        (t (rec (car x) (rec (cdr x) acc))))))
        (rec x nil)))
(defmacro alias (new_fn org_fn)
  `(setf (fdefinition ',new_fn) (fdefinition ',org_fn)))

(alias size length)
(alias uniq remove-duplicates)
(defmacro ! (fn &rest vars)
    "複数の変数を破滅的に変換する
    fn:関数 vars:変数
    使い方： (! #'flatten x y z) → xは、'(1 2 (3 4) 5)だとすると(1 2 3 4 5)が返却。y zも同様。"
    `(progn
        ,@(mapcar (lambda (v)
            `(setf ,v (funcall ,fn ,v)))
            vars)))
(defun mapcar-non-nil (fn lst)
  (remove-if #'null (mapcar fn lst)))
(defun mapcan-non-nil (fn lst)
  (remove-if #'null (mapcan fn lst)))