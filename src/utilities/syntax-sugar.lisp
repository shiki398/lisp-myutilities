(defmacro let1 (var val &body body)
    "letの糖衣マクロ
    ex; (let1 var 10 (princ var)) → 10)"
    `(let ((,var ,val))
        ,@body))