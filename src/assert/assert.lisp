(defun assert-report (result expected actual form &optional caller)
  "assert (result:結果(expectedとactualの比較) expected:期待値 actual:実際の値 form:計算式 caller:呼び出し元)
  resultがnilのとき、失敗のメッセージを出力。"
  (when (not result)
        (format t "~a 期待値：~a~\t実績値：~a~\t評価式：~a~%" (if caller (concatenate 'string "【" caller "】") "") expected actual form))
  result)
(defmacro my-assert (expected fn)
  "my-assert (expected:期待値 fn：計算式)
  expectedとfnの結果が同一でないとき、エラー文面(assert-report)を返却。
  return: reuslt or nil"
  (let ((result (gensym)))
    `(let ((,result ,fn))
       (assert-report (= ,expected ,result)
                      ,expected
                      ,result
                      ',fn))))
